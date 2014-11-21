// Copyright (c) 2012- PPSSPP Project.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 2.0 or later versions.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License 2.0 for more details.

// A copy of the GPL 2.0 should have been included with the program.
// If not, see http://www.gnu.org/licenses/

// Official git repository and contact information can be found at
// https://github.com/hrydgard/ppsspp and http://www.ppsspp.org/.

#include "base/logging.h"
#include "profiler/profiler.h"
#include "Common/ChunkFile.h"
#include "Core/Reporting.h"
#include "Core/Config.h"
#include "Core/Core.h"
#include "Core/CoreTiming.h"
#include "Core/Debugger/SymbolMap.h"
#include "Core/MemMap.h"
#include "Core/MIPS/MIPS.h"
#include "Core/MIPS/MIPSCodeUtils.h"
#include "Core/MIPS/MIPSInt.h"
#include "Core/MIPS/MIPSTables.h"
#include "Core/HLE/ReplaceTables.h"

#include "MipsJit.h"
#include "CPUDetect.h"

void DisassembleMIPS(const u8 *data, int size) {
}

namespace MIPSComp
{

MipsJit::MipsJit(MIPSState *mips) : blocks(mips, this), mips_(mips) {
	logBlocks = 0;
	dontLogBlocks = 0;
	blocks.Init();
	AllocCodeSpace(1024 * 1024 * 16);
	GenerateFixedCode();
	js.startDefaultPrefix = mips_->HasDefaultPrefix();
}

void MipsJit::DoState(PointerWrap &p) {
	auto s = p.Section("Jit", 1, 2);
	if (!s)
		return;

	p.Do(js.startDefaultPrefix);
	if (s >= 2) {
		p.Do(js.hasSetRounding);
		js.lastSetRounding = 0;
	} else {
		js.hasSetRounding = 1;
	}
}

// This is here so the savestate matches between jit and non-jit.
void MipsJit::DoDummyState(PointerWrap &p) {
	auto s = p.Section("Jit", 1, 2);
	if (!s)
		return;

	bool dummy = false;
	p.Do(dummy);
	if (s >= 2) {
		dummy = true;
		p.Do(dummy);
	}
}

void MipsJit::FlushAll() {
	//gpr.FlushAll();
	//fpr.FlushAll();
	FlushPrefixV();
}

void MipsJit::FlushPrefixV() {
	if ((js.prefixSFlag & JitState::PREFIX_DIRTY) != 0) {
		MOVI2R(V0, js.prefixS);
		SW(V0, CTXREG, offsetof(MIPSState, vfpuCtrl[VFPU_CTRL_SPREFIX]));
		js.prefixSFlag = (JitState::PrefixState) (js.prefixSFlag & ~JitState::PREFIX_DIRTY);
	}

	if ((js.prefixTFlag & JitState::PREFIX_DIRTY) != 0) {
		MOVI2R(V0, js.prefixT);
		SW(V0, CTXREG, offsetof(MIPSState, vfpuCtrl[VFPU_CTRL_TPREFIX]));
		js.prefixTFlag = (JitState::PrefixState) (js.prefixTFlag & ~JitState::PREFIX_DIRTY);
	}

	if ((js.prefixDFlag & JitState::PREFIX_DIRTY) != 0) {
		MOVI2R(V0, js.prefixD);
		SW(V0, CTXREG, offsetof(MIPSState, vfpuCtrl[VFPU_CTRL_DPREFIX]));
		js.prefixDFlag = (JitState::PrefixState) (js.prefixDFlag & ~JitState::PREFIX_DIRTY);
	}
}

void MipsJit::ClearCache() {
	blocks.Clear();
	ClearCodeSpace();
	GenerateFixedCode();
}

void MipsJit::InvalidateCache() {
	blocks.Clear();
}

void MipsJit::InvalidateCacheAt(u32 em_address, int length) {
	blocks.InvalidateICache(em_address, length);
}

void MipsJit::EatInstruction(MIPSOpcode op) {
	MIPSInfo info = MIPSGetInfo(op);
	if (info & DELAYSLOT) {
		ERROR_LOG_REPORT_ONCE(ateDelaySlot, JIT, "Ate a branch op.");
	}
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT_ONCE(ateInDelaySlot, JIT, "Ate an instruction inside a delay slot.");
	}

	js.numInstructions++;
	js.compilerPC += 4;
	js.downcountAmount += MIPSGetInstructionCycleEstimate(op);
}

void MipsJit::CompileDelaySlot(int flags) {
	js.inDelaySlot = true;
	MIPSOpcode op = Memory::Read_Opcode_JIT(js.compilerPC + 4);
	MIPSCompileOp(op, this);
	js.inDelaySlot = false;

	if (flags & DELAYSLOT_FLUSH)
		FlushAll();
}


void MipsJit::Compile(u32 em_address) {
	PROFILE_THIS_SCOPE("jitc");
	if (GetSpaceLeft() < 0x10000 || blocks.IsFull()) {
		ClearCache();
	}
	int block_num = blocks.AllocateBlock(em_address);
	JitBlock *b = blocks.GetBlock(block_num);
	DoJit(em_address, b);
	blocks.FinalizeBlock(block_num, jo.enableBlocklink);
}

void MipsJit::RunLoopUntil(u64 globalticks) {
	PROFILE_THIS_SCOPE("jit");
	((void (*)())enterCode)();
}

const u8 *MipsJit::DoJit(u32 em_address, JitBlock *b) {
	js.cancel = false;
	js.blockStart = js.compilerPC = mips_->pc;
	js.lastContinuedPC = 0;
	js.initialBlockSize = 0;
	js.nextExit = 0;
	js.downcountAmount = 0;
	js.curBlock = b;
	js.compiling = true;
	js.inDelaySlot = false;
	js.PrefixStart();
	b->normalEntry = GetCodePtr();
	// Check downcount
	FixupBranch noskip = BLTZ(DOWNCOUNTREG);
	MOVI2R(R_AT, js.blockStart);
	J((const void *)outerLoopPCInR0);
	SetJumpTarget(noskip);

	js.numInstructions = 0;
	while (js.compiling)
	{
		MIPSOpcode inst = Memory::Read_Opcode_JIT(js.compilerPC);
		js.downcountAmount += MIPSGetInstructionCycleEstimate(inst);

		MIPSCompileOp(inst, this);

		js.compilerPC += 4;
		js.numInstructions++;

		// Safety check, in case we get a bunch of really large jit ops without a lot of branching.
		if (GetSpaceLeft() < 0x800 || js.numInstructions >= JitBlockCache::MAX_BLOCK_INSTRUCTIONS)
		{
			FlushAll();
			WriteExit(js.compilerPC, js.nextExit++);
			js.compiling = false;
		}
	}

	b->codeSize = GetCodePtr() - b->normalEntry;

	// Don't forget to zap the newly written instructions in the instruction cache!
	FlushIcache();

	if (js.lastContinuedPC == 0)
		b->originalSize = js.numInstructions;
	else
	{
		// We continued at least once.  Add the last proxy and set the originalSize correctly.
		blocks.ProxyBlock(js.blockStart, js.lastContinuedPC, (js.compilerPC - js.lastContinuedPC) / sizeof(u32), GetCodePtr());
		b->originalSize = js.initialBlockSize;
	}

	return b->normalEntry;
}

void MipsJit::AddContinuedBlock(u32 dest) {
	// The first block is the root block.  When we continue, we create proxy blocks after that.
	if (js.lastContinuedPC == 0)
		js.initialBlockSize = js.numInstructions;
	else
		blocks.ProxyBlock(js.blockStart, js.lastContinuedPC, (js.compilerPC - js.lastContinuedPC) / sizeof(u32), GetCodePtr());
	js.lastContinuedPC = dest;
}

bool MipsJit::DescribeCodePtr(const u8 *ptr, std::string &name) {
	// TODO: Not used by anything yet.
	return false;
}

void MipsJit::Comp_RunBlock(MIPSOpcode op) {
	// This shouldn't be necessary, the dispatcher should catch us before we get here.
	ERROR_LOG(JIT, "Comp_RunBlock should never be reached!");
}

void MipsJit::LinkBlock(u8 *exitPoint, const u8 *checkedEntry) {
	MIPSEmitter emit(exitPoint);
	emit.J(checkedEntry);
	emit.FlushIcache();
}

void MipsJit::UnlinkBlock(u8 *checkedEntry, u32 originalAddress) {
	// Send anyone who tries to run this block back to the dispatcher.
	// Not entirely ideal, but .. pretty good.
	// Spurious entrances from previously linked blocks can only come through checkedEntry
	MIPSEmitter emit(checkedEntry);
	emit.MOVI2R(R_AT, originalAddress);
	emit.SW(R_AT, CTXREG, offsetof(MIPSState, pc));
	emit.J(dispatcher);
	emit.FlushIcache();
}

bool MipsJit::ReplaceJalTo(u32 dest) {
	const ReplacementTableEntry *entry = nullptr;
	u32 funcSize = 0;
	if (!CanReplaceJalTo(dest, &entry, &funcSize)) {
		return false;
	}
	return false;
}

void MipsJit::Comp_ReplacementFunc(MIPSOpcode op) {
}

void MipsJit::Comp_Generic(MIPSOpcode op) {
	FlushAll();
	MIPSInterpretFunc func = MIPSGetInterpretFunc(op);
	if (func)
	{
		SaveDowncount();
		RestoreRoundingMode();
		MOVI2R(V0, js.compilerPC); // TODO: Use gpr
		MovToPC(V0);
		QuickCallFunction(V0, (void *)func);
		ApplyRoundingMode();
		RestoreDowncount();
	}

	const MIPSInfo info = MIPSGetInfo(op);
	if ((info & IS_VFPU) != 0 && (info & VFPU_NO_PREFIX) == 0)
	{
		// If it does eat them, it'll happen in MIPSCompileOp().
		if ((info & OUT_EAT_PREFIX) == 0)
			js.PrefixUnknown();
	}
}

void MipsJit::MovFromPC(MIPSReg r) {
	LW(r, CTXREG, offsetof(MIPSState, pc));
}

void MipsJit::MovToPC(MIPSReg r) {
	SW(r, CTXREG, offsetof(MIPSState, pc));
}

void MipsJit::SaveDowncount() {
	SW(DOWNCOUNTREG, CTXREG, offsetof(MIPSState, downcount));
}

void MipsJit::RestoreDowncount() {
	LW(DOWNCOUNTREG, CTXREG, offsetof(MIPSState, downcount));
}

void MipsJit::WriteDownCount(int offset) {
	int theDowncount = js.downcountAmount + offset;
	MOVI2R(V1, theDowncount);
	SUBU(DOWNCOUNTREG, DOWNCOUNTREG, V1);
}

void MipsJit::WriteDownCountR(MIPSReg reg) {
	SUBU(DOWNCOUNTREG, DOWNCOUNTREG, reg);
}

void MipsJit::RestoreRoundingMode(bool force) {
}

void MipsJit::ApplyRoundingMode(bool force) {
}

void MipsJit::UpdateRoundingMode() {
}

void MipsJit::WriteExit(u32 destination, int exit_num) {
	WriteDownCount();
	JitBlock *b = js.curBlock;
	b->exitAddress[exit_num] = destination;
	b->exitPtrs[exit_num] = GetWritableCodePtr();

	// Link opportunity!
	int block = blocks.GetBlockNumberFromStartAddress(destination);
	if (block >= 0 && jo.enableBlocklink) {
		// It exists! Joy of joy!
		J(blocks.GetBlock(block)->checkedEntry);
		b->linkStatus[exit_num] = true;
	} else {
		MOVI2R(R_AT, destination);
		J((const void *)dispatcherPCInR0);
	}
}

void MipsJit::WriteExitDestInR(MIPSReg Reg) {
	MovToPC(Reg);
	WriteDownCount();
	// TODO: shouldn't need an indirect branch here...
	J((const void *)dispatcher);
}

void MipsJit::WriteSyscallExit() {
	WriteDownCount();
	J((const void *)dispatcherCheckCoreState);
}

#define _RS ((op>>21) & 0x1F)
#define _RT ((op>>16) & 0x1F)
#define _RD ((op>>11) & 0x1F)
#define _FS ((op>>11) & 0x1F)
#define _FT ((op>>16) & 0x1F)
#define _FD ((op>>6) & 0x1F)
#define _POS ((op>>6) & 0x1F)
#define _SIZE ((op>>11) & 0x1F)

//memory regions:
//
// 08-0A
// 48-4A
// 04-05
// 44-45
// mov eax, addrreg
	// shr eax, 28
// mov eax, [table+eax]
// mov dreg, [eax+offreg]
	
}
