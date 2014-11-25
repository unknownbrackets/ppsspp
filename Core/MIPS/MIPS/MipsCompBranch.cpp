// Copyright (c) 2014- PPSSPP Project.

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

#include "Core/Reporting.h"
#include "Core/Config.h"
#include "Core/MemMap.h"
#include "Core/HLE/HLE.h"
#include "Core/HLE/HLETables.h"

#include "Core/MIPS/MIPS.h"
#include "Core/MIPS/MIPSCodeUtils.h"
#include "Core/MIPS/MIPSAnalyst.h"
#include "Core/MIPS/MIPSTables.h"

#include "Core/MIPS/MIPS/MipsJit.h"
#include "Core/MIPS/JitCommon/JitBlockCache.h"

#include "Common/MipsEmitter.h"

#define _RS MIPS_GET_RS(op)
#define _RT MIPS_GET_RT(op)
#define _RD MIPS_GET_RD(op)
#define _FS MIPS_GET_FS(op)
#define _FT MIPS_GET_FT(op)
#define _FD MIPS_GET_FD(op)
#define _SA MIPS_GET_SA(op)
#define _POS  ((op>> 6) & 0x1F)
#define _SIZE ((op>>11) & 0x1F)
#define _IMM16 (signed short)(op & 0xFFFF)
#define _IMM26 (op & 0x03FFFFFF)

using namespace MIPSAnalyst;

namespace MIPSComp
{

MIPSGen::FixupBranch Jit::BranchTypeComp(int type, MIPSReg op1, MIPSReg op2) {
	switch (type) {
		case 0: return BGEZ(op1);
		case 1: return BLTZ(op1);
		case 2: return BGEZ(op1);
		case 3: return BLTZ(op1);
		case 4: return BNE(op1, op2);
		case 5: return BEQ(op1, op2);
		case 6: return BGTZ(op1);
		case 7: return BLEZ(op1);
		case 16: return BGEZ(op1);
		case 17: return BLTZ(op1);
		case 18: return BGEZ(op1);
		case 19: return BLTZ(op1);
		case 20: return BNE(op1, op2);
		case 21: return BEQ(op1, op2);
		case 22: return BGTZ(op1);
		case 23: return BLEZ(op1);
		default:
			_dbg_assert_msg_(CPU,0,"Trying to compile instruction that can't be compiled");
			return MIPSGen::FixupBranch();
	}
}

void Jit::Comp_RelBranch(MIPSOpcode op) {
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT(JIT, "Branch in Comp_RelBranch delay slot at %08x in block starting at %08x", js.compilerPC, js.blockStart);
		return;
	}
	int offset = _IMM16 << 2;
	MIPSGPReg rt = _RT;
	MIPSGPReg rs = _RS;
	u32 targetAddr = js.compilerPC + offset + 4;

	MIPSOpcode delaySlotOp = Memory::Read_Instruction(js.compilerPC+4);
	bool delaySlotIsNice = IsDelaySlotNiceReg(op, delaySlotOp, rt, rs);

	int type = (op>>26);
	bool likely = (type == 20 || type == 21);
	if (!likely && delaySlotIsNice)
		CompileDelaySlot(DELAYSLOT_NICE);

	LW(V0, CTXREG, 4 * _RS);
	LW(V1, CTXREG, 4 * _RT);

	MIPSGen::FixupBranch ptr;
	if (!likely)
	{
		if (!delaySlotIsNice)
			CompileDelaySlot(DELAYSLOT_SAFE_FLUSH);
		else
			FlushAll();
		ptr = BranchTypeComp(op>>26, V0, V1);
	}
	else
	{
		FlushAll();
		ptr = BranchTypeComp(op>>26, V0, V1);
		CompileDelaySlot(DELAYSLOT_FLUSH);
	}

	// Take the branch
	bool andLink = (type == 22 || type == 23);
	if (andLink)
	{
		MOVI2R(V0, js.compilerPC + 8);
		SW(V0, CTXREG, MIPS_REG_RA * 4);
	}

	// Take the branch
	WriteExit(targetAddr, 0);

	SetJumpTarget(ptr);

	// Not taken
	WriteExit(js.compilerPC+8, 1);

	js.compiling = false;
}

void Jit::Comp_RelBranchRI(MIPSOpcode op) {
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT(JIT, "Branch in Comp_RelBranchRI delay slot at %08x in block starting at %08x", js.compilerPC, js.blockStart);
		return;
	}
	int offset = _IMM16 << 2;
	MIPSGPReg rt = _RT;
	MIPSGPReg rs = _RS;
	u32 targetAddr = js.compilerPC + offset + 4;

	MIPSOpcode delaySlotOp = Memory::Read_Instruction(js.compilerPC+4);
	bool delaySlotIsNice = IsDelaySlotNiceReg(op, delaySlotOp, rt, rs);

	int type = (op >> 16) & 0x1F;
	bool likely = (type == 16 || type == 17);
	if (!likely && delaySlotIsNice)
		CompileDelaySlot(DELAYSLOT_NICE);

	LW(V0, CTXREG, 4 * _RS);

	MIPSGen::FixupBranch ptr;
	if (!likely)
	{
		if (!delaySlotIsNice)
			CompileDelaySlot(DELAYSLOT_SAFE_FLUSH);
		else
			FlushAll();
		ptr = BranchTypeComp(type, V0, R_ZERO);
	}
	else
	{
		FlushAll();
		ptr = BranchTypeComp(type, V0, R_ZERO);
		CompileDelaySlot(DELAYSLOT_FLUSH);
	}

	// Take the branch
	bool andLink = (type == 18 || type == 19);
	if (andLink)
	{
		MOVI2R(V0, js.compilerPC + 8);
		SW(V0, CTXREG, MIPS_REG_RA * 4);
	}

	// Take the branch
	WriteExit(targetAddr, 0);

	SetJumpTarget(ptr);

	// Not taken
	WriteExit(js.compilerPC+8, 1);

	js.compiling = false;
}

void Jit::Comp_FPUBranch(MIPSOpcode op) {
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT(JIT, "Branch in FPFlag delay slot at %08x in block starting at %08x", js.compilerPC, js.blockStart);
		return;
	}
	int offset = _IMM16 << 2;
	u32 targetAddr = js.compilerPC + offset + 4;

	MIPSOpcode delaySlotOp = Memory::Read_Instruction(js.compilerPC + 4);
	bool delaySlotIsNice = IsDelaySlotNiceFPU(op, delaySlotOp);

	int type = (op >> 16) & 0x1f;

	bool equalTest = (type == 1) || (type == 3);
	// If likely is set, discard the branch slot if NOT taken.
	bool likely = (type == 2) || (type == 3);
	if (!likely && delaySlotIsNice)
		CompileDelaySlot(DELAYSLOT_NICE);

	LW(V0, CTXREG, 4 * MIPS_REG_FPCOND);

	MIPSGen::FixupBranch ptr;
	if (!likely)
	{
		if (!delaySlotIsNice)
			CompileDelaySlot(DELAYSLOT_SAFE_FLUSH);
		else
			FlushAll();
		if (equalTest)
			ptr = BNEZ(V0); // v0 == 1 or v0 != 0
		else
			ptr = BEQZ(V0); // v0 != 1 or v0 == 0
	}
	else
	{
		FlushAll();
		if (equalTest)
			ptr = BNEZ(V0);
		else
			ptr = BEQZ(V0);
		CompileDelaySlot(DELAYSLOT_FLUSH);
	}

	// Take the branch
	WriteExit(targetAddr, js.nextExit++);

	SetJumpTarget(ptr);
	// Not taken
	WriteExit(js.compilerPC + 8, js.nextExit++);
	js.compiling = false;
}

void Jit::Comp_VBranch(MIPSOpcode op) {
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT(JIT, "Branch in VFPU delay slot at %08x in block starting at %08x", js.compilerPC, js.blockStart);
		return;
	}
	int offset = _IMM16 << 2;
	u32 targetAddr = js.compilerPC + offset + 4;

	MIPSOpcode delaySlotOp = Memory::Read_Instruction(js.compilerPC + 4);

	// Sometimes there's a VFPU branch in a delay slot (Disgaea 2: Dark Hero Days, Zettai Hero Project, La Pucelle)
	// The behavior is undefined - the CPU may take the second branch even if the first one passes.
	// However, it does consistently try each branch, which these games seem to expect.
	bool delaySlotIsBranch = MIPSCodeUtils::IsVFPUBranch(delaySlotOp);
	bool delaySlotIsNice = !delaySlotIsBranch && IsDelaySlotNiceVFPU(op, delaySlotOp);

	int type = (op >> 16) & 3;
	bool equalTest = (type == 1) || (type == 3);
	bool likely = (type == 2) || (type == 3);
	if (!likely && delaySlotIsNice)
		CompileDelaySlot(DELAYSLOT_NICE);
	if (delaySlotIsBranch && (signed short)(delaySlotOp & 0xFFFF) != (signed short)(op & 0xFFFF) - 1)
		ERROR_LOG_REPORT(JIT, "VFPU branch in VFPU delay slot at %08x with different target", js.compilerPC);

	int imm3 = (op >> 18) & 7;

	LW(V0, CTXREG, 4 * MIPS_REG_VFPUCC);
	MOVI2R(V1, 1 << imm3);

	MIPSGen::FixupBranch ptr;
	js.inDelaySlot = true;
	if (!likely)
	{
		if (!delaySlotIsNice && !delaySlotIsBranch)
			CompileDelaySlot(DELAYSLOT_SAFE_FLUSH);
		else
			FlushAll();
		if (equalTest)
			ptr = BEQ(V0, V1);
		else
			ptr = BNE(V0, V1);
	}
	else
	{
		FlushAll();
		if (equalTest)
			ptr = BEQ(V0, V1);
		else
			ptr = BNE(V0, V1);
		if (!delaySlotIsBranch)
			CompileDelaySlot(DELAYSLOT_FLUSH);
	}
	js.inDelaySlot = false;

	// Take the branch
	WriteExit(targetAddr, js.nextExit++);

	SetJumpTarget(ptr);
	// Not taken
	u32 notTakenTarget = js.compilerPC + (delaySlotIsBranch ? 4 : 8);
	WriteExit(notTakenTarget, js.nextExit++);
	js.compiling = false;
}

void Jit::Comp_Jump(MIPSOpcode op) {
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT(JIT, "Branch in Jump delay slot at %08x in block starting at %08x", js.compilerPC, js.blockStart);
		return;
	}
	u32 off = _IMM26 << 2;
	u32 targetAddr = (js.compilerPC & 0xF0000000) | off;

	// Might be a stubbed address or something?
	if (!Memory::IsValidAddress(targetAddr)) {
		if (js.nextExit == 0) {
			ERROR_LOG_REPORT(JIT, "Jump to invalid address: %08x", targetAddr);
		} else {
			js.compiling = false;
		}
		// TODO: Mark this block dirty or something?  May be indication it will be changed by imports.
		return;
	}

	switch (op >> 26) {
	case 2: //j
		CompileDelaySlot(DELAYSLOT_NICE);
		if (jo.continueJumps && js.numInstructions < jo.continueMaxInstructions) {
			AddContinuedBlock(targetAddr);
			// Account for the increment in the loop.
			js.compilerPC = targetAddr - 4;
			// In case the delay slot was a break or something.
			js.compiling = true;
			return;
		}
		FlushAll();
		WriteExit(targetAddr, js.nextExit++);
		break;

	case 3: //jal
		if (ReplaceJalTo(targetAddr))
			return;

		MOVI2R(V0, js.compilerPC + 8);
		SW(V0, CTXREG, MIPS_REG_RA * 4);
		CompileDelaySlot(DELAYSLOT_NICE);
		if (jo.continueJumps && js.numInstructions < jo.continueMaxInstructions) {
			AddContinuedBlock(targetAddr);
			// Account for the increment in the loop.
			js.compilerPC = targetAddr - 4;
			// In case the delay slot was a break or something.
			js.compiling = true;
			return;
		}
		FlushAll();
		WriteExit(targetAddr, js.nextExit++);
		break;

	default:
		_dbg_assert_msg_(CPU,0,"Trying to compile instruction that can't be compiled");
		break;
	}
	js.compiling = false;
}

void Jit::Comp_JumpReg(MIPSOpcode op) {
	if (js.inDelaySlot) {
		ERROR_LOG_REPORT(JIT, "Branch in JumpReg delay slot at %08x in block starting at %08x", js.compilerPC, js.blockStart);
		return;
	}
	MIPSGPReg rs = _RS;
	MIPSGPReg rd = _RD;
	bool andLink = (op & 0x3f) == 9;

	MIPSOpcode delaySlotOp = Memory::Read_Instruction(js.compilerPC + 4);
	bool delaySlotIsNice = IsDelaySlotNiceReg(op, delaySlotOp, rs);
	if (andLink && rs == rd)
		delaySlotIsNice = false;

	MIPSReg destReg = V1;
	if (IsSyscall(delaySlotOp)) {
		LW(V0, CTXREG, 4 * _RS);
		MovToPC(V0);  // For syscall to be able to return.
		if (andLink) {
			MOVI2R(V0, js.compilerPC + 8);
			SW(V0, CTXREG, _RD * 4);
		}
		CompileDelaySlot(DELAYSLOT_FLUSH);
		return;  // Syscall wrote exit code.
	} else if (delaySlotIsNice) {
		if (andLink) {
			MOVI2R(V0, js.compilerPC + 8);
			SW(V0, CTXREG, _RD * 4);
		}
		CompileDelaySlot(DELAYSLOT_NICE);

		LW(V1, CTXREG, 4 * _RS);
		FlushAll();
	} else {
		LW(V1, CTXREG, 4 * _RS);
		if (andLink) {
			MOVI2R(V0, js.compilerPC + 8);
			SW(V0, CTXREG, _RD * 4);
		}
		CompileDelaySlot(DELAYSLOT_NICE);
		FlushAll();
	}

	switch (op & 0x3f)
	{
	case 8: //jr
		break;
	case 9: //jalr
		break;
	default:
		_dbg_assert_msg_(CPU,0,"Trying to compile instruction that can't be compiled");
		break;
	}

	WriteExitDestInR(destReg);
	js.compiling = false;
}

void Jit::Comp_Syscall(MIPSOpcode op) {
	// If we're in a delay slot, this is off by one.
	const int offset = js.inDelaySlot ? -1 : 0;
	WriteDownCount(offset);
	RestoreRoundingMode();
	js.downcountAmount = -offset;

	// TODO: Maybe discard v0, v1, and some temps?  Definitely at?
	FlushAll();

	SaveDowncount();
	// Skip the CallSyscall where possible.
	void *quickFunc = GetQuickSyscallFunc(op);
	if (quickFunc)
	{
		MOVI2R(A0, (u32)(intptr_t)GetSyscallInfo(op));
		QuickCallFunction(V0, quickFunc);
	}
	else
	{
		MOVI2R(A0, op.encoding);
		QuickCallFunction(V0, (void *)&CallSyscall);
	}
	ApplyRoundingMode();
	RestoreDowncount();

	WriteSyscallExit();
	js.compiling = false;
}

void Jit::Comp_Break(MIPSOpcode op)
{
	Comp_Generic(op);
	WriteSyscallExit();
	js.compiling = false;
}

}
