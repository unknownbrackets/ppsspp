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

}
