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


#include "Core/MemMap.h"
#include "Core/MIPS/MIPS.h"
#include "Core/System.h"
#include "Core/CoreTiming.h"
#include "Common/MemoryUtil.h"
#include "Common/CPUDetect.h"
#include "Common/MipsEmitter.h"
#include "Core/MIPS/JitCommon/JitCommon.h"
#include "Core/MIPS/MIPS/MipsJit.h"
#include "Core/MIPS/MIPS/MipsAsm.h"

using namespace MIPSGen;

static const bool enableDebug = false;

// Mappable registers: S0-S7

// STATIC ALLOCATION MIPS:
// S4: Downcounter
extern volatile CoreState coreState;

void JitAt()
{
	MIPSComp::jit->Compile(currentMIPS->pc);
}


void ShowPC(u32 sp) {
	if (currentMIPS) {
		ERROR_LOG(JIT, "ShowPC : %08x  ArmSP : %08x", currentMIPS->pc, sp);
	} else {
		ERROR_LOG(JIT, "Universe corrupt?");
	}
}

namespace MIPSComp {

void Jit::GenerateFixedCode()
{
	enterCode = AlignCode16();

	DEBUG_LOG(JIT, "Base: %08x", (u32)Memory::base);

	// Fixed size (with pad) align to 64-bit
	ADDIU(R_SP, R_SP, -56);
	// Reserve first four for arg0-4
	for (int i = 0; i < 8; ++i) {
		SW(MIPSReg(S0 + i), R_SP, (4 + i) * sizeof(u32));
	}
	SW(S8, R_SP, (4 + 8) * sizeof(u32));
	SW(R_RA, R_SP, (4 + 9) * sizeof(u32));

	MOVI2R(BASEREG, (u32)Memory::base);
	MOVI2R(CTXREG, (u32)mips_);
	MOVI2R(CODEREG, (u32)GetBasePtr());

	RestoreDowncount();
	MovFromPC(R_AT);
	outerLoopPCInAT = GetCodePtr();
	MovToPC(R_AT);
	outerLoop = GetCodePtr();
		SaveDowncount();
		RestoreRoundingMode(true);
		QuickCallFunction(R_AT, &CoreTiming::Advance);
		ApplyRoundingMode(true);
		RestoreDowncount();
		FixupBranch skipToRealDispatch = B(); //skip the sync and compare first time

		dispatcherCheckCoreState = GetCodePtr();

		FixupBranch bailCoreState = BLTZ(DOWNCOUNTREG);

		MOVI2R(R_AT, (u32)&coreState);
		LW(R_AT, R_AT, 0);
		FixupBranch badCoreState = BNEZ(R_AT);
		FixupBranch skipToRealDispatch2 = B(); //skip the sync and compare first time

		dispatcherPCInAT = GetCodePtr();
		// TODO: Do we always need to write PC to RAM here?
		MovToPC(R_AT);

		dispatcher = GetCodePtr();

			FixupBranch bail = BLTZ(DOWNCOUNTREG);

			SetJumpTarget(skipToRealDispatch);
			SetJumpTarget(skipToRealDispatch2);

			dispatcherNoCheck = GetCodePtr();

			LW(R_AT, CTXREG, offsetof(MIPSState, pc));
			ADDU(R_AT, BASEREG, R_AT);
			LW(R_AT, R_AT, 0);
			SRL(T0, R_AT, 24);
			// Subtract
			ADDIU(T0, T0, -(MIPS_EMUHACK_OPCODE >> 24));
			FixupBranch notfound = BNEZ(T0);
				EXT(R_AT, R_AT, 0, 24);
				ADDU(R_AT, R_AT, CODEREG);
				JR(R_AT);
			SetJumpTarget(notfound);
			
			// No block found, let's jit
			SaveDowncount();
			RestoreRoundingMode(true);
			QuickCallFunction(T1, (void *)&JitAt);
			ApplyRoundingMode(true);
			RestoreDowncount();

			B(dispatcherNoCheck); // no point in special casing this

		SetJumpTarget(bail);
		SetJumpTarget(bailCoreState);

		MOVI2R(R_AT, (u32)&coreState);
		LW(R_AT, R_AT, 0);
		BEQZ(R_AT, outerLoop);

	SetJumpTarget(badCoreState);
	breakpointBailout = GetCodePtr();

	SaveDowncount();
	RestoreRoundingMode(true);

	for (int i = 0; i < 8; ++i) {
		LW(MIPSReg(S0 + i), R_SP, (4 + i) * sizeof(u32));
	}
	LW(S8, R_SP, (4 + 8) * sizeof(u32));
	LW(R_RA, R_SP, (4 + 9) * sizeof(u32));
	ADDIU(R_SP, R_SP, 56);

	JRRA();

	// Don't forget to zap the instruction cache!
	FlushIcache();
}

}  // namespace MIPSComp
