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

// PLAN: no more block numbers - crazy opcodes just contain offset within
// dynarec buffer
// At this offset - 4, there is an int specifying the block number.

namespace MIPSComp {

void Jit::GenerateFixedCode()
{
	enterCode = AlignCode16();

	DEBUG_LOG(JIT, "Base: %08x", (u32)Memory::base);

	MOVI2R(BASEREG, (u32)Memory::base);
	MOVI2R(CTXREG, (u32)mips_);
	MOVI2R(CODEREG, (u32)GetBasePtr());

	RestoreDowncount();
	MovFromPC(R_AT);
	outerLoopPCInR0 = GetCodePtr();
	MovToPC(R_AT);
	outerLoop = GetCodePtr();
		SaveDowncount();
		RestoreRoundingMode(true);
		QuickCallFunction(R_AT, &CoreTiming::Advance);
		NOP(); // Delay
		ApplyRoundingMode(true);
		RestoreDowncount();
		FixupBranch skipToRealDispatch = B(); //skip the sync and compare first time
		NOP(); // Delay

		dispatcherCheckCoreState = GetCodePtr();

		// The result of slice decrementation should be in flags if somebody jumped here
		// IMPORTANT - We jump on negative, not carry!!!
		FixupBranch bailCoreState = BLTZ(DOWNCOUNTREG);
		NOP(); // Delay

		MOVI2R(R_AT, (u32)&coreState);
		LW(R_AT, R_AT, 0);
		FixupBranch badCoreState = BNE(R_AT, R_ZERO);
		NOP(); // Delay
		FixupBranch skipToRealDispatch2 = B(); //skip the sync and compare first time
		NOP(); // Delay

		dispatcherPCInR0 = GetCodePtr();
		// TODO: Do we always need to write PC to RAM here?
		MovToPC(R_AT);

		// At this point : flags = EQ. Fine for the next check, no need to jump over it.
		dispatcher = GetCodePtr();

			// The result of slice decrementation should be in flags if somebody jumped here
			// IMPORTANT - We jump on negative, not carry!!!
			FixupBranch bail = BLTZ(DOWNCOUNTREG);
			NOP(); // Delay

			SetJumpTarget(skipToRealDispatch);
			SetJumpTarget(skipToRealDispatch2);

			dispatcherNoCheck = GetCodePtr();

			// No block found, let's jit
			SaveDowncount();
			LW(R_AT, CTXREG, offsetof(MIPSState, pc));
			LW(R_AT, BASEREG, R_AT);
			MOVI2R(V0, MIPS_JITBLOCK_MASK);
			ANDI(V0, R_AT, V0);
			MOVI2R(R_AT, MIPS_EMUHACK_OPCODE);
			FixupBranch notfound = BNE(V0, R_AT);
			NOP(); // Delay
			
			RestoreRoundingMode(true);
			QuickCallFunction(V1, (void *)&JitAt);
			ApplyRoundingMode(true);
			RestoreDowncount();

			B(dispatcherNoCheck); // no point in special casing this
			NOP(); // Delay

		SetJumpTarget(bail);
		SetJumpTarget(bailCoreState);

		MOVI2R(R_AT, (u32)&coreState);
		LW(R_AT, R_AT, 0);
		BEQ(R_AT, R_ZERO, outerLoop);
		NOP(); // Delay

	SetJumpTarget(badCoreState);
	breakpointBailout = GetCodePtr();

	SaveDowncount();
	RestoreRoundingMode(true);
	JRRA();
	NOP(); // Delay

	// Don't forget to zap the instruction cache!
	FlushIcache();
}

}  // namespace MIPSComp
