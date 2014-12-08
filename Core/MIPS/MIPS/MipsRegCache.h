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

#pragma once

#include "../MIPS.h"
#include "../MIPSAnalyst.h"
#include "MipsEmitter.h"
#include "MipsJit.h"

using namespace MIPSGen;

#define BASEREG (S7)
#define CTXREG (S6)
#define CODEREG (S5)
#define DOWNCOUNTREG (S4)
#define SCRATCHREG1 (T0)
#define SCRATCHREG2 (T1)

// S0-S3, S8: mapped MIPS regs (saved)
// S4 = down count reg
// S5 = code reg
// S6 = MIPS context (allegrex)
// S7 = base pointer

enum {
	TOTAL_MAPPABLE_MIPSREGS = 36,
};

// MIPSN = MIPS Native, as opposed to PSP's MIPS Allegrex
struct RegMIPSN {
	MIPSGPReg mipsReg;  // if -1, no mipsreg attached.
	bool isDirty;  // Should the register be written back?
};

enum RegMIPSLoc {
	ML_IMM,
	ML_MIPSNREG,
	// In a native MIPS reg, but as a pre-adjusted pointer, not the actual reg.
	ML_MIPSNREG_AS_PTR,
	// In a native MIPS reg, but also has a known immediate value.
	ML_MIPSNREG_IMM,
	ML_MEM,
};

struct RegMIPS {
	// Where is this MIPS register?
	RegMIPSLoc loc;
	// Data (only one of these is used, depending on loc. Could make a union).
	u32 imm;
	MIPSReg reg;  // reg index
	bool spillLock;  // if true, this register cannot be spilled.
	// If loc == ML_MEM, it's back in its location in the CPU context struct.
};

#undef MAP_DIRTY
#undef MAP_NOINIT
// Initing is the default so the flag is reversed.
enum {
	MAP_DIRTY = 1,
	MAP_NOINIT = 2 | MAP_DIRTY,
};

namespace MIPSComp {
	struct MIPSJitOptions;
}

class MIPSRegCache {
public:
	MIPSRegCache(MIPSState *mips, MIPSComp::MIPSJitOptions *options);
	~MIPSRegCache() {}

	void Init(MIPSEmitter *emitter);
	void Start(MIPSAnalyst::AnalysisResults &stats);

	// Protect the native MIPS register containing a PSP MIPS register from spilling, to ensure that
	// it's being kept allocated.
	void SpillLock(MIPSGPReg reg, MIPSGPReg reg2 = MIPS_REG_INVALID, MIPSGPReg reg3 = MIPS_REG_INVALID, MIPSGPReg reg4 = MIPS_REG_INVALID);
	void ReleaseSpillLock(MIPSGPReg reg);
	void ReleaseSpillLocks();

	void SetImm(MIPSGPReg reg, u32 immVal);
	bool IsImm(MIPSGPReg reg) const;
	u32 GetImm(MIPSGPReg reg) const;
	// Optimally set a register to an imm value (possibly using another register.)
	void SetRegImm(MIPSReg reg, u32 imm);

	// Returns a native MIPS register containing the requested PSP MIPS register.
	MIPSReg MapReg(MIPSGPReg reg, int mapFlags = 0);
	MIPSReg MapRegAsPointer(MIPSGPReg reg);  // read-only, non-dirty.

	bool IsMappedAsPointer(MIPSGPReg reg);

	void MapInIn(MIPSGPReg rd, MIPSGPReg rs);
	void MapDirtyIn(MIPSGPReg rd, MIPSGPReg rs, bool avoidLoad = true);
	void MapDirtyInIn(MIPSGPReg rd, MIPSGPReg rs, MIPSGPReg rt, bool avoidLoad = true);
	void MapDirtyDirtyIn(MIPSGPReg rd1, MIPSGPReg rd2, MIPSGPReg rs, bool avoidLoad = true);
	void MapDirtyDirtyInIn(MIPSGPReg rd1, MIPSGPReg rd2, MIPSGPReg rs, MIPSGPReg rt, bool avoidLoad = true);
	void FlushNativeReg(MIPSReg r);
	void FlushR(MIPSGPReg r);
	void FlushBeforeCall();
	void FlushAll();
	void DiscardR(MIPSGPReg r);

	MIPSReg R(MIPSGPReg preg); // Returns a cached register, while checking that it's NOT mapped as a pointer
	MIPSReg RPtr(MIPSGPReg preg); // Returns a cached register, while checking that it's mapped as a pointer

	void SetEmitter(MIPSEmitter *emitter) { emit_ = emitter; }

	// For better log output only.
	void SetCompilerPC(u32 compilerPC) { compilerPC_ = compilerPC; }

	int GetGuestRegOffset(MIPSGPReg r);

private:
	const MIPSReg *GetMIPSAllocationOrder(int &count);
	void MapRegTo(MIPSReg reg, MIPSGPReg mipsReg, int mapFlags);
	int FlushGetSequential(MIPSGPReg startMipsReg, bool allowFlushImm);
	MIPSReg FindBestToSpill(bool unusedOnly);

	MIPSState *mips_;
	MIPSComp::MIPSJitOptions *options_;
	MIPSEmitter *emit_;
	u32 compilerPC_;

	enum {
		NUM_MIPSNREG = 32,
		NUM_MIPSREG = TOTAL_MAPPABLE_MIPSREGS,
	};

	// nr for 'native' reg to differentiate it from the PSP's MIPS Allegrex
	RegMIPSN nr[NUM_MIPSNREG];
	RegMIPS mr[NUM_MIPSREG];
};

