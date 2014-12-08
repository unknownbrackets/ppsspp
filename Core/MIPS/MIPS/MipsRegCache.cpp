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

#include "Core/MemMap.h"
#include "Core/MIPS/MIPSAnalyst.h"
#include "Core/Reporting.h"
#include "Common/MipsEmitter.h"
#include "MipsRegCache.h"

#ifndef offsetof
#include "stddef.h"
#endif

using namespace MIPSGen;

MIPSRegCache::MIPSRegCache(MIPSState *mips, MIPSComp::MIPSJitOptions *options) : mips_(mips), options_(options) {
}

void MIPSRegCache::Init(MIPSEmitter *emitter) {
	emit_ = emitter;
}

void MIPSRegCache::Start(MIPSAnalyst::AnalysisResults &stats) {
	for (int i = 0; i < NUM_MIPSNREG; i++) {
		nr[i].mipsReg = MIPS_REG_INVALID;
		nr[i].isDirty = false;
	}
	for (int i = 0; i < NUM_MIPSREG; i++) {
		mr[i].loc = ML_MEM;
		mr[i].reg = INVALID_REG;
		mr[i].imm = -1;
		mr[i].spillLock = false;
	}
}

const MIPSReg *MIPSRegCache::GetMIPSAllocationOrder(int &count) {
	// Note that R_AT and T0-T3 are reserved as scratch for now. Probably can drop T2 and T3 once Regcache is complete.
	// S0-S3+S8 are registers we could use for static allocation
	// S4-S7 are in use for static allocation already
	// T4-T8 could be used as temporaries
	// GP is used for PIC code.
	// T9 may be required in certain ABIs to store the address of a called function. Immediately after the call it would become usable again.
	// V0-V1 can likely be used as long as they are marked dirty when a function returns with a value
	// A0-A3 could be used as temporaries if it can be made not to interfere with a QuickCallFunction
	static const MIPSReg allocationOrder[] = {
		S0, S1, S2, S3, S8, T4, T5, T6, T7, T8
	};
	count = sizeof(allocationOrder) / sizeof(const int);
	return allocationOrder;
}

void MIPSRegCache::FlushBeforeCall() {
	// S0-S3+S8 are preserved. Others need flushing.
	FlushNativeReg(T4);
	FlushNativeReg(T5);
	FlushNativeReg(T6);
	FlushNativeReg(T7);
	FlushNativeReg(T8);
}

MIPSReg MIPSRegCache::MapRegAsPointer(MIPSGPReg mipsReg) {  // read-only, non-dirty.
	// If already mapped as a pointer, bail.
	if (mr[mipsReg].loc == ML_MIPSNREG_AS_PTR) {
		return mr[mipsReg].reg;
	}
	// First, make sure the register is already mapped.
	MapReg(mipsReg, 0);
	// If it's dirty, flush it.
	MIPSReg mipsnReg = mr[mipsReg].reg;
	if (nr[mipsnReg].isDirty) {
		emit_->SW(mipsnReg, CTXREG, GetGuestRegOffset(nr[mipsnReg].mipsReg));
	}
	// Convert to a pointer by adding the base and clearing off the top bits.
	// If SP, we can probably avoid the top bit clear, let's play with that later.
	emit_->EXT(mipsnReg, mipsnReg, 0, 30);    // &= 0x3FFFFFFF
	emit_->ADDU(mipsnReg, BASEREG, mipsnReg);
	nr[mipsnReg].isDirty = false;
	nr[mipsnReg].mipsReg = mipsReg;
	mr[mipsReg].loc = ML_MIPSNREG_AS_PTR;
	return mipsnReg;
}

bool MIPSRegCache::IsMappedAsPointer(MIPSGPReg mipsReg) {
	return mr[mipsReg].loc == ML_MIPSNREG_AS_PTR;
}

void MIPSRegCache::SetRegImm(MIPSReg reg, u32 imm) {
	// Maybe try to check if there's a simpler way first, like ARM

	// Okay, so it's a bit more complex.  Let's see if we have any useful regs with imm values.
	for (int i = 0; i < NUM_MIPSREG; i++) {
		const auto &mreg = mr[i];
		if (mreg.loc != ML_MIPSNREG_IMM)
			continue;

		if ((imm - mreg.imm < 32767) || (mreg.imm - imm < 32767)) {
			emit_->ADDIU(reg, mreg.reg, imm - mreg.imm);
			return;
		}
		// This could be common when using an address.
		if ((mreg.imm & 0x3FFFFFFF) == imm) {
			emit_->EXT(reg, mreg.reg, 0, 30);   // &= 0x3FFFFFFF
			return;
		}
		// TODO: All sorts of things are possible here, shifted adds, ands/ors, etc.
	}

	// No luck.  Let's go with a regular load.
	emit_->MOVI2R(reg, imm);
}

void MIPSRegCache::MapRegTo(MIPSReg reg, MIPSGPReg mipsReg, int mapFlags) {
	nr[reg].isDirty = (mapFlags & MAP_DIRTY) ? true : false;
	if ((mapFlags & MAP_NOINIT) != MAP_NOINIT) {
		if (mipsReg == MIPS_REG_ZERO) {
			// Shouldn't get here
		} else {
			switch (mr[mipsReg].loc) {
			case ML_MEM:
				emit_->LW(reg, CTXREG, GetGuestRegOffset(mipsReg));
				mr[mipsReg].loc = ML_MIPSNREG;
				break;
			case ML_IMM:
				SetRegImm(reg, mr[mipsReg].imm);
				nr[reg].isDirty = true;  // IMM is always dirty.

				// If we are mapping dirty, it means we're gonna overwrite.
				// So the imm value is no longer valid.
				if (mapFlags & MAP_DIRTY)
					mr[mipsReg].loc = ML_MIPSNREG;
				else
					mr[mipsReg].loc = ML_MIPSNREG_IMM;
				break;
			default:
				mr[mipsReg].loc = ML_MIPSNREG;
				break;
			}
		}
	} else {
		if (mipsReg == MIPS_REG_ZERO) {
			// This way, if we SetImm() it, we'll keep it.
			mr[mipsReg].loc = ML_MIPSNREG_IMM;
			mr[mipsReg].imm = 0;
		} else {
			mr[mipsReg].loc = ML_MIPSNREG;
		}
	}
	nr[reg].mipsReg = mipsReg;
	mr[mipsReg].reg = reg;
}

MIPSReg MIPSRegCache::FindBestToSpill(bool unusedOnly) {
	int allocCount;
	const MIPSReg *allocOrder = GetMIPSAllocationOrder(allocCount);

	static const int UNUSED_LOOKAHEAD_OPS = 3;

	for (int i = 0; i < allocCount; i++) {
		MIPSReg reg = allocOrder[i];
		if (nr[reg].mipsReg != MIPS_REG_INVALID && mr[nr[reg].mipsReg].spillLock)
			continue;

		if (unusedOnly) {
			bool unused = true;
			for (int ahead = 1; ahead <= UNUSED_LOOKAHEAD_OPS; ++ahead) {
				MIPSOpcode laterOp = Memory::Read_Instruction(compilerPC_ + ahead * sizeof(u32));
				// If read, it might need to be mapped again.  If output, it might not need to be stored.
				if (MIPSAnalyst::ReadsFromGPReg(laterOp, nr[reg].mipsReg) || MIPSAnalyst::GetOutGPReg(laterOp) == nr[reg].mipsReg) {
					unused = false;
				}
			}

			if (!unused) {
				continue;
			}
		}

		return reg;
	}

	return INVALID_REG;
}

// TODO: Somewhat smarter spilling - currently simply spills the first available, should do
// round robin or FIFO or something.
MIPSReg MIPSRegCache::MapReg(MIPSGPReg mipsReg, int mapFlags) {
	if (mipsReg == MIPS_REG_ZERO)
		return R_ZERO;

	// Let's see if it's already mapped. If so we just need to update the dirty flag.
	// We don't need to check for ML_NOINIT because we assume that anyone who maps
	// with that flag immediately writes a "known" value to the register.
	if (mr[mipsReg].loc == ML_MIPSNREG || mr[mipsReg].loc == ML_MIPSNREG_IMM) {
		MIPSReg mipsnReg = mr[mipsReg].reg;
		if (nr[mipsnReg].mipsReg != mipsReg) {
			ERROR_LOG_REPORT(JIT, "Register mapping out of sync! %i", mipsReg);
		}
		if (mapFlags & MAP_DIRTY) {
			// Mapping dirty means the old imm value is invalid.
			mr[mipsReg].loc = ML_MIPSNREG;
			nr[mipsnReg].isDirty = true;
		}
		return (MIPSReg)mr[mipsReg].reg;
	} else if (mr[mipsReg].loc == ML_MIPSNREG_AS_PTR) {
		// Was mapped as pointer, now we want it mapped as a value, presumably to
		// add or subtract stuff to it. Later we could allow such things but for now
		// let's just convert back to a register value by reloading from the backing storage.
		MIPSReg mipsnReg = mr[mipsReg].reg;
		if ((mapFlags & MAP_NOINIT) != MAP_NOINIT) {
			emit_->LW(mipsnReg, CTXREG, GetGuestRegOffset(mipsReg));
		}
		mr[mipsReg].loc = ML_MIPSNREG;
		if (mapFlags & MAP_DIRTY) {
			nr[mipsnReg].isDirty = true;
		}
		return (MIPSReg)mr[mipsReg].reg;
	}

	// Okay, not mapped, so we need to allocate a native MIPS register.

	int allocCount;
	const MIPSReg *allocOrder = GetMIPSAllocationOrder(allocCount);

	MIPSReg desiredReg = INVALID_REG;
	// Try to "statically" allocate the first 6 regs after v0.
	int desiredOrder = allocCount - (6 - (mipsReg - (int)MIPS_REG_V0));
	if (desiredOrder >= 0 && desiredOrder < allocCount)
		desiredReg = allocOrder[desiredOrder];

	if (desiredReg != INVALID_REG) {
		if (nr[desiredReg].mipsReg == MIPS_REG_INVALID) {
			// With this placement, we may be able to optimize flush.
			MapRegTo(desiredReg, mipsReg, mapFlags);
			return desiredReg;
		}
	}

allocate:
	for (int i = 0; i < allocCount; i++) {
		MIPSReg reg = allocOrder[i];

		if (nr[reg].mipsReg == MIPS_REG_INVALID) {
			// That means it's free. Grab it, and load the value into it (if requested).
			MapRegTo(reg, mipsReg, mapFlags);
			return reg;
		}
	}

	// Still nothing. Let's spill a reg and goto 10.
	// TODO: Use age or something to choose which register to spill?
	// TODO: Spill dirty regs first? or opposite?
	MIPSReg bestToSpill = FindBestToSpill(true);
	if (bestToSpill == INVALID_REG) {
		bestToSpill = FindBestToSpill(false);
	}

	if (bestToSpill != INVALID_REG) {
		// ERROR_LOG(JIT, "Out of registers at PC %08x - spills register %i.", mips_->pc, bestToSpill);
		FlushNativeReg(bestToSpill);
		goto allocate;
	}

	// Uh oh, we have all them spilllocked....
	ERROR_LOG_REPORT(JIT, "Out of spillable registers at PC %08x!!!", mips_->pc);
	return INVALID_REG;
}

void MIPSRegCache::MapInIn(MIPSGPReg rd, MIPSGPReg rs) {
	SpillLock(rd, rs);
	MapReg(rd);
	MapReg(rs);
	ReleaseSpillLocks();
}

void MIPSRegCache::MapDirtyIn(MIPSGPReg rd, MIPSGPReg rs, bool avoidLoad) {
	SpillLock(rd, rs);
	bool load = !avoidLoad || rd == rs;
	MapReg(rd, load ? MAP_DIRTY : MAP_NOINIT);
	MapReg(rs);
	ReleaseSpillLocks();
}

void MIPSRegCache::MapDirtyInIn(MIPSGPReg rd, MIPSGPReg rs, MIPSGPReg rt, bool avoidLoad) {
	SpillLock(rd, rs, rt);
	bool load = !avoidLoad || (rd == rs || rd == rt);
	MapReg(rd, load ? MAP_DIRTY : MAP_NOINIT);
	MapReg(rt);
	MapReg(rs);
	ReleaseSpillLocks();
}

void MIPSRegCache::MapDirtyDirtyIn(MIPSGPReg rd1, MIPSGPReg rd2, MIPSGPReg rs, bool avoidLoad) {
	SpillLock(rd1, rd2, rs);
	bool load1 = !avoidLoad || rd1 == rs;
	bool load2 = !avoidLoad || rd2 == rs;
	MapReg(rd1, load1 ? MAP_DIRTY : MAP_NOINIT);
	MapReg(rd2, load2 ? MAP_DIRTY : MAP_NOINIT);
	MapReg(rs);
	ReleaseSpillLocks();
}

void MIPSRegCache::MapDirtyDirtyInIn(MIPSGPReg rd1, MIPSGPReg rd2, MIPSGPReg rs, MIPSGPReg rt, bool avoidLoad) {
	SpillLock(rd1, rd2, rs, rt);
	bool load1 = !avoidLoad || (rd1 == rs || rd1 == rt);
	bool load2 = !avoidLoad || (rd2 == rs || rd2 == rt);
	MapReg(rd1, load1 ? MAP_DIRTY : MAP_NOINIT);
	MapReg(rd2, load2 ? MAP_DIRTY : MAP_NOINIT);
	MapReg(rt);
	MapReg(rs);
	ReleaseSpillLocks();
}

void MIPSRegCache::FlushNativeReg(MIPSReg r) {
	if (nr[r].mipsReg == MIPS_REG_INVALID) {
		// Nothing to do, reg not mapped.
		if (nr[r].isDirty) {
			ERROR_LOG_REPORT(JIT, "Dirty but no mipsreg?");
		}
		return;
	}
	if (nr[r].mipsReg != MIPS_REG_INVALID) {
		auto &mreg = mr[nr[r].mipsReg];
		if (mreg.loc == ML_MIPSNREG_IMM) {
			// We know its immedate value, no need to STR now.
			mreg.loc = ML_IMM;
			mreg.reg = INVALID_REG;
		} else {
			if (nr[r].isDirty && mreg.loc == ML_MIPSNREG)
				emit_->SW(r, CTXREG, GetGuestRegOffset(nr[r].mipsReg));
			mreg.loc = ML_MEM;
			mreg.reg = INVALID_REG;
			mreg.imm = 0;
		}
	}
	nr[r].isDirty = false;
	nr[r].mipsReg = MIPS_REG_INVALID;
}

void MIPSRegCache::DiscardR(MIPSGPReg mipsReg) {
	const RegMIPSLoc prevLoc = mr[mipsReg].loc;
	if (prevLoc == ML_MIPSNREG || prevLoc == ML_MIPSNREG_AS_PTR || prevLoc == ML_MIPSNREG_IMM) {
		MIPSReg mipsnReg = mr[mipsReg].reg;
		nr[mipsnReg].isDirty = false;
		nr[mipsnReg].mipsReg = MIPS_REG_INVALID;
		mr[mipsReg].reg = INVALID_REG;
		mr[mipsReg].loc = ML_MEM;
		mr[mipsReg].imm = 0;
	}
}

void MIPSRegCache::FlushR(MIPSGPReg r) {
	switch (mr[r].loc) {
	case ML_IMM:
		// IMM is always "dirty".
		if (r != MIPS_REG_ZERO) {
			SetRegImm(SCRATCHREG1, mr[r].imm);
			emit_->SW(SCRATCHREG1, CTXREG, GetGuestRegOffset(r));
		}
		break;

	case ML_MIPSNREG:
	case ML_MIPSNREG_IMM:
		if (mr[r].reg == INVALID_REG) {
			ERROR_LOG_REPORT(JIT, "FlushR: MipsReg %d had bad native MIPSReg", r);
		}
		if (nr[mr[r].reg].isDirty) {
			if (r != MIPS_REG_ZERO) {
				emit_->SW((MIPSReg)mr[r].reg, CTXREG, GetGuestRegOffset(r));
			}
			nr[mr[r].reg].isDirty = false;
		}
		nr[mr[r].reg].mipsReg = MIPS_REG_INVALID;
		break;

	case ML_MIPSNREG_AS_PTR:
		// Never dirty.
		if (nr[mr[r].reg].isDirty) {
			ERROR_LOG_REPORT(JIT, "MIPSNREG_AS_PTR cannot be dirty (yet)");
		}
		nr[mr[r].reg].mipsReg = MIPS_REG_INVALID;
		break;

	case ML_MEM:
		// Already there, nothing to do.
		break;

	default:
		ERROR_LOG_REPORT(JIT, "FlushR: MipsReg %d with invalid location %d", r, mr[r].loc);
		break;
	}
	mr[r].loc = ML_MEM;
	mr[r].reg = INVALID_REG;
	mr[r].imm = 0;
}

void MIPSRegCache::FlushAll() {
	for (int i = 0; i < NUM_MIPSREG; i++) {
		MIPSGPReg mipsReg = MIPSGPReg(i);
		FlushR(mipsReg);
	}

	// Sanity check
	for (int i = 0; i < NUM_MIPSNREG; i++) {
		if (nr[i].mipsReg != MIPS_REG_INVALID) {
			ERROR_LOG_REPORT(JIT, "Flush fail: nr[%i].mipsReg=%i", i, nr[i].mipsReg);
		}
	}
}

void MIPSRegCache::SetImm(MIPSGPReg r, u32 immVal) {
	if (r == MIPS_REG_ZERO && immVal != 0)
		ERROR_LOG(JIT, "Trying to set immediate %08x to r0", immVal);

	if (mr[r].loc == ML_MIPSNREG_IMM && mr[r].imm == immVal) {
		// Already have that value, let's keep it in the reg.
		return;
	}
	// Zap existing value if cached in a reg
	if (mr[r].reg != INVALID_REG) {
		nr[mr[r].reg].mipsReg = MIPS_REG_INVALID;
		nr[mr[r].reg].isDirty = false;
	}
	mr[r].loc = ML_IMM;
	mr[r].imm = immVal;
	mr[r].reg = INVALID_REG;
}

bool MIPSRegCache::IsImm(MIPSGPReg r) const {
	if (r == MIPS_REG_ZERO) return true;
	return mr[r].loc == ML_IMM || mr[r].loc == ML_MIPSNREG_IMM;
}

u32 MIPSRegCache::GetImm(MIPSGPReg r) const {
	if (r == MIPS_REG_ZERO) return 0;
	if (mr[r].loc != ML_IMM && mr[r].loc != ML_MIPSNREG_IMM) {
		ERROR_LOG_REPORT(JIT, "Trying to get imm from non-imm register %i", r);
	}
	return mr[r].imm;
}

int MIPSRegCache::GetGuestRegOffset(MIPSGPReg r) {
	if (r < 32)
		return r * 4;
	switch (r) {
	case MIPS_REG_HI:
		return offsetof(MIPSState, hi);
	case MIPS_REG_LO:
		return offsetof(MIPSState, lo);
	case MIPS_REG_FPCOND:
		return offsetof(MIPSState, fpcond);
	case MIPS_REG_VFPUCC:
		return offsetof(MIPSState, vfpuCtrl[VFPU_CTRL_CC]);
	default:
		ERROR_LOG_REPORT(JIT, "bad mips register %i", r);
		return 0;  // or what?
	}
}

void MIPSRegCache::SpillLock(MIPSGPReg r1, MIPSGPReg r2, MIPSGPReg r3, MIPSGPReg r4) {
	mr[r1].spillLock = true;
	if (r2 != MIPS_REG_INVALID) mr[r2].spillLock = true;
	if (r3 != MIPS_REG_INVALID) mr[r3].spillLock = true;
	if (r4 != MIPS_REG_INVALID) mr[r4].spillLock = true;
}

void MIPSRegCache::ReleaseSpillLock(MIPSGPReg reg) {
	mr[reg].spillLock = false;
}

MIPSReg MIPSRegCache::R(MIPSGPReg mipsReg) {
	if (mipsReg == MIPS_REG_ZERO)
		return R_ZERO;

	if (mr[mipsReg].loc == ML_MIPSNREG || mr[mipsReg].loc == ML_MIPSNREG_IMM) {
		return (MIPSReg)mr[mipsReg].reg;
	} else {
		ERROR_LOG_REPORT(JIT, "Reg %i not in native mips reg. compilerPC = %08x", mipsReg, compilerPC_);
		return INVALID_REG;  // BAAAD
	}
}

MIPSReg MIPSRegCache::RPtr(MIPSGPReg mipsReg) {
	if (mr[mipsReg].loc == ML_MIPSNREG_AS_PTR) {
		return (MIPSReg)mr[mipsReg].reg;
	} else {
		ERROR_LOG_REPORT(JIT, "Reg %i not in native mips reg as pointer. compilerPC = %08x", mipsReg, compilerPC_);
		return INVALID_REG;  // BAAAD
	}
}
