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

// WARNING - THIS LIBRARY IS NOT THREAD SAFE!!! (cargo culted but probably true)

#pragma once

#include <vector>
#include <stdint.h>

#include "Common.h"
#include "MsgHandler.h"

namespace MIPSGen {

enum MIPSReg {
	R_ZERO = 0,
	R_AT,
	V0, V1,

	A0 = 4, A1 = 5, A2 = 6, A3 = 7, A4 = 8, A5 = 9, A6 = 10, A7 = 11,
	// Alternate names depending on ABI.
	T0 = 8, T1 = 9, T2 = 10, T3 = 11,

	T4, T5, T6, T7,
	S0, S1, S2, S3, S4, S5, S6, S7,
	T8, T9,
	K0, K1,
	R_GP, R_SP, R_FP,
	R_RA,

	F_BASE = 32,
	F0 = 32, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15,
	F16, F17, F18, F19, F20, F21, F22, F23, F24, F25, F26, F27, F28, F29, F30, F31,

	INVALID_REG = 0xFFFFFFFF
};

enum {
	// All 32 except: ZERO, K0/K1 (kernel), RA.  The rest are only convention.
	NUMGPRs = 32 - 1 - 2 - 1,
	NUMFPRs = 32,
};

enum FixupBranchType {
	// 16-bit immediate jump/branch (to pc + (simm16 + 1 ops).)
	BRANCH_16,
	// 26-bit immediate jump/branch (to pc's 4 high bits + imm * 4.)
	BRANCH_26,
};

// Beware of delay slots.
struct FixupBranch {
	u8 *ptr;
	FixupBranchType type;
};

class MIPSXEmitter {
public:
	MIPSXEmitter() : code_(0), lastCacheFlushEnd_(0) {
	}
	MIPSXEmitter(u8 *code_ptr) : code_(code_ptr), lastCacheFlushEnd_(code_ptr) {
		SetCodePtr(code_ptr);
	}
	virtual ~MIPSXEmitter() {
	}

	void SetCodePtr(u8 *ptr);
	void ReserveCodeSpace(u32 bytes);
	const u8 *AlignCode16();
	const u8 *AlignCodePage();
	const u8 *GetCodePtr() const;
	u8 *GetWritableCodePtr();
	void FlushIcache();
	void FlushIcacheSection(u8 *start, u8 *end);

	// 20 bits valid in code.
	void BREAK(u32 code);

	void NOP() {
		SLL(R_ZERO, R_ZERO, 0);
	}

	FixupBranch J();
	void J(const void *func);
	FixupBranch JAL();
	void JAL(const void *func);

	inline FixupBranch B() {
		return BEQ(R_ZERO, R_ZERO);
	}
	inline void B(const void *func) {
		return BEQ(R_ZERO, R_ZERO, func);
	}
	FixupBranch BEQ(MIPSReg rs, MIPSReg rt);
	void BEQ(MIPSReg rs, MIPSReg rt, const void *func);
	FixupBranch BNE(MIPSReg rs, MIPSReg rt);
	void BNE(MIPSReg rs, MIPSReg rt, const void *func);
	inline FixupBranch BEQZ(MIPSReg rs) {
		return BEQ(rs, R_ZERO);
	}
	inline void BEQZ(MIPSReg rs, const void *func) {
		return BEQ(rs, R_ZERO, func);
	}
	inline FixupBranch BNEZ(MIPSReg rs) {
		return BNE(rs, R_ZERO);
	}
	inline void BNEZ(MIPSReg rs, const void *func) {
		return BNE(rs, R_ZERO, func);
	}
	FixupBranch BLEZ(MIPSReg rs);
	void BLEZ(MIPSReg rs, const void *func);
	FixupBranch BGTZ(MIPSReg rs);
	void BGTZ(MIPSReg rs, const void *func);

	void SetJumpTarget(const FixupBranch &branch);

	void LW(MIPSReg dest, MIPSReg base, s16 offset);
	void SW(MIPSReg value, MIPSReg base, s16 offset);

	void SLL(MIPSReg rd, MIPSReg rt, u8 sa);
	void SRL(MIPSReg rd, MIPSReg rt, u8 sa);
	void SRA(MIPSReg rd, MIPSReg rt, u8 sa);

	// The imm is sign extended before these.
	void ADDIU(MIPSReg rt, MIPSReg rs, s16 imm);
	void SLTIU(MIPSReg rt, MIPSReg rs, s16 imm);

	// The imm is zero extended before these.
	void ANDI(MIPSReg rt, MIPSReg rs, s16 imm);
	void ORI(MIPSReg rt, MIPSReg rs, s16 imm);
	void XORI(MIPSReg rt, MIPSReg rs, s16 imm);

	// Clears the lower bits.
	void LUI(MIPSReg rt, s16 imm);

	void QuickCallFunction(MIPSReg scratchreg, const void *func);
	template <typename T> void QuickCallFunction(MIPSReg scratchreg, T func) {
		QuickCallFunction(scratchreg, (const void *)func);
	}

protected:
	inline void Write32(u32 value) {
		*code32_++ = value;
	}

	// Less parenthesis.
	inline void Write32Fields(u8 pos1, u32 v1) {
		*code32_++ = (v1 << pos1);
	}
	inline void Write32Fields(u8 pos1, u32 v1, u8 pos2, u32 v2) {
		*code32_++ = (v1 << pos1) | (v2 << pos2);
	}
	inline void Write32Fields(u8 pos1, u32 v1, u8 pos2, u32 v2, u8 pos3, u32 v3) {
		*code32_++ = (v1 << pos1) | (v2 << pos2) | (v3 << pos3);
	}
	inline void Write32Fields(u8 pos1, u32 v1, u8 pos2, u32 v2, u8 pos3, u32 v3, u8 pos4, u32 v4) {
		*code32_++ = (v1 << pos1) | (v2 << pos2) | (v3 << pos3) | (v4 << pos4);
	}
	inline void Write32Fields(u8 pos1, u32 v1, u8 pos2, u32 v2, u8 pos3, u32 v3, u8 pos4, u32 v4, u8 pos5, u32 v5) {
		*code32_++ = (v1 << pos1) | (v2 << pos2) | (v3 << pos3) | (v4 << pos5) | (v5 << pos5);
	}
	inline void Write32Fields(u8 pos1, u32 v1, u8 pos2, u32 v2, u8 pos3, u32 v3, u8 pos4, u32 v4, u8 pos5, u32 v5, u8 pos6, u32 v6) {
		*code32_++ = (v1 << pos1) | (v2 << pos2) | (v3 << pos3) | (v4 << pos5) | (v5 << pos5) | (v6 << pos6);
	}

	static void SetJumpTarget(const FixupBranch &branch, const void *dst);
	FixupBranch MakeFixupBranch(FixupBranchType type);

private:
	union {
		u8 *code_;
		u32 *code32_;
	};
	u8 *lastCacheFlushEnd_;
};

// Everything that needs to generate machine code should inherit from this.
// You get memory management for free, plus, you can use all the LUI etc functions without
// having to prefix them with gen-> or something similar.
class MIPSXCodeBlock : public MIPSXEmitter {
public:
	MIPSXCodeBlock() : region(nullptr), region_size(0) {
	}
	virtual ~MIPSXCodeBlock() {
		if (region) {
			FreeCodeSpace();
		}
	}

	// Call this before you generate any code.
	void AllocCodeSpace(int size);

	// Always clear code space with breakpoints, so that if someone accidentally executes
	// uninitialized, it just breaks into the debugger.
	void ClearCodeSpace();

	// Call this when shutting down. Don't rely on the destructor, even though it'll do the job.
	void FreeCodeSpace();

	bool IsInSpace(const u8 *ptr) const {
		return ptr >= region && ptr < region + region_size;
	}

	// Can possibly be undone. Will write protect the entire code region.
	// Start over if you need to change the code, though (call FreeCodeSpace(), AllocCodeSpace().)
	void WriteProtect();
	void UnWriteProtect();

	void ResetCodePtr() {
		SetCodePtr(region);
	}

	size_t GetSpaceLeft() const {
		return region_size - (GetCodePtr() - region);
	}

	u8 *GetBasePtr() {
		return region;
	}

	size_t GetOffset(const u8 *ptr) const {
		return ptr - region;
	}

protected:
	u8 *region;
	size_t region_size;
};

};