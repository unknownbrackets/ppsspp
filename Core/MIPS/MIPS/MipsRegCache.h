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

using namespace MIPSGen;

#define BASEREG (S7)
#define CTXREG (S6)
#define CODEREG (S5)
#define DOWNCOUNTREG (S4)
#define SCRATCHREG1 (V0)
#define SCRATCHREG2 (V1)

