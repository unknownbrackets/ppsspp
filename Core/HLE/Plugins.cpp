// Copyright (c) 2013- PPSSPP Project.

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

#include "Core/FileSystems/MetaFileSystem.h"
#include "Core/HLE/HLE.h"
#include "Core/HLE/Plugins.h"
#include "Core/HLE/sceKernelModule.h"
#include "Core/System.h"

static const std::string GAME_TXT_PATH = "ms0:/seplugins/game.txt";

namespace HLEPlugins {
	void Load(const std::string &filename) {
		std::string error_string = "";
		SceUID module = __KernelLoadModule(filename, &error_string);
		if (!error_string.empty()) {
			ERROR_LOG(HLE, "Unable to load plugin %s: %s", filename.c_str(), error_string.c_str());
			return;
		}
		if (module < 0) {
			ERROR_LOG(HLE, "Unable to load plugin %s: %08x", filename.c_str(), module);
			return;
		}

		int ret = __KernelStartModule(module, 0, 0, 0, NULL, NULL);
		if (ret < 0) {
			ERROR_LOG(HLE, "Unable to start plugin %s: %08x", filename.c_str(), ret);
		}

		INFO_LOG(HLE, "Loaded plugin: %s", filename.c_str());
	}

	void ProcessGameFile(const std::string &pluginConfig) {
		for (size_t pos = 0, end = pluginConfig.size(); pos < end; ++pos) {
			// Skip leading whitespace and newlines.
			pos = pluginConfig.find_first_not_of(" \t\r\n", pos);
			if (pos == pluginConfig.npos)
				break;

			size_t next = pluginConfig.find_first_of("\r\n", pos);
			if (next == pluginConfig.npos)
				next = end;

			const std::string line = pluginConfig.substr(pos, next - pos);
			size_t separator = line.find(' ');
			if (separator < line.size() - 1 && line[separator + 1] == '1') {
				Load(line.substr(0, separator));
			}
			pos = next;
		}
	}

	void Init() {
		PSPFileInfo fileInfo = pspFileSystem.GetFileInfo(GAME_TXT_PATH);
		if (fileInfo.exists)
		{
			std::string pluginConfig;
			pluginConfig.resize(fileInfo.size);
			u32 fd = pspFileSystem.OpenFile(GAME_TXT_PATH, FILEACCESS_READ);
			pspFileSystem.ReadFile(fd, (u8 *)&pluginConfig[0], fileInfo.size);
			pspFileSystem.CloseFile(fd);

			ProcessGameFile(pluginConfig);
		}
	}
};