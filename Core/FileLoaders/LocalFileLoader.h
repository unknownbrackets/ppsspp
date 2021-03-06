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

#pragma once

#include <mutex>
#include "Common/CommonTypes.h"
#include "Core/Loaders.h"
#ifdef _WIN32
typedef void *HANDLE;
#endif

class LocalFileLoader : public FileLoader {
public:
	LocalFileLoader(const std::string &filename);
	LocalFileLoader(const int fd, const std::string &filename);
	virtual ~LocalFileLoader();

	virtual bool Exists() override;
	virtual bool IsDirectory() override;
	virtual s64 FileSize() override;
	virtual std::string GetPath() const override;
	virtual size_t ReadAt(s64 absolutePos, size_t bytes, size_t count, void *data, Flags flags = Flags::NONE) override;

private:
#ifndef _WIN32
	void DetectSizeFd();
	int fd_;
#else
	HANDLE handle_;
#endif
	u64 filesize_;
	std::string filename_;
	std::mutex readLock_;
	bool isOpenedByFd_;
};
