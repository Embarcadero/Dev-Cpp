#ifndef FXHANDLE_INCLUDE
#define FXHANDLE_INCLUDE

#include <string>
#include <d3dx9.h>

class FXHandle {
	public:
		FXHandle(const char* name);
		~FXHandle();

		char* name;
		D3DXHANDLE handle;
};

#endif
