#ifndef FXVARIABLE_INCLUDE
#define FXVARIABLE_INCLUDE

#include <cstdio>
#include "FXHandle.h" // inheritance

class Texture;
class RenderTarget;

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

class DLLIMPORT FXVariable : public FXHandle {
	private:
		LPDIRECT3DBASETEXTURE9 texturevalue;
		float floatvalue;
		unsigned int intvalue;
	public:
		FXVariable(LPD3DXEFFECT FX,const char* name);
		~FXVariable();
		void Set(void* value);
		void Set(float value);
		void Set(unsigned int value);
		void SetTexture(LPDIRECT3DBASETEXTURE9 value);
		void SetTexture(Texture* value);
		void SetTexture(RenderTarget* value);
};

#endif
