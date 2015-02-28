#ifndef FLOAT4_H
#define FLOAT4_H

#include <d3dx9math.h>

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

class DLLIMPORT float4 : public D3DXVECTOR4 {
	public:
		float4();
		float4(float x,float y,float z,float w);
		float4(const D3DXVECTOR4& rvalue);

		float Length();
		float Dot(const float4& rvalue);
		float4 SetLength(float value);
		float4 Normalize();
};

#endif
