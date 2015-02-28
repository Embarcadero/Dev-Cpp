#ifndef FLOAT3_H
#define FLOAT3_H

#include <d3dx9math.h>
#include <algorithm> // std::max

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

class DLLIMPORT float3 : public D3DXVECTOR3 {
	public:
		float3();
		float3(float x);
		float3(float x,float y,float z);
		float3(const D3DXVECTOR3& rvalue);

		float Length();
		float Dot(const float3& rvalue);
		float3 Min(const float3& rvalue);
		float3 Max(const float3& rvalue);
		float3 SetLength(float value);
		float3 Cross(const float3& rvalue);
		float3 Normalize();
		float3 Transform(const D3DXMATRIX& matrix);
		float3 TransformNormal(const D3DXMATRIX& matrix); // x,y,z,0
		float3 TransformCoord(const D3DXMATRIX& matrix); // x,y,z,1
};

#endif
