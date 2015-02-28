#ifndef FLOAT4X4_H
#define FLOAT4X4_H

#include <d3dx9math.h>

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

class DLLIMPORT float4x4 : public D3DXMATRIX {
	public:
		float4x4();
		float4x4(const D3DXMATRIX& rvalue);
		float4x4 EulerRotation(const D3DXVECTOR3& angle);
		float4x4 EulerRotationDeg(const D3DXVECTOR3& angle);
		float4x4 Translation(const D3DXVECTOR3& translation);
		float4x4 Scaling(const D3DXVECTOR3& scaling);
		float4x4 Identity();
		float4x4 Inverse(); // does not apply, only returns result
		float Determinant();
};

#endif
