#include "float4x4.h"
#include "resource.h"

float4x4::float4x4() {

}
float4x4::float4x4(const D3DXMATRIX& rvalue) {
	_11 = rvalue._11;
	_12 = rvalue._12;
	_13 = rvalue._13;
	_14 = rvalue._14;
	_21 = rvalue._21;
	_22 = rvalue._22;
	_23 = rvalue._23;
	_24 = rvalue._24;
	_31 = rvalue._31;
	_32 = rvalue._32;
	_33 = rvalue._33;
	_34 = rvalue._34;
	_41 = rvalue._41;
	_42 = rvalue._42;
	_43 = rvalue._43;
	_44 = rvalue._44; // TODO: memset?
}

float4x4 float4x4::EulerRotation(const D3DXVECTOR3& angle) {
	return *D3DXMatrixRotationYawPitchRoll(this,angle.y,angle.x,angle.z);
}
float4x4 float4x4::EulerRotationDeg(const D3DXVECTOR3& angle) {
	D3DXVECTOR3 anglerad;
	anglerad.x = DegToRad(angle.x);
	anglerad.y = DegToRad(angle.y);
	anglerad.z = DegToRad(angle.z);
	return *D3DXMatrixRotationYawPitchRoll(this,anglerad.y,anglerad.x,anglerad.z);
}
float4x4 float4x4::Translation(const D3DXVECTOR3& translation) {
	return *D3DXMatrixTranslation(this,translation.x,translation.y,translation.z);
}
float4x4 float4x4::Scaling(const D3DXVECTOR3& scaling) {
	return *D3DXMatrixScaling(this,scaling.x,scaling.y,scaling.z);
}

float4x4 float4x4::Identity() {
	_11 = 1;
	_12 = 0;
	_13 = 0;
	_14 = 0;
	_21 = 0;
	_22 = 1;
	_23 = 0;
	_24 = 0;
	_31 = 0;
	_32 = 0;
	_33 = 1;
	_34 = 0;
	_41 = 0;
	_42 = 0;
	_43 = 0;
	_44 = 1;
	return *this;
}

float4x4 float4x4::Inverse() {
	float4x4 tmp;
	return *D3DXMatrixInverse(&tmp,NULL,this);
}

float float4x4::Determinant() {
	return D3DXMatrixDeterminant(this);
}
float4x4 float4x4::LookAt(const D3DXVECTOR3& from,const D3DXVECTOR3& to) {
	
	// basis #1
	float3 x = to - from;
	x.Normalize();

	// basis #2
	float3 y = GramSchmidt2(x,float3(0,1,0));
	y.Normalize();
	
	// basis #3
	float3 z = GramSchmidt3(x,y,float3(0,0,1));
	z.Normalize();

	// 4x4, "1"
	Identity();
	
	// insert vectors in rows (DX uses vec*matrix)
	_11 = x.x;
	_12 = x.y;
	_13 = x.z;
	
	_21 = y.x;
	_22 = y.y;
	_23 = y.z;
	
	_31 = z.x;
	_32 = z.y;
	_33 = z.z;
	
	// Flip or swap one if determinant = -1...
	if(fequals(Determinant(),-1)) { // lame epsilon trick
		_31 = -z.x;
		_32 = -z.y;
		_33 = -z.z;
	}
	return *this;
}
