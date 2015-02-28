#include "float3.h"

float3::float3() {
}
float3::float3(float x) {
	this->x = x;
	this->y = x;
	this->z = x;
}
float3::float3(float x,float y,float z) {
	this->x = x;
	this->y = y;
	this->z = z;
}
float3::float3(const D3DXVECTOR3& rvalue) {
	x = rvalue.x;
	y = rvalue.y;
	z = rvalue.z;
}

float float3::Length() {
	return D3DXVec3Length(this);
}
float float3::Dot(const float3& rvalue) {
	return D3DXVec3Dot(this,&rvalue);
}
float3 float3::Min(const float3& rvalue) {
	float3 dummy;
	dummy.x = std::min(x,rvalue.x);
	dummy.y = std::min(y,rvalue.x);
	dummy.z = std::min(z,rvalue.x);
	return dummy;
}
float3 float3::Max(const float3& rvalue) {
	float3 dummy;
	dummy.x = std::max(x,rvalue.x);
	dummy.y = std::max(y,rvalue.x);
	dummy.z = std::max(z,rvalue.x);
	return dummy;
}
float3 float3::SetLength(float value) {
	Normalize();
	x *= value;
	y *= value;
	z *= value;
	return *this;
}
float3 float3::Cross(const float3& rvalue) {
	float3 dummy;
	return *D3DXVec3Cross(&dummy,this,&rvalue);
}
float3 float3::Normalize() {
	return *D3DXVec3Normalize(this,this);
}
float3 float3::Transform(const D3DXMATRIX& matrix) {
	D3DXVECTOR4 dummy; // float4 niet gebruiken, we zijn onafhankelijk zo
	D3DXVec3Transform(&dummy,this,&matrix);
	return float3(dummy.x,dummy.y,dummy.z);
}
float3 float3::TransformNormal(const D3DXMATRIX& matrix) {
	float3 dummy;
	D3DXVec3TransformNormal(&dummy,this,&matrix);
	return dummy;
}
float3 float3::TransformCoord(const D3DXMATRIX& matrix) {
	float3 dummy;
	D3DXVec3TransformCoord(&dummy,this,&matrix);
	return dummy;
}
