#include <d3dx9.h>

#include "float4.h"

float4::float4() {
}
float4::float4(float x,float y,float z,float w) {
	this->x = x;
	this->y = y;
	this->z = z;
	this->w = w;
}
float4::float4(const D3DXVECTOR4& rvalue) {
	x = rvalue.x;
	y = rvalue.y;
	z = rvalue.z;
	w = rvalue.w;
}

float float4::Length() {
	return D3DXVec4Length(this);
}
float float4::Dot(const float4& rvalue) {
	return D3DXVec4Dot(this,&rvalue);
}
float4 float4::SetLength(float value) {
	Normalize();
	x *= value;
	y *= value;
	z *= value;
	w *= value;
	return *this;
}
float4 float4::Normalize() {
	D3DXVec4Normalize(this,this);
	return *this;
}
