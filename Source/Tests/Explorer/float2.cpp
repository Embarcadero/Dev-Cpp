#include <d3dx9.h>

#include "float2.h"

float2::float2() {
}
float2::float2(float x,float y) {
	this->x = x;
	this->y = y;
}
float2::float2(const D3DXVECTOR2& rvalue) {
	x = rvalue.x;
	y = rvalue.y;
}
float2::float2(const POINT& rvalue) {
	x = rvalue.x;
	y = rvalue.y;
}

float float2::Length() {
	D3DXVECTOR2 a(x,y);
	return D3DXVec2Length(&a);
}
float float2::Dot(const float2& rvalue) {
	D3DXVECTOR2 a(x,y);
	D3DXVECTOR2 b(rvalue.x,rvalue.y);
	return D3DXVec2Dot(&a,&b);
}
float2 float2::SetLength(float value) {
	Normalize(); // Set length to 1
	x *= value;
	y *= value; // multiply
	return *this;
}
float2 float2::Normalize() {
	D3DXVECTOR2 a(x,y);
	D3DXVec2Normalize(&a,&a);
	x = a.x; // store result!
	y = a.y;
	return *this;
}
