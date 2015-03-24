#include "FXVariable.h"
#include "Texture.h"
#include "Resource.h" // FX
#include "RenderTarget.h"

FXVariable::FXVariable(LPD3DXEFFECT FX,const char* name) : FXHandle(FX,name) {
	texturevalue = NULL;
	floatvalue = 0.0f;
	intvalue = 0;
	handle = FX->GetParameterByName(NULL,name);
	if(!handle) {
		Globals::console->Write("ERROR: cannot create D3DXHANDLE to parameter '%s'\r\n",name);
	}
}
FXVariable::~FXVariable() {
}
void FXVariable::Set(void* value) {
	Globals::FX->SetValue(handle,value,D3DX_DEFAULT); // skip size validation...
}
void FXVariable::Set(float value) {
	if(floatvalue != value) {
		floatvalue = value;
		Globals::FX->SetValue(handle,&value,D3DX_DEFAULT); // skip size validation...
	}
}
void FXVariable::Set(unsigned int value) {
	if(intvalue != value) {
		intvalue = value;
		FX->SetValue(handle,&value,D3DX_DEFAULT); // skip size validation...
	}
}
void FXVariable::SetTexture(LPDIRECT3DBASETEXTURE9 value) {
	if(texturevalue != value) { // prevent slow texture state changes
		texturevalue = value;
		FX->SetTexture(handle,texturevalue);
	}
}
void FXVariable::SetTexture(Texture* value) {
	SetTexture(value->GetD3DInterface());
}
void FXVariable::SetTexture(RenderTarget* value) {
	SetTexture(value->GetTexture());
}
