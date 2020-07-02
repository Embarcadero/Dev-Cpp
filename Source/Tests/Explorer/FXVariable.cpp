#include <cstdio>

#include "resource.h"
#include "FXVariable.h"

FXVariable::FXVariable(const char* name) : FXHandle(name) {
	current = NULL;
	handle = FX->GetParameterByName(NULL,name);
	if(!handle) {
		
		// Gebruik nog geen UI, want die bestaat nog niet altijd...
		char buffer[256];
		snprintf(buffer,256,"Error creating D3DXHANDLE to %s\r\n",name);
		MessageBox(hwnd,buffer,"Error",MB_ICONERROR);
	}
}

FXVariable::~FXVariable() {
}

void FXVariable::Set(void* data) {
	FX->SetValue(handle,data,D3DX_DEFAULT); // skip size validation...
}
void FXVariable::Set(float data) {
	FX->SetValue(handle,&data,D3DX_DEFAULT); // skip size validation...
}
void FXVariable::Set(unsigned int data) {
	FX->SetValue(handle,&data,D3DX_DEFAULT); // skip size validation...
}

void FXVariable::SetTexture(LPDIRECT3DBASETEXTURE9 texture) {
	if(texture != current) { // prevent slow texture state changes
		current = texture;
		FX->SetTexture(handle,texture);
	}
}
void FXVariable::SetTexture(Texture* texture) {
	SetTexture(texture->pointer);
}
void FXVariable::SetTexture(RenderTarget* target) {
	SetTexture(target->GetTexture());
}
