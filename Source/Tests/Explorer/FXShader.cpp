#include <cstdio>

#include "FXShader.h"
#include "Scene.h"
#include "Console.h"

FXShader::FXShader(const char* name) : FXHandle(name) {
	Get(name);
	Reset();
}

FXShader::~FXShader() {
}

void FXShader::Get(const char* name) {
	handle = FX->GetTechniqueByName(name);
	if(!handle) { // Gebruik geen UI, want die hangt van het slagen van ons af...
		char buffer[512];
		snprintf(buffer,512,"Error creating D3DXHANDLE of shader %s\r\n",name);
		MessageBox(hwnd,buffer,"Error",MB_ICONERROR);
	}
}

void FXShader::Reset() {
	if(scene) {
		begin = scene->objects->begin(); // random value
	} else {
		begin = nulldummy;
	}
	end = begin; // end == begin means unused
	unused = true;
}

void FXShader::Print() {
	console->WriteVar("name",name);
	console->WriteVar("handle",handle);
//	console->WriteVar("nulldummy",nulldummy);
	if(!unused) {
		console->WriteVar("(*begin)->name",(*begin)->GetName());
		console->WriteVar("(*end)->name)",(*end)->GetName());
	}
	console->WriteVar("unused",unused);
}
