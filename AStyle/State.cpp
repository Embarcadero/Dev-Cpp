#include "State.h"

State::State(D3DRENDERSTATETYPE state) {
	this->renderstate = state;
	this->value = 0;
}
State::~State() {
}
DWORD State::Get() {
	return value;
}
void State::Set(DWORD value) {
	if(this->value != value) {
		this->value = value;
		d3ddev->SetRenderState(renderstate,value);
	}
}
