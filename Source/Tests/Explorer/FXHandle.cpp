#include "FXHandle.h"

FXHandle::FXHandle(const char* name) {
	handle = NULL;
	this->name = strdup(name);
}

FXHandle::~FXHandle() {
}
