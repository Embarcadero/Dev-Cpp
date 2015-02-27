#include "Key.h"

Key::Key(WPARAM key) {
	this->key = key;
	OnClick = NULL;
	down = false;
}

Key::Key(WPARAM key,void (*OnClick)(void* data)) {
	this->key = key;
	this->OnClick = OnClick;
	down = false;
}

Key::~Key() {
}

bool Key::Press() {
	if(down) {
		return false; // already down
	} else {
		down = true;
		if(OnClick) {
			OnClick(NULL);
		}
		return true; // key went down
	}
}

bool Key::Release() {
	if(down) {
		down = false;
		return true; // key went back up
	} else {
		return false;
	}
}

bool Key::IsDown() {
	return down;
}
