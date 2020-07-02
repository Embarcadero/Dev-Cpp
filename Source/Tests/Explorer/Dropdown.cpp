#include "Dropdown.h"

Dropdown::Dropdown(int left,int top,int width,int height) : Component(left,top,width,height) {
	
	dropped = false;
	
	itemheight = height;
	
	selindex = -1; // niets dus
	
	backcolor = float4(0.15f,0.15f,0.15f,1.0f);

	type = ctDropdown;
}
Dropdown::~Dropdown() {
	Clear();
}
void Dropdown::Toggle() {
	if(dropped) {
		Resize(this->width,itemheight);
		dropped = false;
	} else {
		Resize(this->width,(1 + items.size())*itemheight);
		dropped = true;
	}
}
void Dropdown::AddItem(const char* text) {
	items.push_back(std::string(text));
}
const char* Dropdown::GetItem(unsigned int index) {
	if(items.size() > index) {
		return items[index].c_str();
	} else {
		return NULL;
	}
}
const char* Dropdown::GetSelection() {
	if(selindex != -1) {
		return GetItem(selindex);
	} else {
		return NULL;
	}
}
unsigned int Dropdown::GetItemCount() {
	return items.size();
}
void Dropdown::SetSelection(const char* text) {
	
	selindex = -1;
	
	for(unsigned int i = 0;i < items.size();i++) {
		if(!strcmp(items[i].c_str(),text)) {
			selindex = i;
			break;
		}
	}
}
void Dropdown::Clear() {
	items.clear();
}
void Dropdown::DrawText(LPD3DXFONT font,LPD3DXSPRITE sprite) {
	
	// Maak een rect van alleen het bovenste deel
	RECT textrect;
	GetRect(&textrect);
	textrect.bottom = abstop + itemheight;

	// teken de huidige selectie
	sprite->Begin(D3DXSPRITE_ALPHABLEND);
	if(selindex != -1) {
		font->DrawText(sprite,GetSelection(),-1,&textrect,DT_NOCLIP|DT_CENTER|DT_VCENTER,D3DCOLOR_XRGB(255,255,255));
	}

	// teken hele lijst labels eronder als we uitklappen
	if(dropped) {
		for(unsigned int i = 0;i < GetItemCount();i++) {
			textrect.top += itemheight;
			textrect.bottom += itemheight;
			font->DrawText(sprite,GetItem(i),-1,&textrect,DT_NOCLIP|DT_CENTER|DT_VCENTER,D3DCOLOR_XRGB(255,255,255));
		}
	}
	sprite->End();
}
