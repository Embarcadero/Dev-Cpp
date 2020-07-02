#include "Edit.h"

Edit::Edit(int left,int top,int width,int height,const char* text) : Label(left,top,width,height,text,false) {
	
	focused = false;
	
	OnReturn = NULL;
	
	backcolor = float4(0.15f,0.15f,0.15f,1.0f);
	focuscolor = float4(0.5f,0.5f,0.5f,1.0f);

	type = ctEdit;
}

Edit::~Edit() {
}

void Edit::DrawText(LPD3DXFONT font,LPD3DXSPRITE sprite) {
	
	// Center it a bit
	RECT textrect;
	GetRect(&textrect);
	InflateRect(&textrect,-2,-2);
	
	// And draw
	sprite->Begin(D3DXSPRITE_ALPHABLEND);
	font->DrawText(sprite,caption.c_str(),std::min(1024,(int)caption.length()),&textrect,DT_NOCLIP,D3DCOLOR_XRGB(255,255,255));
	sprite->End();
}
