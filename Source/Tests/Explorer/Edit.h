#ifndef EDIT_INCLUDE
#define EDIT_INCLUDE

#include "Label.h"

class DLLIMPORT Edit : public Label {
	public:
		Edit(int left,int top,int width,int height,const char* text);
		~Edit();
		
		// Andere kleur als we aan het typen zijn
		float4 focuscolor;
		bool focused;
		
		// Function pointer voor enter
		void (*OnReturn)(Component* Sender);
		
		// Rendering
		void DrawText(LPD3DXFONT font,LPD3DXSPRITE sprite);
};

#endif
