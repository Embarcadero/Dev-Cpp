#ifndef DROPDOWN_INCLUDE
#define DROPDOWN_INCLUDE

#include "Component.h"

#include <string>

class DLLIMPORT Dropdown : public Component {
	std::vector<std::string> items;
	public:
		Dropdown(int left,int top,int width,int height);
		~Dropdown();
		
		int itemheight;
		bool dropped;
		int selindex;
		
		// Collapse or not to collapse
		void Toggle();
		void AddItem(const char* text);
		const char* GetItem(unsigned int index);
		const char* GetSelection();
		unsigned int GetItemCount();
		void Clear();
		void SetSelection(const char* text);
		void DrawText(LPD3DXFONT font,LPD3DXSPRITE sprite);
};

#endif
