#ifndef PANEL_INCLUDE
#define PANEL_INCLUDE

#include <windows.h>
#include <vector>
using std::vector;
//#include "Renderer.h"
//#include "Console.h"
#include "Model.h"
#include "float4.h"
#include "float4x4.h"

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

enum ComponentType {
    ctBase,
    ctButton,
    ctLabel,
    ctWindow,
    ctEdit,
    ctBevel,
    ctDropdown,
};

class DLLIMPORT Component {
	private:
		bool visible;
		int left;
		int top;
		int width;
		int height;
	public:
		Component(int left,int top,int width,int height);
		~Component();
		void GetRect(RECT* result);
		Model* plane;
		void CreatePlane();
		void Move(int dx,int dy);
		void SetPos(int x,int y);
		void Resize(int width,int height);
		void AddChild(Component* child);
		bool IsVisible();
		void SetVisible(bool value);
		void OnResetDevice();

		// TODO: make private?
		Component* parent;

		int absleft;
		int abstop;
		float4 backcolor;
		ComponentType type;
		float4x4 matWorld;
		vector<Component*> children;

		// Listeners
		void (*OnShow)(Component* Sender); // TODO: TimeEvent?
};

#endif
