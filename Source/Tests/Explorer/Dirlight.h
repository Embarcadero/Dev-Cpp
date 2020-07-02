#ifndef DIRLIGHT_INCLUDE
#define DIRLIGHT_INCLUDE

#include <list>
#include "float3.h"
#include "float4x4.h"

class Lights;

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

class DLLIMPORT Dirlight {
	friend class Lights;
	
	float3 dir;
	float3 lookat;
	float3 up;
	float3 color;
	bool enabled;
	bool castshadows;
	
	// Matrices voor de renderer
	float4x4 matView;
	float4x4 matProj;
	float4x4 matViewProj;
	
	std::list<Dirlight*>::iterator bufferlocation;
	public:
		Dirlight(float3 dir,float3 color,bool enabled);
		~Dirlight();
	
		float3 GetColor();
		float3 GetDirection();
		float3 GetLookat();
		float3 GetUp();
		bool GetEnabled();
		bool GetCastShadows();
		void SetDirection(float3 dir);
		void SetColor(float3 color);
		void SetEnabled(bool enabled);
		void SetCastShadows(bool castshadows);
		
		// Create VP projection so that whole scene fits into frustum
		void GetProjectionPoint();
		void GetProjection();
		
		void Print();
};

#endif
