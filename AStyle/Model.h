#ifndef MODEL_INCLUDE
#define MODEL_INCLUDE

#include <list>
#include <cstdio>
#include "Renderer.h"
#include "Console.h"
#include "Heightmap.h"
#include "Resource.h"

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

struct VERTEX {
	float3 pos;
	float2 tex;
	float3 nor;
	float3 tan;
	float3 bin;
};

class DLLIMPORT Model {
	private:
		Model(); // create empty shell
		Model(const char* filename,bool sendtogpu); // load OBJ
		~Model();

		friend class Models; // manages creation and destruction
		std::list<Model*>::iterator bufferlocation;
	public:
		__int64 GetSize();
		void FreeBuffers();
		void CreateTangents();
		void Optimize();
		void SendToGPU();
		void GetBoundingSphere();
		void LoadFromOBJ(const char* filename);
		void Load2DQuad(float left,float top,float right,float bottom);
		void LoadPlane(unsigned int tiling,unsigned int textiling,float edgelen,
		               Heightmap* height);
		void LoadBuffer(VERTEX* vb,unsigned int* ib,unsigned int numvertices,
		                unsigned int numindices);
		void LoadParticle();
		void Print();

		LPDIRECT3DVERTEXBUFFER9 vertexbuffer; // GPU ram copies
		LPDIRECT3DINDEXBUFFER9 indexbuffer; // GPU ram copies
		float r;
		float3 center;
		VERTEX* localvertexbuffer; // copies for CPU access
		unsigned int* localindexbuffer; // copies for CPU access
		unsigned int numvertices;
		unsigned int numfaces;
		char* fullpath;
		char* filename;
};

#endif
