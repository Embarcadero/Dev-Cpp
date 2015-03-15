#ifndef OBJECT_INCLUDE
#define OBJECT_INCLUDE

#include <cfloat>
#include <cstdio>
#include <vector>
using std::vector;
#include <list>
using std::list;
#include "float3.h"
#include "float4x4.h"

#if BUILDING_DLL
#define DLLIMPORT __declspec(dllexport)
#else
#define DLLIMPORT __declspec(dllimport)
#endif

class Object;
class Model;
class Animation;
class Material;

struct DLLIMPORT Collision {
	Object* object;
	float3 point;
	float t;
};

class DLLIMPORT DetailLevel {
	public:
		DetailLevel(Model* model,float maxdistance) {
			this->model = model;
			this->maxdistance = maxdistance;
		}
		Model* model;
		float maxdistance;
};

class Object;
typedef std::list<Object*>::iterator ObjectIterator;

class DLLIMPORT Object {
	private:
		friend class Objects;
		char* name;
		Animation* animation; // remove when deleting object
		vector<DetailLevel*> detaillevels;
		ObjectIterator bufferlocation;
		float3 worldcenter; // calculated on Update
		float worldr; // calculated on Update
		bool visible; // calculated on Update and camera change
		float4x4 matTranslation;
		float4x4 matScaling;
		float4x4 matRotation;
		float4x4 matWorld;
		float4x4 matWorldInverse;
		unsigned int updatecount;
		void Update();
		Object(const char* name);
		Object(const char* objectpath,const float3& pos,const float4x4& rot,float scale); // from .object file
		Object(const char* name,const char* modelpath,const char* materialpath,const float3& pos,const float4x4& rot,float scale); // from .obj + .mtl
		~Object();
	public:

		void LoadFromFile(const char* objectpath);
		Collision IntersectSphere(float3 worldpos,float3 worlddir);
		Collision IntersectModel(float3 worldpos,float3 worlddir);
		Animation* GetAnimation();
		const char* GetName();
		Model* GetBoundingModel();
		float3 GetTranslation();
		float4x4 GetTranslationMatrix();
		float3 GetScaling();
		float4x4 GetScalingMatrix();
		//float3 GetRotation();
		float4x4 GetRotationMatrix();
		float4x4 GetWorldTransformMatrix();
		float4x4 GetInvWorldTransformMatrix();
		void ClearDetailLevels();
		void AddDetailLevel(const char* modelpath);
		void AddDetailLevel(Model* model);
		void AddDetailLevel(Model* model,float maxdistance);
		void AddDetailLevel(DetailLevel* detaillevel);
		DetailLevel* GetDetailLevel(float distance);
		void SetTranslation(const float3& pos);
		void SetTranslation(const float4x4& pos);
		void SetRotation(const float3& rotation);
		void SetRotationDeg(const float3& rotation);
		void SetRotation(const float4x4& rotation);
		void SetScaling(float scaling);
		void Move(const float3& dir);
		void SetName(const char* text);
		void SetAnimation(Animation* animation);
		void BeginUpdate();
		void EndUpdate();
		void Print();
		void Reset();
		float3 GetWorldCenter();
		float GetWorldRadius();
		bool IsVisible();
		void SetVisible(bool value);

		// Events
		void (*OnClick)(Object* Sender);

		// TODO: private?
		bool castshadows;
		Model* boundingmodel;
		Material* material;
};

bool CompareObject(Object* a,Object* b);

#endif
