#include "Objects.h"
#include "Object.h"
#include "TimeEvent.h"
#include "Console.h"
#include "Material.h"
#include "float4x4.h"

Objects::Objects() {
	updatecount = 0;
	OnUpdate = new TimeEvent();
}
Objects::~Objects() {
	delete OnUpdate;
	BeginUpdate(); // make sure Update doesn't happen
	Clear();
}
Object* Objects::Add(Object* object) {
	BeginUpdate();
	{
		list.push_back(object);
		object->bufferlocation = --list.end();
	}
	EndUpdate(); // sort
	return object;
}
//Object* Objects::AddPlane(const char* name,const char* matpath,const float3& pos,const float3& rot,float edgelen,unsigned int tiling,unsigned int textiling,Heightmap* height) {
//	Object* object = new Object(name);
//
//	// Worldposmatrix setten
//	object->SetTranslation(pos);
//	object->SetRotationDeg(rot);
//	object->SetScaling(1.0f);
//
//	// Load plane to GPU
//	Model* plane = models->Add();
//	plane->LoadPlane(tiling,textiling,edgelen,height);
//	plane->SendToGPU();
//
//	// Assign to all LODs
//	object->AddDetailLevel(plane);
//
//	// Daarna de texture instellen
//	object->material->LoadFromFile(matpath);
//	object->Update();
//
//	return object;
//}
Object* Objects::GetByName(const char* name) {
	for(std::list<Object*>::iterator i = begin(); i != end(); i++) {
		if(!strcmp((*i)->name,name)) {
			return *i;
		}
	}
	return NULL;
}
void Objects::Delete(Object* thisobject) {
	if(thisobject) {
		BeginUpdate();
		list.erase(thisobject->bufferlocation);
		EndUpdate();
	}
}
void Objects::Clear() {
	BeginUpdate();
	{
		std::list<Object*>::iterator i = list.begin();
		while(i != list.end()) { // Make sure we can remove items while iterating
			std::list<Object*>::iterator next = std::next(i); // Store next item iterator (as current will be invalidated)
			delete *i; // Delete current
			i = next; // Goto next
		}
	}
	EndUpdate();
}
void Objects::Update() {
	list.sort(CompareObject); // Sorteer de objects om batches te maken
	OnUpdate->Execute(0); // Apply frustrum culling for example
}
void Objects::BeginUpdate() {
	updatecount++;
}
void Objects::EndUpdate() {
	updatecount--;
	if(updatecount == 0) {
		Update();
	}
}
std::list<Object*>::iterator Objects::begin() {
	return list.begin();
}
std::list<Object*>::iterator Objects::end() {
	return list.end();
}
unsigned int Objects::size() {
	return list.size();
}
void Objects::Print() {
	console->WriteVar("list.size()",(int)list.size());
	for(std::list<Object*>::iterator i = begin(); i != end(); i++) {
		(*i)->Print();
	}
}
