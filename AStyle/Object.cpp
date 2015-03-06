#include "Object.h"

Object::Object(const char* name) {
	updatecount = 0;
	BeginUpdate();
	{
		// Create separate material class
		material = new Material();

		// Clear properties
		Reset();

		// Use name we received
		SetName(name);

		// Calculate some properties after setting up

	}
	EndUpdate();

	// Add ourself to the global dump
	scene->objects->Add(this);
}
Object::Object(const char* objectpath,const float3& pos,const float3& rot,float scale) {
	updatecount = 0;
	BeginUpdate();
	{
		// Create separate material class
		material = new Material();

		// Clear properties
		Reset();

		// Load everything from the .object file
		LoadFromFile(objectpath);

		// Set position separately
		SetTranslation(pos);
		SetRotationDeg(rot); // only difference between one below
		SetScaling(scale);
	}
	EndUpdate();

	// Add ourself to the global dump
	scene->objects->Add(this);
}
Object::Object(const char* objectpath,const float3& pos,const float4x4& rot,float scale) {
	updatecount = 0;
	BeginUpdate();
	{
		// Create separate material class
		material = new Material();

		// Clear properties
		Reset();

		// Load everything from .object file
		LoadFromFile(objectpath);

		// Set position
		SetTranslation(pos);
		SetRotation(rot); // only difference between one above
		SetScaling(scale);
	}
	EndUpdate();

	// Add ourself to the global dump
	scene->objects->Add(this);
}
Object::Object(const char* name,const char* modelpath,const char* materialpath,const float3& pos,const float3& rot,float scale) {
	updatecount = 0;
	BeginUpdate();
	{
		// Create separate material class
		material = new Material();

		// Clear properties
		Reset();

		// Set name directly instead of from .object file
		SetName(name);

		// Set position
		SetTranslation(pos);
		SetRotationDeg(rot);
		SetScaling(scale);

		// Get full model path
		char fullmodelpath[MAX_PATH];
		GetFullPath(modelpath,"Data\\Models",fullmodelpath);

		// Add top level detail from model path
		AddDetailLevel(models->Add(fullmodelpath,true),50);

		// Add other detail levels from .lodX files
		char lodfilename[MAX_PATH];
		if(FileExists(ChangeFileExt(fullmodelpath,".lod1.obj",lodfilename))) {
			AddDetailLevel(models->Add(lodfilename,true),250);
		}
		if(FileExists(ChangeFileExt(fullmodelpath,".lod2.obj",lodfilename))) {
			AddDetailLevel(models->Add(lodfilename,true),1000);
		}
		if(FileExists(ChangeFileExt(fullmodelpath,".lod3.obj",lodfilename))) {
			AddDetailLevel(models->Add(lodfilename,true)); // FLT_MAX
		}

		// Set material from material path
		material->LoadFromFile(materialpath);
	}
	EndUpdate();

	// Add ourself to the global dump
	scene->objects->Add(this);
}
Object::Object(const char* name,const char* modelpath,const char* materialpath,const float3& pos,const float4x4& rot,float scale) {
	updatecount = 0;
	BeginUpdate();
	{
		// Create separate material class
		material = new Material();

		// Clear properties
		Reset();

		// Set name directly instead of from .object file
		SetName(name);

		// Set position
		SetTranslation(pos);
		SetRotation(rot); // only difference from item above
		SetScaling(scale);

		// Get full model path
		char fullmodelpath[MAX_PATH];
		GetFullPath(modelpath,"Data\\Models",fullmodelpath);

		// Add top level detail from model path
		AddDetailLevel(models->Add(fullmodelpath,true),50);

		// Add other detail levels from .lodX files
		char lodfilename[MAX_PATH];
		if(FileExists(ChangeFileExt(fullmodelpath,".lod1.obj",lodfilename))) {
			AddDetailLevel(models->Add(lodfilename,true),250);
		}
		if(FileExists(ChangeFileExt(fullmodelpath,".lod2.obj",lodfilename))) {
			AddDetailLevel(models->Add(lodfilename,true),1000);
		}
		if(FileExists(ChangeFileExt(fullmodelpath,".lod3.obj",lodfilename))) {
			AddDetailLevel(models->Add(lodfilename,true)); // FLT_MAX
		}

		// Set material from material path
		material->LoadFromFile(materialpath);
	}
	EndUpdate();

	// Add ourself to the global dump
	scene->objects->Add(this);
}
Object::~Object() {
	delete[] name;
	for(unsigned int i = 0; i < detaillevels.size(); i++) {
		delete detaillevels[i]; // do not delete model though
	}
	delete animation;
	// we do not own boundingmodel
	delete material;

	// Remove ourselves from the global dump
	scene->objects->Delete(this);
}
void Object::Reset() {
	ClearDetailLevels();
	boundingmodel = NULL;

	matRotation.Identity();
	matTranslation.Identity();
	matScaling.Identity();
	matWorld.Identity();
	matWorldInverse.Identity();

	worldcenter = float3(0,0,0);
	worldr = 0.0f;

	animation = NULL;

	castshadows = true;

	name = NULL; // TODO: memory leak!

	OnClick = NULL;

	material->Clear();
}
void Object::BeginUpdate() {
	updatecount++;
}
void Object::EndUpdate() {
	updatecount--;
	if(updatecount == 0) {
		Update();
	}
}
void Object::LoadFromFile(const char* objectpath) {
	BeginUpdate();
	{
		// Reset all
		Reset();

		// Dump for line parts
		char line[1024];
		char word1[512];
		char word2[512];
		float token3;

		// Get absolute path
		char fullobjectpath[MAX_PATH];
		GetFullPath(objectpath,"Data\\Objects",fullobjectpath);

		// Try to open file
		FILE* objectfile = fopen(fullobjectpath,"r");
		if(objectfile == NULL) {
			console->Write("Error opening object file '%s'\r\n",fullobjectpath);
			return;
		}

		// Read it
		while(fgets(line,sizeof(line),objectfile)) {
			if(sscanf(line,"%[^ #\n]",word1) == 1) {
				if(!strcmp(word1,"name")) {
					if(sscanf(line,"name %s",word2) == 1) {
						SetName(word2);
					} else {
						console->Write("Error reading line:\r\n%s\r\n",line);
					}
				} else if(!strcmp(word1,"material")) {
					if(sscanf(line,"material %s",word2) == 1) {
						material->LoadFromFile(word2);
					} else {
						console->Write("Error reading line:\r\n%s\r\n",line);
					}
				} else if(!strcmp(word1,"castshadows")) {
					if(sscanf(line,"castshadows %s",word2) == 1) {
						castshadows = (bool)atoi(word2);
					} else {
						console->Write("Error reading line:\r\n%s\r\n",line);
					}
				} else if(!strcmp(word1,"detaillevel")) {
					if(sscanf(line,"detaillevel %s %f",word2,&token3) == 2) {
						AddDetailLevel(models->Add(word2,true),token3);
					} else {
						console->Write("Error reading line:\r\n%s\r\n",line);
					}
				} else if(!strcmp(word1,"boundingmodel")) {
					if(sscanf(line,"boundingmodel %s",word2) == 1) {
						boundingmodel = models->Add(word2,false); // don't send to GPU to save time
					} else {
						console->Write("Error reading line:\r\n%s\r\n",line);
					}
				} else {
					console->Write("Unknown command \"%s\" in file \"%s\"\r\n",word1,line);
				}
			}
		}
		fclose(objectfile);
	}
	EndUpdate();
}
void Object::SetName(const char* text) {
	delete[] name;
	name = strdup(text);
}
void Object::SetAnimation(Animation* animation) {

	// animation stops, don't delete it
	if(animation == NULL) {
		this->animation = NULL;
		return;
	}

	// animation is swapped with another? delete old
	if(this->animation) {
		delete this->animation;
	}

	// apply new
	this->animation = animation;
}
float3 Object::GetTranslation() {
	return float3(matTranslation._41,
	              matTranslation._42,
	              matTranslation._43);
}
float4x4 Object::GetTranslationMatrix() {
	return matTranslation;
}
float3 Object::GetScaling() {
	return float3(matScaling._11,
	              matScaling._22,
	              matScaling._33);
}
float4x4 Object::GetScalingMatrix() {
	return matScaling;
}
float4x4 Object::GetRotationMatrix() {
	return matRotation;
}
float4x4 Object::GetWorldTransformMatrix() {
	return matWorld;
}
float4x4 Object::GetInvWorldTransformMatrix() {
	return matWorldInverse;
}
void Object::SetRotation(const float3& rotation) {
	BeginUpdate();
	matRotation.EulerRotation(rotation);
	EndUpdate();
}
void Object::SetRotationDeg(const float3& rotation) {
	BeginUpdate();
	matRotation.EulerRotationDeg(rotation);
	EndUpdate();
}
void Object::SetRotation(const float4x4& rotation) {
	BeginUpdate();
	matRotation = rotation; // ez
	EndUpdate();
}
void Object::SetTranslation(const float3& translation) {
	BeginUpdate();
	matTranslation.Translation(translation);
	EndUpdate();
}
void Object::SetTranslation(const float4x4& translation) {
	BeginUpdate();
	matTranslation = translation;
	EndUpdate();
}
void Object::SetScaling(float scaling) {
	BeginUpdate();
	matScaling.Scaling(float3(scaling));
	EndUpdate();
}
void Object::Move(const float3& dir) {
	SetTranslation(GetTranslation() + dir);
}
void Object::AddDetailLevel(const char* modelpath) {
	AddDetailLevel(new DetailLevel(models->Add(modelpath,true),FLT_MAX));
}
void Object::AddDetailLevel(Model* model) {
	AddDetailLevel(new DetailLevel(model,FLT_MAX));
}
void Object::AddDetailLevel(Model* model,float maxdistance) {
	AddDetailLevel(new DetailLevel(model,maxdistance));
}
void Object::AddDetailLevel(DetailLevel* detaillevel) {
	for(int i = detaillevels.size()-1; i >= 0; i++) {
		if(detaillevels[i]->maxdistance < detaillevel->maxdistance) {
			detaillevels.insert(detaillevels.begin()+i+1,detaillevel);
			return;
		}
	}

	// if all other lods are used for higher distances, dump at the front
	detaillevels.insert(detaillevels.begin(),detaillevel);
}
void Object::ClearDetailLevels() {
	for(unsigned int i = 0; i < detaillevels.size(); i++) {
		delete detaillevels[i];
	}
	detaillevels.clear();
}
DetailLevel* Object::GetDetailLevel(float distance) {
	if(detaillevels.size() == 1) {
		return detaillevels[0];
	}
	for(int i = detaillevels.size()-2; i >= 0; i++) {
		if(detaillevels[i]->maxdistance < distance) {
			return detaillevels[i+1];
		}
	}
	return NULL;
}
void Object::Update() {
	matWorld = matRotation*matScaling*matTranslation;
	matWorldInverse = matWorld.Inverse();

	// Get world data from first LOD
	DetailLevel* firstlod = GetDetailLevel(0);
	if(firstlod) {
		worldr = firstlod->model->r;
		worldcenter = firstlod->model->center; // verandert bij rotate...
	} else {
		console->Write("Cannot analyse object '%s' because it has no LODs\r\n",name);
		worldr = 0;
		worldcenter = 0;
		visible = false;
		return;
	}

	// Apply scaling and transformation to approximation
	worldr *= GetScaling().x;
	worldcenter = worldcenter.Transform(matWorld);

	// Check if object is still visible by applying frustrum culling
	visible = camera->IsVisible(this); // reapplied on camera move
}
float3 Object::GetWorldCenter() {
	return worldcenter;
}
float Object::GetWorldRadius() {
	return worldr;
}
bool Object::IsVisible() {
	return visible;
}
void Object::SetVisible(bool value) {
	this->visible = value;
}
void Object::Print() {

	console->Write("\r\n----- Info for class Object -----\r\n\r\n");

	for(unsigned int i = 0; i < detaillevels.size(); i++) {
		console->WriteVar("maxdistance",detaillevels[i]->maxdistance);
		detaillevels[i]->model->Print();
	}

	// textures, tiling, diffuse, specular, shininess, shader, visible
	material->Print();

	console->WriteVar("matRotation",matRotation);
	console->WriteVar("matTranslation",matTranslation);
	console->WriteVar("matScaling",matScaling);
	console->WriteVar("matWorld",matWorld);
	console->WriteVar("worldcenter",worldcenter);
	console->WriteVar("worldr",worldr);
	console->WriteVar("visible",visible);
	console->WriteVar("name",name);
	console->WriteVar("castshadows",castshadows);

	console->Write("\r\n----- End of info -----\r\n\r\n");
}
bool CompareObject(Object* a,Object* b) {
	return a->material->shaderindex < b->material->shaderindex;
}
Animation* Object::GetAnimation() {
	return animation;
}
const char* Object::GetName() {
	return name;
}
Model* Object::GetBoundingModel() {
	if(boundingmodel) { // separate bounding model
		return boundingmodel;
	} else {
		DetailLevel* firstlod = GetDetailLevel(0); // otherwise, select first LOD (slowest)
		if(firstlod) {
			return firstlod->model;
		} else {
			return NULL;
		}
	}
}
Collision Object::IntersectSphere(float3 worldpos,float3 worlddir) {

	Collision result = {0};

	// Ray dir
	float3 r1 = worlddir;

	// Ray pos
	float3 r0 = worldpos;

	// Sphere: (p - p0) dot (p - p0) = r*r
	// Ray: r0 + t * r1
	// Solve for t: (r0 + t * r1 - p0) dot (r0 + t * r1 - p0) - r*r = 0

	float3 p0 = worldcenter;
	float3 r0minusp0 = r0 - p0;

	// a = r1 dot r1
	float a = r1.Dot(r1);

	// b = 2 * r1 dot (r0 - p0)
	float b = 2.0f * r1.Dot(r0minusp0);

	// c = (r0 - p0) dot (r0 - p0) - r^2
	float c = r0minusp0.Dot(r0minusp0) - worldr*worldr;

	// ABC
	float d = b*b - 4.0f * a * c;

	// d >= 0 -> pass
	if(d >= 0.0f) {
		result.object = this;
		result.t = (-b - sqrt(d))/(2.0f * a); // return lowest time
		result.point = worldpos + result.t * worlddir;
	}

	return result;
}
Collision Object::IntersectModel(float3 worldpos,float3 worlddir) {

	Collision result = {0};

	// Ray dir
	float3 r1 = worlddir;

	// Ray pos
	float3 r0 = worldpos;

	float minimumtime = 1.0f; // 1 means end of ray

	Model* model = GetBoundingModel();
	if(!model) {
		console->Write("Cannot intersect object %s without mesh\r\n",name);
		return result; // object without a mesh assigned to it
	}

	// http://www.lighthouse3d.com/tutorials/maths/ray-triangle-intersection/
	float3 r0obj = r0.Transform(matWorldInverse);
	float3 r1obj = r1.TransformNormal(matWorldInverse);

	// Walk the index buffer
	for(unsigned int j = 0; j < 3 * model->numfaces; j+=3) {

		// Ga alle triangles af...
		float3 p0 = model->localvertexbuffer[model->localindexbuffer[j]].pos;
		float3 p1 = model->localvertexbuffer[model->localindexbuffer[j+1]].pos;
		float3 p2 = model->localvertexbuffer[model->localindexbuffer[j+2]].pos;

		// Triangle: (1 - u - v) * p0 + u * p1 + v * p2
		// Ray: r0 + t * r1
		// [-r1,p1 - p0,p2 - p0][t,u,v]^T = r0 - p0]
		// inverteer, cramer:

		// Edge vectors
		float3 E1 = p1 - p0;
		float3 E2 = p2 - p0;

		// Recurring factors
		float3 T = r0obj - p0;
		float3 P = r1obj.Cross(E2);
		float3 Q = T.Cross(E1);

		// Inverse van determinant, handig voor berekenen inverse
		float recipdet = 1.0f/(P.Dot(E1));

		// Bereken zo weinig mogelijk
		float u = recipdet * (P.Dot(T));
		float v = recipdet * (Q.Dot(r1obj));
		if(u >= 0.0f and v >= 0.0f and u + v <= 1.0f) {

			float t = recipdet * (Q.Dot(E2));
			if(t >= 0.0f and t < minimumtime) {
				result.object = this;
				minimumtime = t;
			}
		}
	}

	if(result.object) { // collision van triangle!
		result.t = minimumtime;
		result.point = r0 + result.t * r1; // 1x berekenen
	}

	return result;
}
