#include "Console.h"
#include "Dirlight.h"
#include "Renderer.h"
#include "Options.h"

Dirlight::Dirlight(float3 dir,float3 color,bool enabled) {
	this->dir = dir.Normalize();
	this->color = color;
	this->enabled = enabled;
	
	up = float3(0.0f,1.0f,0.0f);
	lookat = float3(0.0f,0.0f,0.0f);
	castshadows = false; // slow, so off by default
	scene->lights->AddDirlight(this); // add to global dump
}
Dirlight::~Dirlight() {
	scene->lights->DeleteDirlight(this);
}

float3 Dirlight::GetColor() {
	return color;
}
float3 Dirlight::GetDirection() {
	return dir;
}
float3 Dirlight::GetLookat() {
	return lookat;
}
float3 Dirlight::GetUp() {
	return up;
}
bool Dirlight::GetEnabled() {
	return enabled;
}
bool Dirlight::GetCastShadows() {
	return castshadows;
}
void Dirlight::SetDirection(float3 dir) {
	this->dir = dir;
	this->dir.Normalize();
}
void Dirlight::SetColor(float3 color) {
	scene->lights->BeginUpdate();
	
	this->color = color;
	
	scene->lights->EndUpdate();
}
void Dirlight::SetEnabled(bool enabled) {
	if(this->enabled != enabled) {
		scene->lights->BeginUpdate();
		
		this->enabled = enabled;
		
		scene->lights->EndUpdate();
	}
}
void Dirlight::SetCastShadows(bool castshadows) {
	if(this->castshadows != castshadows) {
		scene->lights->BeginUpdate();
		
		this->castshadows = castshadows;
		
		scene->lights->EndUpdate();
	}
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Kijken hoe groot de shadowmap moet zijn om alle te tekenen objecten te bevatten
*/
void Dirlight::GetProjection() {
	if(!castshadows || !enabled) {
		return;
	}
	
	float maxx = 1.0f;
	float maxy = 1.0f;
//	float minz = 0.0f;
//	float maxz = 1.0f;
	
	D3DXMatrixLookAtLH(&matView,&dir,&lookat,&up);
//	D3DXMatrixOrthoLH(&matProj,maxx,maxy,minz,maxz);
	D3DXMatrixOrthoLH(&matProj,maxx,maxy,-4000.0f,4000.0f);
	D3DXMatrixMultiply(&matViewProj,&matView,&matProj);

	for(std::list<Object*>::iterator i = scene->objects->begin();i != scene->objects->end();i++) {
		Object* object = *i;
		
		// Don't stretch frustum to fit these
		if(!object->castshadows) {
			continue;
		}
		
		float3 modeldirvector = object->worldcenter - camera->GetPos();
		float modeldirlength = std::max(0.0f,modeldirvector.Length() - object->worldr);
		
		// Also don't stretch frustum to fit these
		if(modeldirlength < options->shadowdistance) {
			
			// World * View * Proj = Screenpos
			float3 screenpos = object->worldcenter.Transform(matViewProj);

			// http://stackoverflow.com/questions/3717226/radius-of-projected-sphere
			float screenradius = object->worldr / screenpos.z; // assume 180 degrees viewing angle
			
			// Test four points of our sphere: top left bottom right
			float3 points[4];
			points[0] = screenpos - float3(screenradius,0,0); // left
			points[1] = screenpos + float3(0,screenradius,0); // top
			points[2] = screenpos + float3(screenradius,0,0); // right
			points[3] = screenpos - float3(0,screenradius,0); // bottom
			
			for(int i = 0;i < 4;i++) {
				maxx = std::max(maxx,fabsf(points[i].x));
				maxy = std::max(maxy,fabsf(points[i].y));
//				if(screenpos.z >= 0) {
//					maxz = std::max(maxz,points[i].z);
//				} else {
//					minz = std::min(minz,points[i].z);
//				}
			}
		}
	}
	
	// En maak de nieuwe matrix aan
	D3DXMatrixOrthoLH(&matProj,maxx,maxy,-4000.0f,4000.0f); // we have depth to spare, so add some buffer
	D3DXMatrixMultiply(&matViewProj,&matView,&matProj);
}

void Dirlight::Print() {
	
	console->Write("\r\n----- Info for class Dirlight -----\r\n\r\n");
	
	console->WriteVar("dir",dir);
	console->WriteVar("lookat",lookat);
	console->WriteVar("up",up);
	console->WriteVar("color",color);
	console->WriteVar("enabled",enabled);
	console->WriteVar("castshadows",castshadows);
	console->WriteVar("matView",matView);
	console->WriteVar("matProj",matProj);
	console->WriteVar("matViewProj",matViewProj);
	
	console->Write("\r\n----- End of info -----\r\n\r\n");
}
