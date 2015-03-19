#include "Skybox.h"
#include "Lights.h"
#include "Camera.h"
#include "Renderer.h"
#include "Object.h"
#include "Dirlight.h"
#include "Scene.h"

Skybox::Skybox() {
	skybox = NULL;
	sunobject = NULL;
	sunlight = NULL;
}
Skybox::~Skybox() {
	Clear();
}

void Skybox::Clear() {
	if(skybox) {
		scene->DeleteObject(skybox);
	}
	if(sunobject) {
		scene->DeleteObject(sunobject);
	}
	delete sunlight;
	scene->lights->SetAmbientlight(0); // TODO: remember old value
}
void Skybox::SetSkybox(const char* modelpath,const char* materialpath) {
	if(skybox) {
		scene->DeleteObject(skybox); // TODO: reuse
	}
	skybox = scene->AddObject("Skybox",modelpath,materialpath,0,0,1);
	skybox->castshadows = false;
}
void Skybox::SetSunlight(const char* modelpath,const char* materialpath) {
	if(sunobject) {
		scene->DeleteObject(sunobject); // TODO: reuse
	}
	sunobject = scene->AddObject("Sunobject",modelpath,materialpath,0,0,1);
	sunobject->castshadows = false;

	// Add light source too
	if(!sunlight) {
		sunlight = new Dirlight(
		    float3(0,0,0),
		    float3(1.4,1.4,1.2),
		    false);
		sunlight->SetCastShadows(true);
	}

	// Add some skylight
	scene->lights->SetAmbientlight(0.05);
}
void Skybox::OnLostDevice() {
}
void Skybox::OnResetDevice() {
}
void Skybox::OnUpdateTime(double dt) {
	scene->lights->BeginUpdate();
	{
		// Get skybox radius
		float skyboxradius;
		if(skybox) {
			skyboxradius = skybox->GetWorldRadius();
		} else {
			skyboxradius = 4000.0f;	// default, 4000 is max view distance by default
		}

		// Beweeg zonlicht
		sunlight->SetDirection(float3(-0.65f,
		                              -cos((2.0f*D3DX_PI/(60.0f * 60 * 24))*renderer->GetTime()),
		                              -sin((2.0f*D3DX_PI/(60.0f * 60 * 24))*renderer->GetTime())));

		// roteer plane zonobject naar camera
		sunobject->SetWorldTransForm(camera->GetMatViewInverse());

		// Behoud translate
		float3 worldpos = 0.95f*sunlight->GetDirection()*skyboxradius + camera->GetPos();
		sunobject->matWorld._41 = worldpos.x;
		sunobject->matWorld._42 = worldpos.y;
		sunobject->matWorld._43 = worldpos.z;

		// Onthoud ook waar het midden zit
		sunobject->worldcenter = sunobject->worldcenter.Transform(sunobject->matWorld);

		// Skybox centreren op camerapos
		if(skybox) {
			skybox->SetTranslation(camera->GetPos());
		}

		// Update mixing between day time and nighttime texture
		float sunheight = sunlight->GetDirection().y;
		if(skybox) {
			skybox->material->mixer = float3(
			                              std::max(0.0f,1.5f*sunheight + 0.4f),
			                              std::max(0.0f,-sunheight + 0.8f),
			                              0);
		}

		// Disable it during the night
		if(sunheight < 0) {
			sunlight->SetEnabled(false);
		} else {
			sunlight->SetEnabled(true);
		}
	}
	scene->lights->EndUpdate(); // update GPU data once
}
void Skybox::StaticOnUpdateTime(void* sender,double data) {
	((Skybox*)sender)->OnUpdateTime(data);
}
