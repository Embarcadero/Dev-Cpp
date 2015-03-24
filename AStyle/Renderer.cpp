#include "Renderer.h"
#include "Object.h" // drawing
#include "State.h"
#include "Clock.h"
#include "RenderTarget.h"
#include "FXVariable.h"
#include "FXShader.h"
#include "float2.h"
#include "float3.h"
#include "float4.h"
#include "float4x4.h"
#include "TimeEvent.h"
#include "Interface.h"
#include "Camera.h"
#include "Scene.h"
#include "Material.h"
#include "Options.h"
#include "Resource.h"
#include "Console.h"
#include "Models.h"
#include "Texture.h"
#include "Lights.h"
using namespace Globals;
using namespace Utils;

Renderer::Renderer(HWND hwnd) {
	// Should remain valid
	this->hwnd = hwnd;

	// Use this to measure frame times
	frameclock = new Clock(true); // start immediately to measure startup delay?

	// Create optimized render states
	cullmodestate = new State(D3DRS_CULLMODE);
	fillmodestate = new State(D3DRS_FILLMODE);
	alphateststate = new State(D3DRS_ALPHATESTENABLE);
	alphablendstate = new State(D3DRS_ALPHABLENDENABLE);
	alpharefstate = new State(D3DRS_ALPHAREF);
	alphafuncstate = new State(D3DRS_ALPHAFUNC);
	alphasrcblendstate = new State(D3DRS_SRCBLEND);
	alphadestblendstate = new State(D3DRS_DESTBLEND);
	zenablestate = new State(D3DRS_ZENABLE);
	zwriteenablestate = new State(D3DRS_ZWRITEENABLE);

	// Create custom render targets
	AmbientDataTex = new RenderTarget();
	AmbientFactorTex = new RenderTarget();
	FloatTex1 = new RenderTarget();
	FloatTex2 = new RenderTarget();
	HalfFloatTex1 = new RenderTarget();
	HalfFloatTex2 = new RenderTarget();

	// Create time events
	OnRenderFrame = new TimeEvent();

	// Reset D3D resources
	standard = NULL;
	normalmap = NULL;
	tooltipfontwhite = NULL;
	tooltipfontshadow = NULL;
	interfacefont = NULL;
	interfacesprite = NULL;
	tooltipfontsprite = NULL;
	backbuffercolor = NULL;
	backbufferdepth = NULL;
	d3d = NULL;
	properties = {0};

	// Tooltip defaults
	tooltipmode = ttmFramerate;
	tooltip[0] = 0;

	// Time defaults
	timemulti = 1;
	time = 0.0f;
	paused = false;
	valid = false;

	// Fit render target to hwnd
	RECT rcClient;
	GetClientRect(hwnd,&rcClient);

	// Startup device using these default options
	memset(&presentparameters,0,sizeof(presentparameters));
	presentparameters.Windowed = true; // always start windowed
	presentparameters.SwapEffect = D3DSWAPEFFECT_DISCARD; // fast swapping
	presentparameters.BackBufferCount = 1;
	presentparameters.hDeviceWindow = hwnd;
	presentparameters.BackBufferFormat = D3DFMT_X8R8G8B8;
	presentparameters.EnableAutoDepthStencil = true;
	presentparameters.BackBufferWidth = rcClient.right;
	presentparameters.BackBufferHeight = rcClient.bottom;
	presentparameters.AutoDepthStencilFormat = D3DFMT_D24X8; // 24bit depth
	presentparameters.MultiSampleType = D3DMULTISAMPLE_NONE; // no anti aliasing
	presentparameters.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE; // no vsync
	presentparameters.Flags = D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL;

	// Try to connect with the GPU
	d3d = Direct3DCreate9(D3D_SDK_VERSION);
	if(!d3d) {
		Globals::console->Write("ERROR: call to Direct3DCreate9 failed\r\n");
		return;
	}

	// Check if we can use a device
	if(d3d->GetAdapterCount() > 0) {

		// Get device info
		if(d3d->GetAdapterIdentifier(0,0,&properties) == D3DERR_INVALIDCALL) {
			Globals::console->Write("ERROR: cannot get adapter info for adapter 0\r\n");
			return; // nope
		}

		// Attempt to connect
		DWORD flags;
		D3DDEVTYPE devtype;
		if(Globals::options->usesoftware) {
			devtype = D3DDEVTYPE_REF;
			flags = D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_PUREDEVICE;
		} else {
			devtype = D3DDEVTYPE_HAL;
			flags = D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_PUREDEVICE;
		}
		switch(d3d->CreateDevice(D3DADAPTER_DEFAULT,devtype,hwnd,flags,&presentparameters,&Globals::d3ddev)) {
			case D3DERR_DEVICELOST: {
				Globals::console->Write("ERROR: cannot create device 0: D3DERR_DEVICELOST\r\n");
				return;
			}
			case D3DERR_INVALIDCALL: {
				Globals::console->Write("ERROR: cannot create device 0: D3DERR_INVALIDCALL\r\n");
				return;
			}
			case D3DERR_NOTAVAILABLE: {
				Globals::console->Write("ERROR: cannot create device 0: D3DERR_NOTAVAILABLE\r\n");
				return;
			}
			case D3DERR_OUTOFVIDEOMEMORY: {
				Globals::console->Write("ERROR: cannot create device 0: D3DERR_OUTOFVIDEOMEMORY\r\n");
				return;
			}
		}
	} else {
		Globals::console->Write("ERROR: adapter count equal to 0\r\n");
		return;
	}

	// Create accelerated fonts
	// Assume it works
	HDC hdc = GetDC(hwnd);
	D3DXCreateFont(Globals::d3ddev,Utils::GetFontSizePt(12,hdc),0,0,0,0,0,OUT_TT_ONLY_PRECIS,0,0,"Consolas",&tooltipfontwhite);
	D3DXCreateFont(Globals::d3ddev,Utils::GetFontSizePt(12,hdc),0,FW_BLACK,0,0,0,OUT_TT_ONLY_PRECIS,0,0,"Consolas",&tooltipfontshadow);
	D3DXCreateFont(Globals::d3ddev,Utils::GetFontSizePt(9,hdc),0,0,0,0,0,OUT_TT_ONLY_PRECIS,0,0,"Segoe UI",&interfacefont);
	D3DXCreateSprite(Globals::d3ddev,&interfacesprite);
	D3DXCreateSprite(Globals::d3ddev,&tooltipfontsprite);
	ReleaseDC(hwnd,hdc);

	// Compile shaders once
	// Get all shader resource handles
	LPD3DXBUFFER error = NULL;
	D3DXCreateEffectFromFile(Globals::d3ddev,"Data\\Shaders\\main.fxo",0,0,D3DXFX_NOT_CLONEABLE|D3DXSHADER_PREFER_FLOW_CONTROL,0,&FX,&error);
	if(error) {
		Globals::console->Write("ERROR: cannot compile shaders: '%s'\r\n",(char*)error->GetBufferPointer());
		return;
	}

	// Main shaders
	shaders.push_back(new FXShader(FX,"PerVertex"));
	shaders.push_back(new FXShader(FX,"PureColor"));
	shaders.push_back(new FXShader(FX,"PureTexture"));
	shaders.push_back(new FXShader(FX,"PureTextureMultitexConst"));
	shaders.push_back(new FXShader(FX,"NoShade"));
	shaders.push_back(new FXShader(FX,"PerPixel"));
	shaders.push_back(new FXShader(FX,"PerPixelColor"));
	shaders.push_back(new FXShader(FX,"PerPixelSpecular"));
	shaders.push_back(new FXShader(FX,"PerPixelGooch"));
	shaders.push_back(new FXShader(FX,"PerPixelMinnaert"));
	shaders.push_back(new FXShader(FX,"PerPixelMultitexConst"));
	shaders.push_back(new FXShader(FX,"PerPixelMultitexMixer"));
	shaders.push_back(new FXShader(FX,"PerPixelOrenNayar"));
	shaders.push_back(new FXShader(FX,"PerPixelCookTorrance"));
	shaders.push_back(new FXShader(FX,"PerPixelNormal"));
	shaders.push_back(new FXShader(FX,"PerPixelNormalSpecular"));
	shaders.push_back(new FXShader(FX,"PerPixelNormalParallax"));
	shaders.push_back(new FXShader(FX,"PerPixelNormalSpecularParallax"));
	shaders.push_back(new FXShader(FX,"PerPixelNormalSpecularAmbient"));

	// Post processing shaders
	TechShadowMap = new FXShader(FX,"ShadowMap");
	TechAmbientMap = new FXShader(FX,"AmbientMap");
	TechGaussian = new FXShader(FX,"GaussFilter");
	TechSSAOFactor = new FXShader(FX,"SSAOFactor");
	TechSSAOBlur = new FXShader(FX,"SSAOBlur");
	TechBrightPass = new FXShader(FX,"BrightPass");
	TechBrightBlur = new FXShader(FX,"BrightBlur");
	TechToneMap = new FXShader(FX,"ToneMap");
	TechStock = new FXShader(FX,"Stock");

	// Variables
	// TODO: lower case
	FXMatWorld = new FXVariable(FX,"World");
	FXMatWorldView = new FXVariable(FX,"WorldView");
	FXMatWorldViewProj = new FXVariable(FX,"WorldViewProj");
	FXMatLightWorldViewProj = new FXVariable(FX,"LightWorldViewProj");

	// Misc variables
	FXMaterialdiffuse = new FXVariable(FX,"materialdiffuse");
	FXMaterialspecular = new FXVariable(FX,"materialspecular");
	FXMaterialshininess = new FXVariable(FX,"materialshininess");
	FXMaterialTiling = new FXVariable(FX,"materialtiling");
	FXMaterialMixer = new FXVariable(FX,"materialmixer");
	FXPurecolor = new FXVariable(FX,"purecolor");
	FXFilter = new FXVariable(FX,"filter");
	FXNumaniso = new FXVariable(FX,"numaniso");
	FXWidth = new FXVariable(FX,"width");
	FXHeight = new FXVariable(FX,"height");
	FXInvwidth = new FXVariable(FX,"invwidth");
	FXInvheight = new FXVariable(FX,"invheight");
	FXTimevar = new FXVariable(FX,"timevar");

	// Texture handles
	FXScreentex1 = new FXVariable(FX,"screentex1");
	FXScreentex2 = new FXVariable(FX,"screentex2");
	FXScreentex3 = new FXVariable(FX,"screentex3");
	FXDiffusetex = new FXVariable(FX,"diffusetex");
	FXSpeculartex = new FXVariable(FX,"speculartex");
	FXNormaltex = new FXVariable(FX,"normaltex");
	FXParallaxtex = new FXVariable(FX,"parallaxtex");
	FXAmbienttex = new FXVariable(FX,"ambienttex");
	FXShadowtex = new FXVariable(FX,"shadowtex");

	// Screenspacequad
	screenspacequad = Globals::models->Add();
	screenspacequad->Load2DQuad(-1.0f,-1.0f,1.0f,1.0f);
	screenspacequad->SendToGPU();

	// Yay!
	valid = true;
}
Renderer::~Renderer() {
	delete frameclock;
	delete cullmodestate;
	delete fillmodestate;
	delete alphateststate;
	delete alphablendstate;
	delete alpharefstate;
	delete alphafuncstate;
	delete alphasrcblendstate;
	delete alphadestblendstate;
	delete zenablestate;
	delete zwriteenablestate;
	delete AmbientDataTex;
	delete AmbientFactorTex;
	delete FloatTex1;
	delete FloatTex2;
	delete HalfFloatTex1;
	delete HalfFloatTex2;
	// FXVariable does not need to be freed
	// TODO: FXShader does not need to be freed?
	Utils::SafeRelease(standard);
	Utils::SafeRelease(normalmap);
	Utils::SafeRelease(tooltipfontwhite);
	Utils::SafeRelease(tooltipfontshadow);
	Utils::SafeRelease(interfacefont);
	Utils::SafeRelease(interfacesprite);
	Utils::SafeRelease(tooltipfontsprite);
	Utils::SafeRelease(backbuffercolor);
	Utils::SafeRelease(backbufferdepth);
	Utils::SafeRelease(FX);
	Utils::SafeRelease(d3d);
	Utils::SafeRelease(Globals::d3ddev); // TODO: global :(
	delete OnRenderFrame;
}
void Renderer::OnLostDevice() {
	// Update present parameters to conform to options
	presentparameters.MultiSampleType = (D3DMULTISAMPLE_TYPE)options->aasamples;
	if(presentparameters.MultiSampleType > 0) {
		presentparameters.MultiSampleQuality = options->aaquality;
	} else {
		presentparameters.MultiSampleQuality = 0;
	}
	if(!presentparameters.Windowed) {
		presentparameters.BackBufferWidth = options->backbufferwidth;
		presentparameters.BackBufferHeight = options->backbufferheight;
		presentparameters.FullScreen_RefreshRateInHz = options->refreshrate;
	} else {
		presentparameters.FullScreen_RefreshRateInHz = 0; // no limit
	}
	if(options->enablevsync) {
		presentparameters.PresentationInterval = D3DPRESENT_INTERVAL_ONE;
	} else {
		presentparameters.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
	}

	// Save saveable D3D resources (saves time)
	tooltipfontwhite->OnLostDevice();
	tooltipfontshadow->OnLostDevice();
	interfacefont->OnLostDevice();
	interfacesprite->OnLostDevice();
	tooltipfontsprite->OnLostDevice();

	// Inform other parts of engine
	FX->OnLostDevice();
	scene->OnLostDevice();
	ui->OnLostDevice();

	// Remove render targets in VRAM
	AmbientDataTex->Clear();
	AmbientFactorTex->Clear();
	FloatTex1->Clear();
	FloatTex2->Clear();
	HalfFloatTex1->Clear();
	HalfFloatTex2->Clear();

	// Delete unsavable stuff :(
	SafeRelease(backbuffercolor);
	SafeRelease(backbufferdepth);
	SafeRelease(standard);
	SafeRelease(normalmap);

	// Apply settings and start over
	d3ddev->Reset(&presentparameters);

	// Recover saveable D3D resources (saves time)
	tooltipfontwhite->OnResetDevice();
	tooltipfontshadow->OnResetDevice();
	interfacefont->OnResetDevice();
	interfacesprite->OnResetDevice();
	tooltipfontsprite->OnResetDevice();

	// Inform other parts of engine
	FX->OnResetDevice();
	scene->OnResetDevice();
	ui->OnResetDevice();

	// Recreate resources
	OnResetDevice();
}
void Renderer::OnResetDevice() {
	// Reset static options.
	alpharefstate->Set(30);
	alphafuncstate->Set(D3DCMP_GREATER);
	alphasrcblendstate->Set(D3DBLEND_SRCALPHA);
	alphadestblendstate->Set(D3DBLEND_INVSRCALPHA);

	// Recreate unsavable stuff :)
	d3ddev->GetBackBuffer(0,0,D3DBACKBUFFER_TYPE_MONO,&backbuffercolor);
	d3ddev->GetDepthStencilSurface(&backbufferdepth);
	D3DVERTEXELEMENT9 standarddeclare[] = {
		{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
		{0, 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
		{0, 20, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0},
		D3DDECL_END()
	};
	D3DVERTEXELEMENT9 normalmapdeclare[] = {
		{0, 0,  D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
		{0, 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
		{0, 20, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0},
		{0, 32, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TANGENT, 0},
		{0, 44, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BINORMAL, 0},
		D3DDECL_END()
	};
	d3ddev->CreateVertexDeclaration(standarddeclare,&standard);
	d3ddev->CreateVertexDeclaration(normalmapdeclare,&normalmap);

	// Recreate render targets in VRAM
	if(options->ssaosamples > 0) {
		AmbientDataTex->Create(presentparameters.BackBufferWidth,
		                       presentparameters.BackBufferHeight,
		                       D3DFMT_A32B32G32R32F,
		                       false); // Data, zoals normalen en diepte
		AmbientFactorTex->Create(presentparameters.BackBufferWidth,
		                         presentparameters.BackBufferHeight,
		                         D3DFMT_R32F,
		                         false); // Factor, zonder blur
	}
	if(options->enablehdr) {
		FloatTex1->Create(presentparameters.BackBufferWidth,
		                  presentparameters.BackBufferHeight,
		                  D3DFMT_A16B16G16R16F,
		                  false);
		FloatTex2->Create(presentparameters.BackBufferWidth,
		                  presentparameters.BackBufferHeight,
		                  D3DFMT_A16B16G16R16F,
		                  false);
		HalfFloatTex1->Create(presentparameters.BackBufferWidth/2,
		                      presentparameters.BackBufferHeight/2,
		                      D3DFMT_A16B16G16R16F,
		                      false);
		HalfFloatTex2->Create(presentparameters.BackBufferWidth/2,
		                      presentparameters.BackBufferHeight/2,
		                      D3DFMT_A16B16G16R16F,
		                      false);
	}

	// Reset FX constants
	const char* option = options->texturefilter; // scheelt getyp
	if(!strcmp(option,"Bilinear")) {
		FXFilter->Set((unsigned int)D3DTEXF_POINT); // D3DSAMP_MAXANISOTROPY
	} else if(!strcmp(option,"Trilinear")) {
		FXFilter->Set((unsigned int)D3DTEXF_LINEAR);
	} else if(!strcmp(option,"Anisotropic")) {
		FXFilter->Set((unsigned int)D3DTEXF_ANISOTROPIC);
	}
	FXNumaniso->Set((unsigned int)options->afsamples);

	// Back buffer dimensions
	FXWidth->Set(presentparameters.BackBufferWidth);
	FXHeight->Set(presentparameters.BackBufferHeight);
	FXInvwidth->Set(1.0f/(float)presentparameters.BackBufferWidth);
	FXInvheight->Set(1.0f/(float)presentparameters.BackBufferHeight);

	// Shadow parameters
	float offset = 0.5f + (0.5f/(float)options->shadowmapsize);
	D3DXMATRIX ShadowOffset = D3DXMATRIX(0.5f,     0.0f,     0.0f,     0.0f,
	                                     0.0f,    -0.5f,     0.0f,     0.0f,
	                                     0.0f,     0.0f,     1.0f,     0.0f,
	                                     offset, offset,     0.0f,     1.0f);
	FX->SetMatrix("ShadowOffset",&ShadowOffset);
	FX->SetFloat("invshadowmapsize",1.0f/(float)options->shadowmapsize);
	FX->SetBool("enableshadows",options->shadowmapsize > 0);
	FX->SetFloat("shadowbias",options->shadowbias);
	FX->SetFloat("shadowcoeff",0.1f);

	// Shadow parameters
	FX->SetInt("ssaosamples",options->ssaosamples);
	FX->SetFloat("ssaomultiplier",options->ssaomultiplier);
	FX->SetFloat("ssaodepthbias",options->ssaodepthbias);
	FX->SetFloat("ssaodepthmultiplier",options->ssaodepthmultiplier);
	FX->SetFloat("ssaoradius",options->ssaoradius);

	// HDR parameters
	FX->SetFloat("hdrexposure",options->hdrexposure);
	FX->SetFloat("hdrbloomthreshold",options->hdrbloomthreshold);
	FX->SetFloat("hdrbloommultiplier",options->hdrbloommultiplier);

	// Misc. parameters
	FX->SetFloat("minviewdistance",options->minviewdistance);
	FX->SetFloat("maxviewdistance",options->maxviewdistance);

	// Inform camera of back buffer change
	if(camera) {
		camera->BeginUpdate();
		camera->SetMinViewDistance(options->minviewdistance);
		camera->SetMaxViewDistance(options->maxviewdistance);
		camera->SetRatio((float)presentparameters.BackBufferWidth/(float)presentparameters.BackBufferHeight);
		camera->EndUpdate();
	}
}
void Renderer::PrintTooltip() {
	switch(tooltipmode) {
		case ttmNone: {
			break;
		}
		case ttmFramerate: {
			snprintf(tooltip,
			         sizeof(tooltip),
			         "Framerate: %.1f\n",
			         currentframe.framerate);
			break;
		}
		case ttmTiming: {
			snprintf(tooltip,
			         sizeof(tooltip),
			         "Framerate: %.1f\n",
			         "Frametime: %.3fms (%.3fms .. %.3fms)\n"
			         "Time: %02d:%02d",
			         currentframe.framerate,
			         currentframe.frametime,
			         currentframe.frametimelow,
			         currentframe.frametimehigh,
			         GetClockTimeHours(),
			         GetClockTimeMins());
			break;
		}
		case ttmTimingDevice: {
			snprintf(tooltip,
			         sizeof(tooltip),
			         "Device: %s\n"
			         "Framecount: %d\n"
			         "Framerate: %.1f\n"
			         "Frametime: %.3fms (%.3fms .. %.3fms)\n"
			         "Time: %02d:%02d",
			         properties.Description,
			         currentframe.index,
			         currentframe.framerate,
			         currentframe.frametime,
			         currentframe.frametimelow,
			         currentframe.frametimehigh,
			         GetClockTimeHours(),
			         GetClockTimeMins());
			break;
		}
		case ttmFull: {
			snprintf(tooltip,
			         sizeof(tooltip),
			         "Device: %s\n"
			         "Camera: %.4f %.4f %.4f, Angle: %.2f %.2f\n"
			         "Framecount: %d\n"
			         "Framerate: %.1f\n"
			         "Frametime: %.3fms (%.3fms .. %.3fms)\n"
			         "Triangles: %d\n"
			         "Vertices: %d\n"
			         "Draw calls: %d\n"
			         "Time: %02d:%02d (%dx)",
			         properties.Description,
			         camera->GetPos().x,
			         camera->GetPos().y,
			         camera->GetPos().z,
			         RadToDeg(camera->GetAngleH()),
			         RadToDeg(camera->GetAngleV()),
			         currentframe.index,
			         currentframe.framerate,
			         currentframe.frametime,
			         currentframe.frametimelow,
			         currentframe.frametimehigh,
			         currentframe.facecount,
			         currentframe.vertexcount,
			         currentframe.callcount,
			         GetClockTimeHours(),
			         GetClockTimeMins(),
			         timemulti);
			break;
		}
	}
}
void Renderer::SetTooltipMode(TooltipMode mode) {
	tooltipmode = mode;
	// repaint next frame
}
int Renderer::GetTimeMulti() {
	return timemulti;
}
void Renderer::SetTimeMulti(int value) {
	timemulti = std::max(0,value);
}
double Renderer::GetTime() {
	return time;
}
int Renderer::GetClockTimeSec() {
	return (int)fmod(time,60.0f);
}
int Renderer::GetClockTimeMins() {
	float timemins = floorf(time/60.0f);
	return (int)fmod(timemins,60.0f);
}
int Renderer::GetClockTimeHours() {
	float timehours = floorf(time/3600.0f);
	return (int)fmod(timehours,24.0f);
}
void Renderer::SetTime(int hours,int mins) {
	time = hours*3600 + mins*60;
}
void Renderer::AddTime(int hours,int mins) {
	time += hours*3600 + mins*60;
}
void Renderer::UpdateTime() {
	// Get length of last frame
	double frametime = frameclock->Reset();
	double gametime = frametime * timemulti;

	// Store frametime
	currentframe.frametime = frametime;

	// Store framerate averaged over 0.5sec
	if(frameinfo.size() > 0) {
		int sumcount = 0;
		double frametimesum = 0;

		// Only average the frames inside the check time (prefer newest)
		for(int i = (int)frameinfo.size() - 1; i >= 0; i--) {
			sumcount++;
			frametimesum += frameinfo[i].frametime;
			if(frametimesum > 0.5) {
				break;
			}
		}

		// Get average FPS
		currentframe.framerate = 1.0/(frametimesum/sumcount);
	} else {
		currentframe.framerate = 0;
	}

	// Store frametime lows
	currentframe.frametimelow = DBL_MAX; // always exists
	for(unsigned int i = 1; i < frameinfo.size(); i++) {
		currentframe.frametimelow = std::min(currentframe.frametimelow,frameinfo[i].frametime);
	}

	// Store frametime highs
	currentframe.frametimehigh = 0; // always exists
	for(unsigned int i = 1; i < frameinfo.size(); i++) {
		currentframe.frametimehigh = std::max(currentframe.frametimehigh,frameinfo[i].frametime);
	}

	// Store frame index
	if(frameinfo.size() > 0) {
		currentframe.index = GetLastFrame()->index + 1;
	} else {
		currentframe.index = 0;
	}

	// Add to big list
	frameinfo.insert(frameinfo.begin(),currentframe);
	if(frameinfo.size() > 100) { // keep only 100 items
		frameinfo.pop_back(); // remove last
	}

	// Update game time
	time += gametime;
	FXTimevar->Set((float)time);

	// Inform everyone that a frame has been rendered...
	OnRenderFrame->Execute(frametime);
}
void Renderer::FlushFrames() {
	frameinfo.clear();
}
FrameInfo* Renderer::GetLastFrame() {
	if(frameinfo.size() > 0) {
		return &frameinfo.back();
	} else {
		return NULL;
	}
}
float Renderer::GetFrameRate() {
	FrameInfo* info = GetLastFrame();
	if(info) {
		return info->framerate;
	} else {
		return 0;
	}
}
void Renderer::DrawTextLine(const char* text,int left,int top) {
	RECT rect = {0};

	// Begin sprite usage
	tooltipfontsprite->Begin(D3DXSPRITE_ALPHABLEND);

	// Draw shadow
	rect.left = left+1;
	rect.top = top+1;
	tooltipfontshadow->DrawText(tooltipfontsprite,text,-1,&rect,DT_NOCLIP,D3DCOLOR_XRGB(100,100,100));

	// Draw text
	rect.left = left;
	rect.top = top;
	tooltipfontwhite->DrawText(tooltipfontsprite,text,-1,&rect,DT_NOCLIP,D3DCOLOR_XRGB(255,255,255));

	// End sprite usage
	tooltipfontsprite->End();
}
void Renderer::DrawTextBlock(const char* text,int left,int top,int right,int bottom) {
	RECT rect = {0};

	// Begin sprite usage
	tooltipfontsprite->Begin(D3DXSPRITE_ALPHABLEND);

	// Draw shadow
	rect.left = left+1;
	rect.top = top+1;
	rect.right = right+1;
	rect.bottom = bottom+1;
	tooltipfontshadow->DrawText(tooltipfontsprite,text,-1,&rect,DT_WORDBREAK,D3DCOLOR_XRGB(100,100,100));

	// Draw text
	rect.left = left;
	rect.top = top;
	rect.right = right;
	rect.bottom = bottom;
	tooltipfontwhite->DrawText(tooltipfontsprite,text,-1,&rect,DT_WORDBREAK,D3DCOLOR_XRGB(255,255,255));

	// End sprite usage
	tooltipfontsprite->End();
}
void Renderer::DrawTextureFullScreen(LPDIRECT3DTEXTURE9 texture) {
	// Apply this texture to the objects to be drawn
	FXScreentex1->SetTexture(texture);

	// Draw a square right in front of the camera
	BeginTechnique(TechStock);
	DrawModel(screenspacequad);
	EndTechnique();
}
float2 Renderer::GetCenteringCorner(float2 windowsize) { // TODO: move to interface or utils?
	float2 result;
	result.x = presentparameters.BackBufferWidth/2 - windowsize.x/2;
	result.y = presentparameters.BackBufferHeight/2 - windowsize.y/2;
	return result;
}
void Renderer::DrawComponent(Component* component) {
	// Apply transform directly
	FXMatWorldViewProj->Set(component->GetWorldTransform());

	// Choose color
	switch(component->type) {
		case ctLabel:
		case ctDropdown:
		case ctBase:
		case ctBevel:
		case ctWindow: {
			FXPurecolor->Set(component->backcolor);
			break;
		}
		case ctButton: {
			Button* button = (Button*)component;
			if(button->down) {
				FXPurecolor->Set(button->downcolor);
			} else if(button->hot) {
				FXPurecolor->Set(button->hotcolor);
			} else {
				FXPurecolor->Set(button->backcolor);
			}
			break;
		}
		case ctEdit: {
			Edit* edit = (Edit*)component;
			if(edit->focused) {
				FXPurecolor->Set(edit->focuscolor);
			} else {
				FXPurecolor->Set(edit->backcolor);
			}
			break;
		}
	}
	FX->CommitChanges();

	// Draw base plane
	DrawModel(component->plane);

	// Draw label
	switch(component->type) {
		case ctDropdown: { // draw list
			((Dropdown*)component)->DrawText(interfacefont,interfacesprite);
			break;
		}
		case ctWindow: { // draw centered
			((Window*)component)->DrawText(interfacefont,interfacesprite);
			break;
		}
		case ctLabel: { // draw simple label
			((Label*)component)->DrawText(interfacefont,interfacesprite);
			break;
		}
		case ctEdit: { // draw simple label
			((Edit*)component)->DrawText(interfacefont,interfacesprite);
			break;
		}
		case ctButton: { // draw centered
			((Button*)component)->DrawText(interfacefont,interfacesprite);
			break;
		}
		default: {
		} // disable compiler warning
	}

	// Then draw children
	for(int i = component->children.size() - 1; i >= 0; i--) {
		Component* child = component->children[i];
		if(child->IsVisible()) {
			DrawComponent(child);
		}
	}
}
void Renderer::DrawInterface(Interface* thisinterface) {
	if(ui->componentlist.size() == 0) {
		return;
	}

	// Draw transparent fonts
	alphablendstate->Set(true);

	// Draw all top level components
	BeginTechnique(shaders[1]); // TODO: remove hardcoded 1
	for(int i = thisinterface->componentlist.size() - 1; i >= 0; i--) {
		Component* thiscomponent = thisinterface->componentlist[i];
		if(thiscomponent->IsVisible()) {
			DrawComponent(thiscomponent);
		}
	}
	EndTechnique();

	// Disable alpha blend again (really slow if used everywhere)
	alphablendstate->Set(false);
}
void Renderer::GetResolutions(std::vector<D3DDISPLAYMODE>& list) {
	unsigned int count = d3d->GetAdapterModeCount(D3DADAPTER_DEFAULT,D3DFMT_X8R8G8B8);
	list.resize(count);
	for(unsigned int i = 0; i < count; i++) {
		d3d->EnumAdapterModes(D3DADAPTER_DEFAULT,D3DFMT_X8R8G8B8,i,&list[i]);
	}
}
float3 Renderer::GetPixelWorldRay(float2 pixelpos) {
	float2 projectionpos = PixelsToProjection(pixelpos);

	// Muis in view coords
	float3 mouseview(
	    projectionpos.x / camera->GetMatProj()._11,
	    projectionpos.y / camera->GetMatProj()._22,
	    1.0f);

	// Muis in world space (z = z)
	return mouseview.TransformNormal(camera->GetMatViewInverse());
}
float2 Renderer::PixelsToProjection(float2 pixelpos) {
	float2 result;
	result.x =  2.0f * pixelpos.x / (float)presentparameters.BackBufferWidth  - 1.0f;
	result.y = -2.0f * pixelpos.y / (float)presentparameters.BackBufferHeight + 1.0f;
	return result; // [-1,1] voor x en y, resultaat van P(V(W(x)))
}
float2 Renderer::ProjectionToPixels(float2 projectionpos) {
	float2 result;
	result.x =  (projectionpos.x * presentparameters.BackBufferWidth  + presentparameters.BackBufferWidth)  / 2.0f;
	result.y = (-projectionpos.y * presentparameters.BackBufferHeight + presentparameters.BackBufferHeight) / 2.0f;
	return result;
}
void Renderer::Begin(bool clear) {
	d3ddev->BeginScene();
	if(clear) {
		d3ddev->Clear(0, 0, D3DCLEAR_TARGET, 0, 1.0f, 0);
	}
}
void Renderer::End() {
	d3ddev->EndScene();
	d3ddev->Present(NULL,NULL,NULL,NULL);
}
void Renderer::EndScene() {
	d3ddev->EndScene();
}
void Renderer::Present() {
	d3ddev->Present(NULL,NULL,NULL,NULL);
}
void Renderer::BeginTechnique(D3DXHANDLE tech) {
	FX->SetTechnique(tech);
	FX->Begin(0,D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);
}
void Renderer::BeginTechnique(FXShader* shader) {
	FX->SetTechnique(shader->GetD3DXHandle());
	FX->Begin(0,D3DXFX_DONOTSAVESTATE); // pure device, much faster
	FX->BeginPass(0);
}
void Renderer::EndTechnique() {
	FX->EndPass();
	FX->End();
}
void Renderer::ScheduleSaveBuffers() {
	saveframes = true;
}
void Renderer::SaveBuffers() {
	char buffer[1024];

	// Save all frames next to the executable
	if(scene->lights->castshadows) {
		snprintf(buffer,sizeof(buffer),"%s\\Shadowmap.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,scene->lights->ShadowTex->GetTopSurface(),NULL,NULL);
	}
	if(options->ssaosamples > 0) {
		snprintf(buffer,sizeof(buffer),"%s\\AmbientData.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,AmbientDataTex->GetTopSurface(),NULL,NULL);
		snprintf(buffer,sizeof(buffer),"%s\\AmbientFactor.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,AmbientFactorTex->GetTopSurface(),NULL,NULL);
	}
	if(options->enablehdr) {
		snprintf(buffer,sizeof(buffer),"%s\\FullPingPong1.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,FloatTex1->GetTopSurface(),NULL,NULL);
		snprintf(buffer,sizeof(buffer),"%s\\FullPingPong2.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,FloatTex2->GetTopSurface(),NULL,NULL);
		snprintf(buffer,sizeof(buffer),"%s\\HalfPingPong1.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,HalfFloatTex1->GetTopSurface(),NULL,NULL);
		snprintf(buffer,sizeof(buffer),"%s\\HalfPingPong2.png",exepath);
		D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,HalfFloatTex2->GetTopSurface(),NULL,NULL);
	}
	snprintf(buffer,sizeof(buffer),"%s\\FinalFrame.png",exepath);
	D3DXSaveSurfaceToFile(buffer,D3DXIFF_PNG,backbuffercolor,NULL,NULL);

	// Tell the world we are done
	Globals::console->Write("INFO: saved all buffers to '%s'",exepath);

	// Don't repeat
	saveframes = false;
}
float2 Renderer::GetUITextExtent(const char* text) {
	RECT textrect = {0};
	interfacefont->DrawText(NULL,text,-1,&textrect,DT_CALCRECT,D3DCOLOR_XRGB(255,255,255));
	return float2(textrect.right,textrect.bottom);
}
float2 Renderer::GetTooltipTextExtent(const char* text) {
	RECT textrect = {0};
	tooltipfontwhite->DrawText(NULL,text,-1,&textrect,DT_CALCRECT,D3DCOLOR_XRGB(255,255,255));
	return float2(textrect.right,textrect.bottom);
}
void Renderer::UpdateBufferSize() {
	if(d3ddev) {
		// Only respond when windowed
		if(presentparameters.Windowed) {

			// Get new buffer size
			RECT rcClient;
			GetClientRect(hwnd,&rcClient);

			// Are we not minimizing?
			if(rcClient.right > 0) {

				// Apply new buffer size
				presentparameters.BackBufferWidth = rcClient.right;
				presentparameters.BackBufferHeight = rcClient.bottom;
				paused = false; // kick start
				OnLostDevice();
			} else {

				// Pause when minimizing
				paused = true;
			}
		}
	}
}
void Renderer::SetFullScreen(bool value) {
	// Remember old window size
	static RECT oldwindowsize = {0};

	// This mode is already set, don't bother
	if(!value == presentparameters.Windowed) {
		return;
	}

	// Tell D3D to go fullscreen (or not)
	presentparameters.Windowed = !value;

	if(value) {

		// Save old window size
		GetWindowRect(hwnd,&oldwindowsize);

		// Hide frame and stretch window to screen
		SetWindowLongPtr(hwnd,GWL_STYLE,WS_VISIBLE);
		SetWindowPos(hwnd,
		             NULL,
		             0,
		             0,
		             GetSystemMetrics(SM_CXSCREEN),
		             GetSystemMetrics(SM_CYSCREEN),
		             SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_FRAMECHANGED);

	} else {

		// Unhide frame
		SetWindowLongPtr(hwnd,GWL_STYLE,WS_VISIBLE|WS_OVERLAPPEDWINDOW);
		SetWindowPos(hwnd,NULL,0,0,0,0,SWP_NOACTIVATE|SWP_NOSENDCHANGING|SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_FRAMECHANGED);

		// Fit window to client (do NOT set everything using one SetWindowPos call!!!)
		SetWindowPos(hwnd,
		             NULL,
		             oldwindowsize.left,
		             oldwindowsize.top,
		             oldwindowsize.right - oldwindowsize.left,
		             oldwindowsize.bottom - oldwindowsize.top,
		             SWP_NOZORDER|SWP_FRAMECHANGED);

		// Remember the maximized state, fix?
		if((oldwindowsize.right - oldwindowsize.left) == GetSystemMetrics(SM_CXSCREEN)) {
			ShowWindow(hwnd,SW_MAXIMIZE);
		}
	}

	OnLostDevice();
}
void Renderer::ToggleFullScreen() {
	SetFullScreen(presentparameters.Windowed);
}
void Renderer::SetCameraTransforms(Object* object) {
	// Calculate matrices
	float4x4 matWorld = object->GetWorldTransformMatrix();
	float4x4 matWorldView = matWorld * camera->GetMatView();
	float4x4 matWorldViewProj = matWorld * camera->GetMatViewProj();

	// Set FX variables
	FXMatWorld->Set(matWorld);
	FXMatWorldView->Set(matWorldView);
	FXMatWorldViewProj->Set(matWorldViewProj);
}
void Renderer::SetLightTransforms(Object* object,float4x4 lightprojection) {
	// Calculate matrices
	float4x4 matLightWorldViewProj = object->GetWorldTransformMatrix() * lightprojection;

	// Set FX variables
	FXMatLightWorldViewProj->Set(matLightWorldViewProj);
}
void Renderer::SetMaterial(Object* object) {
	Material* material = object->material;

	// Diffuse, only apply when texture is used
	if(material->diffusetex) {
		FXMaterialdiffuse->Set(&material->diffuse);
		FXDiffusetex->SetTexture(material->diffusetex);
	} else {
		FXPurecolor->Set(material->color);
	}

	// Idem for specular
	if(material->speculartex) {
		FXMaterialspecular->Set(&material->specular);
		FXMaterialshininess->Set(&material->shininess);
		FXSpeculartex->SetTexture(material->speculartex);
	}

	// Tiling for multitexturing (else not needed?)
	if(material->multitex) {
		FXMaterialTiling->Set(material->tiling);
		FXMaterialMixer->Set(material->mixer);
	}

	// And apply special textures
	if(material->normaltex) {
		FXNormaltex->SetTexture(material->normaltex);
	}
	if(material->parallaxtex) {
		FXParallaxtex->SetTexture(material->parallaxtex);
	}
	if(material->ambienttex) {
		FXAmbienttex->SetTexture(material->ambienttex);
	}
	FX->CommitChanges();

	// Set generic D3D render states
	// TODO: make sure we don't make these calls too often
	cullmodestate->Set(material->cullmode);
	fillmodestate->Set(material->fillmode);
	alphateststate->Set(material->alphatest);
	alphablendstate->Set(material->alphablend);
//	State* alpharefstate;
//	State* alphafuncstate;
//	State* alphasrcblend;
//	State* alphadestblend;
}
void Renderer::SetSSAOVariables(Object* thisobject) {
	// Transformation matrices
	SetCameraTransforms(thisobject);

	// We samplen diffuse voor alpha
	if(thisobject->material->diffusetex) {
		FXDiffusetex->SetTexture(thisobject->material->diffusetex);
	}
	FX->CommitChanges();
}
void Renderer::SetShadowMapVariables(Object* object,float4x4 lightprojection) {
	// Transformeren vanaf lichtbron X
	SetLightTransforms(object,lightprojection);

	// We samplen diffuse voor alpha
	if(object->material->diffusetex) {
		FXDiffusetex->SetTexture(object->material->diffusetex);
	}

	FX->CommitChanges();
}
void Renderer::SetGenericVariables(Object* object,float4x4 lightprojection) {

	// Transformation matrices
	SetCameraTransforms(object);

	// Sample from here
	SetLightTransforms(object,lightprojection);

	// Textures, diffuse, specular, ...
	SetMaterial(object);

	// Send to GPU
	FX->CommitChanges();
}
void Renderer::DrawModel(Model* model) {
	// Add to stats
	currentframe.facecount += model->numfaces;
	currentframe.vertexcount += model->numvertices;
	currentframe.callcount++;

	// Prepare state machine
	d3ddev->SetStreamSource(0, model->vertexbuffer,0, sizeof(VERTEX));
	d3ddev->SetIndices(model->indexbuffer);
	d3ddev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, model->numvertices, 0, model->numfaces);
}
void Renderer::PassShader(FXShader* thisshader) {
	// Skip empty shaders the low cost way (don't call BeginPass)
	if(!thisshader->HasValidRange()) {
		return;
	}

	// Walk the provided range
	BeginTechnique(thisshader->GetD3DXHandle());
	for(ObjectIterator i = thisshader->GetBegin(); i != thisshader->GetEnd(); i++) {
		Object* object = *i;
		if(object->IsVisible()) {
			// Get distance to nearest part of bounding sphere
			float3 modeldir = object->GetWorldCenter() - camera->GetPos();
			float distance = std::max(0.0f,modeldir.Length() - object->GetWorldRadius());

			// Determine which LOD we should draw
			DetailLevel* detaillevel = object->GetDetailLevel(distance);
			if(detaillevel) {
				SetGenericVariables(object,scene->lights->matLightViewProj); // Sample shadows from ONE caster
				DrawModel(detaillevel->model);
			}
		}
	}
	EndTechnique();
}
void Renderer::PassEffect(FXShader* effect,LPDIRECT3DSURFACE9 target) {
	d3ddev->SetRenderTarget(0,target);
	BeginTechnique(effect);
	DrawModel(screenspacequad);
	EndTechnique();
}
void Renderer::DrawScene(Scene* scene) {
	if(!scene) {
		return;
	}

	UpdateTime();

	// Draw special effects buffers with normal maps
	d3ddev->SetVertexDeclaration(standard);

	// Fix de per-objectstates
	zenablestate->Set(1); // improve performance by culling
	zwriteenablestate->Set(1); // improve performance by culling
	fillmodestate->Set(D3DFILL_SOLID); // fill shadow/ssao map with solid colors
	cullmodestate->Set(D3DCULL_NONE); // cull back faces (lessens load)
	alphateststate->Set(false); // use manual alpha testing
	alphablendstate->Set(false); // don't use blending either

	// Shadows
	if(options->shadowmapsize > 0 && scene->lights->castshadows) {
		// Unbind FXShadowtex the stupid way
		for(int i = 0; i < 16; i++) {
			d3ddev->SetTexture(i,NULL);
		}

		// Draw to shadow map
		d3ddev->SetRenderTarget(0,scene->lights->ShadowTex->GetTopSurface());
		d3ddev->SetDepthStencilSurface(scene->lights->ShadowTex->GetDepthTopSurface());
		d3ddev->Clear(0, 0, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER, D3DCOLOR_ARGB(255,255,255,255), 1, 0); // TODO: clear R only?

		// Draw all objects to the shadow map
		BeginTechnique(TechShadowMap);
		for(ObjectIterator i = scene->objects->begin(); i != scene->objects->end(); i++) {
			Object* object = *i;
			if(object->castshadows) {
				// Get distance to nearest part of bounding sphere
				float3 modeldir = object->GetWorldCenter() - camera->GetPos();
				float distance = std::max(0.0f,modeldir.Length() - object->GetWorldRadius());
				if(distance < options->shadowdistance) {

					// Determine which LOD we should draw
					DetailLevel* detaillevel = object->GetDetailLevel(distance);
					if(detaillevel) {
						SetShadowMapVariables(object,scene->lights->matLightViewProj); // Sample shadows from ONE caster
						DrawModel(detaillevel->model);
					}
				}
			}
		}
		EndTechnique();

		// Use this texture as the final
		FXShadowtex->SetTexture(scene->lights->ShadowTex); // TODO: Deze moet unbound worden???
	}

	// SSAO
	if(options->ssaosamples > 0) {

		// Draw to SSAO map
		d3ddev->SetRenderTarget(0,AmbientDataTex->GetTopSurface());
		d3ddev->SetDepthStencilSurface(backbufferdepth);
		d3ddev->Clear(0, 0, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER, 0.0f, 1.0f, 0);

		// Render de SSAO-coefficient naar een texture ter grootte van het scherm
		BeginTechnique(TechAmbientMap);
		for(ObjectIterator i = scene->objects->begin(); i != scene->objects->end(); i++) {
			Object* object = *i;
			if(object->IsVisible()) {
				// Get distance to nearest part of bounding sphere
				float3 modeldir = object->GetWorldCenter() - camera->GetPos();
				float distance = std::max(0.0f,modeldir.Length() - object->GetWorldRadius());
				if(distance < options->ssaodistance) {

					// Determine which LOD we should draw
					DetailLevel* detaillevel = object->GetDetailLevel(distance);
					if(detaillevel) {
						SetSSAOVariables(object);
						DrawModel(detaillevel->model);
					}
				}
			}
		}
		EndTechnique();
	}

	// Nu gaan we het beeld vullen (m.b.v. de special maps)
	if(options->enablehdr) {
		d3ddev->SetRenderTarget(0,FloatTex1->GetTopSurface());
	} else {
		d3ddev->SetRenderTarget(0,backbuffercolor);
	}
	d3ddev->SetDepthStencilSurface(backbufferdepth);

	// TODO: Clear altijd?
	d3ddev->Clear(0, 0, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER, 0, 1.0f, 0);

	// Shaders, Per vertex
	// vertex decl. staat al goed
	for(unsigned int i = 0; i < 5; i++) {
		PassShader(shaders[i]);
	}

	// Shaders, Per pixel
	for(unsigned int i = 5; i < 14; i++) {
		PassShader(shaders[i]);
	}

	// Shaders, Per pixel, tangent space
	d3ddev->SetVertexDeclaration(normalmap);
	for(unsigned int i = 14; i < 19; i++) {
		PassShader(shaders[i]);
	}

	// Zorg ervoor dat de screen space quads snel getekend worden
	d3ddev->SetVertexDeclaration(standard);

	// Zorgt voor meer tempo
	d3ddev->SetRenderState(D3DRS_ZENABLE,0);
	d3ddev->SetRenderState(D3DRS_ZWRITEENABLE,0);

	// Zet de per-objectstates vast voor screen space quads
	fillmodestate->Set(D3DFILL_SOLID);
	cullmodestate->Set(D3DCULL_CCW);
	alphateststate->Set(false);
	alphablendstate->Set(false);

	// Pas tone mapping en bloom toe
	if(options->enablehdr) {

		// Apply occlusion to FP buffers only
		if(options->ssaosamples > 0) {

			// Zorg ervoor dat het volgende in de samplers terecht komt:
			// 1 bevat de data benodigd voor de occlusion factor, zonder blur
			// 2 bevat de occlusion factor, zonder blur
			// 3 bevat de de originele kleur, waar de factor overheen moet

			// Use normal and depth data from AmbientDataTex to compute an occlusion factor (not blurred)
			FXScreentex1->SetTexture(AmbientDataTex);
			PassEffect(TechSSAOFactor,AmbientFactorTex->GetTopSurface());

			// Use normal and depth data from AmbientDataTex
			FXScreentex2->SetTexture(AmbientFactorTex); // occlusion factor, 100% size
			FXScreentex3->SetTexture(FloatTex1); // color to modify
			PassEffect(TechSSAOBlur,FloatTex2->GetTopSurface()); // to this
		}

		// Zorg ervoor dat het volgende in de samplers terecht komt:
		// 1 bevat het origineel in FP-formaat
		// 2 bevat de bright pass, zonder blur
		// 3 bevat de bright pass, met blur

		// Gooi een bright pass over FloatTex heen en sla op in HalfFloatTex1
		if(options->ssaosamples > 0) {
			FXScreentex1->SetTexture(FloatTex2);
		} else {
			FXScreentex1->SetTexture(FloatTex1);
		}
		PassEffect(TechBrightPass,HalfFloatTex1->GetTopSurface()); // to this

		// Gooi een gaussian blur over HalfFloatTex1 heen en sla op in HalfFloatTex2
		FXScreentex2->SetTexture(HalfFloatTex1); // plak deze op de quad, deze gaan we uitsmeren
		PassEffect(TechBrightBlur,HalfFloatTex2->GetTopSurface());

		// Sla als resultaat de tonemap van het origineel plus de blur op
		FXScreentex3->SetTexture(HalfFloatTex2); // uitgesmeerde blur
		PassEffect(TechToneMap,backbuffercolor);
	}

	// Teken de GUI, alle toplevels
	DrawInterface(ui); // Teken alle top levels

	// En als allerlaatst de tooltip
	if(tooltipmode != 0) {
		PrintTooltip();
		DrawTextLine(tooltip,0,0);
	}
}
