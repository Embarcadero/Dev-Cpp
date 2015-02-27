#include "Resource.h"
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Van deze wordt verwacht dat ze er altijd zijn
*/
DLLIMPORT class Renderer* renderer = NULL;
DLLIMPORT class Camera* camera = NULL;
DLLIMPORT class Interface* ui = NULL;
DLLIMPORT class Scene* scene = NULL;
DLLIMPORT class Models* models = NULL;
DLLIMPORT class Textures* textures = NULL;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Verplaats deze naar buiten de engine?
*/
DLLIMPORT class Options* options = NULL;
DLLIMPORT class Console* console = NULL;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Misc.
*/
DLLIMPORT LPD3DXEFFECT FX; // De shaders
DLLIMPORT LPDIRECT3DDEVICE9 d3ddev; // De videokaart
DLLIMPORT HWND hwnd = NULL; // Het venster
DLLIMPORT char exepath[MAX_PATH] = ""; // Current directory
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Utils
*/
DLLIMPORT void InitEngine() {

	// Begin altijd met andere random waardes
	srand(GetTickCount());

	// Sla de current directory op
	char fullexepath[MAX_PATH];
	GetModuleFileName(NULL,fullexepath,MAX_PATH);
	ExtractFilePath(fullexepath,exepath);

	// De console moet als eerste geladen worden zodat we tijdens het opstarten iets naar het log kunnen schrijven
	console = new Console();

	// Laad opties eerst, zodat we niet twee keer het device hoeven te (re)setten
	options = new Options("JohanGame.ini");

	// Caches die ervoor zorgen dat we geen kopietjes inladen
	models = new Models(); // onafhankelijk
	textures = new Textures(); // onafhankelijk
	// TODO: materials?

	// Remaining singletons
	renderer = new Renderer(); // maakt device en FX, heeft models, textures nodig
	camera = new Camera(); // heeft Renderer nodig (voor FX)
	scene = new Scene(); // heeft Renderer nodig
	ui = new Interface();

	// Voeg toe aan ui (dat deden ze zelf niet, omdat ze nog niet bestonden)
	ui->AddComponent(console);
	ui->AddComponent(options);

	// Tell the drivers we wish to start rendering by setting resolution
	if(options->fullscreen) {

		// Load the highest resolution at 60Hz
		if(options->maxfsresolution) {
			options->FindMaxResolution();
		}
		renderer->SetFullScreen(options->fullscreen); // resets device for us
	} else {
		renderer->OnLostDevice();
	}
}
DLLIMPORT void DeleteEngine() {
	delete ui; // mikt ook dialogs weg
	ui = NULL;
	delete camera;
	camera = NULL;
	delete scene;
	scene = NULL;
	delete renderer; // gooit device weg
	renderer = NULL;

	// Gooi buffers weg
	delete textures;
	delete models;
}
DLLIMPORT std::vector<float3> ComputeSSAOKernel(int size) {
	std::vector<float3> result;
	result.resize(size);
	for(unsigned int i = 0; i < result.size(); i++) {
		// uniform scale
		result[i].x = RandomRange(-1.0f,1.0f);
		result[i].y = RandomRange(-1.0f,1.0f);
		result[i].z = RandomRange(-1.0f,1.0f);

		// quadratic scale within [0.1 .. 1]
		float scale = (i / float(size)) * (i / float(size));
		scale = 0.1f * (1 - scale) + 1.0f * scale;
		result[i] *= scale;
	}
	return result;
}
DLLIMPORT std::vector<float3> ComputeGaussKernel(int size,float sigma2) {
	std::vector<float3> result;
	result.resize(size*size);
	for(int i = 0; i < (int)result.size(); i++) { // float3 = [x y coeff]
		result[i].x = (i % size) - 2;
		result[i].y = (i / size) - 2;
		result[i].z = 1.0f/(2*pi*sigma2) * expf(-(powf(result[i].x - 0,
		                                        2)/(2 * sigma2) + powf(result[i].y - 0,2)/(2 * sigma2))); // zeros for p0
	}
	return result;
}
DLLIMPORT float RandomRange(float min,float max) {
	// Scale the range to [0,1]
	float baserange = (float)rand()/(float)RAND_MAX;

	// Then scale to [0,(max-min)]
	float scalerange = baserange*(max - min);

	// And move by min to [min,max]
	float finalrange = scalerange + min;
	return finalrange;
}
DLLIMPORT char* ExtractFileName(const char* text,char* result) {
	char* lastslash = strrchr(text,'\\');
	if(lastslash == NULL) { // geen pad gegeven, dus text is alleen de naam
		sprintf(result,"%s",text);
	} else { // vanaf laatste slash kopieren
		strcpy(result,++lastslash);
	}

	return result;
}
DLLIMPORT char* ExtractFilePath(const char* text,char* result) {
	char* lastslash = strrchr(text,'\\');
	if(lastslash == NULL) { // geen mappad gegeven,niks returnen
		result[0] = 0;
	} else { // wel mappad gegeven, kopieren t/m dat ding
		lastslash++; // neem slash mee
		strncpy(result,text,lastslash-text);
		result[lastslash-text] = '\0'; // En zet er een 0 achter
	}

	return result;
}
DLLIMPORT char* ExtractFileExt(const char* text,char* result) {
	char* period = strrchr(text,'.');
	if(period == NULL) { // geen extensie gegeven, niks returnen
		result[0] = 0;
	} else { // wel extensie gegeven, kopieren vanaf dat ding
		strcpy(result,++period);
	}

	return result;
}
DLLIMPORT char* ChangeFileExt(const char* text,const char* newext,
                              char* result) {

	// Sla alles op TOT punt...
	char* period = strrchr(text,'.');
	if(period == NULL) { // geen extensie gegeven, mooi
		result[0] = 0;
	} else { // wel extensie gegeven, kopieren tot dat ding
		strncpy(result,text,period-text);
		result[period-text] = '\0'; // En zet er een 0 achter
	}

	// gooi de nieuwe ext erachter
	strcat(result,newext);

	return result;
}
DLLIMPORT void IntToFourChar(int val,char* dest) {
	dest[0] = (val >> 0) & 0xFF;
	dest[1] = (val >> 8) & 0xFF;
	dest[2] = (val >> 16) & 0xFF;
	dest[3] = (val >> 24) & 0xFF;
}
DLLIMPORT void GetCPUName(char* cpuname,int size) {

#define cpuid(func,ax,bx,cx,dx)\
	__asm__ __volatile__ ("cpuid": "=a" (ax), "=b" (bx), "=c" (cx), "=d" (dx) : "a" (func));

	if(size < 49) {
		console->Write("49 characters needed to write CPU name, received %d\r\n",size);
		return;
	}

	unsigned int eax = 0;
	unsigned int ebx = 0;
	unsigned int ecx = 0;
	unsigned int edx = 0;

	// Type CPU #1
	cpuid(0x80000002,eax,ebx,ecx,edx);
	IntToFourChar(eax,&cpuname[0]);
	IntToFourChar(ebx,&cpuname[4]);
	IntToFourChar(ecx,&cpuname[8]);
	IntToFourChar(edx,&cpuname[12]);

	// Type CPU #2
	cpuid(0x80000003,eax,ebx,ecx,edx);
	IntToFourChar(eax,&cpuname[16]);
	IntToFourChar(ebx,&cpuname[20]);
	IntToFourChar(ecx,&cpuname[24]);
	IntToFourChar(edx,&cpuname[28]);

	// Type CPU #3
	cpuid(0x80000004,eax,ebx,ecx,edx);
	IntToFourChar(eax,&cpuname[32]);
	IntToFourChar(ebx,&cpuname[36]);
	IntToFourChar(ecx,&cpuname[40]);
	IntToFourChar(edx,&cpuname[44]);

	// Add terminator
	cpuname[48] = 0;
}
DLLIMPORT void GetGPUName(char* cpuname,int size) {
	if(renderer) {
		strncpy(cpuname,renderer->properties.Description,size);
	}
}
DLLIMPORT int GetFontSizePt(int points,HDC hdc) {
	return -MulDiv(points, GetDeviceCaps(hdc, LOGPIXELSY), 72);
}
DLLIMPORT void GetCSIDLPath(int csidl,char* result) {
	SHGetFolderPath(NULL,csidl,NULL,SHGFP_TYPE_CURRENT,result);
}
DLLIMPORT void SafeRelease(IUnknown* resource) {
	if(resource) {
		resource->Release();
	}
}
DLLIMPORT int CountChar(const char* text,char token) {
	int len = strlen(text);
	int result = 0;
	for(int i = 0; i < len; i++) {
		if(text[i] == token) {
			result++;
		}
	}
	return result;
}
DLLIMPORT char* TrimLeft(char* text) {
	while(isspace(*text)) {
		*text = 0; // zero fill
		text++;
	}
	return text;
}
DLLIMPORT char* TrimRight(char* text) {
	char* end = text + strlen(text) - 1;
	while(isspace(*end)) {
		*end = 0; // zero fill
		end--;
	}
	return text;
}
DLLIMPORT char* Trim(char* text) {
	return TrimLeft(TrimRight(text));
}
DLLIMPORT bool FileExists(const char* path) {
	WIN32_FIND_DATA info;
	HANDLE file = FindFirstFile(path,&info);
	if(file == INVALID_HANDLE_VALUE) {
		return false;
	} else {
		FindClose(file);
		return true;
	}
}
DLLIMPORT float DegToRad(float degin) {
	return degin/180.0f*pi;
}
DLLIMPORT float RadToDeg(float radin) {
	return radin*180.0f/pi;
}
DLLIMPORT void EnumerateFiles(const char* folder,const char* filter,
                              std::vector<char*>* list) {

	WIN32_FIND_DATA FindFileData;
	HANDLE FindResult;

	char newfolder[MAX_PATH];
	sprintf(newfolder,"%s*.*",folder);

	// Spawn onszelf eerst in alle mappen
	FindResult = FindFirstFile(newfolder,&FindFileData);
	if(FindResult != INVALID_HANDLE_VALUE) {

		// Nu gaan we alles opnoemen (beginnend met FindFirstFile)
		do {
			if(FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
				if(FindFileData.cFileName[0] != '.') {

					// Mappen gaan we ook doorzoeken
					sprintf(newfolder,"%s%s\\",folder,FindFileData.cFileName);
					EnumerateFiles(newfolder,filter,list);
				}
			}
		} while(FindNextFile(FindResult,&FindFileData));
	}
	FindClose(FindResult);

	char filename[MAX_PATH];
	sprintf(filename,"%s%s",folder,filter);

	// Zoek dan de huidige map door
	FindResult = FindFirstFile(filename,&FindFileData);
	if(FindResult != INVALID_HANDLE_VALUE) {

		// Nu gaan we weer alles opnoemen (beginnend met FindFirstFile)
		do {
			if(!(FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {

				// Store filename relative to folder
				//	char* finalpath = new char[MAX_PATH];
				//	sprintf(finalpath,"%s%s",folder,FindFileData.cFileName);
				list->push_back(strdup(FindFileData.cFileName));
			}
		} while(FindNextFile(FindResult,&FindFileData));
	}
	FindClose(FindResult);
}
DLLIMPORT unsigned int Faculty(unsigned int n) {
	unsigned int result = 1;
	for(unsigned int i = 2; i <= n; i++) {
		result *= i;
	}
	return result;
}
DLLIMPORT unsigned int Binomial(unsigned int n,unsigned int k) {
	return Faculty(n) / (Faculty(k) * Faculty(n - k));
}
DLLIMPORT float3 GramSchmidt2(float3 v1,float3 x2) {

	// v2 = x2 - dot(v1 * x2)/dot(v1,v1) * v1
	float factor1 = v1.Dot(x2)/v1.Dot(v1);

	return x2 - factor1 * v1;
}
DLLIMPORT float3 GramSchmidt3(float3 v1,float3 v2,float3 x3) {

	// v3 = x3 - dot(v1,x3)/dot(v1,v1) * v1 - dot(v2,x3)/dot(v2,v2) * v2
	float factor1 = v1.Dot(x3)/v1.Dot(v1);
	float factor2 = v2.Dot(x3)/v2.Dot(v2);

	return x3 - factor1 * v1 - factor2 * v2;
}
DLLIMPORT bool fequals(float x1,float x2) {
	return x1 + 0.000001f >= x2 && x1 - 0.000001f <= x2;
}
DLLIMPORT void GetFullPath(const char* file,const char* folder,char* fullpath) {
	if(file[1] != ':') { // prepend exe dir
		sprintf(fullpath,"%s\\%s\\%s",exepath,folder,file);
	} else {
		strcpy(fullpath,file);
	}
}
DLLIMPORT char* StripQuotes(const char* text) {

	unsigned int start = 0;
	unsigned int length = strlen(text);

	if(text[length-1] == '"') {
		length--;
	}
	if(text[0] == '"') {
		start++;
		length--;
	}

	char* noquotes = new char[length + 1];
	strncpy(noquotes,text + start,length);
	noquotes[length] = 0;

	return noquotes;
}
