#include <windows.h>

// Tried to use wmain, but GCC/MinGW doesn't define it cause it's MS specific
// So, I'm sticking to the Windows API for now

int main() {
	// Fill an argv array and argc similar to the standard ones
	int argc;
	wchar_t** argv = CommandLineToArgvW(GetCommandLineW(),&argc);
	
	// Then build our selection to pass to devcpp.exe
	wchar_t argtodev[400] = L"-c .\\config ";
	for(unsigned int i = 1;i < argc;i++) {
		wcscat(argtodev,&argv[i][0]);
	}
	
	HINSTANCE returnvalue = ShellExecuteW(NULL,L"open",L"devcpp.exe",argtodev,NULL,SW_SHOWNORMAL);
	if((int)returnvalue <= 32) {
		if(GetLastError()==ERROR_FILE_NOT_FOUND) {
			MessageBoxW(NULL,L"devcpp.exe",L"File not found",MB_OK);
		} else {
			MessageBoxW(NULL,L"no worky!",L"Error",MB_OK);
		}
	}
	
	// Free the strings pointed to by argv
	LocalFree(argv);
	return 0;
}
