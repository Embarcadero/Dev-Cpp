#include <windows.h>
#include <string>
using std::wstring;

// Tried to use wmain, but GCC/MinGW doesn't define it because it's MS specific
// So, I'm sticking to the Windows API for now

int main() {
	// Fill an argv array and argc similar to the standard ones
	int ArgumentCount = 0;
	wchar_t** ArgumentInput = CommandLineToArgvW(GetCommandLineW(),&ArgumentCount);
	
	// Then build our selection to pass to devcpp.exe
	wstring ArgumentsToDev = L"-c .\\config ";
	for(int i = 1;i < ArgumentCount;i++) {
		ArgumentsToDev.append(ArgumentInput[i]);
	}
	
	// Free the strings pointed to by argv
	LocalFree(ArgumentInput);
	
	// Attempt to execute
	int Result = (INT_PTR)ShellExecuteW(NULL,L"open",L"devcpp.exe",ArgumentsToDev.c_str(),NULL,SW_SHOWNORMAL);
	if(Result <= 32) {
		switch(Result) {
			case ERROR_FILE_NOT_FOUND: {
				MessageBoxW(NULL,L"devcpp.exe",L"File not found",MB_OK);
				break;
			}
			default: {
				MessageBoxW(NULL,L"An unspecified error has occured!",L"Error",MB_OK);
				break;
			}
		}
	}
	return 0;
}
