// Execute & Pause
// Runs a program, then keepsd the console window open after it finishes

#include <stdio.h>
#include <windows.h>
#include <conio.h>

char* GetErrorMessage(const char* format,...) { // make wide?

	va_list parameters;
	va_start(parameters,format);

	char* result = NULL;
	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,//|FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		GetLastError(),
		MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),
		(LPSTR)&result,
		0,
		&parameters);
		
	va_end(parameters);
	
	return result; // use localfree!
}

int main(int argc, char** argv) {

	// First make sure we aren't going to read nonexistent arrays
	if(argc < 2) {
		MessageBox(NULL,"Usage:\n\nConsolePauser.exe <filename> <parameters>\n","Info",MB_ICONINFORMATION);
		return 0;
	}

	// Then build the to-run application command
	char cmd[MAX_PATH] = "";
	for(int i = 1;i < argc;i++) {
		strcat(cmd,&argv[i][0]);
		if(i != (argc-1)) { // Leave out the last space
			strcat(cmd," ");
		}
	}
	
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	memset(&si,0,sizeof(si));
	si.cb = sizeof(si);
	memset(&pi,0,sizeof(pi));

	if(!CreateProcess(NULL, cmd, NULL, NULL, false, 0, NULL, NULL, &si, &pi)) {
		
		// Put error in body
		char* errortext = GetErrorMessage(cmd);
		
		char fulltext[1024]; // should be enough
		snprintf(fulltext,1024,"Error launching program (%lu):\r\n\r\n%s",GetLastError(),errortext);
		
		MessageBox(NULL,fulltext,"Error",MB_ICONERROR);
		LocalFree(errortext);
		return 1;
	}
	
	WaitForSingleObject(pi.hProcess, INFINITE); // Wait for it to finish
	
	//SetCursor(LoadCursor(GetModuleHandle(0),IDC_ARROW));

	DWORD retval = 0;
	GetExitCodeProcess(pi.hProcess, &retval);
	printf("\n--------------------------------");
	printf("\nProcess exited with return value %lu",retval);
	printf("\nPress any key to continue . . . ");
	getch();

	return 0;
}
