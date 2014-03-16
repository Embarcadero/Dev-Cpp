// Execute & Pause
// Runs a program, then keeps the console window open after it finishes

#include <stdio.h>
#include <windows.h>
#include <conio.h>

const char* GetErrorMessage(char* result,int size) { // TODO: make wide?
	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,GetLastError(),MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),(LPSTR)&result,size,NULL);
	
	// Strip newlines at the end
	int length = strlen(result)-1;
	while(length >= 0 && isspace(result[length])) {
		result[length] = 0;
		length--;
	}
	return result;
}

int main(int argc, char** argv) {
	
	// First make sure we aren't going to read nonexistent arrays
	if(argc < 2) {
		printf("\n--------------------------------");
		printf("\nUsage: ConsolePauser.exe <filename> <parameters>");
		printf("\nPress any key to continue . . . ");
		getch();
		return 0;
	}
	
	// Make us look like the paused program
	SetConsoleTitle(argv[1]);
	
	// Then build the to-run application command
	char command[32768] = ""; // the maximum amount CreateProcess can handle
	for(int i = 1;i < argc;i++) {
		strcat(command,&argv[i][0]);
		if(i != (argc-1)) { // Leave out the last space
			strcat(command," ");
		}
	}
	
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	memset(&si,0,sizeof(si));
	si.cb = sizeof(si);
	memset(&pi,0,sizeof(pi));
	
	// Create and immediately wait to prevent the cursor from switching to "loading"
	if(!CreateProcess(NULL, command, NULL, NULL, false, 0, NULL, NULL, &si, &pi)) {
		char errorbuffer[1024];
		printf("\n--------------------------------");
		printf("\nFailed to execute \"%s\":",command);
		printf("\nError %lu: %s",GetLastError(),GetErrorMessage(errorbuffer,1024));
		printf("\nPress any key to continue . . . ");
		getch();
		return 1;
	}
	WaitForSingleObject(pi.hProcess, INFINITE); // Wait for it to finish
	
	DWORD retval = 0;
	GetExitCodeProcess(pi.hProcess, &retval);
	printf("\n--------------------------------");
	printf("\nProcess exited with return value %lu",retval);
	printf("\nPress any key to continue . . . ");
	getch();
	return 0;
}
