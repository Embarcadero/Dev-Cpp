// Execute & Pause
// Runs a program, then keeps the console window open after it finishes

#include <string>
using std::string;
#include <stdio.h>
#include <windows.h>

#define MAX_COMMAND_LENGTH 32768
#define MAX_ERROR_LENGTH 2048

LONGLONG GetClockTick() {
	LARGE_INTEGER dummy;
	QueryPerformanceCounter(&dummy);
	return dummy.QuadPart;
}

LONGLONG GetClockFrequency() {
	LARGE_INTEGER dummy;
	QueryPerformanceFrequency(&dummy);
	return dummy.QuadPart;
}

void PauseExit(int exitcode) {
	system("pause");
	exit(exitcode);
}

string GetErrorMessage() {
	string result(MAX_ERROR_LENGTH,0);

	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,GetLastError(),MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),&result[0],result.size(),NULL);

	// Clear newlines at end of string
	for(int i = result.length()-1;i >= 0;i--) {
		if(isspace(result[i])) {
			result[i] = 0;
		} else {
			break;
		}
	}
	return result;
}

string GetCommand(int argc,char** argv) {
	string result;
	for(int i = 1;i < argc;i++) {
		// Quote the arguments in case they contain spaces
		// Could use additional quoting code around the argument
		if(string(argv[i]).find(" ")!=string::npos) {
			result += string("\"") + string(argv[i]) + string("\"");
		} else {
			result += string(argv[i]);
		}

		// Add a space except for the last argument
		if(i != (argc-1)) {
			result += string(" ");
		}
	}

	if(result.length() > MAX_COMMAND_LENGTH) {
		printf("\n--------------------------------");
		printf("\nError: Length of command line string is over %d characters\n",MAX_COMMAND_LENGTH);
		PauseExit(EXIT_FAILURE);
	}

	return result;
}

DWORD ExecuteCommand(string& command) {
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	memset(&si,0,sizeof(si));
	si.cb = sizeof(si);
	memset(&pi,0,sizeof(pi));

	if(!CreateProcess(NULL, (LPSTR)command.c_str(), NULL, NULL, false, 0, NULL, NULL, &si, &pi)) {
		printf("\n--------------------------------");
		printf("\nFailed to execute \"%s\":",command.c_str());
		printf("\nError %lu: %s\n",GetLastError(),GetErrorMessage().c_str());
		PauseExit(EXIT_FAILURE);
	}
	WaitForSingleObject(pi.hProcess, INFINITE); // Wait for it to finish

	DWORD result = 0;
	GetExitCodeProcess(pi.hProcess, &result);
	return result;
}

int main(int argc, char** argv) {

	// First make sure we aren't going to read nonexistent arrays
	if(argc < 2) {
		printf("\n--------------------------------");
		printf("\nUsage: ConsolePauser.exe <filename> <parameters>\n");
		PauseExit(EXIT_SUCCESS);
	}

	// Make us look like the paused program
	SetConsoleTitle(argv[1]);

	// Then build the to-run application command
	string command = GetCommand(argc,argv);

	// Save starting timestamp
	LONGLONG starttime = GetClockTick();

	// Then execute said command
	DWORD returnvalue = ExecuteCommand(command);

	// Get ending timestamp
	LONGLONG endtime = GetClockTick();
	double seconds = (endtime - starttime) / (double)GetClockFrequency();

	// Done? Print return value of executed program
	printf("\n--------------------------------");
	printf("\nProcess exited after %.4g seconds with return value %lu\n",seconds,returnvalue);
	PauseExit(EXIT_SUCCESS);
}
