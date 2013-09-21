// Execute & Pause
// Runs a program, then holds the console window open after it finishes

#include <stdio.h>
#include <windows.h>
#include <conio.h>
#include <winnt.h>

inline void pause() {
	printf("Press any key to continue . . . ");
	_getch();
}

int main(int argc, char** argv) {

	// First make sure we aren't going to read nonexistent arrays
	if(argc < 2) {
		MessageBox(NULL,"Usage:\n\nConsolePauser.exe <filename> <parameters>\n","Info",MB_ICONINFORMATION);
		return 0;
	}
	
	// Then check if we are even running a console program by reading the exe header
	bool HasConsole;
	HANDLE hImage;
	DWORD  MoreDosHeader[16];
	DWORD bytes;
	ULONG  ntSignature;
	IMAGE_DOS_HEADER      image_dos_header;
	IMAGE_FILE_HEADER     image_file_header;
	IMAGE_OPTIONAL_HEADER image_optional_header;

	// Open the reference file
	hImage = CreateFile(&argv[1][0],GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);

	// Read the MS-DOS image header.
	ReadFile(hImage,&image_dos_header, sizeof(IMAGE_DOS_HEADER),&bytes,NULL);

	// Read more MS-DOS header.
	ReadFile(hImage,MoreDosHeader,sizeof(MoreDosHeader),&bytes,NULL);

	// Get actual COFF header.
	ReadFile(hImage, &ntSignature, sizeof(ULONG),&bytes,NULL);

	// Read more
	ReadFile(hImage,&image_file_header,IMAGE_SIZEOF_FILE_HEADER,&bytes,NULL);

	// Read optional header.
	ReadFile(hImage,&image_optional_header,IMAGE_SIZEOF_NT_OPTIONAL_HEADER,&bytes,NULL);

	// Results!
	HasConsole = (image_optional_header.Subsystem == IMAGE_SUBSYSTEM_WINDOWS_CUI);

	// Then build the to-run application command
	char cmd[MAX_PATH] = "";
	for (int i = 1;i < argc;i++) {
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
		MessageBox(NULL,cmd,"Couldn't create process",MB_ICONERROR);
		return 1;
	}
	
	// GUI program? skip the waiting and pausing stuff
	if(HasConsole) {
		WaitForSingleObject(pi.hProcess, INFINITE); // Wait for it to finish
	
		DWORD retval;
		GetExitCodeProcess(pi.hProcess, &retval);
		if(retval == 0) {
			printf("\n\nProcess exited normally.\n");
		} else {
			printf("\n\nProcess exited with return value %lu\n",retval);
		}
	
		pause();		
	}
	return 0;
}
