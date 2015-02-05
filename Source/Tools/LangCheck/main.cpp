#include <windows.h>
#include "LangFile.h"
#include "LangCompare.h"

#define ID_ENGLISHFILETEXT	1001
#define ID_ENGLISHFILEINPUT	1002
#define ID_OTHERFILETEXT	1003
#define ID_OTHERFILEINPUT	1004
#define ID_COMPAREBUTTON	1005
#define ID_LOGBOX			1006

/* This is where all the input to the window goes to */
LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam,LPARAM lParam) {
	switch(Message) {
		case WM_DESTROY: {
			PostQuitMessage(0);
			break;
		}

		case WM_COMMAND: {
			switch(LOWORD(wParam)) {
				case ID_COMPAREBUTTON: {
					// Clear log
					SetWindowText(LogBoxHandle,"");
					
					// Get reference language filename
					LangFile englishfile;
					char buffer[1024];
					HWND EnglishFileInput = GetDlgItem(hwnd, ID_ENGLISHFILEINPUT);
					GetWindowText(EnglishFileInput, buffer, 1024);

					// Open reference language
					englishfile.OpenFile(buffer);

					// Get compare language filename
					LangFile otherfile;
					HWND OtherFileInput = GetDlgItem(hwnd, ID_OTHERFILEINPUT);
					GetWindowText(OtherFileInput, buffer, 1024);

					// Open reference language
					otherfile.OpenFile(buffer);
					
					// Compare them
					LangCompare compare(englishfile,otherfile);
					break;
				}
			}
			break;
		}

		case WM_CREATE: {
			// Create fancy fonts
			HDC hdc = GetDC(hwnd);
			HFONT font1 = CreateFont(-MulDiv(10,GetDeviceCaps(hdc, LOGPIXELSY),72),0,0,0,0,0,
			                        0,0,0,0,0,0,0,"Segoe UI");
			HFONT font2 = CreateFont(-MulDiv(10,GetDeviceCaps(hdc, LOGPIXELSY),72),0,0,0,0,0,
			                        0,0,0,0,0,0,0,"Consolas");
			                        
			// Create english
			HWND EnglishFileText = CreateWindow("STATIC","English (Reference) File:",
			                                    WS_CHILD|WS_VISIBLE,10,10,200,20,hwnd,(HMENU)ID_ENGLISHFILETEXT,
			                                    GetModuleHandle(NULL),NULL);
			SendMessage(EnglishFileText,WM_SETFONT,(WPARAM)font1,0);
			HWND EnglishFileInput = CreateWindowEx(WS_EX_CLIENTEDGE,"EDIT",
			                                       "test\\English.lng",
			                                       WS_CHILD|WS_VISIBLE,10,30,610,26,hwnd,(HMENU)ID_ENGLISHFILEINPUT,
			                                       GetModuleHandle(NULL),NULL);
			SendMessage(EnglishFileInput,WM_SETFONT,(WPARAM)font1,0);

			// Create other inputs
			HWND OtherFileText = CreateWindow("STATIC","Other Language File:",
			                                  WS_CHILD|WS_VISIBLE,10,70,200,20,hwnd,(HMENU)ID_OTHERFILETEXT,
			                                  GetModuleHandle(NULL),NULL);
			SendMessage(OtherFileText,WM_SETFONT,(WPARAM)font1,0);
			HWND OtherFileInput = CreateWindowEx(WS_EX_CLIENTEDGE,"EDIT",
			                                     "test\\Chinese.lng",
			                                     WS_CHILD|WS_VISIBLE,10,90,610,26,hwnd,(HMENU)ID_OTHERFILEINPUT,
			                                     GetModuleHandle(NULL),NULL);
			SendMessage(OtherFileInput,WM_SETFONT,(WPARAM)font1,0);

			// Create compare button
			HWND CompareButton = CreateWindow("BUTTON","Validate",
			                                  WS_CHILD|WS_VISIBLE,10,130,200,30,hwnd,(HMENU)ID_COMPAREBUTTON,
			                                  GetModuleHandle(NULL),NULL);
			SendMessage(CompareButton,WM_SETFONT,(WPARAM)font1,0);

			// Create log box
			LogBoxHandle = CreateWindowEx(WS_EX_CLIENTEDGE,"EDIT","",
			                              WS_CHILD|WS_VISIBLE|ES_READONLY|ES_MULTILINE|WS_VSCROLL,10,170,610,270,hwnd,
			                              (HMENU)ID_LOGBOX,
			                              GetModuleHandle(NULL),NULL);
			SendMessage(LogBoxHandle,WM_SETFONT,(WPARAM)font2,0);
			
			// Release handles
			ReleaseDC(hwnd,hdc);
			break;
		}

		/* All other messages (a lot of them) are processed using default procedures */
		default:
			return DefWindowProc(hwnd, Message, wParam, lParam);
	}
	return 0;
}

/* The 'main' function of Win32 GUI programs: this is where execution starts */
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow) {
	WNDCLASSEX wc; /* A properties struct of our window */
	HWND hwnd; /* A 'HANDLE', hence the H, or a pointer to our window */
	MSG msg; /* A temporary location for all messages */

	/* zero out the struct and set the stuff we want to modify */
	memset(&wc,0,sizeof(wc));
	wc.cbSize		 = sizeof(WNDCLASSEX);
	wc.lpfnWndProc	 = WndProc; /* This is where we will send messages to */
	wc.hInstance	 = hInstance;
	wc.hCursor		 = LoadCursor(NULL, IDC_ARROW);

	/* White, COLOR_WINDOW is just a #define for a system color, try Ctrl+Clicking it */
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW);
	wc.lpszClassName = "WindowClass";
	wc.hIcon		 = LoadIcon(NULL, IDI_APPLICATION); /* Load a standard icon */
	wc.hIconSm		 = LoadIcon(NULL,
	                            IDI_APPLICATION); /* use the name "A" to use the project icon */

	if(!RegisterClassEx(&wc)) {
		MessageBox(NULL, "Window Registration Failed!","Error!",
		           MB_ICONEXCLAMATION|MB_OK);
		return 0;
	}

	hwnd = CreateWindowEx(WS_EX_CLIENTEDGE,"WindowClass","LangCheck",
	                      WS_VISIBLE|(WS_OVERLAPPEDWINDOW^WS_THICKFRAME),
	                      CW_USEDEFAULT, /* x */
	                      CW_USEDEFAULT, /* y */
	                      640, /* width */
	                      480, /* height */
	                      NULL,NULL,hInstance,NULL);

	if(hwnd == NULL) {
		MessageBox(NULL, "Window Creation Failed!","Error!",MB_ICONEXCLAMATION|MB_OK);
		return 0;
	}

	/*
		This is the heart of our program where all input is processed and
		sent to WndProc. Note that GetMessage blocks code flow until it receives something, so
		this loop will not produce unreasonably high CPU usage
	*/
	while(GetMessage(&msg, NULL, 0, 0) > 0) { /* If no error is received... */
		TranslateMessage(&msg); /* Translate key codes to chars if present */
		DispatchMessage(&msg); /* Send it to WndProc */
	}
	return msg.wParam;
}
