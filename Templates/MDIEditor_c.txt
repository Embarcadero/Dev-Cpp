#include <windows.h>
#include <commctrl.h>

#include "main.h"

LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK MDIChildWndProc(HWND hwnd, UINT Message, WPARAM wParam,LPARAM lParam);

char g_szAppName[] = "MyMDIWindow";
char g_szChild[] = "MyMDIChild";
HINSTANCE g_hInst;
HWND g_hMDIClient, g_hStatusBar, g_hToolBar;
HWND g_hMainWindow;

BOOL LoadFile(HWND hEdit, LPSTR pszFileName) {
	HANDLE hFile;
	BOOL bSuccess = FALSE;
	
	hFile = CreateFile(pszFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
	OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if(hFile != INVALID_HANDLE_VALUE) {
		DWORD dwFileSize;
		dwFileSize = GetFileSize(hFile, NULL);
		if(dwFileSize != 0xFFFFFFFF) {
			LPSTR pszFileText;
			pszFileText = LPSTR(GlobalAlloc(GPTR, dwFileSize + 1));
			if(pszFileText != NULL) {
				DWORD dwRead;
				if(ReadFile(hFile, pszFileText, dwFileSize, &dwRead, NULL)) {
					pszFileText[dwFileSize] = 0; // Null terminator
					if(SetWindowText(hEdit, pszFileText))
						bSuccess = TRUE; // It worked!
				}
				GlobalFree(pszFileText);
			}
		}
		CloseHandle(hFile);
	}
	return bSuccess;
}

BOOL SaveFile(HWND hEdit, LPSTR pszFileName) {
	HANDLE hFile;
	BOOL bSuccess = FALSE;
	
	hFile = CreateFile(pszFileName, GENERIC_WRITE, 0, NULL,
	CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if(hFile != INVALID_HANDLE_VALUE) {
		DWORD dwTextLength;
		dwTextLength = GetWindowTextLength(hEdit);
		if(dwTextLength > 0) {// No need to bother if there's no text.	
			LPSTR pszText;
			pszText = LPSTR(GlobalAlloc(GPTR, dwTextLength + 1));
			if(pszText != NULL) {
				if(GetWindowText(hEdit, pszText, dwTextLength + 1)) {
					DWORD dwWritten;
					if(WriteFile(hFile, pszText, dwTextLength, &dwWritten, NULL))
						bSuccess = TRUE;
				}
				GlobalFree(pszText);
			}
		}
		CloseHandle(hFile);
	}
	return bSuccess;
}

BOOL GetFileName(HWND hwnd, LPSTR pszFileName, BOOL bSave) {
	OPENFILENAME ofn;
	
	ZeroMemory(&ofn, sizeof(ofn));
	pszFileName[0] = 0;
	
	ofn.lStructSize = sizeof(ofn);
	ofn.hwndOwner = hwnd;
	ofn.lpstrFilter = "Text Files (*.txt)\0*.txt\0All Files (*.*)\0*.*\0\0";
	ofn.lpstrFile = pszFileName;
	ofn.nMaxFile = MAX_PATH;
	ofn.lpstrDefExt = "txt";
	
	if(bSave) {
		ofn.Flags = OFN_EXPLORER | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY |
		OFN_OVERWRITEPROMPT;
		if(!GetSaveFileName(&ofn))
			return FALSE;
	} else {
		ofn.Flags = OFN_EXPLORER | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;
		if(!GetOpenFileName(&ofn))
			return FALSE;
	}
	return TRUE;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,LPSTR lpszCmdParam, int nCmdShow) {
	MSG  Msg;
	WNDCLASSEX WndClassEx;
	
	g_hInst = hInstance;
	
	WndClassEx.cbSize          = sizeof(WNDCLASSEX);
	WndClassEx.style           = CS_HREDRAW | CS_VREDRAW;
	WndClassEx.lpfnWndProc     = WndProc;
	WndClassEx.cbClsExtra      = 0;
	WndClassEx.cbWndExtra      = 0;
	WndClassEx.hInstance       = hInstance;
	WndClassEx.hIcon           = LoadIcon(NULL, IDI_APPLICATION);
	WndClassEx.hCursor         = LoadCursor(NULL, IDC_ARROW);
	WndClassEx.hbrBackground   = (HBRUSH)(COLOR_3DSHADOW+1);
	WndClassEx.lpszMenuName    = "MAIN";
	WndClassEx.lpszClassName   = g_szAppName;
	WndClassEx.hIconSm         = LoadIcon(NULL, IDI_APPLICATION);

	if(!RegisterClassEx(&WndClassEx)) {
		MessageBox(0, "Could Not Register Window", "Oh Oh...",MB_ICONEXCLAMATION | MB_OK);
		return -1;
	}

	WndClassEx.lpfnWndProc     = MDIChildWndProc;
	WndClassEx.lpszMenuName    = NULL;
	WndClassEx.lpszClassName   = g_szChild;
	WndClassEx.hbrBackground   = (HBRUSH)(COLOR_3DFACE+1);

	if(!RegisterClassEx(&WndClassEx)) {
		MessageBox(0, "Could Not Register Child Window", "Oh Oh...",
		MB_ICONEXCLAMATION | MB_OK);
		return -1;
	}

	g_hMainWindow = CreateWindowEx(WS_EX_APPWINDOW,g_szAppName,"MDI File Editor",WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		0, 0, hInstance, NULL);

	if (g_hMainWindow == NULL){
		MessageBox(0, "No Window", "Oh Oh...", MB_ICONEXCLAMATION | MB_OK);
		return -1;
	}

	ShowWindow(g_hMainWindow, nCmdShow);
	UpdateWindow(g_hMainWindow);

	while(GetMessage(&Msg, NULL, 0, 0)) {
		if (!TranslateMDISysAccel(g_hMDIClient, &Msg)) {
			TranslateMessage(&Msg);
			DispatchMessage(&Msg);
		}
	}
	return Msg.wParam;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam) {
	switch(Message) {
		case WM_CREATE: {
			CLIENTCREATESTRUCT ccs;
			int iStatusWidths[] = {200, 300, -1};
			TBADDBITMAP tbab;
			TBBUTTON tbb[9];

			// Find window menu where children will be listed
			ccs.hWindowMenu  = GetSubMenu(GetMenu(hwnd), 2);
			ccs.idFirstChild = ID_MDI_FIRSTCHILD;
			g_hMDIClient = CreateWindowEx(WS_EX_CLIENTEDGE, "mdiclient", NULL,
			WS_CHILD | WS_CLIPCHILDREN | WS_VSCROLL | WS_HSCROLL,
			CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
			hwnd, (HMENU)ID_MDI_CLIENT, g_hInst, (LPVOID)&ccs);
			ShowWindow(g_hMDIClient, SW_SHOW);
			
			g_hStatusBar = CreateWindowEx(0, STATUSCLASSNAME, NULL,
			WS_CHILD | WS_VISIBLE | SBARS_SIZEGRIP, 0, 0, 0, 0,
			hwnd, (HMENU)ID_STATUSBAR, g_hInst, NULL);
			SendMessage(g_hStatusBar, SB_SETPARTS, 3, (LPARAM)iStatusWidths);
			SendMessage(g_hStatusBar, SB_SETTEXT, 2, (LPARAM)"Toolbar & Statusbar Example");
			
			g_hToolBar = CreateWindowEx(0, TOOLBARCLASSNAME, NULL,
			WS_CHILD | WS_VISIBLE, 0, 0, 0, 0,
			hwnd, (HMENU)ID_TOOLBAR, g_hInst, NULL);
			
			// Send the TB_BUTTONSTRUCTSIZE message, which is required for
			// backward compatibility.
			SendMessage(g_hToolBar, TB_BUTTONSTRUCTSIZE, (WPARAM)sizeof(TBBUTTON), 0);
			
			tbab.hInst = HINST_COMMCTRL;
			tbab.nID = IDB_STD_SMALL_COLOR;
			SendMessage(g_hToolBar, TB_ADDBITMAP, 0, (LPARAM)&tbab);
			
			ZeroMemory(tbb, sizeof(tbb));
			
			tbb[0].iBitmap = STD_FILENEW;
			tbb[0].fsState = TBSTATE_ENABLED;
			tbb[0].fsStyle = TBSTYLE_BUTTON;
			tbb[0].idCommand = CM_FILE_NEW;
			
			tbb[1].iBitmap = STD_FILEOPEN;
			tbb[1].fsState = TBSTATE_ENABLED;
			tbb[1].fsStyle = TBSTYLE_BUTTON;
			tbb[1].idCommand = CM_FILE_OPEN;
			
			tbb[2].iBitmap = STD_FILESAVE;
			tbb[2].fsStyle = TBSTYLE_BUTTON;
			tbb[2].idCommand = CM_FILE_SAVE;
			
			tbb[3].fsStyle = TBSTYLE_SEP;
			
			tbb[4].iBitmap = STD_CUT;
			tbb[4].fsStyle = TBSTYLE_BUTTON;
			tbb[4].idCommand = CM_EDIT_CUT;
			
			tbb[5].iBitmap = STD_COPY;
			tbb[5].fsStyle = TBSTYLE_BUTTON;
			tbb[5].idCommand = CM_EDIT_COPY;
			
			tbb[6].iBitmap = STD_PASTE;
			tbb[6].fsStyle = TBSTYLE_BUTTON;
			tbb[6].idCommand = CM_EDIT_PASTE;
			
			tbb[7].fsStyle = TBSTYLE_SEP;
			
			tbb[8].iBitmap = STD_UNDO;
			tbb[8].fsStyle = TBSTYLE_BUTTON;
			tbb[8].idCommand = CM_EDIT_UNDO;
			
			SendMessage(g_hToolBar, TB_ADDBUTTONS, 9, (LPARAM)&tbb);
			return 0;
		}
		case WM_COMMAND: {
			switch(LOWORD(wParam)) {
				case CM_FILE_EXIT:
					PostMessage(hwnd, WM_CLOSE, 0, 0);
					break;
				case CM_FILE_NEW: {
					MDICREATESTRUCT mcs;
					HWND hChild;
					
					mcs.szTitle = "[Untitled]";
					mcs.szClass = g_szChild;
					mcs.hOwner  = g_hInst;
					mcs.x = mcs.cx = CW_USEDEFAULT;
					mcs.y = mcs.cy = CW_USEDEFAULT;
					mcs.style = MDIS_ALLCHILDSTYLES;
					
					hChild = (HWND)SendMessage(g_hMDIClient, WM_MDICREATE,0, (LPARAM)&mcs);
					if(!hChild) {
						MessageBox(hwnd, "MDI Child creation failed.", "Oh Oh...",MB_ICONEXCLAMATION | MB_OK);
					}
					break;
				}
				case CM_FILE_OPEN: {
					MDICREATESTRUCT mcs;
					HWND hChild;
					char szFileName[MAX_PATH];
					
					if(!GetFileName(hwnd, szFileName, FALSE))
						break;

					mcs.szTitle = szFileName;
					mcs.szClass = g_szChild;
					mcs.hOwner  = g_hInst;
					mcs.x = mcs.cx = CW_USEDEFAULT;
					mcs.y = mcs.cy = CW_USEDEFAULT;
					mcs.style = MDIS_ALLCHILDSTYLES;

					hChild = (HWND)SendMessage(g_hMDIClient, WM_MDICREATE, 0, (LPARAM)&mcs);

					if(!hChild) {
						MessageBox(hwnd, "MDI Child creation failed.", "Oh Oh...",
						MB_ICONEXCLAMATION | MB_OK);
					}
					break;
				}
				case CM_WINDOW_TILEHORZ:
					PostMessage(g_hMDIClient, WM_MDITILE, MDITILE_HORIZONTAL, 0);
					break;
				case CM_WINDOW_TILEVERT:
					PostMessage(g_hMDIClient, WM_MDITILE, MDITILE_VERTICAL, 0);
					break;
				case CM_WINDOW_CASCADE:
					PostMessage(g_hMDIClient, WM_MDICASCADE, 0, 0);
					break;
				case CM_WINDOW_ARRANGE:
					PostMessage(g_hMDIClient, WM_MDIICONARRANGE, 0, 0);
					break;
				default: {
					if(LOWORD(wParam) >= ID_MDI_FIRSTCHILD){
						DefFrameProc(hwnd, g_hMDIClient, Message, wParam, lParam);
					} else {
						HWND hChild;
						hChild = (HWND)SendMessage(g_hMDIClient, WM_MDIGETACTIVE,0,0);
						if(hChild){
							SendMessage(hChild, WM_COMMAND, wParam, lParam);
						}
					}
				}
			}
			break;
		}
		case WM_SIZE: {
			RECT rectClient, rectStatus, rectTool;
			UINT uToolHeight, uStatusHeight, uClientAlreaHeight;
			
			SendMessage(g_hToolBar, TB_AUTOSIZE, 0, 0);
			SendMessage(g_hStatusBar, WM_SIZE, 0, 0);
			
			GetClientRect(hwnd, &rectClient);
			GetWindowRect(g_hStatusBar, &rectStatus);
			GetWindowRect(g_hToolBar, &rectTool);
			
			uToolHeight = rectTool.bottom - rectTool.top;
			uStatusHeight = rectStatus.bottom - rectStatus.top;
			uClientAlreaHeight = rectClient.bottom;
			
			MoveWindow(g_hMDIClient, 0, uToolHeight, rectClient.right, uClientAlreaHeight - uStatusHeight - uToolHeight, TRUE);
			break;
		}
		case WM_CLOSE:
			DestroyWindow(hwnd);
			break;
		case WM_DESTROY:
			PostQuitMessage(0);
			break;
		default:
			return DefFrameProc(hwnd, g_hMDIClient, Message, wParam, lParam);
	}
	return 0;
}

LRESULT CALLBACK MDIChildWndProc(HWND hwnd, UINT Message, WPARAM wParam,LPARAM lParam) {
	switch(Message) {
		case WM_CREATE: {
			char szFileName[MAX_PATH];
			HWND hEdit;
			
			hEdit = CreateWindowEx(WS_EX_CLIENTEDGE, "EDIT", "", WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | ES_MULTILINE | ES_WANTRETURN,
				CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
			hwnd, (HMENU)IDC_CHILD_EDIT, g_hInst, NULL);
			
			SendMessage(hEdit, WM_SETFONT,
			(WPARAM)GetStockObject(DEFAULT_GUI_FONT), MAKELPARAM(TRUE, 0));
			
			GetWindowText(hwnd, szFileName, MAX_PATH);
			if(*szFileName != '[') {
				if(!LoadFile(hEdit, szFileName)) {
					MessageBox(hwnd, "Couldn't Load File.", "Error.",MB_OK | MB_ICONEXCLAMATION);
					return -1; //cancel window creation
				}
			}
			break;
		}
		case WM_SIZE:
			if(wParam != SIZE_MINIMIZED)
				MoveWindow(GetDlgItem(hwnd, IDC_CHILD_EDIT), 0, 0, LOWORD(lParam),HIWORD(lParam), TRUE);
			break;
		case WM_MDIACTIVATE: {
			HMENU hMenu, hFileMenu;
			BOOL EnableFlag;
			char szFileName[MAX_PATH];
			
			hMenu = GetMenu(g_hMainWindow);
			if(hwnd == (HWND)lParam){      //being activated
				EnableFlag = TRUE;
			} else{
				EnableFlag = FALSE;    //being de-activated
			}
			EnableMenuItem(hMenu, 1, MF_BYPOSITION | (EnableFlag ? MF_ENABLED : MF_GRAYED));
			EnableMenuItem(hMenu, 2, MF_BYPOSITION | (EnableFlag ? MF_ENABLED : MF_GRAYED));
			
			hFileMenu = GetSubMenu(hMenu, 0);
			EnableMenuItem(hFileMenu, CM_FILE_SAVE, MF_BYCOMMAND | (EnableFlag ? MF_ENABLED : MF_GRAYED));
			EnableMenuItem(hFileMenu, CM_FILE_SAVEAS, MF_BYCOMMAND | (EnableFlag ? MF_ENABLED : MF_GRAYED));
			
			DrawMenuBar(g_hMainWindow);
			
			SendMessage(g_hToolBar, TB_ENABLEBUTTON, CM_FILE_SAVE, MAKELONG(EnableFlag, 0));
			SendMessage(g_hToolBar, TB_ENABLEBUTTON, CM_EDIT_UNDO, MAKELONG(EnableFlag, 0));
			SendMessage(g_hToolBar, TB_ENABLEBUTTON, CM_EDIT_CUT, MAKELONG(EnableFlag, 0));
			SendMessage(g_hToolBar, TB_ENABLEBUTTON, CM_EDIT_COPY, MAKELONG(EnableFlag, 0));
			SendMessage(g_hToolBar, TB_ENABLEBUTTON, CM_EDIT_PASTE, MAKELONG(EnableFlag, 0));
			
			GetWindowText(hwnd, szFileName, MAX_PATH);
			SendMessage(g_hStatusBar, SB_SETTEXT, 0, (LPARAM)(EnableFlag ? szFileName : ""));
			break;
		}
		case WM_SETFOCUS:
			SetFocus(GetDlgItem(hwnd, IDC_CHILD_EDIT));
			break;
		case WM_COMMAND: {
			switch(LOWORD(wParam)) {
				case CM_FILE_SAVE: {
					char szFileName[MAX_PATH];
					
					GetWindowText(hwnd, szFileName, MAX_PATH);
					if(*szFileName != '[') {
						if(!SaveFile(GetDlgItem(hwnd, IDC_CHILD_EDIT), szFileName)) {
							MessageBox(hwnd, "Couldn't Save File.", "Error.",MB_OK | MB_ICONEXCLAMATION);
							return 0;
						}
					} else {
						PostMessage(hwnd, WM_COMMAND,MAKEWPARAM(CM_FILE_SAVEAS, 0), 0);
					}
					return 0;
				}
				case CM_FILE_SAVEAS: {
					char szFileName[MAX_PATH];

					if(GetFileName(hwnd, szFileName, TRUE)) {
						if(!SaveFile(GetDlgItem(hwnd, IDC_CHILD_EDIT), szFileName)) {
							MessageBox(hwnd, "Couldn't Save File.", "Error.",MB_OK | MB_ICONEXCLAMATION);
							return 0;
						} else {
							SetWindowText(hwnd, szFileName);
						}
					}
					return 0;
				}
				case CM_EDIT_UNDO:
					SendDlgItemMessage(hwnd, IDC_CHILD_EDIT, EM_UNDO, 0, 0);
					break;
				case CM_EDIT_CUT:
					SendDlgItemMessage(hwnd, IDC_CHILD_EDIT, WM_CUT, 0, 0);
					break;
				case CM_EDIT_COPY:
					SendDlgItemMessage(hwnd, IDC_CHILD_EDIT, WM_COPY, 0, 0);
					break;
				case CM_EDIT_PASTE:
					SendDlgItemMessage(hwnd, IDC_CHILD_EDIT, WM_PASTE, 0, 0);
					break;
			}
			return 0;
		}
	}
	return DefMDIChildProc(hwnd, Message, wParam, lParam);
}
