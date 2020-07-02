#include "resource.h"

HWND LogBoxHandle;
void Log(const char* format,...) {
	if(!LogBoxHandle) {
		return;
	}
	
	va_list parameters;

	char text[2048] = "";

	// Format final message
	va_start(parameters,format);
	vsnprintf(text,2048,format,parameters);
	va_end(parameters);
	
	// Append to log
	int OldLen = SendMessage(LogBoxHandle,WM_GETTEXTLENGTH,0,0);
	SendMessage(LogBoxHandle,EM_SETSEL,(WPARAM)OldLen,(LPARAM)OldLen);
	SendMessage(LogBoxHandle,EM_REPLACESEL,0,(LPARAM)text);
}
