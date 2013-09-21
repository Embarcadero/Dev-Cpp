#include <windows.h>

int main() {
	ShellExecute(NULL,"open","devcpp.exe","-c .\\config",NULL,SW_SHOW);
	return 0;
}
