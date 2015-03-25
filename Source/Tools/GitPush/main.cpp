#include <windows.h>
#include <stdio.h>

void GitExecute(const char* command) {
	system(command);
}

void GitAdd(const char* path) {
	char buffer[512];
	snprintf(buffer,512,"git add --ignore-removal %s",path);
	GitExecute(buffer);
}

void GitCommit() {
	char buffer[512];
	char comment[512];
	printf("Comment? ");
	scanf("%[^\n]",comment);
	snprintf(buffer,512,"git commit -a -m \"%s\"",comment);
	GitExecute(buffer);
}

void GitPush() {
	GitExecute("git push origin master");
}

int main() {
	// Add all folders
	GitAdd("AStyle");
	GitAdd("Help");
	GitAdd("Icons");
	GitAdd("Lang");
	GitAdd("Source");
	GitAdd("Templates");
	
	// Add setup scripts
	GitAdd("*.nsi");

	// Add project resources
	GitAdd("devcpp.exe.manifest");
	GitAdd("devcpp.ico");
	
	// Add useful text files
	GitAdd("COPYING.txt");
	GitAdd("NEWS.txt");
		
	// Add git files
	GitAdd(".gitignore");
	
	// commit with comment
	GitCommit();
	
	// push
	GitPush();
	return 0;
}
