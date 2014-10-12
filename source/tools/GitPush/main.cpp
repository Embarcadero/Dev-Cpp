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
	char buffer[512];
	GitExecute("git push origin master");
}

int main() {
	// add source files
	GitAdd("source source");
	GitAdd("devcpp.ico");
	GitAdd("*.nsi");
	GitAdd("NEWS.txt");
	
	// commit with comment
	GitCommit();
	
	// push
	GitPush();
	return 0;
}
