#include <stdio.h>

void Foo() {
	printf("Hello World!\n");
}

int main(int argc, char** argv) {
	char input = 0;

	printf("Hello! This is a console application.\n");
	printf("Press q to quit, press a to execute foo.\n");
	while(1) {
		if(scanf("%c",&input) == 1) {
			if(input == 'a') {
				Foo();
			} else if(input == 'q') {
				break;
			} else if(input != '\n') {
				printf("Unknown command '%c'! Ignoring...\n",input);
			}
		} else {
			printf("Invalid input! Ignoring...\n");
		}
	}

	return 0;
}
