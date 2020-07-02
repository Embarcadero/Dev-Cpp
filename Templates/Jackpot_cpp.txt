#include <iostream>
#include <stdlib.h>
#include <time.h>

using namespace std;

void Start();
void GetResults();

int i, j, life, maxrand;
char c;

void Start() {
	i = 0;
	j = 0;
	life = 0;
	maxrand = 6;
	
	cout << "Select difficulty mode:\n"; // the user has to select a difficutly level
	cout << "1 : Easy (0-15)\n";
	cout << "2 : Medium (0-30)\n";
	cout << "3 : Difficult (0-50)\n";
	cout << "or type another key to quit\n";
	c = 30;

	cin >> c;                   // read the user's choice
	cout << "\n";

	switch (c) {
		case '1':
			maxrand = 15;  // the random number will be between 0 and maxrand
			break;
		case '2':
			maxrand = 30;
			break;
		case '3':
			maxrand = 50;
			break;
		default:
			exit(0);
		break;
	}

	life = 5;         // number of lifes of the player
	srand((unsigned)time(NULL)); // init Rand() function
	j = rand() % maxrand;  // j get a random value between 0 and maxrand
	
	GetResults();
}

void GetResults() {
	if (life <= 0) { // if player has no more life then he loses
		cout << "You lose !\n\n";
		Start();
	}

	cout << "Type a number: \n";
	cin >> i;
	
	if((i>maxrand) || (i<0)) { // if the user number isn't correct, restart
		cout << "Error: number not between 0 and \n" << maxrand;
		GetResults();
	}

	if(i == j) {
		cout << "YOU WIN!\n\n"; // the user found the secret number
		Start();
	} else if(i>j) {
		cout << "Too BIG\n";
		life = life - 1;
		cout << "Lives remaining: " << life << "\n\n";
		GetResults();
	} else if(i<j) {
		cout << "Too SMALL\n";
		life = life - 1;
		cout << "Lives remaining: " << life << "\n\n";
		GetResults();
	}
}

int main() {
	cout << "** Jackpot game **\n";
	cout << "The goal of this game is to guess a number.\n";
	cout << "Jackpot will tell you if the number is too big or too\n";
	cout << "small compared to the secret number to find.\n\n";
	Start();
	return 0;
}