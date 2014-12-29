/*
	Simple Arrays
*/
int SimpleVar;
int SimpleDefinedVar = 100;
const int ConstVar = 0xBB;
int VarListA = 2, VarListB = 0, VarListC = 3;
#define C int C
void (*FunctionPointer)(int) = 0;

/*
	Arrays
*/
float Array[] = {1,2,3,4,5};
float DefinedArray[4] = {1,2,3,4};
float ZeroedArray[5] = {};
float PartiallyZeroedArray[6] = {1,2};

/*
	Char arrays
*/
const char* ConstTextA = "This is a bit of text";
char ConstTextB[] = "This is a bit of text too";
char TextBuffer[256];
const char* ConstTextListA = "waw", ConstTextListB = "waw", ConstTextListC = "waw";

int main() {
	C;
}
