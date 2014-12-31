// if, else, endif test
#if WIN32
int Correct01;
#else
int Incorrect01;
#endif

// defined test
#if defined(WIN32)
int Correct02;
#else
int Incorrect02;
#endif

// !defined, elif test
#if !defined(WIN32)
int Incorrect03;
#elif defined(WIN32)
int Correct03;
#else
int Incorrect03;
#endif

// user defined test, "&&" test
#define PI 3.14159
#if !defined(WIN32)
int Incorrect04;
#elif defined(WIN32) && defined(PI)
int Correct04;
#else
int Incorrect04;
#endif

// "and" test
#define A
#define B
#if defined(A) and !defined(C)
int Correct05;
#else
int Incorrect05;
#endif

// ifdef test
#ifdef WIN32
int Correct07;
#else
int Incorrect07;
#endif

// ifndef test
#ifndef WIN33
int Correct08;
#else
int Incorrect08;
#endif

// undef test
#define UndefA
#define UndefB
#undef UndefB
#if defined(UndefA) and !defined(UndefB)
int Correct09;
#else
int Incorrect09;
#endif

// undef test 2
#undef UndefA
#undef UndefB
#if defined(UndefA) or defined(UndefB)
int Incorrect10;
#else
int Correct10;
#endif

// || test
#define OrA
#if defined(OrA) or defined(OrB)
int Correct11;
#else
int Incorrect11;
#endif

int main(){
}
