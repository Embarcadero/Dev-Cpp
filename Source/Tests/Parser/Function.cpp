#include "Function.h"
/*
	Global functions
*/
void FunctionDeclDefPair() {
	return;
}
void FunctionA() {
}
void FunctionB(int a, int* p, ...) {
}
void FunctionC(const char* arg = "Hello") {
}
inline void FunctionD(int a) {
}
/*
	Member functions
*/
class DummyClass {
	private:
		void PrivateMemberFunction();
	protected:
		void ProtectedMemberFunction();
	public:
		// Constructor/destructor test
		DummyClass();
		~DummyClass();
		void PublicMemberFunction();
		
		// Conversion operators (to int)
	    operator int*(); // ERROR: currently shows type "operator"
	    operator const int*() const; // ERROR: currently shows type "operator const"
	
		// Operation with DummyClass
	    DummyClass& operator += (const DummyClass& v);
	    DummyClass& operator -= (const DummyClass& v);
	    DummyClass& operator *= (const DummyClass& v);
	    DummyClass& operator /= (const DummyClass& v);
	
		// Operation with constants
	    DummyClass operator + () const;
	    DummyClass operator - () const;
	    DummyClass operator * () const;
	    DummyClass operator / (int) const; // needs type

		// Comparison operators
	    bool operator == (const DummyClass& v) const;
	    bool operator != (const DummyClass& v) const;
};

DummyClass::DummyClass() {
}
DummyClass::~DummyClass() {
}
void DummyClass::PrivateMemberFunction() {
}
void DummyClass::ProtectedMemberFunction() {
}
void DummyClass::PublicMemberFunction() {
}

// Conversion operators (to int)
inline DummyClass::operator int*() { // ERROR: does not link with class
}
inline DummyClass::operator const int*() const { // ERROR: does not link with class
}

// Operation with DummyClass
inline DummyClass& DummyClass::operator += (const DummyClass& v) {
}
inline DummyClass& DummyClass::operator -= (const DummyClass& v) {
}
inline DummyClass& DummyClass::operator *= (const DummyClass& v) {
}
inline DummyClass& DummyClass::operator /= (const DummyClass& v) {
}

// Operation with constants
inline DummyClass DummyClass::operator + () const {
}
inline DummyClass DummyClass::operator - () const {
}
inline DummyClass DummyClass::operator * () const {
}
inline DummyClass DummyClass::operator / (int) const { // needs type
}

// Comparison operators
inline bool DummyClass::operator == (const DummyClass& v) const {
}
inline bool DummyClass::operator != (const DummyClass& v) const {
}

int main() {
}
