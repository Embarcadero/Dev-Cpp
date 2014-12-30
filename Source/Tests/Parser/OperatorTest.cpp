class DummyClass {
	public:
		// Conversion operators (to int)
	    operator int*(); // ERROR: currently shows type "operator"
	    operator const int*() const; // ERROR: currently shows type "operator const"
};

// Conversion operators (to int)
inline DummyClass::operator int*() { // ERROR: does not link with class
}
inline DummyClass::operator const int*() const { // ERROR: does not link with class
}
