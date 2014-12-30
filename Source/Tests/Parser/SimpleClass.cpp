class ProtoClass {
	public:
		ProtoClass();
		ProtoClass(int a);
		ProtoClass(int a, int b);
		~ProtoClass();
		void Foo() {
		}
		void Bar();
};

ProtoClass::ProtoClass() {
	
}

ProtoClass::ProtoClass(int a) {
	
}

ProtoClass::ProtoClass(int a, int b) {
	
}

ProtoClass::~ProtoClass() {
	
}

void ProtoClass::Bar() {
	
}

int main() {
	ProtoClass A;
	A.Foo();
	A.Bar();
}
