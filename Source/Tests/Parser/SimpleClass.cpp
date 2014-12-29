class ProtoClass {
	public:
		ProtoClass();
		~ProtoClass();
		void Foo() {
		}
		void Bar();
};

ProtoClass::ProtoClass() {
	
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
