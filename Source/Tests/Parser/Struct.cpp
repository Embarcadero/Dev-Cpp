/*
	Correct forward struct, struct
*/
struct UnfinishedStructName; // forward declaration
struct StructName {
	int VarA;
	float VarB;
	bool VarC;
	struct Child {
		float ChildVar;
	};
};

/*
	Correct forward class, class and derived class
*/
class UnfinishedClassName; // forward declaration
class ClassName {
	int PrivVarA;
	protected:
		int ProtVarA;
	public:
		int VarA;
		float VarB;
		bool VarC;
		struct Child {
			float ChildVar;
		};
};
class DerivedClassName : public ClassName {
	public:
		int VarD;
};

/*
	Correct union
*/
union UnionName {
	struct PartA {
		int PartAVar;
	};
	struct PartB {
		int PartBVar;
	};
};

/*
	Correct typedef
*/
typedef struct UnfinishedStructName UnfinishedClassName;

/*
	Complicated typedef
*/
typedef struct tagRECT {
  LONG left;
  LONG top;
  LONG right;
  LONG bottom;
} RECT,*PRECT,*NPRECT,*LPRECT;

/*
	Correct enum
*/
enum EnumName {
	A,
	B,
	C,
	D,
	E
};

/*
	Struct usage. Hover above statements to check if they are parsed correctly
*/
int main() {
	StructName;
	StructName.VarA;
	StructName.VarB;
	StructName.VarC;
	StructName.Child.ChildVar;
	ClassName.VarA;
	ClassName.VarB;
	ClassName.VarC;
	ClassName.Child.ChildVar;
	DerivedClassName.VarA;
	DerivedClassName.VarB;
	DerivedClassName.VarC;
	DerivedClassName.Child.ChildVar;
	DerivedClassName.VarD;
	UnionName.PartA;
	UnionName.PartB;
}
