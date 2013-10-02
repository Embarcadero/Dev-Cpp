#include <stdio.h>
#include <string>
using std::string;
#include <vector>
using std::vector;

vector<int> hashtable = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	1,
	2,
	3,
	4,
	5,
	6,
	7,
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	15,
	16,
	17,
	18,
	19,
	20,
	21,
	22,
	23,
	24,
	25,
	26,
	0,
	0,
	0,
	0,
	0,
	0,
	2,
	3,
	4,
	5,
	6,
	7,
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	15,
	16,
	17,
	18,
	19,
	20,
	21,
	22,
	23,
	24,
	25,
	26,
	27,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0};

vector<string> keywords = {
  "alignas",
  "alignof",
  "and",
  "and_eq",
  "asm",
  "auto",
  "bitand",
  "bitor",
  "bool",
  "break",
  "case",
  "catch",
  "char",
  "char16_t",
  "char32_t",
  "class",
  "compl",
  "const",
  "constexpr",
  "const_cast",
  "continue",
  "decltype",
  "default",
  "delete",
  "do",
  "double",
  "dynamic_cast",
  "else",
  "enum",
  "explicit",
  "export",
  "extern",
  "false",
  "float",
  "for",
  "friend",
  "goto",
  "if",
  "inline",
  "int",
  "long",
  "mutable",
  "namespace",
  "new",
  "noexcept",
  "not",
  "not_eq",
  "nullptr",
  "operator",
  "or",
  "or_eq",
  "private",
  "protected",
  "public",
  "register",
  "reinterpret_cast",
  "return",
  "short",
  "signed",
  "sizeof",
  "static",
  "static_assert",
  "static_cast",
  "struct",
  "switch",
  "template",
  "this",
  "thread_local",
  "throw",
  "true",
  "try",
  "typedef",
  "typeid",
  "typename",
  "union",
  "unsigned",
  "using",
  "virtual",
  "void",
  "volatile",
  "wchar_t",
  "while",
  "xor",
  "xor_eq"
};

//asm do if return try
//auto double inline short typedef
//bool dynamic_cast int signed typeid
//break else long sizeof typename
//case enum mutable static union
//catch explicit namespace static_assert unsigned
//char export new static_cast using
//class extern operator struct virtual
//const false private switch void
//const_cast float protected template volatile
//continue for public this wchar_t
//default friend register throw while
//delete goto reinterpret_cast true
//and and_eq bitand bitor compl not
//not_eq or or_eq xor xor_eq

int GetHash(const char* text) {
	int result = 0;
	while(*text == '_' or (*text >= '0' and *text <= '9') or (*text >= 'a' and *text <= 'z') or (*text >= 'A' and *text <= 'Z')) {
		result += hashtable[(int)*text];
		text++;
	}
	return result;
}

int main() {
	FILE* text = fopen("t.txt","wb");
	if(text) {
		
		// Create a list of keywords sorted by hash
		vector<vector<string>> hashedkeywords;
		for(int i = 0;i < (int)keywords.size();i++) {
			int hash = GetHash(keywords[i].c_str());
			
			// Only expand
			if((int)hashedkeywords.size() < hash + 1) {
				hashedkeywords.resize(hash + 1);
			}
			hashedkeywords[hash].push_back(keywords[i]);
		}
	
		// Save function bodies
		for(int i = 0;i < (int)hashedkeywords.size();i++) {
			if(hashedkeywords[i].size() == 0) {
				continue;
			} else {
				fprintf(text,"function TSynCppSyn.Func%d: TtkTokenKind;\r\n",i);
				fprintf(text,"begin\r\n");
				for(int j = 0;j < (int)hashedkeywords[i].size();j++) {
					fprintf(text,"  if KeyComp('%s') then Result := tkKey else\r\n",hashedkeywords[i][j].c_str());
				}
				fprintf(text,"  Result := tkIdentifier\r\n");
				fprintf(text,"end;\r\n\r\n");
			}
		}
	
		// Save function declarations
		for(int i = 0;i < (int)hashedkeywords.size();i++) {
			if(hashedkeywords[i].size() > 0) {
				fprintf(text,"    function Func%d: TtkTokenKind;\r\n",i);
			}
		}

		// Save function assignments
		for(int i = 0;i < (int)hashedkeywords.size();i++) {
			if(hashedkeywords[i].size() > 0) {
				fprintf(text,"  fIdentFuncTable[%d] := Func%d;\r\n",i,i);
			}
		}
		fclose(text);
	}
}
