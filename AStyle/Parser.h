#ifndef PARSER_INCLUDE
#define PARSER_INCLUDE

#include "Variable.h"
#include <windows.h>
#include <stdio.h>
#include "Messagebox.h"
#include "Console.h"
#include "Resource.h"
#include "Scene.h"
#include "Renderer.h"
#include "Bezier.h"

enum TokenKind {
    ttWhile,
    ttIf,
    ttElse,
    ttOpenParenth, // (
    ttCloseParenth, // )
    ttOpenBracket, // {
    ttCloseBracket, // }
    ttOpenSquare, // [
    ttCloseSquare, // ]
    ttVarType,
    ttUnknown,
    ttMulDiv,
    ttAddSub,
    ttAssign,
    ttMember,
};

class Token {
	public:
		~Token() {
			if(text) {
				delete[] text;
			}
			if(data) {
				delete data;
			}
		}

		char* text;
		TokenKind type;
		unsigned int complement;
		unsigned int line;
		Var* data; // used to store temp values
};

class Parser {
	private:
		std::vector<Var*> vars;
		std::vector<Token*> tokens;
		unsigned int index;
		unsigned int line;
		int level;
		bool printtiming;
		bool savetokens;
		// remove variables if out of scope
		void DecScope();
		void IncScope();

		// Determine value of text
		int GetIntValue(const char* text);
		float GetFloatValue(const char* text);
		char* GetStringValue(const char* text);
		float3* GetFloat3Value(const char* text);
		Var* GetVariable(const char* text);

		// Determine value of text (including temps from Evaluate)
		int GetTokenIntValue(Token* token);
		float GetTokenFloatValue(Token* token);
		float3* GetTokenFloat3Value(Token* token);
		VarKind GetTokenKind(Token* token);

		// All-in-one for evaluating statements
		void GetExpression(std::vector<Token*>& result);
		Var* Evaluate(std::vector<Token*>& tokens);
		Var* Evaluate(Token* token); // much faster

		// Execute statements and modify index
		void HandleVar();
		Var* HandleFunction(Var* parent);
		void HandleAssignment(Var* lvalue);
		bool HandleBoolean();

		void Clear();
		void Tokenize(const char* file);
		void Optimize();
		void Parse();

		// Tokenizer helper
		void AddToken(int length,char* ptr);
	public:
		Parser(bool printtiming,bool savetokens);
		Parser(const char* file);
		~Parser();



		void AddArg(Var* value);
		void Execute(const char* file);
};

#endif
