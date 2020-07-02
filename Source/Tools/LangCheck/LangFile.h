#ifndef LANGFILE_H
#define LANGFILE_H

#include <vector>
using std::vector;
#include <string>
using std::string;
#include <stdio.h>
#include <windows.h>
#include "resource.h"

struct LangItem {
	int ID;
	string text;
	string formatspec;
};

class LangFile {
	public:
		LangFile();
		LangFile(const char* FileName);
		~LangFile();
		void OpenFile(const char* FileName);
		LangItem* FindID(int ID);
		vector<LangItem> contents;
};

#endif
