#ifndef LANGCOMPARE_H
#define LANGCOMPARE_H

#include "LangFile.h"

class LangCompare {
	private:
		LangFile& reference;
		LangFile& compare;
		void Compare();
	public:
		LangCompare(LangFile& reference, LangFile& compare);
		~LangCompare();
	protected:
};

#endif
