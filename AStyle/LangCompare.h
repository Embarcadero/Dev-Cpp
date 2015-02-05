#ifndef LANGCOMPARE_H
#define LANGCOMPARE_H

#include "LangFile.h"

class LangCompare {
	private:
		LangFile& reference;
		LangFile& compare;
		void Compare();
	public:
		LangCompare(const LangFile& reference, const LangFile& compare);
		~LangCompare();
	protected:
};

#endif
