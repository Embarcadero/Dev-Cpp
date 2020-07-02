#ifndef FXSHADER_INCLUDE
#define FXSHADER_INCLUDE

#include <list>

#include "FXHandle.h"
#include "Object.h"

class FXShader : public FXHandle {
	public:
		FXShader(const char* name);
		~FXShader();
		
		std::list<Object*>::iterator nulldummy;
		std::list<Object*>::iterator begin; // zelfde naam als std::list
		std::list<Object*>::iterator end;
		bool unused;
		
		void Get(const char* name);
		void Reset();
		void Print();
};

#endif
