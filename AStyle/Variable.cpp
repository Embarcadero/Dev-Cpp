#include "Variable.h"

Var::Var(const char* type,const char* name,int level) {
	SetTypeFromString(type);
	this->name = strdup(name); // copy
	this->level = level;
}
Var::Var(VarKind type,const char* name,int level) {
	this->type = type;
	this->name = strdup(name); // copy
	this->level = level;
}
void Var::SetTypeFromString(const char* type) {
	if(!strcmp(type,"int")) {
		this->type = vtInt;
	} else if(!strcmp(type,"float")) {
		this->type = vtFloat;
	} else if(!strcmp(type,"string")) {
		this->type = vtString;
	} else if(!strcmp(type,"Object")) {
		this->type = vtObject;
		this->objectvalue = NULL; // call default constructor?
	} else if(!strcmp(type,"Path")) {
		this->type = vtPath;
		this->pathvalue = new Path();
	} else if(!strcmp(type,"float3")) {
		this->type = vtFloat3;
		this->float3value = new float3();
	} else if(!strcmp(type,"Timer")) {
		this->type = vtTimer;
		this->timervalue = NULL; // function must return this
	} else {
		this->type = vtUnknown;
		console->Write("Unknown type \"%s\"\r\n",type);
	}
}
Var::~Var() {
	delete[] name;
	switch(type) {
		case vtPath: {
			delete pathvalue;
			break;
		}
		case vtString: {
			delete[] stringvalue;
			break;
		}
		case vtFloat3: {
			delete float3value;
			break;
		}
	}
	// object and path only hold pointer, don't own data
}
int Var::GetIntValue() {
	switch(type) {
		case vtInt: {
			return intvalue;
		}
		case vtFloat: {
			return (int)floatvalue;
		}
		default: {
			return 0;
		}
	}
}
float Var::GetFloatValue() {
	switch(type) {
		case vtInt: {
			return intvalue;
		}
		case vtFloat: {
			return floatvalue;
		}
		default: {
			return 0.0f;
		}
	}
}
float3* Var::GetFloat3Value() {
	switch(type) {
		case vtFloat3: {
			return float3value;
			break;
		}
		default: {
			return new float3(0,0,0);
		}
	}
}
char* Var::GetStringValue() {
	switch(type) {
		case vtInt: {
			char* buf = new char[128];
			snprintf(buf,128,"%d",intvalue);
			return buf;
		}
		case vtFloat: {
			char* buf = new char[128];
			snprintf(buf,128,"%g",floatvalue);
			return buf;
		}
		case vtString: {
			return strdup(stringvalue);
		}
		case vtObject: {
			char* buf = new char[128];
			snprintf(buf,128,"%s",objectvalue->GetName());
			return buf;
		}
		case vtFloat3: {
			char* buf = new char[128];
			snprintf(buf,128,"[%g %g %g]",
			         float3value->x,
			         float3value->y,
			         float3value->z);
			return buf;
			break;
		}
		default: {
			return strdup("");
		}
	}
}
bool Var::Equals(Var* rvalue) {
	switch(type) {
		case vtInt: {
			return GetIntValue() == rvalue->GetIntValue();
		}
		case vtFloat: {
			return GetFloatValue() == rvalue->GetFloatValue();
		}
		case vtString: {
			char* lvaluetext = GetStringValue();
			char* rvaluetext = rvalue->GetStringValue();
			bool result = !strcmp(lvaluetext,rvaluetext);
			delete[] lvaluetext;
			delete[] rvaluetext;
			return result;
		}
		case vtFloat3: {
			return float3value == rvalue->float3value;
		}
		case vtObject: {
			return objectvalue == rvalue->objectvalue; // compare pointers...
		}
		default: {
			return false;
		}
	}
}
bool Var::Greater(Var* rvalue) {
	switch(type) {
		case vtInt: {
			return GetIntValue() > rvalue->GetIntValue();
		}
		case vtFloat: {
			return GetFloatValue() > rvalue->GetFloatValue();
		}
		default: {
			return false;
		}
	}
}
bool Var::Less(Var* rvalue) {
	switch(type) {
		case vtInt: {
			return GetIntValue() < rvalue->GetIntValue();
		}
		case vtFloat: {
			return GetFloatValue() < rvalue->GetFloatValue();
		}
		default: {
			return false;
		}
	}
}
