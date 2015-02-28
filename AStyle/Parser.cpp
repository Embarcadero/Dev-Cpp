#include "Parser.h"

Parser::Parser(bool printtiming,bool savetokens) {
	this->printtiming = printtiming;
	this->savetokens = savetokens;
}
Parser::Parser(const char* file) {
	savetokens = false;
	printtiming = false;
	Execute(file);
}
Parser::~Parser() {
	Clear();
}
void Parser::IncScope() {
	level++;
}
void Parser::DecScope() {
	level--;
	for(unsigned int i = 0; i < vars.size(); i++) {
		if(vars[i]->level > level) { // remove out of scope variables
			delete vars[i];
			vars.erase(vars.begin() + i);
			i--;
		}
	}
}
Var* Parser::GetVariable(const char* text) {
	for(unsigned int i = 0; i < vars.size(); i++) {
		if(!strcmp(text,vars[i]->name)) {
			return vars[i];
		}
	}
	return NULL;
}
VarKind Parser::GetTokenKind(Token* token) {
	if(token->data) {
		return token->data->type; // cache
	} else {
		Var* variable = GetVariable(token->text);
		if(variable) {
			return variable->type;
		} else if(strchr(token->text,'.')) { // string can contain this too...
			return vtFloat;
		} else if(strchr(token->text,'\"')) { // idem
			return vtString;
		} else if(strchr(token->text,'[') and strchr(token->text,']')) { // [1 2 3]
			return vtFloat3;
		} else {
			for(unsigned int i = 0; i < strlen(token->text); i++) {
				if(!isdigit(token->text[i]) or token->text[i] == '-') {
					return vtUnknown; // NOT an int
				}
			}
			return vtInt;
		}
	}
}
Var* Parser::Evaluate(Token* token) {
	Var* result = GetVariable(token->text);
	if(result) { // don't create a copy...
		return result;
	} else {
		Var* result = new Var(GetTokenKind(token),"tmp",level);
		switch(result->type) {
			case vtInt: {
				result->intvalue = GetIntValue(token->text);
				break;
			}
			case vtFloat: {
				result->floatvalue = GetFloatValue(token->text);
				break;
			}
			case vtFloat3: {
				result->float3value = GetFloat3Value(token->text);
				break;
			}
			default: {
				result->intvalue = 0; // sets enum bits to zero
			}
		}
		return result;
	}
}
Var* Parser::Evaluate(std::vector<Token*>&
                      tokens) { // do not delete tokens, only remove from list

	// Evaluate first value/constant only
	if(tokens.size() == 1) {
		Var* result = Evaluate(tokens[0]);
		tokens.clear();
		return result;
	}

	// Loop to evaluate * and / from left to right
	unsigned int index = 1;
	while(index + 1 < tokens.size()) {
		if(tokens[index]->type == ttMulDiv) {

			Token* lvalue = tokens[index-1];
			Token* rvalue = tokens[index+1];

			// Store result in lvalue data slot, remove rvalue pointer
			Var* result = new Var(GetTokenKind(lvalue),"tmp",level);

			// lvalue determines operation result type, delete rvalue
			switch(result->type) {
				case vtInt: {
					if(!strcmp(tokens[index]->text,"*")) {
						result->intvalue = GetTokenIntValue(lvalue) * GetTokenIntValue(rvalue);
					} else {
						result->intvalue = GetTokenIntValue(lvalue) / GetTokenIntValue(rvalue);
					}
					break;
				}
				case vtFloat: {
					if(!strcmp(tokens[index]->text,"*")) {
						result->floatvalue = GetTokenFloatValue(lvalue) * GetTokenFloatValue(rvalue);
					} else {
						result->floatvalue = GetTokenFloatValue(lvalue) / GetTokenFloatValue(rvalue);
					}
					break;
				}
				default: {
					result->intvalue = 0;
				}
			}

			// put result in lvalue
			delete lvalue->data;
			lvalue->data = result;

			// Before we remove the pointers, set data of rvalue to NULL
			tokens[index+1]->data = NULL;

			// remove operator and rvalue
			tokens.erase(tokens.begin() + index,tokens.begin() + index + 2);
		} else {
			index += 2; // try next operator
		}
	}

	// Loop to evaluate * and / from left to right
	index = 1;
	while(index + 1 < tokens.size()) {
		if(tokens[index]->type == ttAddSub) {

			Token* lvalue = tokens[index-1];
			Token* rvalue = tokens[index+1];

			// Store result in lvalue data slot, remove rvalue pointer
			Var* result = new Var(GetTokenKind(lvalue),"tmp",level);

			// lvalue determines operation result type, delete rvalue
			switch(result->type) {
				case vtInt: {
					if(!strcmp(tokens[index]->text,"+")) {
						result->intvalue = GetTokenIntValue(lvalue) + GetTokenIntValue(rvalue);
					} else {
						result->intvalue = GetTokenIntValue(lvalue) - GetTokenIntValue(rvalue);
					}
					break;
				}
				case vtFloat: {
					if(!strcmp(tokens[index]->text,"+")) {
						result->floatvalue = GetTokenFloatValue(lvalue) + GetTokenFloatValue(rvalue);
					} else {
						result->floatvalue = GetTokenFloatValue(lvalue) - GetTokenFloatValue(rvalue);
					}
					break;
				}
				default: {
					result->intvalue = 0;
				}
			}

			// put result in lvalue
			delete lvalue->data;
			lvalue->data = result;

			// Before we remove the pointers, set data of rvalue to NULL
			tokens[index+1]->data = NULL;

			// remove operator and rvalue
			tokens.erase(tokens.begin() + index,tokens.begin() + index + 2);
		} else {
			index += 2; // try next operator
		}
	}

	// Now, the last value should contain the final value
	Var* result = tokens[0]->data;
	tokens[0]->data = NULL;
	tokens.clear();
	return result;
}
int Parser::GetIntValue(const char* text) {
	Var* variable = GetVariable(text); // Is this a variable name?
	if(variable) {
		return variable->GetIntValue();
	} else { // literal value, interpret as int
		int tmp = 0;
		sscanf(text,"%d",&tmp);
		return tmp;
	}
}
float Parser::GetFloatValue(const char* text) {
	Var* variable = GetVariable(text);
	if(variable) {
		return variable->GetFloatValue();
	} else { // literal value, interpret as float
		float tmp = 0.0f;
		sscanf(text,"%f",&tmp);
		return tmp;
	}
}
char* Parser::GetStringValue(const char*
                             text) { // always returns copy, don't forget to free
	Var* variable = GetVariable(text);
	if(variable) {
		return variable->GetStringValue();
	} else {
		return strdup(text);
	}
}
float3* Parser::GetFloat3Value(const char* text) {
	Var* variable = GetVariable(text);
	if(variable) {
		return variable->GetFloat3Value();
	} else { // literal value, interpret as float3
		float3* tmp = new float3();
		sscanf(text,"[%g %g %g]",
		       &tmp->x,
		       &tmp->y,
		       &tmp->z);
		return tmp;
	}
}
int Parser::GetTokenIntValue(Token* token) {
	if(token->data) {
		return token->data->GetIntValue();
	} else {
		return GetIntValue(token->text);
	}
}
float Parser::GetTokenFloatValue(Token* token) {
	if(token->data) {
		return token->data->GetFloatValue();
	} else {
		return GetFloatValue(token->text);
	}
}
float3* Parser::GetTokenFloat3Value(Token* token) {
	if(token->data) {
		return token->data->GetFloat3Value();
	} else {
		return GetFloat3Value(token->text);
	}
}
bool Parser::HandleBoolean() {

	unsigned int oldindex = index + 1; // opening brace index

	// Evaluate left part
	Var* lvalue = Evaluate(tokens[index + 2]);

	// obtain operator
	char* op = tokens[index + 3]->text;

	// Evaluate right part
	Var* rvalue = Evaluate(tokens[index + 4]);

	// step past ending )
	index = tokens[oldindex]->complement + 1;

	if(!strcmp(op,"==")) {
		return lvalue->Equals(rvalue);
	} else if(!strcmp(op,"!=")) {
		return !lvalue->Equals(rvalue);
	} else if(!strcmp(op,">")) {
		return lvalue->Greater(rvalue);
	} else if(!strcmp(op,"<=")) {
		return !lvalue->Greater(rvalue);
	} else if(!strcmp(op,"<")) {
		return lvalue->Less(rvalue);
	} else if(!strcmp(op,">=")) {
		return !lvalue->Less(rvalue);
	} else {
		console->Write("Unknown operation \"%s\"\r\n",op);
		return false;
	}
}
void Parser::HandleVar() {

	// obtain pieces
	char* type = tokens[index]->text;
	char* name = tokens[index + 1]->text;

	if(index + 2 < tokens.size() && tokens[index + 2]->type == ttAssign) {
		index++; // step past variable type, onto name
	} else {
		index+=2; // step past name
	}

	// Add, value will be set by HandleStatement
	vars.push_back(new Var(type,name,level));
}
Var* Parser::HandleFunction(Var* parent) {

	int oldindex = index;

	std::vector<char*> args;

	// Extract name
	char* function = tokens[index]->text;

	// Step over starting (
	while(index < tokens.size()) {
		if(tokens[index]->type == ttOpenParenth) {
			index++; // step over )
			break;
		}
		index++;
	}

	// Extract arguments until )
	while(index < tokens.size()) {
		if(tokens[index]->type == ttCloseParenth) {
			index++; // step over )
			break;
		} else { // commas and quotes have already been removed
			args.push_back(tokens[index]->text);
		}
		index++;
	}

	Var* result = NULL;

	// global functions
	if(parent == NULL) {
		if(!strcmp(function,"AddObject")) {
			if(args.size() == 8) { // add from .object file

				float3 pos(GetFloatValue(args[1]),GetFloatValue(args[2]),
				           GetFloatValue(args[3]));
				float3 rot(GetFloatValue(args[4]),GetFloatValue(args[5]),
				           GetFloatValue(args[6]));

				// Return object itself
				result = new Var(vtObject,"tmp",level);
				result->objectvalue = new Object(args[0],pos,rot,GetFloatValue(args[7]));
			} else if(args.size() == 9) { // add, name not provided
				float3 pos(GetFloatValue(args[2]),GetFloatValue(args[3]),
				           GetFloatValue(args[4]));
				float3 rot(GetFloatValue(args[5]),GetFloatValue(args[6]),
				           GetFloatValue(args[7]));

				// Use model file name as name
				char name[MAX_PATH];
				ExtractFileName(args[0],name);

				// Return object itself
				result = new Var(vtObject,"tmp",level);
				result->objectvalue = new Object(name,args[0],args[1],pos,rot,
				                                 GetFloatValue(args[8]));
			} else if(args.size() == 10) { // add, name provided
				float3 pos(GetFloatValue(args[3]),GetFloatValue(args[4]),
				           GetFloatValue(args[5]));
				float3 rot(GetFloatValue(args[6]),GetFloatValue(args[7]),
				           GetFloatValue(args[8]));

				// Return object itself
				result = new Var(vtObject,"tmp",level);
				result->objectvalue = new Object(args[0],args[1],args[2],pos,rot,
				                                 GetFloatValue(args[9]));
			} else {
				console->Write("Expected 8 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"AddPlane")) {
			if(args.size() > 9) {

				float3 pos(GetFloatValue(args[1]),GetFloatValue(args[2]),
				           GetFloatValue(args[3]));
				float3 rot(GetFloatValue(args[4]),GetFloatValue(args[5]),
				           GetFloatValue(args[6]));

				scene->objects->AddPlane("ProceduralPlane",args[0],pos,rot,
				                         GetFloatValue(args[7]),GetIntValue(args[8]),GetIntValue(args[9]),NULL);
			} else {
				console->Write("Expected 10 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetGround")) {
			if(args.size() > 5) {
				scene->AddHeightMap(args[0],args[1],GetFloatValue(args[2]),
				                    GetFloatValue(args[3]),GetFloatValue(args[4]),GetIntValue(args[5]));
			} else {
				console->Write("Expected 6 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetTime")) {
			if(args.size() > 1) {
				renderer->SetTime(GetIntValue(args[0]),GetIntValue(args[1]));
			} else {
				console->Write("Expected 2 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetTimeMulti")) {
			if(args.size() > 0) {
				renderer->SetTimeMulti(GetIntValue(args[0]));
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"AddDirlight")) {
			if(args.size() > 5) {

				float3 dir(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				float3 col(GetFloatValue(args[3]),GetFloatValue(args[4]),
				           GetFloatValue(args[5]));

				new Dirlight(dir,col,true);
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"AddSpotlight")) {
			if(args.size() > 9) {

				float3 pos(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				float3 lat(GetFloatValue(args[3]),GetFloatValue(args[4]),
				           GetFloatValue(args[5])); // look at
				float3 col(GetFloatValue(args[6]),GetFloatValue(args[7]),
				           GetFloatValue(args[8]));

				new Spotlight(pos,lat,col,GetFloatValue(args[9]),true);
			} else {
				console->Write("Expected 10 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"AddPointlight")) {
			if(args.size() > 5) {

				float3 pos(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				float3 col(GetFloatValue(args[3]),GetFloatValue(args[4]),
				           GetFloatValue(args[5]));

				scene->lights->AddPointlight(pos,col,true);
			} else {
				console->Write("Expected 6 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetCameraPos")) {
			if(args.size() > 2) {

				float3 pos(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));

				camera->SetPos(pos);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetCameraLookAt")) {
			if(args.size() > 2) {

				float3 pos(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));

				camera->SetLookAt(pos);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetCameraDir")) {
			if(args.size() > 2) {

				float3 dir(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));

				camera->SetDir(dir);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetCameraFOV")) {
			if(args.size() > 0) {
				camera->SetFOV(DegToRad(GetFloatValue(args[0])));
			} else {
				console->Write("Expected 1 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"MessageBox")) {
			if(args.size() > 0) {
				char* val = GetStringValue(args[0]);
				new Messagebox(val);
				delete[] val;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"RandomRange")) {
			if(args.size() > 1) {
				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = RandomRange(GetFloatValue(args[0]),GetFloatValue(args[1]));
			} else {
				console->Write("Expected 2 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"ClearScene")) {
			scene->Clear();
		} else if(!strcmp(function,"LoadScene")) {
			if(args.size() > 0) {
				scene->Load(args[0]);
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"Execute")) {
			if(args.size() > 0) {
				Parser parser(args[0]);
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetClockTimeHours")) {
			result = new Var(vtInt,"tmp",level);
			result->intvalue = renderer->GetClockTimeHours();
		} else if(!strcmp(function,"GetClockTimeMins")) {
			result = new Var(vtInt,"tmp",level);
			result->intvalue = renderer->GetClockTimeMins();
		} else if(!strcmp(function,"GetObjectByName")) {
			if(args.size() > 0) {
				result = new Var(vtObject,"tmp",level);
				result->objectvalue = scene->objects->GetByName(args[0]);
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetObjectCount")) {
			result = new Var(vtInt,"tmp",level);
			result->intvalue = scene->objects->size();
		} else {
			console->Write("Unknown global function \"%s\"\r\n",function);
		}

		// object member functions
	} else if(parent->type == vtObject) {
		if(!strcmp(function,"Translate")) {
			if(args.size() > 2) {
				float3 dir(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				parent->objectvalue->Move(dir);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetPosition")) {
			if(args.size() > 2) {
				float3 dir(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				parent->objectvalue->SetTranslation(dir);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"SetRotation")) {
			if(args.size() > 2) {
				float3 dir(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				parent->objectvalue->SetRotation(dir);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"LookAt")) {
			if(args.size() == 1) {
				float3 pos = *GetFloat3Value(args[0]);
				float4x4 rotation = LookAt(parent->objectvalue->GetTranslation(),pos);
				parent->objectvalue->SetRotation(rotation);
			} else if(args.size() > 2) {
				float3 pos(GetFloatValue(args[0]),GetFloatValue(args[1]),
				           GetFloatValue(args[2]));
				float4x4 rotation = LookAt(parent->objectvalue->GetTranslation(),pos);
				parent->objectvalue->SetRotation(rotation);
			} else {
				console->Write("Expected 3 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetPosition")) {
			result = new Var(vtFloat3,"tmp",level);
			result->float3value = new float3(parent->objectvalue->GetTranslation());
		} else {
			console->Write("Unknown Object function \"%s\"\r\n",function);
		}

		// path member functions
	} else if(parent->type == vtPath) {
		if(!strcmp(function,"AddToTail")) {
			std::vector<float3> points;

			int pointcount = args.size()/3; // convert array of scalars to float3
			for(int i = 0; i < pointcount; i++) {
				float3 point = float3(
				                   GetFloatValue(args[i]),
				                   GetFloatValue(args[i+1]),
				                   GetFloatValue(args[i+2]));
				points.push_back(point);
			}
			parent->pathvalue->AddToTail(new Curve(points));
		} else if(!strcmp(function,"AddObjectAt")) {
			if(args.size() > 3) {

				float t = GetFloatValue(args[1]);

				// Spawn on path
				float3 pos = parent->pathvalue->GetPoint(t);

				// With proper direction
				float4x4 rot = parent->pathvalue->GetAngle(t);

				// Use model file name as name
				char name[MAX_PATH];
				ExtractFileName(args[0],name);

				// Return object
				result = new Var(vtObject,"tmp",level);
				result->objectvalue = new Object(name,args[1],args[2],pos,rot,
				                                 GetFloatValue(args[3]));
			} else {
				console->Write("Expected 4 arguments for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetLength")) {
			result = new Var(vtFloat,"tmp",level);
			result->floatvalue = parent->pathvalue->GetLength();
		} else if(!strcmp(function,"GetPointX")) {
			if(args.size() > 0) {

				float t = GetFloatValue(args[0]);

				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = parent->pathvalue->GetPoint(t).x;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetPointY")) {
			if(args.size() > 0) {

				float t = GetFloatValue(args[0]);

				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = parent->pathvalue->GetPoint(t).y;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetPointZ")) {
			if(args.size() > 0) {

				float t = GetFloatValue(args[0]);

				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = parent->pathvalue->GetPoint(t).z;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetDirX")) {
			if(args.size() > 0) {

				float t = GetFloatValue(args[0]);

				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = parent->pathvalue->GetTangent(t).x;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetDirY")) {
			if(args.size() > 0) {

				float t = GetFloatValue(args[0]);

				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = parent->pathvalue->GetTangent(t).y;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else if(!strcmp(function,"GetDirZ")) {
			if(args.size() > 0) {

				float t = GetFloatValue(args[0]);

				result = new Var(vtFloat,"tmp",level);
				result->floatvalue = parent->pathvalue->GetTangent(t).z;
			} else {
				console->Write("Expected 1 argument for function %s, got %d\r\n",function,
				               args.size());
			}
		} else {
			console->Write("Unknown Path function \"%s\"\r\n",function);
		}
	}

	// Step to ending )
	index = tokens[oldindex + 1]->complement + 1;

	return result; // don't forget to delete
}
void Parser::GetExpression(std::vector<Token*>& result) {

	unsigned int start = index;
	unsigned int end = index;

	if(end + 1 < tokens.size() and (tokens[end + 1]->type == ttMulDiv
	                                or tokens[end + 1]->type == ttAddSub)) { // >0 operators
		end++; // start on top of first one
		while(end + 2 < tokens.size() and (tokens[end + 2]->type == ttMulDiv
		                                   or tokens[end + 2]->type == ttAddSub)) {
			end += 2; // check next
		}
		end++; // step to last value
	}

	// end now points to the last operator, start points to
	for(unsigned int i = start; i <= end; i++) {
		result.push_back(tokens[i]);
	}
}
void Parser::HandleAssignment(Var* lvalue) { // parent variable

	// Get lvalue and operator
	char* op = tokens[index + 1]->text;
	index += 2; // step over assignment operator

	// Handle fast operators first to make them fast :P
	if(!strcmp(op,"++")) {
		switch(lvalue->type) {
			case vtInt: {
				lvalue->intvalue++;
				return;
			}
			case vtFloat: {
				lvalue->floatvalue++;
				return;
			}
			default: {
				console->Write("Operator++ not supported for \"%s\" at line %u\r\n",
				               tokens[index]->text,tokens[index]->line);
				return;
			}
		}
	} else if(!strcmp(op,"--")) {
		switch(lvalue->type) {
			case vtInt: {
				lvalue->intvalue--;
				return;
			}
			case vtFloat: {
				lvalue->floatvalue--;
				return;
			}
			default: {
				console->Write("Operator-- not supported for \"%s\" at line %u\r\n",
				               tokens[index]->text,tokens[index]->line);
				return;
			}
		}
	}

	Var* rvalue = NULL;

	if(index + 1 < tokens.size()
	        and tokens[index+1]->type == ttOpenParenth) { // C style assignment
		rvalue = HandleFunction(NULL); // steps over ending )
	} else if(index + 3 < tokens.size() and tokens[index + 1]->type == ttMember
	          and tokens[index + 3]->type == ttOpenParenth) { // member function assignment
		Var* parent = GetVariable(tokens[index]->text);
		if(parent) {
			index += 2; // step onto function name
			rvalue = HandleFunction(parent); // steps over ending ) too
		} else  {
			console->Write("Unknown parent \"%s\"\r\n",tokens[index]->text);
			return;
		}
	} else { // Evaluate expression after assignment operator
		std::vector<Token*> expression;
		GetExpression(expression); // creates copies of pointers
		index += expression.size(); // we've processed it, step over
		rvalue = Evaluate(expression);
	}

	// Functie faalde bijvoorbeeld
	if(!rvalue) {
		console->Write("Error evaluating expression before \"%s\"\r\n",
		               tokens[index]->text);
		return;
	}

	if(!strcmp(op,"+=")) {
		switch(lvalue->type) {
			case vtInt: {
				lvalue->intvalue += rvalue->GetIntValue();
				break;
			}
			case vtFloat: {
				lvalue->floatvalue += rvalue->GetFloatValue();
				break;
			}
			default: {
				console->Write("Unsupported operator \"%s\" on variable \"%s\"\r\n",op,
				               lvalue->name);
				break;
			}
		}
	} else if(!strcmp(op,"-=")) {
		switch(lvalue->type) {
			case vtInt: {
				lvalue->intvalue -= rvalue->GetIntValue();
				break;
			}
			case vtFloat: {
				lvalue->floatvalue -= rvalue->GetFloatValue();
				break;
			}
			default: {
				console->Write("Unsupported operator \"%s\" on variable \"%s\"\r\n",op,
				               lvalue->name);
				break;
			}
		}
	} else if(!strcmp(op,"=")) {
		switch(lvalue->type) {
			case vtInt: {
				lvalue->intvalue = rvalue->GetIntValue();
				break;
			}
			case vtFloat: {
				lvalue->floatvalue = rvalue->GetFloatValue();
				break;
			}
			case vtObject: {
				lvalue->objectvalue = rvalue->objectvalue;
				break;
			}
			case vtFloat3: {
				lvalue->float3value = rvalue->float3value;
				break;
			}
			default: {
				console->Write("Unsupported operator \"%s\" on variable \"%s\"\r\n",op,
				               lvalue->name);
				break;
			}
		}
	} else {
		console->Write("Unknown operator \"%s\"\r\n",op);
	}
}
void Parser::Clear() {
	for(unsigned int i = 0; i < tokens.size(); i++) {
		delete tokens[i];
	}
	tokens.clear();

	for(unsigned int i = 0; i < vars.size(); i++) {
		delete vars[i];
	}
	vars.clear();
}
void Parser::AddToken(int length,char* ptr) {

	// Add token split by delimiters
	Token* newtoken = new Token();

	// Set name
	newtoken->text = new char[length + 1];
	strncpy(newtoken->text,ptr,length);
	newtoken->text[length] = 0;
	newtoken->line = line;

	tokens.push_back(newtoken);
}
void Parser::Tokenize(const char* file) {

	char fullpath[MAX_PATH];
	GetFullPath(file,"Data\\Scripts",fullpath);

	line = 1;

	FILE* script = fopen(fullpath,"rb");
	if(script == NULL) {
		console->Write("Error opening script %s\r\n",fullpath);
		return;
	}

	// How big is the file?
	fseek(script,0,SEEK_END);
	unsigned int size = ftell(script);
	fseek(script,0,SEEK_SET);

	// Read the whole thing
	char* buffer = new char[size+1];
	fread(buffer,sizeof(char),size,script);
	buffer[size] = 0;

	// Tokenize, but we need the splitters too, so don't use strtok
	char* ptr = buffer;
	while(*ptr != 0) { // TODO: werkt niet, fixen met switch ofzo

		unsigned int tokenlength = strcspn(ptr,"\"#(){}[],. \r\n\t\0");

		// Ignore empty tokens
		if(tokenlength > 0) {

			// Don't cut up floats
			if(*(ptr + tokenlength) == '.' and isdigit(*(ptr + tokenlength - 1))) {
				char* extrapart = ptr + tokenlength; // start at
				tokenlength += strcspn(extrapart,"\"#(){}[], \r\n\t\0");
			}

			// Add delimited text
			AddToken(tokenlength,ptr);

			// Advance to delimiter
			ptr += tokenlength;
		}

		if(*ptr == '#') { // comment,ignore rest of line
			ptr += strcspn(ptr,"\r\n\0");
		} else if(*ptr == '\"') { // add whole string as one token

			ptr++; // step over
			tokenlength = strcspn(ptr,"\"");

			// Add delimited text, without quotes (they did their job)
			AddToken(tokenlength,ptr);

			// Advance to delimiter
			ptr += tokenlength;
		} else if(*ptr == '[') { // don't cut up float3s
			tokenlength = strcspn(ptr,"]")+1;
			AddToken(tokenlength,ptr);
			ptr += tokenlength;
		} else if(*ptr == '\n') {
			line++;
		} else if(!isspace(*ptr) && *ptr != ',') { // () {}
			AddToken(1,ptr);
		}

		// Always advance beyond delim
		ptr++;
	}

	delete[] buffer;

	// Separate ++ and -- chars from their variable
	index = 0;
	while(index + 1 < tokens.size()) {

		char* token = tokens[index]->text;
		int length = strlen(token);

		// Does this one end with ++ or --?
		if(length > 2 and (!strcmp(&token[length-2],"++")
		                   or !strcmp(&token[length-2],"--"))) {

			// Copy ++ or -- to new token, insert after this one
			Token* newtoken = new Token();
			newtoken->text = new char[length + 1];
			strcpy(newtoken->text,&token[length-2]);
			newtoken->text[length] = 0;
			tokens.insert(tokens.begin() + index + 1,newtoken);

			// Remove from current
			tokens[index]->text[length-2] = 0; // waste two bytes
		}

		index++;
	}

	// Set token types to speed up parsing
	index = 0;
	while(index < tokens.size()) {

		char* text = tokens[index]->text;

		// Set type if applicable
		if(!strcmp(text,"while")) {
			tokens[index]->type = ttWhile;
		} else if(!strcmp(text,"if")) {
			tokens[index]->type = ttIf;
		} else if(!strcmp(text,"else")) {
			tokens[index]->type = ttElse;
		} else if(!strcmp(text,"int") or !strcmp(text,"float")
		          or !strcmp(text,"string") or !strcmp(text,"Object") or !strcmp(text,"Path")
		          or !strcmp(text,"float3")) {
			tokens[index]->type = ttVarType;
		} else if(!strcmp(text,"*") or !strcmp(text,"/")) {
			tokens[index]->type = ttMulDiv;
		} else if(!strcmp(text,"+") or !strcmp(text,"-")) {
			tokens[index]->type = ttAddSub;
		} else if(!strcmp(text,"+=") or !strcmp(text,"=") or !strcmp(text,"-=")
		          or !strcmp(text,"++") or !strcmp(text,"--")) {
			tokens[index]->type = ttAssign;
		} else if(!strcmp(text,"{")) {
			tokens[index]->type = ttOpenBracket;
		} else if(!strcmp(text,"}")) {
			tokens[index]->type = ttCloseBracket;
		} else if(!strcmp(text,"(")) {
			tokens[index]->type = ttOpenParenth;
		} else if(!strcmp(text,")")) {
			tokens[index]->type = ttCloseParenth;
		} else if(!strcmp(text,"[")) {
			tokens[index]->type = ttOpenSquare;
		} else if(!strcmp(text,"]")) {
			tokens[index]->type = ttCloseSquare;
		} else if(!strcmp(text,".")) {
			tokens[index]->type = ttMember;
		} else {
			tokens[index]->type = ttUnknown;
		}

		index++;
	}

	fclose(script);

	if(savetokens) {

		// Save to exe path
		char fullpath[MAX_PATH];
		GetFullPath("Tokens.txt","",fullpath);
		FILE* tokenoutput = fopen(fullpath,"w");

		for(unsigned int i = 0; i < tokens.size(); i++) {
			Token* thistoken = tokens[i];
			fprintf(tokenoutput,"Index = %u:\tLine = %u\tText = %s\n",i,thistoken->line,
			        thistoken->text);
		}
		fclose(tokenoutput);
	}
}
void Parser::Optimize() {

	index = 0;
	level = 0;

	while(index < tokens.size()) {
		if(tokens[index]->type == ttOpenBracket) {

			int levelchange = 0;
			int oldindex = index;

			// Create pair
			while(index < tokens.size()) {
				if(tokens[index]->type == ttOpenBracket) {
					levelchange++;
				} else if(tokens[index]->type == ttCloseBracket) {
					levelchange--;

					if(levelchange == 0) {
						tokens[oldindex]->complement = index;
						tokens[index]->complement = oldindex;
						break;
					}
				}
				index++;
			}

			// Return to start
			index = oldindex + 1;

		} else if(tokens[index]->type == ttOpenParenth) {

			int levelchange = 0;
			int oldindex = index;

			// Create pair
			while(index < tokens.size()) {
				if(tokens[index]->type == ttOpenParenth) {
					levelchange++;
				} else if(tokens[index]->type == ttCloseParenth) {
					levelchange--;

					if(levelchange == 0) {
						tokens[oldindex]->complement = index;
						tokens[index]->complement = oldindex;
						break;
					}
				}
				index++;
			}

			// Return to start
			index = oldindex + 1;
		} else {
			index++;
		}
	}
}
void Parser::Parse() {

	// Store this to be able to walk through if/else stairs
	bool codeflowresult = false;

	index = 0;
	level = 0;

	while(index < tokens.size()) {
		if(index + 1 < tokens.size() and tokens[index + 1]->type == ttOpenParenth) {
			if(tokens[index]->type == ttWhile or tokens[index]->type == ttIf) { // code flow

				// Step onto {
				codeflowresult = HandleBoolean();

				if(codeflowresult) {
					index++; // step over {
					IncScope();
				} else {
					index = tokens[index]->complement + 1; // step over complementary }
				}
			} else { // normal function, no assignments
				Var* result = HandleFunction(NULL);
				if(result) {// delete return values
					delete result;
				}
			}
		} else if(tokens[index]->type == ttElse) { // code flow
			if(tokens[index+1]->type == ttOpenBracket) { // else
				if(codeflowresult) { // did the previous if return true?
					index = tokens[index + 1]->complement + 1; // step over else part
				} else {
					IncScope(); // enter else scope
					index += 2; // step over {
				}
			} else if(tokens[index+1]->type == ttIf) { // else if
				if(codeflowresult) { // an if has already been handled, step over

					// Find opening { after if
					while(index < tokens.size()) {
						if(tokens[index]->type == ttOpenBracket) {
							break;
						}
						index++;
					}

					// step over if block
					index = tokens[index]->complement + 1;
				} else { // try this if
					index++; // else with an if, stop on it
				}
			}
		} else if(tokens[index]->type == ttOpenSquare) { // array assignment, ignore
			index = tokens[index]->complement + 1; // step over ]
		} else if(tokens[index]->type ==
		          ttCloseBracket) { // end of code block, check if it's a loop

			int oldindex = index;

			// Exit from scope, return to {
			DecScope();

			// This has to exist, otherwhise optimizing fails
			index = tokens[index]->complement;

			// Then find while/if keyword
			while(index < tokens.size()) {
				if(tokens[index]->type == ttCloseParenth) {
					index = tokens[index]->complement; // hop over ()
				} else if(tokens[index]->type == ttWhile) {
					break; // yay
				} else if(tokens[index]->type == ttIf or tokens[index]->type == ttElse) {
					index = oldindex + 1; // twas an if, go back to ending brace
					break;
				}
				index--;
			}
		} else if(tokens[index]->type ==
		          ttVarType) { // declaration, don't handle assignments here
			HandleVar();
		} else { // assume var operation value
			Var* lvalue = GetVariable(tokens[index]->text);

			if(lvalue) {

				// Assignment
				if(index + 1 < tokens.size() && tokens[index + 1]->type == ttAssign) {
					HandleAssignment(lvalue);

					// Execute member function, no return values
				} else if(index + 3 < tokens.size() && tokens[index + 1]->type == ttMember
				          && tokens[index + 3]->type == ttOpenParenth) {
					index += 2;
					Var* result = HandleFunction(lvalue); // needs to start on top of function name
					if(result) { // delete return values
						delete result;
					}
				} else {
					console->Write("Unknown operation on variable \"%s\" at line %u\r\n",
					               tokens[index]->text,tokens[index]->line);
					index++;
				}
			} else { // ???
				console->Write("Unexpected token \"%s\" at line %u\r\n",tokens[index]->text,
				               tokens[index]->line);
				index++;
			}
		}
	}

	// vars are removed on next run
}
void Parser::AddArg(Var* value) {
	vars.push_back(value);
}
void Parser::Execute(const char* file) {

	// Obtain full path if needed
	char fullpath[MAX_PATH];
	GetFullPath(file,"Data\\Scripts",fullpath);

	if(printtiming) {
		console->WriteBeginTimer("Tokenizing... ");
	}

	Tokenize(fullpath);

	if(printtiming) {
		console->WriteEndTimer("");
	}

	if(printtiming) {
		console->WriteBeginTimer("Optimizing... ");
	}

	Optimize();

	if(printtiming) {
		console->WriteEndTimer("");
	}

	if(printtiming) {
		console->WriteBeginTimer("Parsing... ");
	}

	Parse();

	if(printtiming) {
		console->WriteEndTimer("");
	}
}
