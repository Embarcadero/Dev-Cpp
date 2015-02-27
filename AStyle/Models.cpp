#include <cstdio>
#include "Dialogs.h"
#include "Models.h"

Models::Models() {
}
Models::~Models() {
	Clear();
}
Model* Models::Add() {
	return Add(new Model());
}
Model* Models::Add(const char* filename,bool sendtogpu) {

	// Obtain full path
	char fullpath[MAX_PATH];
	GetFullPath(filename,"Data\\Models",fullpath);

	// Did we add that one already?
	Model* search = GetByFullPath(fullpath);
	if(search) {
		return search;
	}

	// Nope. Add to list
	return Add(new Model(filename,sendtogpu));
}
Model* Models::AddPlane(unsigned int tiling,unsigned int textiling,
                        float edgelen,Heightmap* height,bool sendtogpu) {
	Model* model = new Model();
	model->LoadPlane(tiling,textiling,edgelen,height);
	if(sendtogpu) {
		model->SendToGPU();
	}
	return Add(model); // add to dump
}
Model* Models::Add(Model* model) {
	list.push_back(model);
	model->bufferlocation = --list.end();
	return model;
}
Model* Models::GetByFileName(const char* filename) {
	for(std::list<Model*>::iterator i = list.begin(); i != list.end(); i++) {
		if((*i)->filename && !strcmp(filename,(*i)->filename)) {
			return *i;
		}
	}
	return NULL;
}
Model* Models::GetByFullPath(const char* fullpath) {
	for(std::list<Model*>::iterator i = list.begin(); i != list.end(); i++) {
		if((*i)->fullpath && !strcmp(fullpath,(*i)->fullpath)) {
			return *i;
		}
	}
	return NULL;
}
void Models::Delete(Model* model) {
	if(model) {
		list.erase(model->bufferlocation);
		delete model;
	}
}
void Models::Clear() {
	for(std::list<Model*>::iterator i = list.begin(); i != list.end(); i++) {
		delete *i;
	}
	list.clear();
}
void Models::Print() {
	console->WriteVar("list.size()",(int)list.size());
	for(std::list<Model*>::iterator i = list.begin(); i != list.end(); i++) {
		(*i)->Print();
	}
}
void Models::SaveToCSV() {

	// Save next to exe
	char finalpath[MAX_PATH];
	snprintf(finalpath,finalpath,"%s\\%s",exepath,"Models.csv");

	// Save grand total too
	unsigned int totalvertices = 0;
	unsigned int totalfaces = 0;
	unsigned int totalbytes = 0;

	FILE* file = fopen(finalpath,"wb");
	if(file) {
		fprintf(file,"Full path;Vertices;Faces;Size (bytes)\r\n");
		for(std::list<Model*>::iterator i = list.begin(); i != list.end(); i++) {
			Model* model = *i;
			fprintf(file,"%s;%u;%u;%llu\r\n",
			        model->fullpath,
			        model->numvertices,
			        model->numfaces,
			        model->GetSize());

			totalvertices += model->numvertices;
			totalfaces += model->numfaces;
			totalbytes += model->GetSize();
		}

		// TODO: create function
		if(totalbytes > 1024*1024*1024) { // 1 GiB
			fprintf(file,";%u;%u;%u (%.2f GiB)\r\n",totalvertices,totalfaces,totalbytes,
			        totalbytes/1024.0f/1024.0f/1024.0f);
		} else if(totalbytes > 1024*1024) { // 1 MiB
			fprintf(file,";%u;%u;%u (%.2f MiB)\r\n",totalvertices,totalfaces,totalbytes,
			        totalbytes/1024.0f/1024.0f);
		} else if(totalbytes > 1024) { // 1 KiB
			fprintf(file,";%u;%u;%u (%.2f KiB)\r\n",totalvertices,totalfaces,totalbytes,
			        totalbytes/1024.0f);
		} else {
			fprintf(file,";%u;%u;%u bytes\r\n",totalvertices,totalfaces,totalbytes);
		}

		fclose(file);
	}
}
