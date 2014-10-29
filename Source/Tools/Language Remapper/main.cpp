/*
	This program can open language table files and remap their ID's. The formula used can be found in the function
	called OpenFixSortSave.
	
	Basically, it starts counting, and if a gap of more than one is found between items, 1000
	ID's are skipped and leftovers are trimmed. So a range like 1..10 15..20 ends up like: 1..10 1001..1005 and so 
	forth.
	
	The code also sorts ID's. Missing entries in translations are filled by items in the reference language.
	This language is assumed to be English.lng (see int main()).
	
	I almost forgot: this program can also be used to update the LangIDs.inc file!
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "translation.h"
#include "mutation.h"

// The reference language is stored globally
Translation *ReferenceTranslationList = (Translation*)calloc(5000,sizeof(Translation));
Mutation *ReferenceIDs = (Mutation*)calloc(5000,sizeof(Mutation));
int refnumentries = 0;
char refversion[100] = "";
char reflanguage[100] = "";
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Opens any translations, sorts it in its own list and adds the English translation in the gaps and saves
*/
void OpenFixSortSave(const char *openfilename,const char *savefilename) {
	
	// Storage
	char currentline[500] = "";
	char word1[500] = "";
	char word2[500] = "";
	
	int currentid = 0;
	int idtowrite = 0;
	
	char version[100] = "";
	char language[100] = "";

	// Read data
	Translation* OtherTranslationList = (Translation*)calloc(5000,sizeof(Translation));
	
	FILE* openlanguagetable = fopen(openfilename,"r");
	if(openlanguagetable == NULL) {
		printf("Error opening file %s\n",openfilename);
		return;
	}
	
	while(fgets(currentline,sizeof(currentline),openlanguagetable)) {
		if(sscanf(currentline,"%[^=]=%[^\n]",word1,word2) == 2) {
			if(!strcmp(word1,"Ver")) {
				strcpy(version,word2);
			} else if(!strcmp(word1,"Lang")) {
				strcpy(language,word2);
			} else if(sscanf(word1,"%d",&currentid) == 1) {
				
				// Don't just add it, add it to the spot where English stores it...
				for(int j = 0;j < refnumentries;j++) {
					if(ReferenceTranslationList[j].id == currentid) {
						OtherTranslationList[j].id = currentid;
						strcpy(OtherTranslationList[j].text,word2);
						break;
					}
				}
			}
		}
	}
	fclose(openlanguagetable);
	
	// Fix the gaps
	for(int i = 0;i < refnumentries;i++) {
		if(ReferenceTranslationList[i].id != OtherTranslationList[i].id) {
			
			// Missing entry, assume English is complete
			printf("Missing ID in %s: %d\n",openfilename,ReferenceTranslationList[i].id);
			OtherTranslationList[i].id = ReferenceTranslationList[i].id;
			strcpy(OtherTranslationList[i].text,ReferenceTranslationList[i].text);
		}
	}
	
	// When finished filling, sort
	QuickSortTranslation(OtherTranslationList,0,refnumentries-1);
	
	// Save
	FILE* newlanguagetable = fopen(savefilename,"w");
	if(newlanguagetable == NULL) {
		printf("Error opening file %s\n",savefilename);
		return;
	}
	
	fprintf(newlanguagetable,"[lang]\n");
	fprintf(newlanguagetable,"Lang=%s\n",language);
	fprintf(newlanguagetable,"Ver=%s\n\n",version);		
	for(int j = 0;j < refnumentries;j++) {
		if(j > 0 && OtherTranslationList[j].id != OtherTranslationList[j-1].id+1) {
			idtowrite+=1000;
			idtowrite-=(idtowrite%1000);
			fprintf(newlanguagetable,"\n\n");
		}
		idtowrite++;
	
		// Fix for incomplete translations...
		fprintf(newlanguagetable,"%d=%s\n",idtowrite,OtherTranslationList[j].text);
	}
	fclose(newlanguagetable);
	
	// Free the table too
	free(OtherTranslationList);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Opens a reference language (English most of the time) and saves in ReferenceTranslationList
*/
void CacheReferenceLanguage(const char *path) {
	
	// Storage
	char currentline[500] = "";
	char word1[500] = "";
	char word2[500] = "";
	
	int currentid = 0;
	refnumentries = 0;

	// If we we're able to, open the old file
	FILE* languagetable = fopen(path,"r");
	if(languagetable == NULL) {
		printf("Error opening file %s\n",path);
		return;
	}
	
	while(fgets(currentline,sizeof(currentline),languagetable)) {
		if(sscanf(currentline,"%[^=]=%[^\n]",word1,word2) == 2) {
			if(!strcmp(word1,"Ver")) {
				strcpy(refversion,word2);
			} else if(!strcmp(word1,"Lang")) {
				strcpy(reflanguage,word2);
			} else if(sscanf(word1,"%d",&currentid) == 1) {
				ReferenceTranslationList[refnumentries].id = currentid;
				ReferenceIDs[refnumentries].oldid = currentid;
				strcpy(ReferenceTranslationList[refnumentries].text,word2);
				refnumentries++;
			}
		}
	}
	fclose(languagetable);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Saves the content of ReferenceTranslationList
*/
void SaveFixedReferenceLanguage(const char *path) {
	
	// Storage
	int idtowrite = 0;
	
	QuickSortTranslation(ReferenceTranslationList,0,refnumentries-1);
	QuickSortMutation(ReferenceIDs,0,refnumentries-1);
	
	FILE* newlanguagetable = fopen(path,"w");
	if(newlanguagetable == NULL) {
		printf("Error opening file %s\n",path);
		return;
	}
		
	fprintf(newlanguagetable,"[lang]\n");
	fprintf(newlanguagetable,"Lang=%s\n",reflanguage);
	fprintf(newlanguagetable,"Ver=%s\n",refversion);
	for(int j = 0;j < refnumentries;j++) {
		if(j > 0 && ReferenceTranslationList[j].id != ReferenceTranslationList[j-1].id+1) {
			idtowrite+=1000;
			idtowrite-=(idtowrite%1000);
			fprintf(newlanguagetable,"\n\n");
		}
		idtowrite++;
		ReferenceIDs[j].newid = idtowrite;
	
		// Fix for incomplete translations...
		fprintf(newlanguagetable,"%d=%s\n",idtowrite,ReferenceTranslationList[j].text);
	}
	fclose(newlanguagetable);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Prints the ID comparison table for debugging purposes...
*/
void PrintMutation() {
	for(int i = 0;i < refnumentries;i++) {
		printf("%d -> %d\n",ReferenceIDs[i].oldid,ReferenceIDs[i].newid);
	}
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Opens the ID table and replaces all ID's by the ones set in ReferenceIDs
*/
void OpenFixSaveIDFile(const char* oldpath,const char* newpath) {
	
	// Storage
	char currentline[1000] = "";
	char word1[500] = "";
	char word2[200] = "";
	
	int currentid = 0;

	FILE *oldlanguageids = fopen(oldpath,"r");
	if(oldlanguageids == NULL) {
		printf("Error opening file %s\n",oldpath);
		return;
	}
	
	FILE* newlanguageids = fopen(newpath,"w");
	if(newlanguageids == NULL) {
		printf("Error opening file %s\n",newpath);
		return;
	}
	
	while(fgets(currentline,sizeof(currentline),oldlanguageids)) {
		if(sscanf(currentline,"%[^=]= %[^;]",word1,word2) == 2) {
			
			// Convert the line to the correct format and find the corresponding new ID
			if(sscanf(word2,"%d",&currentid) == 1) {
				for(int i = 0;i < refnumentries;i++) {
					if(ReferenceIDs[i].oldid == currentid) {
						currentid = ReferenceIDs[i].newid;
						break;
					}
				}
				
				// Append the new stuff to the new file
				fprintf(newlanguageids,"%s = %d;\n",word1,currentid);
			}
		} else {
			fprintf(newlanguageids,"%s",currentline);
		}
	}
	fclose(newlanguageids);
	fclose(oldlanguageids);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Pretty much all the code the 'end user' needs to use can be found here... 
*/
int main() {
	
	// Cache the reference language
	CacheReferenceLanguage("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\English.lng");
	
	// Open up other languages, fill missing entries with the entries from the Reference Language
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Bulgarian.lng","D:\\Bulgarian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Catalan.lng","D:\\Catalan.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Chinese.lng","D:\\Chinese.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Chinese_TC.lng","D:\\Chinese_TC.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Croatian.lng","D:\\Croatian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Czech.lng","D:\\Czech.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Danish.lng","D:\\Danish.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Dutch.lng","D:\\Dutch.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Estonian.lng","D:\\Estonian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\French.lng","D:\\French.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Galego.lng","D:\\Galego.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\German.lng","D:\\German.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Greek.lng","D:\\Greek.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Hungarian.lng","D:\\Hungarian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Italian.lng","D:\\Italian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Korean.lng","D:\\Korean.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Latvian.lng","D:\\Latvian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Norwegian.lng","D:\\Norwegian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Polish.lng","D:\\Polish.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Portuguese.lng","D:\\Portuguese.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Romanian.lng","D:\\Romanian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Russian.lng","D:\\Russian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Slovak.lng","D:\\Slovak.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Slovenian.lng","D:\\Slovenian.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Spanish.lng","D:\\Spanish.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\SpanishCastellano.lng","D:\\SpanishCastellano.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Swedish.lng","D:\\Swedish.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Turkish.lng","D:\\Turkish.lng");
	OpenFixSortSave("C:\\Program Files (x86)\\Dev-Cpp\\Lang\\Ukrainian.lng","D:\\Ukrainian.lng");
	
	// And save the reference language too in the new format
//	SaveFixedReferenceLanguage("D:\\English.lng");
	
	// Debugging, print the ID changes...
//	PrintMutation();
	
	// Then update the ID file
//	OpenFixSaveIDFile("C:\\Program Files (x86)\\Dev-Cpp\\source\\LangIDs.inc","D:\\LangIDs.inc");
	return 0;
}
