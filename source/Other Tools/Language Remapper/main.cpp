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

// The reference language is stored globally
struct Translation {
	int id;
	char text[500];
};
Translation *ReferenceTranslationList = (Translation*)malloc(5000*sizeof(Translation));
struct Mutation {
	int oldid;
	int newid;
};
Mutation *ReferenceIDs = (Mutation*)malloc(5000*sizeof(Mutation));
int numentries = 0;
char ver[20] = "";
char lang[100] = "";
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Sorts 'list' using the Quicksort algorithm:	http://en.wikipedia.org/wiki/Quicksort
*/
void QuickSortTranslation(Translation *list,int left,int right) {
	int i = left;
	int j = right;
	Translation tmp;
	int pivot = list[(left + right)/2].id;

	while(i <= j) {
		while(list[i].id < pivot) {
			i++;
		}
		while(list[j].id > pivot) {
			j--;
		}

		if(i <= j) {
			tmp.id = list[i].id;
			strcpy(tmp.text,list[i].text);
			
			list[i].id = list[j].id;
			strcpy(list[i].text,list[j].text);

			list[j].id = tmp.id;
			strcpy(list[j].text,tmp.text);

			i++;
			j--;
		}
	}

	if(left < j)
		QuickSortTranslation(list, left, j);
	if(i < right)
		QuickSortTranslation(list, i, right);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Sorts 'list' using the Quicksort algorithm:	http://en.wikipedia.org/wiki/Quicksort
*/
void QuickSortMutation(Mutation *list,int left,int right) {
	int i = left;
	int j = right;
	Mutation tmp;
	int pivot = list[(left + right)/2].oldid;

	while(i <= j) {
		while(list[i].oldid < pivot) {
			i++;
		}
		while(list[j].oldid > pivot) {
			j--;
		}

		if(i <= j) {
			tmp.oldid = list[i].oldid;
			tmp.newid = list[i].newid;
			
			list[i].oldid = list[j].oldid;
			list[i].newid = list[j].newid;

			list[j].oldid = tmp.oldid;
			list[j].newid = tmp.newid;

			i++;
			j--;
		}
	}

	if(left < j)
		QuickSortMutation(list, left, j);
	if(i < right)
		QuickSortMutation(list, i, right);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Opens any translations, sorts it in its own list and adds the English translation in the gaps and saves
*/
void OpenFixSortSave(const char *openfilename,const char *savefilename) {
	// Storage
	char currentline[500] = "";
	char currenttranslation[500] = "";
	int currentid = 0;
	int idtowrite = 0;
	
	char ver[20] = "";
	char lang[100] = "";

	// Read data
	Translation *OtherTranslationList = (Translation*)malloc(5000*sizeof(Translation));
	memset(OtherTranslationList,0,5000*sizeof(Translation));
	
	FILE *openlanguagetable = fopen(openfilename,"r");
	if(openlanguagetable == NULL) {
		printf("Error opening file %s\n",openfilename);
		system("pause");
	} else {
		while(fgets(currentline,sizeof(currentline),openlanguagetable)) {
			if(!strncmp(currentline,"Ver=",4)) {
				sscanf(currentline,"Ver=%[^\n]\n\n",ver);
			} else if(!strncmp(currentline,"Lang=",5)) {
				sscanf(currentline,"Lang=%[^\n]",lang);
			} else {
				sscanf(currentline,"%d=%[^\n]",&currentid,currenttranslation);
				if(currentline[0] != '\n' && currentline[0] != '\r' && currentid != 0 && currentline[0] != '#') {
					
					// Don't just add it, add it to the spot where English stores it...
					for(int j = 0;j < numentries;j++) {
						if(ReferenceTranslationList[j].id == currentid) {
							OtherTranslationList[j].id = currentid;
							strcpy(OtherTranslationList[j].text,currenttranslation);
							break;
						}
					}
				}
			}
		}
	}
	fclose(openlanguagetable);
	
	// Fix the gaps
	for(int i = 0;i < numentries;i++) {
		if(ReferenceTranslationList[i].id != OtherTranslationList[i].id) {
			// Missing entry, assume English is complete
			printf("Missing ID in %s: %d\n",openfilename,ReferenceTranslationList[i].id);
			OtherTranslationList[i].id = ReferenceTranslationList[i].id;
			strcpy(OtherTranslationList[i].text,ReferenceTranslationList[i].text);
		}
	}
	
	// When finished filling, sort
	QuickSortTranslation(OtherTranslationList,0,numentries-1);
	
	// Save
	FILE *newlanguagetable = fopen(savefilename,"w");
	if(newlanguagetable == NULL) {
		printf("Error opening file %s\n",savefilename);
		system("pause");
	} else {
		fprintf(newlanguagetable,"[lang]\n");
		fprintf(newlanguagetable,"Lang=%s\n",lang);
		fprintf(newlanguagetable,"Ver=%s\n\n",ver);		
		for(int j = 0;j < numentries;j++) {
			if(j != 0) {
				if(OtherTranslationList[j].id != OtherTranslationList[j-1].id+1) {
					idtowrite+=1000;
					idtowrite-=(idtowrite%1000);
					fprintf(newlanguagetable,"\n\n");
				}
			}
			idtowrite++;
		
			// Fix for incomplete translations...
			fprintf(newlanguagetable,"%d=%s\n",idtowrite,OtherTranslationList[j].text);
		}
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
	// Clear
	memset(ReferenceTranslationList,0,5000*sizeof(Translation));
	memset(ReferenceIDs,0,5000*sizeof(Mutation));

	// Storage
	char currentline[500] = "";
	char currenttranslation[500] = "";
	int currentid = 0;
	int i = 0;

	// If we we're able to, open the old file
	FILE *languagetable = fopen(path,"r");
	if(languagetable == NULL) {
		printf("Error opening file %s\n",path);
		system("pause");
	} else {
		while(fgets(currentline,sizeof(currentline),languagetable)) {
			if(!strncmp(currentline,"Ver=",4)) {
				sscanf(currentline,"Ver=%[^\n]\n\n",ver);
			} else if(!strncmp(currentline,"Lang=",5)) {
				sscanf(currentline,"Lang=%[^\n]",lang);
			} else {
				sscanf(currentline,"%d=%[^\n]",&currentid,currenttranslation);
				if(currentline[0] != '\n' && currentline[0] != '\r' && currentid != 0 && currentline[0] != '#') {
					ReferenceTranslationList[i].id = currentid;
					ReferenceIDs[i].oldid = currentid;
					strcpy(ReferenceTranslationList[i].text,currenttranslation);
					i++;
				}
			}
			numentries = i;
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
	
	QuickSortTranslation(ReferenceTranslationList,0,numentries-1);
	QuickSortMutation(ReferenceIDs,0,numentries-1);
	
	FILE *newlanguagetable = fopen(path,"w");
	if(newlanguagetable == NULL) {
		printf("Error opening file %s\n",path);
		system("pause");
	} else {
		fprintf(newlanguagetable,"[lang]\n");
		fprintf(newlanguagetable,"Lang=%s\n",lang);
		fprintf(newlanguagetable,"Ver=%s\n",ver);
		for(int j = 0;j < numentries;j++) {
			if(j != 0) {
				if(ReferenceTranslationList[j].id != ReferenceTranslationList[j-1].id+1) {
					idtowrite+=1000;
					idtowrite-=(idtowrite%1000);
					fprintf(newlanguagetable,"\n\n");
				}
			}
			idtowrite++;
			ReferenceIDs[j].newid = idtowrite;
		
			// Fix for incomplete translations...
			fprintf(newlanguagetable,"%d=%s\n",idtowrite,ReferenceTranslationList[j].text);
		}
	}
	fclose(newlanguagetable);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Prints the ID comparison table for debugging purposes...
*/
void PrintMutation() {
	for(int i = 0;i < numentries;i++) {
		printf("%d -> %d\n",ReferenceIDs[i].oldid,ReferenceIDs[i].newid);
	}
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
	Opens the ID table and replaces all ID's by the ones set in ReferenceIDs
*/
void OpenFixSaveIDFile(const char *path,const char *pathnew) {
	// Storage
	char currentidtext[500] = "";
	char currentidnr[200] = "";
	char oldidnr[200] = "";
	char currentline[700] = "";
	int currentidint = 0;

	FILE *oldlanguageids = fopen(path,"r");
	if(oldlanguageids == NULL) {
		printf("Error opening file %s\n",path);
		system("pause");
	} else {
		FILE *newlanguageids = fopen(pathnew,"w");
		if(newlanguageids == NULL) {
			printf("Error opening file %s\n",pathnew);
			system("pause");
		} else {
			while(fgets(currentline,sizeof(currentline),oldlanguageids)) {
				sscanf(currentline,"%[^=]= %[^;]",currentidtext,currentidnr);
					
				// Convert the line to the correct format and find the corresponding new ID
				sscanf(currentidnr,"%d",&currentidint);
				for(int i = 0;i < numentries;i++) {
					if(ReferenceIDs[i].oldid == currentidint) {
						currentidint = ReferenceIDs[i].newid;
						break;
					}
				}
				
				// Append the new stuff to the new file
				if(!strcmp(oldidnr,currentidnr)) {
					fprintf(newlanguageids,"%s",currentline);
				} else {
					fprintf(newlanguageids,"%s = %d;\n",currentidtext,currentidint);
				}
				
				// Store the old number (used to detect comments)
				strcpy(oldidnr,currentidnr);
			}
		}
		fclose(newlanguageids);
	}
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
	SaveFixedReferenceLanguage("D:\\English.lng");
	
	// Debugging, print the ID changes...
//	PrintMutation();
	
	// Then update the ID file
	OpenFixSaveIDFile("C:\\Program Files (x86)\\Dev-Cpp\\source\\LangIDs.inc","D:\\LangIDs.inc");
	system("pause");
	return 0;
}
