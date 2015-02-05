#include "LangCompare.h"

LangCompare::LangCompare(LangFile& referencein,
                         LangFile& comparein) : reference(referencein),compare(comparein)  {
	Compare();
}

LangCompare::~LangCompare() {

}

void LangCompare::Compare() {
	// For all IDs in reference, check if they exist in compare
	for(unsigned int i = 0; i < reference.contents.size(); i++) {
		LangItem* item = &reference.contents;
		if(compare.FindID(item->ID) == NULL) {
			Log("Cannot find reference ID %5d in other language\r\n",item->ID);
		}
	}

	// For all IDs in compare, check if they exist in reference
	for(unsigned int i = 0; i < compare.contents.size(); i++) {
		LangItem* item = &compare.contents;
		if(reference.FindID(item->ID) == NULL) {
			Log("Cannot find comparison ID %5d in reference language\r\n",item->ID);
		}
	}

	// For all found items, compare formatting options
	for(unsigned int i = 0; i < reference.contents.size(); i++) {
		LangItem* item = &reference.contents;
		LangItem* sameitem = compare.FindID(item->ID);
		if(sameitem != NULL and sameitem->formatspec != item->formatspec) {
			Log("Format specifier mismatch at ID %5d: %s vs %s\r\n",
			    item->ID,item->formatspec.c_str(),sameitem->formatspec.c_str());
		}
	}
}
