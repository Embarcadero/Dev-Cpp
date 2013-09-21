struct Translation {
	int id;
	char text[500];
};
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
