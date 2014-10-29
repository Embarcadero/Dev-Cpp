struct Mutation {
	int oldid;
	int newid;
};
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
