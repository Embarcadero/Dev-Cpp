#include "PPMdSubAllocatorVariantI.h"

#include <stdlib.h>
#include <string.h>
#include "stdbool.h"

#define N1 4
#define N2 4
#define N3 4
#define N4 ((128+3-1*N1-2*N2-3*N3)/4)
#define UNIT_SIZE 12
#define N_INDEXES (N1+N2+N3+N4)

static PPMdMemoryBlockVariantI *NextBlock(PPMdMemoryBlockVariantI *self,PPMdSubAllocatorVariantI *alloc);
static void SetNextBlock(PPMdMemoryBlockVariantI *self,PPMdMemoryBlockVariantI *newnext,PPMdSubAllocatorVariantI *alloc);
static bool AreBlocksAvailable(PPMdMemoryBlockVariantI *self);
static void LinkBlockAfter(PPMdMemoryBlockVariantI *self,PPMdMemoryBlockVariantI *p,PPMdSubAllocatorVariantI *alloc);
static void UnlinkBlockAfter(PPMdMemoryBlockVariantI *self,PPMdSubAllocatorVariantI *alloc);
static void *RemoveBlockAfter(PPMdMemoryBlockVariantI *self,PPMdSubAllocatorVariantI *alloc);
static void InsertBlockAfter(PPMdMemoryBlockVariantI *self,void *pv,int NU,PPMdSubAllocatorVariantI *alloc);

static unsigned int I2B(PPMdSubAllocatorVariantI *self,int index);
static void SplitBlock(PPMdSubAllocatorVariantI *self,void *pv,int oldindex,int newindex);
static uint32_t GetUsedMemory(PPMdSubAllocatorVariantI *self);

static void InitVariantI(PPMdSubAllocatorVariantI *self);
static uint32_t AllocContextVariantI(PPMdSubAllocatorVariantI *self);
static uint32_t AllocUnitsVariantI(PPMdSubAllocatorVariantI *self,int num);
static uint32_t _AllocUnits(PPMdSubAllocatorVariantI *self,int index);
static uint32_t ExpandUnitsVariantI(PPMdSubAllocatorVariantI *self,uint32_t oldoffs,int oldnum);
static uint32_t ShrinkUnitsVariantI(PPMdSubAllocatorVariantI *self,uint32_t oldoffs,int oldnum,int newnum);
static void FreeUnitsVariantI(PPMdSubAllocatorVariantI *self,uint32_t offs,int num);

static void GlueFreeBlocks(PPMdSubAllocatorVariantI *self);

static void *_OffsetToPointer(PPMdSubAllocatorVariantI *self,uint32_t offset) { return ((uint8_t *)self)+offset; }
static uint32_t _PointerToOffset(PPMdSubAllocatorVariantI *self,void *pointer) { return ((uintptr_t)pointer)-(uintptr_t)self; }




PPMdSubAllocatorVariantI *CreateSubAllocatorVariantI(int size)
{
	PPMdSubAllocatorVariantI *self=malloc(sizeof(PPMdSubAllocatorVariantI)+size);
	if(!self) return NULL;

	self->core.Init=(void *)InitVariantI;
	self->core.AllocContext=(void *)AllocContextVariantI;
	self->core.AllocUnits=(void *)AllocUnitsVariantI;
	self->core.ExpandUnits=(void *)ExpandUnitsVariantI;
	self->core.ShrinkUnits=(void *)ShrinkUnitsVariantI;
	self->core.FreeUnits=(void *)FreeUnitsVariantI;

    self->SubAllocatorSize=size;

	return self;
}

void FreeSubAllocatorVariantI(PPMdSubAllocatorVariantI *self)
{
	free(self);
}




static void InitVariantI(PPMdSubAllocatorVariantI *self)
{
	int i, k;
	unsigned int diff;

	memset(self->BList,0,sizeof(self->BList));

	self->pText=self->HeapStart;
	self->HighUnit=self->HeapStart+self->SubAllocatorSize;
	diff=UNIT_SIZE*(self->SubAllocatorSize/8/UNIT_SIZE*7);
	self->LowUnit=self->UnitsStart=self->HighUnit-diff;
	self->GlueCount=0;

	for(i=0;i<N1;i++) self->Index2Units[i]=1+i;
    for(i=0;i<N2;i++) self->Index2Units[N1+i]=2+N1+i*2;
    for(i=0;i<N3;i++) self->Index2Units[N1+N2+i]=3+N1+2*N2+i*3;
	for(i=0;i<N4;i++) self->Index2Units[N1+N2+N3+i]=4+N1+2*N2+3*N3+i*4;

	i=0;
    for(k=0;k<128;k++)
	{
        if(self->Index2Units[i]<k+1) i++;
		self->Units2Index[k]=i;
    }
}

static uint32_t AllocContextVariantI(PPMdSubAllocatorVariantI *self)
{
    if(self->HighUnit!=self->LowUnit)
	{
		self->HighUnit-=UNIT_SIZE;
		return _PointerToOffset(self,self->HighUnit);
	}
	else if(AreBlocksAvailable(&self->BList[0])) return _PointerToOffset(self,RemoveBlockAfter(&self->BList[0],self));
	else return _AllocUnits(self,0);
}

static uint32_t AllocUnitsVariantI(PPMdSubAllocatorVariantI *self,int num)
{
	void *units;
	int index=self->Units2Index[num-1];

	if(AreBlocksAvailable(&self->BList[index])) return _PointerToOffset(self,RemoveBlockAfter(&self->BList[index],self));

	units=self->LowUnit;
	self->LowUnit+=I2B(self,index);
	if(self->LowUnit<=self->HighUnit) return _PointerToOffset(self,units);

	self->LowUnit-=I2B(self,index);

	return _AllocUnits(self,index);
}

static uint32_t _AllocUnits(PPMdSubAllocatorVariantI *self,int index)
{
	int i;

	if(self->GlueCount==0)
	{
		GlueFreeBlocks(self);
		if(AreBlocksAvailable(&self->BList[index])) return _PointerToOffset(self,RemoveBlockAfter(&self->BList[index],self));
	}

	for(i=index+1;i<N_INDEXES;i++)
	{
		if(AreBlocksAvailable(&self->BList[i]))
		{
			void *units=RemoveBlockAfter(&self->BList[i],self);
			SplitBlock(self,units,i,index);
			return _PointerToOffset(self,units);
		}
	}

	self->GlueCount--;

	i=I2B(self,index);
	if(self->UnitsStart-self->pText>i)
	{
		self->UnitsStart-=i;
		return _PointerToOffset(self,self->UnitsStart);
	}

	return 0;
}

static uint32_t ExpandUnitsVariantI(PPMdSubAllocatorVariantI *self,uint32_t oldoffs,int oldnum)
{
	uint32_t offs;
	void *oldptr=_OffsetToPointer(self,oldoffs);
	int oldindex=self->Units2Index[oldnum-1];
	int newindex=self->Units2Index[oldnum];
	if(oldindex==newindex) return oldoffs;

	offs=AllocUnitsVariantI(self,oldnum+1);
	if(offs)
	{
		memcpy(_OffsetToPointer(self,offs),oldptr,oldnum*UNIT_SIZE);
		InsertBlockAfter(&self->BList[oldindex],oldptr,oldnum,self);
	}
	return offs;
}

static uint32_t ShrinkUnitsVariantI(PPMdSubAllocatorVariantI *self,uint32_t oldoffs,int oldnum,int newnum)
{
	void *oldptr=_OffsetToPointer(self,oldoffs);
	int oldindex=self->Units2Index[oldnum-1];
	int newindex=self->Units2Index[newnum-1];
	if(oldindex==newindex) return oldoffs;

	if(AreBlocksAvailable(&self->BList[newindex]))
	{
		void *ptr=RemoveBlockAfter(&self->BList[newindex],self);
		memcpy(ptr,oldptr,newnum*UNIT_SIZE);
		InsertBlockAfter(&self->BList[oldindex],oldptr,self->Index2Units[oldindex],self);
		return _PointerToOffset(self,ptr);
	}
	else
	{
		SplitBlock(self,oldptr,oldindex,newindex);
		return oldoffs;
    }
}

static void FreeUnitsVariantI(PPMdSubAllocatorVariantI *self,uint32_t offs,int num)
{
    int index=self->Units2Index[num-1];
	InsertBlockAfter(&self->BList[index],_OffsetToPointer(self,offs),self->Index2Units[index],self);
}



uint32_t GetUsedMemoryVariantI(PPMdSubAllocatorVariantI *self)
{
	uint32_t size=self->SubAllocatorSize-(self->HighUnit-self->LowUnit)-(self->UnitsStart-self->pText);
	int i;

	for(i=0;i<N_INDEXES;i++) size-=UNIT_SIZE*self->Index2Units[i]*self->BList[i].Stamp;

    return size;
}

void SpecialFreeUnitVariantI(PPMdSubAllocatorVariantI *self,uint32_t offs)
{
	void *ptr=_OffsetToPointer(self,offs);
	if((uint8_t *)ptr==self->UnitsStart)
	{
		*(uint32_t *)ptr=0xffffffff;
		self->UnitsStart+=UNIT_SIZE;
	}
	else InsertBlockAfter(&self->BList[0],ptr,1,self);
}

uint32_t MoveUnitsUpVariantI(PPMdSubAllocatorVariantI *self,uint32_t oldoffs,int num)
{
	void *ptr;
	int newnum;
	void *oldptr=_OffsetToPointer(self,oldoffs);
	int index=self->Units2Index[num-1];

	if((uint8_t *)oldptr>self->UnitsStart+16*1024||oldoffs>self->BList[index].next) return oldoffs;

	ptr=RemoveBlockAfter(&self->BList[index],self);
	memcpy(ptr,oldptr,num*UNIT_SIZE);

	newnum=self->Index2Units[index];
	if((uint8_t *)oldptr!=self->UnitsStart) InsertBlockAfter(&self->BList[index],oldptr,newnum,self);
	else self->UnitsStart+=newnum*UNIT_SIZE;

	return _PointerToOffset(self,ptr);
}

void ExpandTextAreaVariantI(PPMdSubAllocatorVariantI *self)
{
	PPMdMemoryBlockVariantI *p;
	unsigned int Count[N_INDEXES];
	int i;

	memset(Count,0,sizeof(Count));

	while((p=(PPMdMemoryBlockVariantI *)self->UnitsStart)->Stamp==0xffffffff)
	{
		PPMdMemoryBlockVariantI *pm=p;
		self->UnitsStart=(uint8_t *)(pm+pm->NU);
		Count[self->Units2Index[pm->NU-1]]++;
		pm->Stamp=0;
    }

	for(i=0;i<N_INDEXES;i++)
	for(p=&self->BList[i];Count[i]!=0;p=NextBlock(p,self))
	while(!NextBlock(p,self)->Stamp)
	{
		UnlinkBlockAfter(p,self);
		self->BList[i].Stamp--;
		if (!--Count[i]) break;
	}
}



static void GlueFreeBlocks(PPMdSubAllocatorVariantI *self)
{
	int i;
	PPMdMemoryBlockVariantI s0,*p0;

	if(self->LowUnit!=self->HighUnit) *self->LowUnit=0;

	p0=&s0;
	s0.next=0;
	for(i=0;i<N_INDEXES;i++)
	{
		while(AreBlocksAvailable(&self->BList[i]))
		{
			PPMdMemoryBlockVariantI *p=(PPMdMemoryBlockVariantI *)RemoveBlockAfter(&self->BList[i],self);
			PPMdMemoryBlockVariantI *p1;
			if(!p->NU) continue;
			while((p1=p+p->NU)->Stamp==0xffffffff)
			{
				p->NU+=p1->NU;
				p1->NU=0;
			}
			LinkBlockAfter(p0,p,self);
			p0=p;
		}
	}

	while(AreBlocksAvailable(&s0))
	{
		int i;
		PPMdMemoryBlockVariantI *p=RemoveBlockAfter(&s0,self);
		int sz=p->NU;
		if(!sz) continue;

		while(sz>128)
		{
			InsertBlockAfter(&self->BList[N_INDEXES-1],p,128,self);
			sz-=128;
			p+=128;
		}

		i=self->Units2Index[sz-1];
		if(self->Index2Units[i]!=sz)
		{
			int k;
			i--;
			k=sz-self->Index2Units[i];
			InsertBlockAfter(&self->BList[k-1],p+(sz-k),k,self);
		}
		InsertBlockAfter(&self->BList[i],p,self->Index2Units[i],self);
	}
	self->GlueCount=1<<13;
}



static PPMdMemoryBlockVariantI *NextBlock(PPMdMemoryBlockVariantI *self,PPMdSubAllocatorVariantI *alloc)
{
	return OffsetToPointer(&alloc->core,self->next);
}

static void SetNextBlock(PPMdMemoryBlockVariantI *self,PPMdMemoryBlockVariantI *newnext,PPMdSubAllocatorVariantI *alloc)
{
	self->next=PointerToOffset(&alloc->core,newnext);
}

static bool AreBlocksAvailable(PPMdMemoryBlockVariantI *self)
{
	return self->next!=0;
}

static void LinkBlockAfter(PPMdMemoryBlockVariantI *self,PPMdMemoryBlockVariantI *p,PPMdSubAllocatorVariantI *alloc)
{
	SetNextBlock(p,NextBlock(self,alloc),alloc);
	SetNextBlock(self,p,alloc);
}

static void UnlinkBlockAfter(PPMdMemoryBlockVariantI *self,PPMdSubAllocatorVariantI *alloc)
{
	SetNextBlock(self,NextBlock(NextBlock(self,alloc),alloc),alloc);
}

static void *RemoveBlockAfter(PPMdMemoryBlockVariantI *self,PPMdSubAllocatorVariantI *alloc)
{
	PPMdMemoryBlockVariantI *p=NextBlock(self,alloc);
	UnlinkBlockAfter(self,alloc);
	self->Stamp--;
	return p;
}

static void InsertBlockAfter(PPMdMemoryBlockVariantI *self,void *pv,int NU,PPMdSubAllocatorVariantI *alloc)
{
	PPMdMemoryBlockVariantI *p=(PPMdMemoryBlockVariantI *)pv;
	LinkBlockAfter(self,p,alloc);
	p->Stamp=0xffffffff;
	p->NU=NU;
	self->Stamp++;
}

static unsigned int I2B(PPMdSubAllocatorVariantI *self,int index) { return UNIT_SIZE*self->Index2Units[index]; }

static void SplitBlock(PPMdSubAllocatorVariantI *self,void *pv,int oldindex,int newindex)
{
	uint8_t *p=((uint8_t *)pv)+I2B(self,newindex);

	int diff=self->Index2Units[oldindex]-self->Index2Units[newindex];
	int i=self->Units2Index[diff-1];
	if(self->Index2Units[i]!=diff)
	{
		int k=self->Index2Units[--i];
		InsertBlockAfter(&self->BList[i],p,k,self);
		p+=k*UNIT_SIZE;
		diff-=k;
	}
	InsertBlockAfter(&self->BList[self->Units2Index[diff-1]],p,diff,self);
}
