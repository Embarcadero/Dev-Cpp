#include "stdbool.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "PPMdVariantI.h"

#define UP_FREQ 5
#define O_BOUND 9

static void RestartModel(PPMdModelVariantI *self);

static void UpdateModel(PPMdModelVariantI *self,PPMdContext *mincontext);
static PPMdContext *CreateSuccessors(PPMdModelVariantI *self,bool skip,PPMdState *p1,PPMdContext *mincontext);
static PPMdContext *ReduceOrder(PPMdModelVariantI *self,PPMdState *state,PPMdContext *startcontext);
static void RestoreModel(PPMdModelVariantI *self,PPMdContext *currcontext,PPMdContext *mincontext,PPMdContext *FSuccessor);

static void ShrinkContext(PPMdContext *self,int newlastindex,bool scale,PPMdModelVariantI *model);
static PPMdContext *CutOffContext(PPMdContext *self,int order,PPMdModelVariantI *model);
static PPMdContext *RemoveBinConts(PPMdContext *self,int order,PPMdModelVariantI *model);

static void DecodeBinSymbolVariantI(PPMdContext *self,PPMdModelVariantI *model);
static void DecodeSymbol1VariantI(PPMdContext *self,PPMdModelVariantI *model);
static void DecodeSymbol2VariantI(PPMdContext *self,PPMdModelVariantI *model);

static void RescalePPMdContextVariantI(PPMdContext *self,PPMdModelVariantI *model);


PPMdModelVariantI *CreatePPMdModelVariantI(InStream *input,
  int suballocsize,int maxorder,int restoration)
{
	PPMdSubAllocatorVariantI *alloc;
	PPMdModelVariantI *self=malloc(sizeof(PPMdModelVariantI));
	if(!self) return NULL;
	alloc=CreateSubAllocatorVariantI(suballocsize);
	if(!alloc)
	{
		free(self);
		return NULL;
	}
	StartPPMdModelVariantI(self, input, alloc, maxorder, restoration);
	return self;
}

void FreePPMdModelVariantI(PPMdModelVariantI *self)
{
	free(self->alloc);
	free(self);
}


void StartPPMdModelVariantI(PPMdModelVariantI *self,InStream *input,
PPMdSubAllocatorVariantI *alloc,int maxorder,int restoration)
{
	PPMdContext *pc;
	int i, m, k, step;

	InitializeRangeCoder(&self->core.coder,input,true,0x8000);

	if(maxorder<2) // TODO: solid mode
	{
		memset(self->core.CharMask,0,sizeof(self->core.CharMask));
		self->core.OrderFall=self->MaxOrder;
		for(pc=self->MaxContext;pc->Suffix;pc=PPMdContextSuffix(pc,&self->core))
		self->core.OrderFall--;
		return;
	}

	self->alloc=alloc;
	self->core.alloc=&alloc->core;

	self->core.RescalePPMdContext=(void *)RescalePPMdContextVariantI;

	self->MaxOrder=maxorder;
	self->MRMethod=restoration;
	self->core.EscCount=1;

	self->NS2BSIndx[0]=2*0;
	self->NS2BSIndx[1]=2*1;
	for(i=2;i<11;i++) self->NS2BSIndx[i]=2*2;
	for(i=11;i<256;i++) self->NS2BSIndx[i]=2*3;

	for(i=0;i<UP_FREQ;i++) self->QTable[i]=i;
	m=UP_FREQ;
	k=1;
	step=1;
	for(i=UP_FREQ;i<260;i++)
	{
		self->QTable[i]=m;
		if(!--k) { m++; step++; k=step; }
	}

	self->DummySEE2Cont.Summ=0xaf8f;
	//self->DummySEE2Cont.Shift=0xac;
	self->DummySEE2Cont.Count=0x84;
	self->DummySEE2Cont.Shift=PERIOD_BITS;

	RestartModel(self);
}

static void RestartModel(PPMdModelVariantI *self)
{
	static const uint16_t InitBinEsc[8]={0x3cdd,0x1f3f,0x59bf,0x48f3,0x64a1,0x5abc,0x6632,0x6051};
	int i, m;
	PPMdState *maxstates;

	InitSubAllocator(self->core.alloc);

	memset(self->core.CharMask,0,sizeof(self->core.CharMask));

	self->core.PrevSuccess=0;
	self->core.OrderFall=self->MaxOrder;
	self->core.RunLength=self->core.InitRL=-((self->MaxOrder<12)?self->MaxOrder:12)-1;

	self->MaxContext=NewPPMdContext(&self->core);
	self->MaxContext->LastStateIndex=255;
	self->MaxContext->SummFreq=257;
	self->MaxContext->States=AllocUnits(self->core.alloc,256/2);

	maxstates=PPMdContextStates(self->MaxContext,&self->core);
	for(i=0;i<256;i++)
	{
		maxstates[i].Symbol=i;
		maxstates[i].Freq=1;
		maxstates[i].Successor=0;
	}

	i=0;
	for(m=0;m<25;m++)
	{
		int k;

		while(self->QTable[i]==m) i++;
		for(k=0;k<8;k++) self->BinSumm[m][k]=BIN_SCALE-InitBinEsc[k]/(i+1);
		for(k=8;k<64;k+=8) memcpy(&self->BinSumm[m][k],&self->BinSumm[m][0],8*sizeof(uint16_t));
	}

	i=0;
	for(m=0;m<24;m++)
	{
		int k;

		while(self->QTable[i+3]==m+3) i++;
        for(k=0;k<32;k++) self->SEE2Cont[m][k]=MakeSEE2(2*i+5,7);
    }
}



int NextPPMdVariantIByte(PPMdModelVariantI *self)
{
	uint8_t byte;
	PPMdContext *mincontext=self->MaxContext;

	if(mincontext->LastStateIndex!=0) DecodeSymbol1VariantI(mincontext,self);
	else DecodeBinSymbolVariantI(mincontext,self);

	while(!self->core.FoundState)
	{
		do
		{
			self->core.OrderFall++;
			mincontext=PPMdContextSuffix(mincontext,&self->core);
			if(!mincontext) return -1;
		}
		while(mincontext->LastStateIndex==self->core.LastMaskIndex);

		DecodeSymbol2VariantI(mincontext,self);
	}

	byte=self->core.FoundState->Symbol;

	if(self->core.OrderFall==0&&(uint8_t *)PPMdStateSuccessor(self->core.FoundState,&self->core)>=self->alloc->UnitsStart)
	{
		self->MaxContext=PPMdStateSuccessor(self->core.FoundState,&self->core);
		//PrefetchData(MaxContext)
	}
	else
	{
		UpdateModel(self,mincontext);
		//PrefetchData(MaxContext)
		if(self->core.EscCount==0) ClearPPMdModelMask(&self->core);
	}

	return byte;
}



static void UpdateModel(PPMdModelVariantI *self,PPMdContext *mincontext)
{
	int minnum, s0;
	uint8_t flag;

	PPMdState fs=*self->core.FoundState;
	PPMdState *state=NULL;
	PPMdContext *currcontext=self->MaxContext;
	PPMdContext *Successor;

	if(fs.Freq<MAX_FREQ/4&&mincontext->Suffix)
	{
		PPMdContext *context=PPMdContextSuffix(mincontext,&self->core);
		if(context->LastStateIndex!=0)
		{
			state=PPMdContextStates(context,&self->core);

			if(state->Symbol!=fs.Symbol)
			{
				do state++;
				while(state->Symbol!=fs.Symbol);

				if(state[0].Freq>=state[-1].Freq)
				{
					SWAP(state[0],state[-1]);
					state--;
				}
			}

			if(state->Freq<MAX_FREQ-9)
			{
				state->Freq+=2;
				context->SummFreq+=2;
			}
		}
		else
		{
			state=PPMdContextOneState(context);
			if(state->Freq<32) state->Freq++;
		}
	}

	if(self->core.OrderFall==0&&fs.Successor)
	{
		PPMdContext *newsuccessor=CreateSuccessors(self,true,state,mincontext);
		SetPPMdStateSuccessorPointer(self->core.FoundState,newsuccessor,&self->core);
		if(!newsuccessor) goto RESTART_MODEL;
		self->MaxContext=newsuccessor;
		return;
	}

	*self->alloc->pText++=fs.Symbol;
	Successor=(PPMdContext *)self->alloc->pText;

	if(self->alloc->pText>=self->alloc->UnitsStart) goto RESTART_MODEL;

	if(fs.Successor)
	{
		if((uint8_t *)PPMdStateSuccessor(&fs,&self->core)<self->alloc->UnitsStart)
		{
			SetPPMdStateSuccessorPointer(&fs,CreateSuccessors(self,false,state,mincontext),&self->core);
		}
	}
	else
	{
		SetPPMdStateSuccessorPointer(&fs,ReduceOrder(self,state,mincontext),&self->core);
	}

	if(!fs.Successor) goto RESTART_MODEL;

	if(--self->core.OrderFall==0)
	{
		Successor=PPMdStateSuccessor(&fs,&self->core);
		if(self->MaxContext!=mincontext) self->alloc->pText--;
	}
	else if(self->MRMethod>MRM_FREEZE)
	{
		Successor=PPMdStateSuccessor(&fs,&self->core);
		self->alloc->pText=self->alloc->HeapStart;
		self->core.OrderFall=0;
	}

	minnum=mincontext->LastStateIndex+1;
	s0=mincontext->SummFreq-minnum-(fs.Freq-1);
	flag=fs.Symbol>=0x40?8:0;

	for(;currcontext!=mincontext;currcontext=PPMdContextSuffix(currcontext,&self->core))
	{
		unsigned int cf, sf, freq;
		PPMdState *currstates, *new;

		int currnum=currcontext->LastStateIndex+1;
		if(currnum!=1)
		{
			if((currnum&1)==0)
			{
				uint32_t states=ExpandUnits(self->core.alloc,currcontext->States,currnum>>1);
				if(!states) goto RESTART_MODEL;
				currcontext->States=states;
			}
			if(3*currnum-1<minnum) currcontext->SummFreq++;
		}
		else
		{
			PPMdState *states=OffsetToPointer(self->core.alloc,AllocUnits(self->core.alloc,1));
			if(!states) goto RESTART_MODEL;
			states[0]=*(PPMdContextOneState(currcontext));
			SetPPMdContextStatesPointer(currcontext,states,&self->core);

			if(states[0].Freq<MAX_FREQ/4-1) states[0].Freq*=2;
			else states[0].Freq=MAX_FREQ-4;

			currcontext->SummFreq=states[0].Freq+self->core.InitEsc+(minnum>3?1:0);
		}

		cf=2*fs.Freq*(currcontext->SummFreq+6);
		sf=s0+currcontext->SummFreq;


		if(cf<6*sf)
		{
			if(cf>=4*sf) freq=3;
			else if(cf>sf) freq=2;
			else freq=1;
			currcontext->SummFreq+=4;
		}
		else
		{
			if(cf>15*sf) freq=7;
			else if(cf>12*sf) freq=6;
			else if(cf>9*sf) freq=5;
			else freq=4;
			currcontext->SummFreq+=freq;
		}

		currcontext->LastStateIndex++;
		currstates=PPMdContextStates(currcontext,&self->core);
		new=&currstates[currcontext->LastStateIndex];
		SetPPMdStateSuccessorPointer(new,Successor,&self->core);
		new->Symbol=fs.Symbol;
		new->Freq=freq;
		currcontext->Flags|=flag;
	}

	self->MaxContext=PPMdStateSuccessor(&fs,&self->core);

	return;

	RESTART_MODEL:
	RestoreModel(self,currcontext,mincontext,PPMdStateSuccessor(&fs,&self->core));
}

static PPMdContext *CreateSuccessors(PPMdModelVariantI *self,bool skip,PPMdState *state,PPMdContext *context)
{
	PPMdContext *upbranch=PPMdStateSuccessor(self->core.FoundState,&self->core);
	PPMdState *statelist[MAX_O];
	PPMdState *onestate;
	PPMdContext ct;
	uint8_t newsym;
	uint8_t sym=self->core.FoundState->Symbol;
	int n=0, i;

	if(!skip)
	{
		statelist[n++]=self->core.FoundState;
		if(!context->Suffix) goto skip;
	}

	if(state)
	{
		context=PPMdContextSuffix(context,&self->core);
		if(PPMdStateSuccessor(state,&self->core)!=upbranch)
		{
			context=PPMdStateSuccessor(state,&self->core);
			goto skip;
		}
		statelist[n++]=state;
		if(!context->Suffix) goto skip;
	}

	do
	{
		context=PPMdContextSuffix(context,&self->core);
		if(context->LastStateIndex!=0)
		{
			state=PPMdContextStates(context,&self->core);
			while(state->Symbol!=sym) state++;

			if(state->Freq<MAX_FREQ-9)
			{
				state->Freq++;
				context->SummFreq++;
			}
		}
		else
		{
			state=PPMdContextOneState(context);
			state->Freq+=(!PPMdContextSuffix(context,&self->core)->LastStateIndex&(state->Freq<24));
		}

		if(PPMdStateSuccessor(state,&self->core)!=upbranch)
		{
			context=PPMdStateSuccessor(state,&self->core);
			break;
		}
		statelist[n++]=state;
	}
	while(context->Suffix);

	skip:

	if(n==0) return context;

	newsym=*(uint8_t *)upbranch;

	ct.LastStateIndex=0;
	ct.Flags=0;
	if(sym>=0x40) ct.Flags|=0x10;
	if(newsym>=0x40) ct.Flags|=0x08;

	onestate=PPMdContextOneState(&ct);
	onestate->Symbol=newsym;
	SetPPMdStateSuccessorPointer(onestate,(PPMdContext *)(((uint8_t *)upbranch)+1),&self->core);

	if(context->LastStateIndex!=0)
	{
		int cf, s0;
		state=PPMdContextStates(context,&self->core);
		while(state->Symbol!=newsym) state++;

		cf=state->Freq-1;
		s0=context->SummFreq-context->LastStateIndex-cf;

		if(2*cf<=s0)
		{
			if(5*cf>s0) onestate->Freq=2;
			else onestate->Freq=1;
		}
		else onestate->Freq=1+((cf+2*s0-3)/s0);
	}
	else onestate->Freq=PPMdContextOneState(context)->Freq;

	for(i=n-1;i>=0;i--)
	{
		PPMdContext *newcontext=(PPMdContext *)OffsetToPointer(self->core.alloc,AllocContext(self->core.alloc));
		if(!newcontext) return NULL;

		memcpy(newcontext,&ct,8);
		SetPPMdContextSuffixPointer(newcontext,context,&self->core);
		SetPPMdStateSuccessorPointer(statelist[i],newcontext,&self->core);

		context=newcontext;
	}

	return context;
}

static PPMdContext *ReduceOrder(PPMdModelVariantI *self,PPMdState *state,PPMdContext *startcontext)
{
	PPMdState *statelist[MAX_O];
	PPMdContext *context=startcontext,*upbranch=(PPMdContext *)self->alloc->pText;
	uint8_t sym=self->core.FoundState->Symbol;

	int n=0, i;

	statelist[n++]=self->core.FoundState;
	self->core.OrderFall++;

    if(state)
    {
		context=PPMdContextSuffix(context,&self->core);
		if(state->Successor) goto skip;
		statelist[n++]=state;
		self->core.OrderFall++;
	}

	for(;;)
	{
		if(!context->Suffix)
		{
			if(self->MRMethod>MRM_FREEZE)
			{
				for(i=0;i<n;i++) SetPPMdStateSuccessorPointer(statelist[i],context,&self->core);
				self->alloc->pText=self->alloc->HeapStart+1;
				self->core.OrderFall=1;
			}
			else
			{
				for(i=0;i<n;i++) SetPPMdStateSuccessorPointer(statelist[i],upbranch,&self->core);
			}
			return context;
		}

        context=PPMdContextSuffix(context,&self->core);

		if(context->LastStateIndex)
		{
			state=PPMdContextStates(context,&self->core);
			while(state->Symbol!=sym) state++;

			if(state->Freq<MAX_FREQ-9)
			{
				state->Freq+=2;
				context->SummFreq+=2;
			}
		}
		else
		{
			state=PPMdContextOneState(context);
			if(state->Freq<32) state->Freq++;
		}

		if(state->Successor) break;

		statelist[n++]=state;
		self->core.OrderFall++;
	}
	skip:

	if(self->MRMethod>MRM_FREEZE)
	{
		PPMdContext *successor=PPMdStateSuccessor(state,&self->core);
		for(i=0;i<n;i++) SetPPMdStateSuccessorPointer(statelist[i],successor,&self->core);

		self->alloc->pText=self->alloc->HeapStart+1;
		self->core.OrderFall=1;

		return successor;
	}
	else
	{
		for(i=0;i<n;i++) SetPPMdStateSuccessorPointer(statelist[i],upbranch,&self->core);
	}

	if(PPMdStateSuccessor(state,&self->core)<=upbranch)
	{
		PPMdState *tmp=self->core.FoundState;
		self->core.FoundState=state;
        SetPPMdStateSuccessorPointer(state,CreateSuccessors(self,false,NULL,context),&self->core);
		self->core.FoundState=tmp;
	}

	if(self->core.OrderFall==1&&startcontext==self->MaxContext)
	{
		self->core.FoundState->Successor=state->Successor;
		self->alloc->pText--;
	}

	return PPMdStateSuccessor(state,&self->core);
}

static void RestoreModel(PPMdModelVariantI *self,PPMdContext *currcontext,PPMdContext *mincontext,PPMdContext *FSuccessor)
{
	PPMdContext *context;
	self->alloc->pText=self->alloc->HeapStart;

	context=self->MaxContext;
	while(context!=currcontext)
	{
		if(context->LastStateIndex==1)
		{
			PPMdState state=*(PPMdContextStates(context,&self->core));
			SpecialFreeUnitVariantI(self->alloc,context->States);

			state.Freq=(state.Freq+11)>>3;
			*(PPMdContextOneState(context))=state;

			context->LastStateIndex=0;
			context->Flags&=0x10;
			if(state.Symbol>=0x40) context->Flags+=0x08;
		}
		else
		{
			ShrinkContext(context,context->LastStateIndex-1,false,self);
		}

		context=PPMdContextSuffix(context,&self->core);
	}

	while(context!=mincontext)
	{
		if(!context->LastStateIndex)
		{
			PPMdContextOneState(context)->Freq=(PPMdContextOneState(context)->Freq+1)>>1;
		}
		else
		{
			context->SummFreq+=4;
			if(context->SummFreq>128+4*context->LastStateIndex)
			ShrinkContext(context,context->LastStateIndex,true,self);
		}

		context=PPMdContextSuffix(context,&self->core);
	}

	if(self->MRMethod>MRM_FREEZE)
	{
		self->MaxContext=FSuccessor;
		if(!(self->alloc->BList[1].Stamp&1)) self->alloc->GlueCount++;
	}
	else if(self->MRMethod==MRM_FREEZE)
	{
		while(self->MaxContext->Suffix) self->MaxContext=PPMdContextSuffix(self->MaxContext,&self->core);

		RemoveBinConts(self->MaxContext,0,self);
		self->MRMethod=self->MRMethod+1;
		self->alloc->GlueCount=0;
		self->core.OrderFall=self->MaxOrder;
	}
	else if(self->MRMethod==MRM_RESTART||GetUsedMemoryVariantI(self->alloc)<(self->alloc->SubAllocatorSize>>1))
	{
		RestartModel(self);
		self->core.EscCount=0;
	}
	else
	{
		while(self->MaxContext->Suffix) self->MaxContext=PPMdContextSuffix(self->MaxContext,&self->core);
		do
		{
			CutOffContext(self->MaxContext,0,self);
			ExpandTextAreaVariantI(self->alloc);
		} while(GetUsedMemoryVariantI(self->alloc)>3*(self->alloc->SubAllocatorSize>>2));

		self->alloc->GlueCount=0;
		self->core.OrderFall=self->MaxOrder;
	}
}



static void ShrinkContext(PPMdContext *self,int newlastindex,bool scale,PPMdModelVariantI *model)
{
	int i, escfreq;
	PPMdState *states;

	self->States=ShrinkUnits(model->core.alloc,self->States,(self->LastStateIndex+2)>>1,(newlastindex+2)>>1);
	self->LastStateIndex=newlastindex;

	if(scale) self->Flags&=0x14;
	else self->Flags&=0x10;

	states=PPMdContextStates(self,&model->core);
	escfreq=self->SummFreq;
	self->SummFreq=0;

	for(i=0;i<=self->LastStateIndex;i++)
	{
		escfreq-=states[i].Freq;
		if(scale) states[i].Freq=(states[i].Freq+1)>>1;
		self->SummFreq+=states[i].Freq;
		if(states[i].Symbol>=0x40) self->Flags|=0x08;
	}

	if(scale) escfreq=(escfreq+1)>>1;

	self->SummFreq+=escfreq;
}

static PPMdContext *CutOffContext(PPMdContext *self,int order,PPMdModelVariantI *model)
{
	int i, oldnum, n;
	PPMdState *states;

	if(self->LastStateIndex==0)
	{
		PPMdState *onestate=PPMdContextOneState(self);
		if((uint8_t *)PPMdStateSuccessor(onestate,&model->core)>=model->alloc->UnitsStart)
		{
			if(order<model->MaxOrder)
			{
				//PrefetchData(p->Successor);
				SetPPMdStateSuccessorPointer(onestate,
				CutOffContext(PPMdStateSuccessor(onestate,&model->core),order+1,model),
				&model->core);
			}
			else onestate->Successor=0;

			if(!onestate->Successor&&order>O_BOUND)
			{
				SpecialFreeUnitVariantI(model->alloc,PointerToOffset(model->core.alloc,self));
				return NULL;
			}

			return self;
		}
		else
		{
			SpecialFreeUnitVariantI(model->alloc,PointerToOffset(model->core.alloc,self));
			return NULL;
		}
	}
	//PrefetchData(self->States);

	oldnum=(self->LastStateIndex+2)>>1;
	self->States=MoveUnitsUpVariantI(model->alloc,self->States,oldnum);

	n=self->LastStateIndex;
	states=PPMdContextStates(self,&model->core);
	for(i=n;i>=0;i--)
	{
		if((uint8_t *)PPMdStateSuccessor(&states[i],&model->core)<model->alloc->UnitsStart)
		{
			states[i].Successor=0;
			SWAP(states[i],states[n]);
			n--;
		}
		else if(order<model->MaxOrder)
		{
			//PrefetchData(state->Successor);
			SetPPMdStateSuccessorPointer(&states[i],
			CutOffContext(PPMdStateSuccessor(&states[i],&model->core),order+1,model),
			&model->core);
		}
		else states[i].Successor=0;
	}

	if(n!=self->LastStateIndex&&order)
	{
		if(n<0)
		{
			FreeUnits(model->core.alloc,self->States,oldnum);
			SpecialFreeUnitVariantI(model->alloc,PointerToOffset(model->core.alloc,self));
			return NULL;
		}
        else if(n==0)
		{
			PPMdState state=*(PPMdContextStates(self,&model->core));
			FreeUnits(model->core.alloc,self->States,oldnum);

			state.Freq=(state.Freq+11)>>3;
			*(PPMdContextOneState(self))=state;

			self->LastStateIndex=0;
			self->Flags&=0x10;
			if(state.Symbol>=0x40) self->Flags+=0x08;
		}
		else ShrinkContext(self,n,self->SummFreq>16*n,model);
	}
	return self;
}

static PPMdContext *RemoveBinConts(PPMdContext *self,int order,PPMdModelVariantI *model)
{
	int i;
	PPMdState *states;

	if(self->LastStateIndex==0)
	{
		PPMdState *state=PPMdContextOneState(self);
		if((uint8_t *)PPMdStateSuccessor(state,&model->core)>=model->alloc->UnitsStart&&order<model->MaxOrder)
		{
			//PrefetchData(onestate->Successor);
			SetPPMdStateSuccessorPointer(state,
			RemoveBinConts(PPMdStateSuccessor(state,&model->core),order+1,model),
			&model->core);
		}
		else state->Successor=0;

		if(!state->Successor)
		{
			PPMdContext *suffix=PPMdContextSuffix(self,&model->core);
			if(suffix->LastStateIndex==0||suffix->Flags==0xff)
			{
				FreeUnits(model->core.alloc,PointerToOffset(model->core.alloc,self),1);
				return NULL;
			}
		}

		return self;
	}
	//PrefetchData(self->States);

	states=PPMdContextStates(self,&model->core);
	for(i=self->LastStateIndex;i>=0;i--)
	{
		if((uint8_t *)PPMdStateSuccessor(&states[i],&model->core)>=model->alloc->UnitsStart&&order<model->MaxOrder)
		{
			//PrefetchData(states[i].Successor);
			SetPPMdStateSuccessorPointer(&states[i],
			RemoveBinConts(PPMdStateSuccessor(&states[i],&model->core),order+1,model),
			&model->core);
		}
		else states[i].Successor=0;
	}

	return self;
}





static void DecodeBinSymbolVariantI(PPMdContext *self,PPMdModelVariantI *model)
{
	PPMdState *rs=PPMdContextOneState(self);

	uint8_t index=model->NS2BSIndx[PPMdContextSuffix(self,&model->core)->LastStateIndex]+model->core.PrevSuccess+self->Flags;
	uint16_t *bs=&model->BinSumm[model->QTable[rs->Freq-1]][index+((model->core.RunLength>>26)&0x20)];

	PPMdDecodeBinSymbol(self,&model->core,bs,196,false);
}

static void DecodeSymbol1VariantI(PPMdContext *self,PPMdModelVariantI *model)
{
	PPMdDecodeSymbol1(self,&model->core,true);
}

static void DecodeSymbol2VariantI(PPMdContext *self,PPMdModelVariantI *model)
{
	SEE2Context *see;

	//uint8_t *pb=(uint8_t *)PPMdContextStates(self);
	//unsigned int t=2*self->LastStateIndex;
	//PrefetchData(pb);
	//PrefetchData(pb+t);
	//PrefetchData(pb+2*t);
	//PrefetchData(pb+3*t);

	if(self->LastStateIndex!=255)
	{
		int n=PPMdContextSuffix(self,&model->core)->LastStateIndex;
 		see=&model->SEE2Cont[model->QTable[self->LastStateIndex+2]-3][
			(self->SummFreq>11*(self->LastStateIndex+1)?1:0)
			+(2*self->LastStateIndex<n+model->core.LastMaskIndex?2:0)
			+self->Flags];
		model->core.scale=GetSEE2Mean(see);
	}
	else
	{
		model->core.scale=1;
		see=&model->DummySEE2Cont;
	}

	PPMdDecodeSymbol2(self,&model->core,see);
}

static void RescalePPMdContextVariantI(PPMdContext *self,PPMdModelVariantI *model)
{
	PPMdState *states=PPMdContextStates(self,&model->core);
	int n=self->LastStateIndex+1;
	int i, escfreq, adder;

	// Bump frequency of found state
	model->core.FoundState->Freq+=4;

	// Divide all frequencies and sort list
	escfreq=self->SummFreq+4;
	adder=(model->core.OrderFall!=0||model->MRMethod>MRM_FREEZE?1:0);
	self->SummFreq=0;

	for(i=0;i<n;i++)
	{
		escfreq-=states[i].Freq;
		states[i].Freq=(states[i].Freq+adder)>>1;
		self->SummFreq+=states[i].Freq;

		// Keep states sorted by decreasing frequency
		if(i>0&&states[i].Freq>states[i-1].Freq)
		{
			// If not sorted, move current state upwards until list is sorted
			PPMdState tmp=states[i];

			int j=i-1;
			while(j>0&&tmp.Freq>states[j-1].Freq) j--;

			memmove(&states[j+1],&states[j],sizeof(PPMdState)*(i-j));
			states[j]=tmp;
		}
	}

	// TODO: add better sorting stage here.

	// Drop states whose frequency has fallen to 0
	if(states[n-1].Freq==0)
	{
		int numzeros=1;
		while(numzeros<n&&states[n-1-numzeros].Freq==0) numzeros++;

		escfreq+=numzeros;

		self->LastStateIndex-=numzeros;
		if(self->LastStateIndex==0)
		{
			PPMdState tmp=states[0];

			tmp.Freq=(2*tmp.Freq+escfreq-1)/escfreq;
			if(tmp.Freq>MAX_FREQ/3) tmp.Freq=MAX_FREQ/3;

			FreeUnits(model->core.alloc,self->States,(n+1)>>1);
			model->core.FoundState=PPMdContextOneState(self);
			*model->core.FoundState=tmp;

			self->Flags=(self->Flags&0x10)+0x08*(tmp.Symbol>=0x40);

			return;
		}

		self->States=ShrinkUnits(model->core.alloc,self->States,(n+1)>>1,(self->LastStateIndex+2)>>1);

		// Added a scope to handle the states overload
		{ 
		int i;
		PPMdState *states;

		states=PPMdContextStates(self,&model->core);
		self->Flags&=~0x08;

		for(i=0;i<=self->LastStateIndex;i++)
		if(states[i].Symbol>=0x40) self->Flags|=0x08;
		}
	}

	self->SummFreq+=(escfreq+1)>>1;
	self->Flags|=0x04; 

	// The found state is the first one to breach the limit, thus it is the largest and also first
	model->core.FoundState=PPMdContextStates(self,&model->core);
}
