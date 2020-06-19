#ifndef PPMdVariantI_h
#define PPMdVariantI_h

#include "PPMdContext.h"
#include "PPMdSubAllocatorVariantI.h"

// PPMd Variant I. Used by WinZip.

#ifdef __cplusplus
extern "C" {
#endif

#define MRM_RESTART 0
#define MRM_CUT_OFF 1
#define MRM_FREEZE 2

typedef struct PPMdModelVariantI
{
	PPMdCoreModel core;

	PPMdSubAllocatorVariantI *alloc;

	uint8_t NS2BSIndx[256],QTable[260]; // constants

	PPMdContext *MaxContext;
	int MaxOrder,MRMethod;
	SEE2Context SEE2Cont[24][32],DummySEE2Cont;
	uint16_t BinSumm[25][64]; // binary SEE-contexts
} PPMdModelVariantI;

PPMdModelVariantI *CreatePPMdModelVariantI(InStream *input,
  int suballocsize,int maxorder,int restoration);
void FreePPMdModelVariantI(PPMdModelVariantI *self);

void StartPPMdModelVariantI(PPMdModelVariantI *self,InStream *input,
PPMdSubAllocatorVariantI *alloc,int maxorder,int restoration);
int NextPPMdVariantIByte(PPMdModelVariantI *self);

#ifdef __cplusplus
}
#endif

#endif
