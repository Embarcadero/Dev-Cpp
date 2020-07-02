#ifndef PPMdSubAllocatorVariantI_h
#define PPMdSubAllocatorVariantI_h

#include "PPMdSubAllocator.h"

#ifdef __cplusplus
extern "C" {
#endif

#pragma pack(push, 1)
typedef struct PPMdMemoryBlockVariantI
{
	uint32_t Stamp;
	uint32_t next;
	uint32_t NU;
} PPMdMemoryBlockVariantI;
#pragma pack(pop)

typedef struct PPMdSubAllocatorVariantI
{
	PPMdSubAllocator core;

	uint32_t GlueCount,SubAllocatorSize;
	uint8_t Index2Units[38],Units2Index[128]; // constants
	uint8_t *pText,*UnitsStart,*LowUnit,*HighUnit;
	PPMdMemoryBlockVariantI BList[38];
	uint8_t HeapStart[0];
} PPMdSubAllocatorVariantI;

PPMdSubAllocatorVariantI *CreateSubAllocatorVariantI(int size);
void FreeSubAllocatorVariantI(PPMdSubAllocatorVariantI *self);

uint32_t GetUsedMemoryVariantI(PPMdSubAllocatorVariantI *self);
void SpecialFreeUnitVariantI(PPMdSubAllocatorVariantI *self,uint32_t offs);
uint32_t MoveUnitsUpVariantI(PPMdSubAllocatorVariantI *self,uint32_t oldoffs,int num);
void ExpandTextAreaVariantI(PPMdSubAllocatorVariantI *self);

#ifdef __cplusplus
}
#endif

#endif
