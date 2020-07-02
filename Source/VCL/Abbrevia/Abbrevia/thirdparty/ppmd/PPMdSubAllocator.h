#ifndef PPMdSubAllocator_h
#define PPMdSubAllocator_h

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct PPMdSubAllocator PPMdSubAllocator;

struct PPMdSubAllocator
{
	void (*Init)(PPMdSubAllocator *self);
	uint32_t (*AllocContext)(PPMdSubAllocator *self);
	uint32_t (*AllocUnits)(PPMdSubAllocator *self,int num);  // 1 unit == 12 bytes, NU <= 128
	uint32_t (*ExpandUnits)(PPMdSubAllocator *self,uint32_t oldoffs,int oldnum);
	uint32_t (*ShrinkUnits)(PPMdSubAllocator *self,uint32_t oldoffs,int oldnum,int newnum);
	void (*FreeUnits)(PPMdSubAllocator *self,uint32_t offs,int num);
};

static void InitSubAllocator(PPMdSubAllocator *self) { self->Init(self); };
static uint32_t AllocContext(PPMdSubAllocator *self) { return self->AllocContext(self); }
static uint32_t AllocUnits(PPMdSubAllocator *self,int num) { return self->AllocUnits(self,num); }
static uint32_t ExpandUnits(PPMdSubAllocator *self,uint32_t oldoffs,int oldnum) { return self->ExpandUnits(self,oldoffs,oldnum); }
static uint32_t ShrinkUnits(PPMdSubAllocator *self,uint32_t oldoffs,int oldnum,int newnum) { return self->ShrinkUnits(self,oldoffs,oldnum,newnum); }
static void FreeUnits(PPMdSubAllocator *self,uint32_t offs,int num) { self->FreeUnits(self,offs,num); }

// TODO: Keep pointers as pointers on 32 bit, and offsets on 64 bit.

static void *OffsetToPointer(PPMdSubAllocator *self,uint32_t offset)
{
	if(!offset) return 0;
	return ((uint8_t *)self)+offset;
}

static uint32_t PointerToOffset(PPMdSubAllocator *self,void *pointer)
{
	if(!pointer) return 0;
	return ((uintptr_t)pointer)-(uintptr_t)self;
}

#ifdef __cplusplus
}
#endif

#endif
