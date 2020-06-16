#ifndef CarrylessRangeCoder_h
#define CarrylessRangeCoder_h

#include <stdint.h>
#include "stdbool.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct InStream InStream;

typedef struct InStream
{
	uint8_t (*nextByte)(InStream *self);
};

static uint8_t InStreamNextByte(InStream *self) { return self->nextByte(self); }

typedef struct CarrylessRangeCoder
{
	InStream *input;
	uint32_t low,code,range,bottom;
	bool uselow;
} CarrylessRangeCoder;

void InitializeRangeCoder(CarrylessRangeCoder *self,InStream *input,bool uselow,int bottom);

uint32_t RangeCoderCurrentCount(CarrylessRangeCoder *self,uint32_t scale);
void RemoveRangeCoderSubRange(CarrylessRangeCoder *self,uint32_t lowcount,uint32_t highcount);

int NextSymbolFromRangeCoder(CarrylessRangeCoder *self,uint32_t *freqtable,int numfreq);
int NextBitFromRangeCoder(CarrylessRangeCoder *self);
int NextWeightedBitFromRangeCoder(CarrylessRangeCoder *self,int weight,int size);

int NextWeightedBitFromRangeCoder2(CarrylessRangeCoder *self,int weight,int shift);

void NormalizeRangeCoder(CarrylessRangeCoder *self);

#ifdef __cplusplus
}
#endif

#endif
