/*
  the header of d16fmem.c
  d16fmem: dos16 raw (cooked minimally) allocator and memory utilities
*/

#ifndef DOS16_FMEM_H
#define DOS16_FMEM_H

#include <dos.h>

#if defined __cplusplus
extern "C" {
#endif

#define dos16_fp_to_linear(p) (((unsigned long)(FP_SEG(p)&0xffffU)<<4)+FP_OFF(p))
#define dos16_linear_to_fp(la) MK_FP((la)>>4,(la)&0xf)

unsigned dos16_segalloc(unsigned para);
int dos16_segfree(unsigned mem_seg);
unsigned dos16_segrealloc(unsigned mem_seg, unsigned para);


void far *dos16_farmalloc(unsigned long size);
void far *dos16_farmalloc_lastfit(unsigned long size);
void dos16_farfree(void far *p);
void far *dos16_farrealloc(void far *p, unsigned long size);

void far *dos16_memalign(unsigned long align, unsigned long size);

unsigned dos16_getallocstate(void);
unsigned dos16_setallocstate(unsigned state);

#define DOS16_FIRSTFIT          0x0000
#define DOS16_BESTFIT           0x0001
#define DOS16_LASTFIT           0x0002
#define DOS16_UMBONLY_FIRSTFIT  0x0140
#define DOS16_UMBONLY_BESTFIT   0x0141
#define DOS16_UMBONLY_LASTFIT   0x0142
#define DOS16_UMB_FIRSTFIT      0x0180
#define DOS16_UMB_BESTFIT       0x0181
#define DOS16_UMB_LASTFIT       0x0182

void dos16_fmemcpy(void far *dst, const void far *src, size_t len);
void dos16_hmemcpy(void far *dst, const void far *src, unsigned long len);
void dos16_fmemset(void far *dst, int c, size_t len);

#if defined __cplusplus
}
#endif

#endif

