/*
  d16fmem: dos16 raw (cooked minimally) allocator and memory utilities

  author: lpproj (https://github.com/lpproj)
  license: `Public Domain'
           (You can use/modify/redistribute it freely BUT NO WARRANTY.)
*/

#include <limits.h>
#include <dos.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "d16fmem.h"

#if defined(_MSC_VER) || defined(__WATCOMC__)
#pragma pack(1)
# define enable _enable
# define disable _disable
#endif

#if defined(LSI_C)
void _asm_d16_sti(char *);
void _asm_d16_cli(char *);
# define enable() _asm_d16_sti("\n\tsti\n")
# define disable() _asm_d16_cli("\n\tcli\n")
#endif

unsigned short dos16_memalloc_errno;
#define seterr(n) (dos16_memalloc_errno = (n))

typedef unsigned char U8;
typedef unsigned short U16;
typedef unsigned long U32;

typedef struct DOS16MCB {
  unsigned char mz;
  U16 psp;
  U16 para;
  unsigned char d5[3];
  unsigned char name[8];
} DOS16MCB;


unsigned dos16_segalloc(unsigned para)
{
  union REGS r;
  
  r.h.ah = 0x48;
  r.x.bx = para;
  intdos(&r, &r);
  if (r.x.cflag) {
    seterr(r.x.ax);
    return 0;
  }
  return r.x.ax;
}

void far *dos16_farmalloc(unsigned long size)
{
  unsigned short seg, para;
  
  para = (size > 0xffff0UL) ? 0xffffU : (unsigned short)((size + 15) >> 4);
  seg = dos16_segalloc(para);
  return MK_FP(seg, 0);
}

void far *dos16_farmalloc_lastfit(unsigned long size)
{
  void far *p;
  unsigned prev_state;
  
  prev_state = dos16_getallocstate();
  dos16_setallocstate(DOS16_LASTFIT);
  p = dos16_farmalloc(size);
  dos16_setallocstate(prev_state);
  return p;

}

int dos16_segfree(unsigned mem_seg)
{
  union REGS r;
  struct SREGS sr;
  DOS16MCB far *mcb;
  
  mcb = (DOS16MCB far *)MK_FP(mem_seg - 1, 0);
  if (mem_seg == 0 || (mcb->mz != 'M' && mcb->mz != 'Z') || mcb->psp <= 8)
    return -1;
  
  r.x.ax = 0x4900;
  sr.es = mem_seg;
  intdosx(&r, &r, &sr);
  if (r.x.cflag) {
    seterr(r.x.ax);
    return r.x.ax;
  }
  return 0;
}

int dos16_segresize(unsigned mem_seg, unsigned para)
{
  union REGS r;
  struct SREGS sr;
  
  r.h.ah = 0x4a;
  r.x.bx = para;
  sr.es = mem_seg;
  intdosx(&r, &r, &sr);
  if (r.x.cflag) {
    seterr(r.x.ax);
    return r.x.ax;
  }
  return 0;
}

void dos16_farfree(void far *p)
{
  unsigned long la;
  
  la = 16UL * FP_SEG(p) + FP_OFF(p);
  if ((la & 0xf) == 0) {
    dos16_segfree(la >> 4);
  }
}

unsigned dos16_segrealloc(unsigned mem_seg, unsigned para)
{
  DOS16MCB far *mcb;
  if (mem_seg == 0) return dos16_segalloc(para);
  mcb = (DOS16MCB far *)MK_FP(mem_seg - 1, 0);
  if ((mcb->mz != 'M' && mcb->mz != 'Z') || mcb->psp <= 8 || mcb->psp == 0xffffU)
    return 0;
  
  if (para == 0) {
#if 1
    return 0;               /* just fail (not free) */
#else
    dos16_segfree(mem_seg); /* (...or free) */
    return 0;
#endif
  }
  if (mcb->para < para) {
    /* expand block */
    unsigned para_av = mcb->para;
    while(1) {
      DOS16MCB far *mcb_trail = (DOS16MCB far *)MK_FP(mem_seg + para_av, 0);
      if (mcb_trail->mz == 'Z') break;
      if (mcb_trail->mz != 'M') {
        return 0; /* MCB corrupt */
      }
      if (mcb_trail->psp != 0) break;
      para_av += mcb_trail->para + 1;
    }
    if (para_av < para) /* do not resize if not enough free memory */
      return 0;
  }
  if (dos16_segresize(mem_seg, para) != 0)
    return 0;
  
  return mem_seg;
}


void far *dos16_farrealloc(void far *p, unsigned long size)
{
  unsigned short mem_seg, para;
  
  if (FP_OFF(p) & 0x0f) return 0L;
  mem_seg = (16UL * FP_SEG(p) + FP_OFF(p)) >> 4;
  para = (size > 0xffff0UL) ? 0xffffU : (unsigned short)((size + 15) >> 4);
  mem_seg = dos16_segresize(mem_seg, para);
  return MK_FP(mem_seg, 0);
}




static U16 dos16_maxsegalloc(void)
{
  union REGS r;
  U16 prev_strategy;
  U16 mem_seg;
  
  r.x.ax = 0x5800;
  intdos(&r, &r);
  if (r.x.cflag) return 0;
  prev_strategy = r.x.ax;
  
  r.x.ax = 0x5801;
  r.x.bx = 0x0001;  /* low memory best fit */
  intdos(&r, &r);
  
  r.h.ah = 0x48;
  r.x.bx = 0xffff;
  intdos(&r, &r); /* will return with error, but ... */
                   /* largest free block size to bx */
  r.h.ah = 0x48;
  intdos(&r, &r);
  mem_seg = r.x.cflag ? 0 : r.x.ax;
  
  r.x.ax = 0x5801;
  r.x.bx = prev_strategy;
  intdos(&r, &r);
  
  return mem_seg;
}

static unsigned dos16_getsetas(unsigned state, int setstate)
{
  unsigned prev_state, prev_umb;
  union REGS r;
  
  r.x.ax = 0x5800;
  intdos(&r, &r);
  if (r.x.cflag) return 0;
  prev_state = r.h.al;
  r.x.ax = 0x5802;
  intdos(&r, &r);
  prev_umb = (r.x.cflag || r.h.al > 1) ? 0 : r.h.al;
  
  if (setstate) {
    if ((state & 0xf0) == 0x40 || (state & 0xf0) == 0x80) state |= 0x0100;
    r.x.ax = 0x5803;
    r.x.bx = state >> 8;
    intdos(&r, &r);
    r.x.ax = 0x5801;
    r.x.bx = state & 0xff;
    intdos(&r, &r);
  }
  
  return (prev_umb << 8) | (prev_state & 0xff);
}
unsigned dos16_getallocstate(void)
{
  return dos16_getsetas(0, 0);
}

unsigned dos16_setallocstate(unsigned state)
{
  return dos16_getsetas(state, 1);
}

void far *dos16_memalign(unsigned long align, unsigned long size)
{
  DOS16MCB far *mcb;
  DOS16MCB far *mcb_fit;
  U16 mem_seg, fit_seg;
  U32 la_memtop, la_memlast;
  U32 la;
  
  if (align & 15U) return 0L;
  if (align == 0) align = 16UL;
  mem_seg = dos16_maxsegalloc();
  if (!mem_seg) return 0L;
  mcb = (DOS16MCB far *)MK_FP(mem_seg - 1, 0);
  if (mcb->mz != 'M' && mcb->mz != 'Z') return 0L;  /* MCB chain is corrupt... */
  
  la_memtop = 16UL * mem_seg;
  la_memlast = 16UL * (mem_seg + mcb->para);
  la = la_memlast - size;
  if (la % align)
    la = la - (la % align);
  else if (align != size) la -= align;
  
  fit_seg = 0;
  while(1) {
    if (la < la_memtop || la >= la_memlast) break;
    if (la + size < la_memlast) {
      fit_seg = la >> 4;
      break;
    }
    la -= align;
  }
  if (fit_seg) {
    U16 n;
    
    mcb_fit = (DOS16MCB far *)MK_FP(fit_seg - 1, 0);
    for(n=0; n<16; ++n) {
      *((unsigned char far *)mcb_fit + n) = *((unsigned char far *)mcb + n);
    }
    mcb_fit->para = mem_seg + mcb->para - fit_seg;
    n = mcb->para - mcb_fit->para - 1;
    if (n > 0 && n < mcb->para) {
      /* free lower memory block manually */
      disable();
      if (mcb->mz == 'Z') mcb->mz = 'M';
      mcb->para = n;
      mcb->psp = 0;
      enable();
    }
    else
      fit_seg = 0;
  }
  if (fit_seg == 0) {
    dos16_segfree(mem_seg);
  }
  else {
    dos16_segrealloc(fit_seg, (unsigned)((15UL+size) >> 4));
  }
  
  return MK_FP(fit_seg, 0);
}

void dos16_fmemset(void far *dst, int c, size_t len)
{
  register unsigned char far *d = dst;
  while(len--) {
    *d++ = (unsigned char)c;
  }
}

void dos16_fmemcpy(void far *dst, const void far *src, size_t len)
{
  register const unsigned char far *s = src;
  register unsigned char far *d = dst;
  while(len > 1) {
    *(unsigned far *)d = *(unsigned far *)s;
    d += sizeof(unsigned);
    s += sizeof(unsigned);
    len -= sizeof(unsigned);
  }
  while(len > 0) {
    *d++ = *s++;
    --len;
  }
}

void dos16_hmemcpy(void far *dst, const void far *src, unsigned long len)
{
  const unsigned copy_once_max = 0x8000U;
  unsigned long s_la, d_la;
  
  s_la = dos16_fp_to_linear(src);
  d_la = dos16_fp_to_linear(dst);
  
  /* assume s_la <= d_la */
  
  while(len > 0) {
    unsigned copy_once = (len>copy_once_max) ? copy_once_max : len;
    dos16_fmemcpy(dos16_linear_to_fp(d_la),
                  dos16_linear_to_fp(s_la),
                  copy_once);
    s_la += copy_once;
    s_la += copy_once;
    len -= copy_once;
  }
}


#if defined(TEST)

void dispmcb(unsigned mcb_seg)
{
  DOS16MCB far *mcb;
  union REGS r;
  struct SREGS sr;
  
  if (mcb_seg == 0) {
    r.x.ax = 0x5200;
    intdosx(&r, &r, &sr);
    mcb_seg = *(U16 far *)MK_FP(sr.es, r.x.bx - 2);
  }
  
  while(1) {
    mcb = (DOS16MCB far *)MK_FP(mcb_seg, 0);
    printf("mcb %04X '%c' mem %04X psp %04X para %04X (%6lubytes)\n", mcb_seg, mcb->mz, mcb_seg+1, mcb->psp, mcb->para, 16UL * mcb->para);
    if (mcb->mz != 'M') break;
    mcb_seg = (mcb_seg+1) + mcb->para;
  }
}

int main(int argc, char *argv[])
{
  void far *p;
  
  dispmcb(0);
  p = dos16_memalign(65536UL, 4096);
  
  printf("memalign %04X:%04X\n", FP_SEG(p), FP_OFF(p));
  dispmcb(0);
  
  return 0;
}
#endif
