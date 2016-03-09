/*
  d16file: dos16 minimal file helper

  author: lpproj (https://github.com/lpproj)
  license: `Public Domain'
           (You can use/modify/redistribute it freely BUT NO WARRANTY.)
*/

#include <dos.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "d16fmem.h"
#include "d16file.h"


static int my_dos16_opsub(unsigned func, const char *pathname, unsigned attr)
{
  union REGS r;
  struct SREGS sr;
  
  r.x.ax = func;
  r.x.cx = attr;
  r.x.dx = FP_OFF(pathname);
  sr.ds = FP_SEG(pathname);
  intdosx(&r, &r, &sr);
  return r.x.cflag ? -1 : r.x.ax;
}

int dos16_rdopen(const char *pathname)
{
  return my_dos16_opsub(0x3d00, pathname, 0);
}

int dos16_creat(const char *pathname)
{
  return my_dos16_opsub(0x3c00, pathname, 0 /* normal file */);
}

int dos16_close(int handle)
{
  union REGS r;
  r.h.ah = 0x3e;
  r.x.bx = handle;
  intdos(&r, &r);
  return r.x.cflag ? r.x.ax : 0;
}

long dos16_lseek(int handle, long offset, int origin)
{
  union REGS r;
  r.h.ah = 0x42;
  r.h.al = origin;
  r.x.bx = handle;
  r.x.cx = ((unsigned long)offset >> 16);
  r.x.dx = 0xffffU & offset;
  intdos(&r, &r);
  
  return r.x.cflag ? -1L : (long)(((unsigned long)(r.x.dx) << 16) | r.x.ax);
}

int dos16_getftime(int handle, DOS16_TM *dtm)
{
  union REGS r;

  r.x.ax = 0x5700;
  r.x.bx = handle;
  intdos(&r, &r);
  if (r.x.cflag) return r.x.ax;
  if (dtm) {
    dtm->dos_date = r.x.dx;
    dtm->dos_time = r.x.cx;
  }
  return 0;
}

int dos16_setftime(int handle, const DOS16_TM *dtm)
{
  union REGS r;
  
  if (!dtm) return -1;
  r.h.ah = 0x68;            /* commit file at first */
  r.x.bx = handle;
  intdos(&r, &r);
  r.x.ax = 0x5701;          /* then, set filetime */
  r.x.bx = handle;
  r.x.cx = dtm->dos_time;
  r.x.dx = dtm->dos_date;
  intdos(&r, &r);
  
  return r.x.cflag ? r.x.ax : 0;
}

int dos16_resetdisk(void)
{
  union REGS r;
  r.x.ax = 0x0d00;
  intdos(&r, &r);
  
  return r.h.ah == 0xff ? -1 : 0;
}

int dos16_fflush(int handle)
{
  union REGS r;
  r.h.ah = 0x68;
  r.x.bx = handle;
  intdos(&r, &r);

  return r.x.cflag ? r.x.ax : 0;
}

static long my_dos16_rw(int do_write, int handle, void far *buf, long length)
{
  const int count_once_max = 16 * 1024; /* less than INT_MAX... */
  union REGS r;
  struct SREGS sr;
  unsigned long la_buf = dos16_fp_to_linear(buf);
  long count = 0;

  if (length < 0) return -1;
  while(length > 0) {
    int count_once = length > count_once_max ? count_once_max : length;
    r.h.ah = do_write ? 0x40 : 0x3f;
    r.x.bx = handle;
    r.x.dx = la_buf & 0x0f;
    sr.ds = la_buf >> 4;
    r.x.cx = count_once;
    intdosx(&r, &r, &sr);
    if (r.x.cflag) {
      count = -1;
      break;
    }
    if (r.x.ax == 0 && !do_write) break;  /* EOF (read) */
    count += r.x.ax;
    la_buf += r.x.ax;
    length -= r.x.ax;
    if (do_write && count_once > r.x.ax) break; /* Disk Full (write) */
  }
  
  return count;
}

long dos16_read(int handle, void far *buf, long length)
{
  return my_dos16_rw(0, handle, buf, length);
}

long dos16_write(int handle, const void far *buf, long length)
{
  return my_dos16_rw(1, handle, (void far *)buf, length);
}

int cpfile_read(struct CPFILEINFO *cp)
{
  int rc = -1;
  int handle;
  
  handle = dos16_rdopen(cp->filename);
  if (handle == -1) return -1;
  cp->length = dos16_lseek(handle, 0L, 2);
  dos16_lseek(handle, 0L, 0);
  if (cp->length >= 0) {
    void far *mem = dos16_farmalloc_lastfit(cp->length);
    if (mem) {
      long n_read = dos16_read(handle, mem, cp->length);
      if (n_read == cp->length) {
        cp->mem = mem;
        dos16_getftime(handle, &(cp->tm));
        rc = 0;
      }
      else
        dos16_farfree(mem);
    }
  }
  dos16_close(handle);
  
  return rc;
}

int cpfile_write(const char *filename, const struct CPFILEINFO *cp)
{
  int rc = -1;
  int handle;
  
  if (!filename || !cp || (!cp->mem && cp->length)) return -1;
  
  handle = dos16_creat(filename);
  if (handle == -1) return -1;
  if (cp->length == 0)
    rc = 0;
  else
    rc = (dos16_write(handle, cp->mem, cp->length) == cp->length) ? 0 : -1;
  if (rc == 0) {
    dos16_fflush(handle);
    rc = dos16_setftime(handle, &(cp->tm)) ? -1 : 0;
  }
  dos16_close(handle);
  
  return rc;
}

