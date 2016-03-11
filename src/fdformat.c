/*
  fdformat: simple floppy disk formatter for FreeDOS(98).
  
  Copyright (C) 2015-2016 sava (t.ebisawa)

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


  (in short term: `under the ZLIB license') 

*/


#include <limits.h>
#include <ctype.h>
#include <dos.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(__TURBOC__)
#include <alloc.h>
#endif

#include "d16fmem.h"
#include "d16file.h"

#if defined(LSI_C)
# define REGS2 REGS
# define int86x2 int86x
#else
# include "int86x2.h"
#endif

#if defined(_MSC_VER) || defined(__WATCOMC__)
#pragma pack(1)
#endif


static unsigned char bootsector_dummy[] = {
#include "bootdumy.h"
};
static unsigned char bootsector_fd98fd[] = {
#include "b_fat12f.h"
};



typedef struct BPBCORE {
  unsigned short bytes_per_sector;      /* +00  +11(0B) bytes per (logical) sector */
  unsigned char sectors_per_cluster;    /* +02  +13(0D) sectors per cluster */
  unsigned short reserved_sectors;      /* +03  +14(0E) reserved sectors */
  unsigned char fats;                   /* +05  +16(10) number of fats */
  unsigned short root_entries;          /* +06  +17(11) number of dirent in rootdir */
  unsigned short sectors;               /* +08  +19(13) number of sectors (if <64K) */
  unsigned char media_descriptor;       /* +10  +21(15) media descriptor */
  unsigned short sectors_per_fat;       /* +11  +22(16) sectors per a fat */
  unsigned short sectors_per_head;      /* +13  +24(18) sectors per a head */
  unsigned short heads_per_track;       /* +15  +26(1A) heads per a track */
} BPBCORE; /* 17 bytes */

typedef struct BOOTSECTOR {
  unsigned char jmp_code[3];            /* +00(00) jump code */
  unsigned char oem_id[8];              /* +03(03) oem id (ASCII) */
  BPBCORE bpb;
  unsigned long hidden_sectors;         /* +28(1C) hidden (physical) sectors */
  unsigned long sectors32;              /* +32(20) number of sectors */
  unsigned char boot_drive;             /* +36(24) boot drive unit (for BIOS) */
  unsigned char boot_head;              /* +37(25) boot drive head (for IBM PC BIOS) */
  unsigned char boot_signature;         /* +38(26) extended boot record signature (0x29) */
  unsigned long volume_serial;          /* +39(27) volume serial number */
  unsigned char label[11];              /* +43(2B) volume label */
  unsigned char fs_info[8];             /* +54(36) file system information */
#if 1
  unsigned long sysPartStart;           /* +62(3E) nec98: first (physical) sector of the partition (same as bsHidden on DOS 5+) */
  unsigned short sysDataOffset;         /* +66(42) nec98: offset of IO.SYS sector (first data sector) */
  unsigned short sysPhysicalBPS;        /* +68(44) nec98: bytes/sector (Physical) */
  char unknown[1];             /* nec98: zero? */
  unsigned long sysRootDirStart;        /* +70(46) fd98: first root directory sector (logical sector, started from the partition) */
  unsigned short sysRootDirSecs;        /* +74(4A) fd98: count of logical sectors root dir uses */
  unsigned long sysFatStart;            /* +76(4C) fd98: first FAT sector (logical sector, started from the partition) */
  unsigned long sysDataStart;           /* +80(50) fd98: first data sector (logical sector, started from the partition) */
  unsigned char data[512 - 2 - 84];     /* +84(54) ... (code/data area for boot loader) */
#else
  unsigned char data[512 - 2 - 62];     /* +62(3E) ... (code/data area for boot loader) */
#endif
  unsigned short signature_aa55;        /* +510(1FE) signature 0x55, 0xAA */
} BOOTSECTOR; /* 512 bytes */



typedef struct FD_FORMAT_TRACK_TABLE {
  unsigned char cylinder;
  unsigned char head;
  unsigned char sector;
  unsigned char sector_scale;
} FD_FORMAT_TRACK_TABLE;



typedef struct FD_FORMAT_INFO {
  char *short_info;
  int size_option;
  BPBCORE *bpb;
} FD_FORMAT_INFO;

union REGS2 r;
struct SREGS sr;

void far *dskbuf;


enum {
  OPTION_DEFAULT = 0,
  OPTION_FD_640,        /* /6 */
  OPTION_FD_720,        /* /9 */
  OPTION_FD_2HC,         /* /5 */
  OPTION_FD_2HD,         /* /M */
  OPTION_FD_1440,        /* /4 */
};

enum {
  DRIVE_UNKNOWN = 0,
  DRIVE_2HD = 1,
  DRIVE_2HD_2MODE,
  DRIVE_2HD_3MODE,
  DRIVE_2DD = 0x11,
  DRIVE_2DD_2MODE,
  DRIVE_2D = 0x21
};

#define DAUA_NO_DRIVE 0xff


BPBCORE bpb_640 = { 512, 2, 1, 2, 0x70, 8*2*80, 0xfb, 2, 8, 2 };
BPBCORE bpb_720 = { 512, 2, 1, 2, 0x70, 9*2*80, 0xf9, 3, 9, 2 };
BPBCORE bpb_2hc = { 512, 1, 1, 2, 0xe0, 15*2*80, 0xf9, 7, 15, 2 };
BPBCORE bpb_2hd = { 1024, 1, 1, 2, 0xc0, 8*2*77, 0xfe, 2, 8, 2 };
BPBCORE bpb_1440 = { 512, 1, 1, 2, 0xe0, 18*2*80, 0xf0, 9, 18, 2 };

FD_FORMAT_INFO fd_format_info[] = {
  { "2DD(640K)", OPTION_FD_640, &bpb_640 },
  { "2DD(720K)", OPTION_FD_720, &bpb_720 },
  { "2HC(1.2M)", OPTION_FD_2HC, &bpb_2hc },
  { "2HD(1.23M)", OPTION_FD_2HD, &bpb_2hd },
  { "1.44M", OPTION_FD_1440, &bpb_1440 },
  { NULL, 0, { 0 } }
};



unsigned char mygetc(int do_flush)
{
  r.x.ax = do_flush ? 0x0c08 : 0x0800;
  int86x2(0x21, &r, &r ,&sr);
  return r.h.al;
}

const unsigned char *fd_bios_errmsg(int err)
{
  const unsigned char *m = "その他のエラー";
  
  if (err == 0) return "";
  switch((err >> 8) & 0xf0) {
    case 0x10: m = "DDAM検出"; break;
    case 0x20: m = "DMA境界エラー"; break;
    case 0x30: m = "セクタ位置範囲指定エラー"; break;
    case 0x40: m = "装置指定が無効"; break;
    case 0x50: m = "ディスクオーバーラン"; break;
    case 0x60: m = "ディスクがありません"; break;
    case 0x70: m = "書き込み保護"; break;
    case 0x90: m = "装置が未接続"; break;
    case 0xa0: case 0xb0: m = "CRCエラー"; break;
    case 0xc0: m = "セクタが見つからない"; break;
    case 0xd0: m = "バッドシリンダ"; break;
    case 0xe0: case 0xf0: m = "アドレスマークが見つからない"; break;
  }
  
  return m;
}

unsigned char drive0_to_daua(unsigned char drive0)
{
  unsigned char daua = 0;
  if (drive0 <= 15)
    daua = *(unsigned char far *)MK_FP(0x60, 0x6c + drive0);
  else if (drive0 <= 'Z' - 'A')
    daua = *(unsigned char far *)MK_FP(0x60, 0x2c86U + drive0 * 2 + 1);    
  return daua ? daua : DAUA_NO_DRIVE;
}

int is_daua_fd(unsigned char daua)
{
  return (daua & 0x1c) == 0x10 && (daua & 0xf0) != 0xd0;
}

unsigned char get_drive_type(unsigned char daua)
{
  unsigned char drv = DRIVE_UNKNOWN;
  unsigned short equip = *(unsigned short far *)MK_FP(0, 0x55c);
  unsigned char da, ua;
  
  da = daua & 0xf0;
  ua = daua & 0x0f;
  if (ua > 3) return drv;
  if ((da == 0x10 || da == 0x30 || da == 0x90) && (equip & (1<<ua))) {
    drv = DRIVE_2HD;
    r.h.ah = 0x84;
    r.h.al = 0x10 | ua;
    int86x2(0x1b, &r, &r, &sr);
    if ((r.h.ah & 0xf0) != 0x40 && (r.h.ah & 0x08))
      drv = DRIVE_2HD_2MODE;
    r.h.ah = 0xc4;
    r.h.al = 0x30 | ua;
    int86x2(0x1b, &r, &r, &sr);
    if ((r.h.ah & 0xf0) != 0x40 && (r.h.ah & 0x04))
      drv = DRIVE_2HD_3MODE;
  }
  else if ((da == 0x70 || da == 0xf0) && (equip & (0x1000U<<ua))) {
     drv = DRIVE_2DD;
    r.h.ah = 0x84;
    r.h.al = 0x70 | ua;
    int86x2(0x1b, &r, &r, &sr);
    if ((r.h.ah & 0xf0) != 0x40 && (r.h.ah & 0x08))
      drv = DRIVE_2DD_2MODE;
  }
  else if (da == 0x50 && (equip & (0x10U<<ua))) {
    drv = DRIVE_2D;
  }
  
  return drv;
}

unsigned char proper_fd_daua(unsigned char daua, unsigned char option)
{
  unsigned char daua_rc = DAUA_NO_DRIVE;
  unsigned char ua = daua & 0x0f;
  
  switch(get_drive_type(daua)) {
    case DRIVE_2HD_3MODE:
      if (option == OPTION_FD_1440) {
        daua_rc = 0x30 | ua;
        break;
      } /* fallthrough */
    case DRIVE_2HD_2MODE:
      if (option == OPTION_FD_640 || option == OPTION_FD_720) {
        daua_rc = 0x10 | ua;
        break;
      } /* fallthrough */
    case DRIVE_2HD:
      if (option == OPTION_FD_2HC || option == OPTION_FD_2HD)
        daua_rc = 0x90 | ua;
      break;
    
    case DRIVE_2DD_2MODE:
      if (option == OPTION_FD_2HC || option == OPTION_FD_2HD) {
        daua_rc = 0xf0 | ua;
        break;
      } /* fallthrough */
    case DRIVE_2DD:
      if (option == OPTION_FD_640 || option == OPTION_FD_720)
        daua_rc = 0x70 | ua;
    
    default:
      break;
  }
  
  return daua_rc;
}

int choose_fd_type(unsigned char daua)
{
  int choosenum = 0;
  unsigned char fdtype[6];
  char *sfdtype[6];
  unsigned char drv = get_drive_type(daua);
  unsigned cnt = 0;
  unsigned c;
  
  if (drv == DRIVE_2DD || drv == DRIVE_2DD_2MODE || drv == DRIVE_2HD_2MODE || drv == DRIVE_2HD_3MODE) {
    fdtype[cnt] = OPTION_FD_640;
    sfdtype[cnt] = "2DD(640K)";
    ++cnt;
    fdtype[cnt] = OPTION_FD_720;
    sfdtype[cnt] = "2DD(720K)";
    ++cnt;
  }
  if (drv == DRIVE_2DD_2MODE || drv == DRIVE_2HD || drv == DRIVE_2HD_2MODE || drv == DRIVE_2HD_3MODE) {
    fdtype[cnt] = OPTION_FD_2HC;
    sfdtype[cnt] = "2HC(1.2M)";
    ++cnt;
    fdtype[cnt] = OPTION_FD_2HD;
    sfdtype[cnt] = "2HD(1.23M)";
    ++cnt;
  }
  if (drv == DRIVE_2HD_3MODE) {
    fdtype[cnt] = OPTION_FD_1440;
    sfdtype[cnt] = "1.44M";
    ++cnt;
  }
  
  if (cnt > 0) {
    unsigned n;
    for(n = 0; n < cnt; ++n) {
      fprintf(stderr, "%u:%s ", n+1, sfdtype[n]);
    }
  }
  
  fflush(stdout);
  c = ' ';
  fprintf(stderr, " =  ");
  do {
    c = mygetc(1);
    if (c >= '1' && c <= '0' + cnt) {
      fprintf(stderr, "\x8" "%c", c);
      choosenum = c - '0';
    }
    if (c == 0x1b) {
      choosenum = 0;
      break;
    }
  } while(c != 13);
  fprintf(stderr, "\n");
  
  return choosenum > 0 ? fdtype[choosenum - 1] : 0;
}

const FD_FORMAT_INFO * lookup_format_info(int size_option)
{
  const FD_FORMAT_INFO *fi = fd_format_info;
  
  for(;;) {
    if (!fi->short_info) return NULL;
    if (fi->size_option == size_option) break;
    ++fi;
  }
  
  return fi;
}


int fd_recalibrate(unsigned char daua)
{
  r.h.ah = 7;
  r.h.al = daua;
  int86x2(0x1b, &r, &r, &sr);
  
  return r.x.cflag ? r.x.ax : 0;
}

static unsigned char calc_sector_scale(unsigned bytes)
{
  switch(bytes) {
    case 128:  return 0;
    case 256:  return 1;
    case 512:  return 2;
    case 1024: return 3;
    case 2048: return 4;
    case 4096: return 5;
    case 8192: return 6;
  }
  return 0xff;
}


static int fd_rw_lba(int do_write, unsigned char daua, const BPBCORE *bpb, unsigned long lba, void far *buf)
{
  int rc = 0;
  unsigned c, h, s;
  if (!is_daua_fd(daua)) return -1;
  s = (lba % bpb->sectors_per_head) + 1;
  h = (lba / bpb->sectors_per_head) % bpb->heads_per_track;
  c = lba / (unsigned long)(bpb->sectors_per_head * bpb->heads_per_track);
  
  r.h.ah = (do_write ? 0x05 : 0x06) | 0x40 | 0x10;
  r.h.al = daua;
  r.x.bx = bpb->bytes_per_sector;
  r.h.ch = calc_sector_scale(r.x.bx);
  r.h.cl = c;
  r.h.dh = h;
  r.h.dl = s;
  r.x.bp = FP_OFF(buf);
  sr.es = FP_SEG(buf);
  int86x2(0x1b, &r, &r, &sr);
  if (r.x.cflag) {
    rc = r.x.ax;
    fd_recalibrate(daua);
  }
  
  return rc;
}

int fd_write_lba(unsigned char daua, const BPBCORE *bpb, unsigned long lba, const void far *buf)
{
  return fd_rw_lba(1, daua, bpb, lba, (void far *)buf);
}
int fd_read_lba(unsigned char daua, const BPBCORE *bpb, unsigned long lba, void far *buf)
{
  return fd_rw_lba(0, daua, bpb, lba, buf);
}


int fd_flush_unit(unsigned char daua)
{
  unsigned char ua = daua & 0x0f;
  
  if (ua <= 3) {
    if ((daua & 0x50) == 0x10) {      /* 1MB IF (640K, 1M and 1.44M modes) */
      *(unsigned char far *)MK_FP(0, 0x564 + 8 * ua) |= 0xc0;
      return 0;
    }
    else if ((daua & 0x70) == 0x70) { /* 640K IF (640K and 1M modes) */
      *(unsigned char far *)MK_FP(0, 0x5d0) |= 0xc0;
      *(unsigned char far *)MK_FP(0, 0x5d8 + 2 * ua) |= 0xc0;
      return 0;
    }
  }
  return -1;
}


unsigned long
tm_to_volume_serial(const struct tm *tm)
{
    /*
      reference: disktut.txt 1993-03-26 (line 1919-)
      serial: xxyy-zzzz
      xx: month + second
      yy: day
      zzzz: year + hour * 0x100 + minute
      
    */
    unsigned xx, yy, zzzz;
    xx = (tm->tm_mon + 1) + tm->tm_sec;
    yy = tm->tm_mday;
    zzzz = tm->tm_year >= 1900 ? tm->tm_year : tm->tm_year + 1900;
    zzzz = zzzz + (tm->tm_hour * 256) + tm->tm_min;
    
    return ((unsigned long)zzzz << 16) | (xx << 8) | yy;
}

unsigned long
current_time_to_volume_serial(void)
{
    time_t t;
    struct tm *tm;
    t = time(NULL);
    tm = localtime(&t);
    return tm_to_volume_serial(tm);
}


static int build_bootsect(void far *buf, const BPBCORE *bpb, const void *bootsect)
{
  BOOTSECTOR far *bs = buf;
  dos16_fmemcpy(bs, bootsect, 512);
  dos16_fmemcpy(&(bs->bpb), bpb, sizeof(*bpb));
  if (bs->boot_signature == 0x29) {
    bs->volume_serial = current_time_to_volume_serial();
    /*
    dos16_fmemcpy(bs->label, "NO NAME    ", 11);
    dos16_fmemcpy(bs->fs_info, "FAT12   ", 8);
    */
  }
  bs->signature_aa55 = 0xaa55;
  return 0;
}

static int retouch_fd98_bootsect(void far *buf, const BPBCORE *bpb)
{
  BOOTSECTOR far *bs = buf;
  
  bs->boot_signature = 0x29;
  dos16_fmemcpy(bs->label, "NO NAME    ", 11);
  dos16_fmemcpy(bs->fs_info, "FAT12   ", 8);
  bs->volume_serial = current_time_to_volume_serial();
  
  bs->sysPhysicalBPS = bpb->bytes_per_sector;
  bs->sysFatStart = bpb->reserved_sectors;
  bs->sysRootDirStart = bs->sysFatStart + bpb->sectors_per_fat * bpb->fats;
  bs->sysRootDirSecs = bpb->root_entries / (bpb->bytes_per_sector / 32);
  bs->sysDataStart = bs->sysRootDirStart + bs->sysRootDirSecs;
  
  return 0;
}


int fd_build_fat12(unsigned char daua, const BPBCORE *bpb, int opt_s)
{
  int rc;
  unsigned long lba_fat, lba_root;
  unsigned dirent_sectors;
  unsigned i, j;
  
  lba_fat = bpb->reserved_sectors;
  lba_root = lba_fat + (bpb->sectors_per_fat * bpb->fats);
  dirent_sectors = bpb->root_entries / (bpb->bytes_per_sector / 32); 

  if (opt_s) {
    build_bootsect(dskbuf, bpb, bootsector_fd98fd);
    retouch_fd98_bootsect(dskbuf, bpb);
  } else {
    build_bootsect(dskbuf, bpb, bootsector_dummy);
  }
  
  rc = fd_write_lba(daua, bpb, 0L, dskbuf);
  if (rc != 0) return rc;
  
  for(i = 0; i < bpb->fats; ++i) {
    dos16_fmemset(dskbuf, 0, bpb->bytes_per_sector);
    for(j = 0; j < bpb->sectors_per_fat; ++j) {
      if (j == 0) {
        ((unsigned char far *)dskbuf)[0] = bpb->media_descriptor;
        ((unsigned char far *)dskbuf)[1] = 0xff;
        ((unsigned char far *)dskbuf)[2] = 0xff;
      }
      rc = fd_write_lba(daua, bpb, lba_fat, dskbuf);
      if (rc != 0) return rc;
      dos16_fmemset(dskbuf, 0, 4);
      ++lba_fat;
    }
  }
  for(i = 0; i < dirent_sectors; ++i) { /* dirent: just clear zero */
    rc = fd_write_lba(daua, bpb, lba_root++, dskbuf);
  }
  
  return rc;
}

static int build_track_table(FD_FORMAT_TRACK_TABLE far *table, unsigned sectors, unsigned interleave)
{
  unsigned i;
  unsigned char c, h, scale;
  unsigned s;
  unsigned char m[256];
  
  if (interleave == 0) interleave = 1;
  
  c = table[0].cylinder;
  h = table[0].head;
  s = table[0].sector;
  if (s != 0) --s;
  scale = table[0].sector_scale;
  if (scale == 0xff) return -1;
  memset(m, 0, sizeof(m));
  for(i=0;;) {
    table[i].cylinder = c;
    table[i].head = h;
    table[i].sector = s + 1;
    table[i].sector_scale = scale;
    m[s] = 1;
    if (++i >= sectors) break;
    /* NEC PC-9800 series technical reference compatible */
    s += interleave;
    if (s >= sectors || m[s]) {
      for(s = 0; m[s]; ++s) ;
    }
  }
  
  return 0;
}


int format_fd_unit_bpb(unsigned char daua, const BPBCORE *bpb, int do_verify, unsigned interleave)
{
  int rc = 0;
  FD_FORMAT_TRACK_TABLE far *table = (FD_FORMAT_TRACK_TABLE far *)dskbuf;
  unsigned cylinders, sectors, heads;
  unsigned c, h;
  unsigned char scale;

  scale = calc_sector_scale(bpb->bytes_per_sector);
  sectors = bpb->sectors_per_head;
  heads = bpb->heads_per_track;
  cylinders = bpb->sectors / (sectors * heads);
  /* workaround for 640K 2DD (format as 720K in physical) */
  if (bpb->bytes_per_sector == 512 && sectors == 8) sectors = 9;
  
  fd_recalibrate(daua);
  for(c = 0; c < cylinders; ++c) {
    for(h = 0; h < heads; ++h) {
      printf("\rcylinder%3u  head%2u", c, h);
      /* build format table for BIOS */
      table[0].cylinder = c;
      table[0].head = h;
      table[0].sector = 1;
      table[0].sector_scale = scale;
      build_track_table(table, sectors, interleave);
      /* format a cylinder */
      r.h.ah = 0x0d | 0x40 | 0x10;
      r.h.al = (unsigned char)daua;
      r.x.bx = sectors * sizeof(FD_FORMAT_TRACK_TABLE);
      r.h.ch = scale;
      r.h.cl = c;
      r.h.dh = h;
      r.h.dl = 0xe5;
      r.x.bp = FP_OFF(table);
      sr.es = FP_SEG(table);
      int86x2(0x1b, &r, &r, &sr);
      if (r.x.cflag) {
        fprintf(stderr, " ... error! (%s)\n", fd_bios_errmsg(r.x.ax));
        return r.x.ax;
      }
      if (do_verify) {
        /* verify current cylinder */
        r.h.ah = 0x01 | 0x40;
        r.h.al = (unsigned char)daua;
        r.h.ch = scale;
        r.h.cl = c;
        r.h.dh = h;
        r.h.dl = 1;
        r.x.bx = bpb->bytes_per_sector * bpb->sectors_per_head;
        r.x.bp = FP_OFF(table);
        sr.es = FP_SEG(table);
        int86x2(0x1b, &r, &r, &sr);
        if (r.x.cflag) {
          fprintf(stderr, " ... verify error! (%s)\n", fd_bios_errmsg(r.x.ax));
          return r.x.ax;
        }
      }
    }
  }
  printf("\n");

  return rc;
}


int format_fd_unit(unsigned char daua, int size_option, int need_system_transfer, unsigned interleave)
{
  const FD_FORMAT_INFO *fi;
  int rc = 0;
  int do_verify = 1;
  
  fi = lookup_format_info(size_option);
  if (fi) daua = proper_fd_daua(daua, fi->size_option);
  if (!fi || daua == DAUA_NO_DRIVE || calc_sector_scale(fi->bpb->bytes_per_sector) == 0xff) {
    fprintf(stderr, "その容量は未対応です\n");
    return -1;
  }
  printf("%s フォーマット中...\n", fi->short_info);
  rc = format_fd_unit_bpb(daua, fi->bpb, do_verify, interleave);
  if (rc == 0) {
    fd_recalibrate(daua);
    rc = fd_build_fat12(daua, fi->bpb, need_system_transfer);
    if (rc != 0) {
      fprintf(stderr, "ファイルシステム作成エラー (%s)\n", fd_bios_errmsg(rc));
    }
  }
  
  if (rc == 0) {
    fd_recalibrate(daua);
    fd_flush_unit(daua);
  } else {
    fprintf(stderr, "フォーマットは失敗しました\n");
  }
  return rc;
}






unsigned char get_bootdrive_num0(void)
{
  r.x.ax = 0x3305;
  r.x.dx = 0;
  int86x2(0x21, &r, &r, &sr); /* DOS4+: get boot drive DL = bootdrive_num1 */
  if (r.h.dl > 0) {
    r.h.al = r.h.dl - 1;
  } else {
    r.x.ax = 0x1900;
    int86x2(0x21, &r, &r, &sr); /* get current drive AL = drive_num0 */
  }
  
  return r.h.al;
}


char *drivestr;
char drivenum0;

int opt4;
int opt5;
int opt6;
int opt9;
int optM;

int optHelp;
int optB;
int optV;
char *optVstr;
int optQ;
int optS;
int optU;
int optFDSize = OPTION_DEFAULT;
int optF;

int optP;
int optI;


static int cmpoptf(const char *s1, const char *s2)
{
  size_t n, n1, n2;
  int c;
  
  n1 = strlen(s1);
  n2 = strlen(s2);
  if (s1 <= s2) return stricmp(s1, s2);
  for(n=0; n<n2; ++n) {
    c = toupper(s1[n]) - toupper(s2[n]);
    if (c != 0) break;
  }
  
  return c;
}
# define eqf(s1,s2) (cmpoptf(s1,s2)==0)

int mygetopt(int argc, char *argv[])
{
  char c, *s;
  
  while(argc > 0) {
    s = *argv;
    c = *s;
    if (c == '-' || c == '/') {
      switch(toupper(*++s)) {
        case '?': optHelp = 1; break;
        case 'B': optB = 1; break;
        case 'M': optM = 1; break;
        case 'Q': optQ = 1; break;
        case 'S': optS = 1; break;
        case 'U': optU = 1; break;
        case '4': opt4 = 1; break;
        case '5': opt5 = 1; break;
        case '6': opt6 = 1; break;
        case '9': opt9 = 1; break;
        case 'P': optP = 1; break;
        case 'I':
          ++s;
          if (*s == ':' || *s == '=') ++s;
          optI = atoi(s);
          break;
        case 'V':
          optV = 1;
          if (s[1] == ':') optVstr = s + 2;
          break;
        case 'F':
          if (s[1] == ':') {
            s += 2;
            if (eqf(s, "640")) opt6 = 1;
            else if (eqf(s, "720") || eqf(s, "2DD")) opt9 = 1;
            else if (eqf(s, "2HD") || eqf(s, "1.23") || eqf(s, "1.25") || eqf(s, "123") || eqf(s, "125")) optM = 1;
            else if (eqf(s, "1.44") || eqf(s, "144")) opt4 = 1;
            else if (eqf(s, "2HC") || strcmp(s, "1.2")==0 || eqf(s, "120")) opt5 = 1;
          }
          break;
      }
    }
    else if (((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) && s[1] == ':') {
      drivestr = s;
      drivenum0 = toupper(c) - 'A';
    }
    --argc;
    ++argv;
  }
  
  if (opt6) optFDSize = OPTION_FD_640;
  if (opt9) optFDSize = OPTION_FD_720;
  if (opt5) optFDSize = OPTION_FD_2HC;
  if (optM) optFDSize = OPTION_FD_2HD;
  if (opt4) optFDSize = OPTION_FD_1440;
  
  return 0;
}

void put_logo(void)
{
  const char msg[] = \
    "Floppy Disk formatter for FreeDOS(98)"
    ", built at " __DATE__ " " __TIME__
    ".";
  printf("%s\n", msg);
}

void put_usage(void)
{
  const char *progname = "FDFORMAT";
  const char msg[] = 
    "フロッピーディスクを初期化します\n"
    "\n"
    "%s d: [/S] [/M|/5|/6|/9|/4|/F:size]\n"
    "\n"
    "  d:       フロッピーディスクのドライブ名(A:〜Z:)\n"
    "\n"
    "  /S       初期化後にFreeDOS(98)のシステムを転送します\n"
    "           （起動中のシステムがFreeDOS(98)である必要があります）\n"
    "\n"
    "  /M       1Mバイト2HDディスクを初期化します\n"
    "  /5       1Mバイト2HCディスク（1セクタ512バイト）を初期化します\n"
    "  /6       640Kバイト2DDディスクを初期化します\n"
    "  /9       720Kバイト2DDディスクを初期化します\n"
    "  /4       1.44Mディスクを初期化します\n"
#if 1
    "  /F:size  初期化ディスク容量を数値などで指定します\n"
    "           (size=640, 720, 2DD, 1.2, 2HC, 1.23, 2HD, 1.44)\n"
#else
    "  /F:640   \n"
    "  /F:720   \n"
    "  /F:2DD   \n"
    "  /F:1.2   \n"
    "  /F:2HC   \n"
    "  /F:1.23  \n"
    "  /F:1.25  \n"
    "  /F:2HD   \n"
    "  /F:1.44  \n"
#endif
    ;
  
  printf(msg, progname);
}

int main(int argc, char *argv[])
{
  int rc = 0;
  CPFILEINFO cfi_kernel, cfi_command;
  char cpy_kernel[] = "@:\\KERNEL.SYS";
  char cpy_command[] = "@:\\COMMAND.COM";
  unsigned char daua;

  setvbuf(stdout, NULL, _IONBF, 0);
  put_logo();

  mygetopt(argc-1, argv+1);
  if (optHelp || !drivestr) {
    put_usage();
    return optHelp ? 0 : -1;
  }
  if (optQ) {
    fprintf(stderr, "クイックフォーマットは現在未対応です\n");
    return 1;
  }
  
  dskbuf = dos16_memalign(65536U, 512 * 18);  /* disk buffer for bios */
  if (!dskbuf) {
    fprintf(stderr, "memory not enough\n");
    exit(-1);
  }
  memset(&cfi_kernel, 0, sizeof(cfi_kernel));
  memset(&cfi_command, 0, sizeof(cfi_command));
  cfi_kernel.filename = cpy_kernel;
  cfi_command.filename = cpy_command;
  
  daua = drive0_to_daua(drivenum0);
  if (!drivestr || daua == DAUA_NO_DRIVE) {
    fprintf(stderr, "ドライブの指定が不正です\n");
    return 1;
  }
  
  if (optS) {
    unsigned char bootdrv = get_bootdrive_num0();
    cfi_kernel.filename[0] = 'A' + bootdrv;
    cfi_command.filename[0] = 'A' + bootdrv;
    if (cpfile_read(&cfi_kernel) != 0) {
      fprintf(stderr, "%s の読み込みエラー、もしくはメモリ不足です\n", cfi_kernel.filename);
      return 1;
    }
    if (cpfile_read(&cfi_command) != 0) {
      fprintf(stderr, "%s の読み込みエラー、もしくはメモリ不足です\n", cfi_command.filename);
      return 1;
    }
  }
  
  
  if (!is_daua_fd(daua)) {
    fprintf(stderr, "フロッピーディスク以外の装置が指定されています\n");
    return 1;
  }
  if (!optP) printf("ドライブ %c: にディスクを挿入し、何かキーを押してください ", drivenum0 + 'A');
  mygetc(1);
  printf("\n");
  if (!optFDSize) {
    printf("ディスクタイプを選んでください\n");
    optFDSize = choose_fd_type(daua);
  }
  if (!optFDSize) return 0;
  
  daua = proper_fd_daua(daua, optFDSize);
  rc = format_fd_unit(daua, optFDSize, optS, optI);
  if (rc == 0) {
    dos16_resetdisk();
    if (optS) {
      char s[65];
      printf("システム転送中...");
      strcpy(s, cpy_kernel);
      s[0] = 'A' + drivenum0;
      rc = cpfile_write(s, &cfi_kernel);
      if (rc == 0) {
        strcpy(s, cpy_command);
        s[0] = 'A' + drivenum0;
        rc = cpfile_write(s, &cfi_command);
      }
      if (rc == 0) {
        printf("終了\n");
      } else {
        fprintf(stderr, " %s の書き込みエラー\n", s);
      }
      dos16_resetdisk();
    }
  }

  return rc;
}

