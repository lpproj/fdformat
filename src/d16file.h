#ifndef DOS16_FILE_H
#define DOS16_FILE_H

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct DOS16_TM {
  unsigned short dos_date;
  unsigned short dos_time;
} DOS16_TM;

typedef struct CPFILEINFO {
  char *filename;
  long length;
  void far *mem;
  DOS16_TM tm;
} CPFILEINFO;


int dos16_rdopen(const char *pathname);
int dos16_creat(const char *pathname);
int dos16_close(int handle);
long dos16_lseek(int handle, long offset, int origin);
long dos16_read(int handle, void far *buf, long length);
long dos16_write(int handle, const void far *buf, long length);

int dos16_getftime(int handle, DOS16_TM *dtm);
int dos16_setftime(int handle, const DOS16_TM *dtm);

int dos16_resetdisk(void);
int dos16_fflush(int handle);

int dos16_tm_to_dostm(DOS16_TM *dostm, const struct tm *tm);
int dos16_dostm_to_tm(struct tm *tm, const DOS16_TM *dostm);


int cpfile_read(struct CPFILEINFO *cp);
int cpfile_write(const char *filename, const struct CPFILEINFO *cp);


#ifdef __cplusplus
}
#endif

#endif

