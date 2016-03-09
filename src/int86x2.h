/*
	int86x2.h
	
[License]
	Public Domain.
	Feel free to use...AT YOUR OWN RISK.


[Description and Revision]

	（LSIC 以外の）int86x だと bp レジスタの受け渡しが出来ないので 
	それっぽいのを作ってみた。ついでに far call もやってみた。 

	試験環境
	TURBO C/C++ 1.01
	OpenWatcom 1.2
	DigitalMars C++ 8.40 (with 8.33DOS)
	LSI-C 3.30c (trial)

	2004-05-22	sava
		initial
	2004-05-24	sava
		非常に姑息な方法で LSI-C 試食版に対応
		（LSI-C の int86x は bp レジスタも扱えるので、int86x2 を使う必要は
		あまりない。動作はおそらく int86x2 のほうが遅い）
	2002-05-26	sava
		_readflags() 追加


[Reference]

int86x2

宣言
	int int86x2(int vector, union REGS2 *ri, union REGS2 *ro, struct SREGS *sr)

引数
	vector
		呼び出す割り込みベクタ番号。
		0 から 0xff まで。上位バイトは無視される。
	ri
		割り込みサービス側に渡すレジスタ内容。
	ro
		割り込みサービスから返されるレジスタ内容。
		以前の値を保存しなくてもいい場合、ri と ro は同じポインタでも
		かまわない。
	sr
		割り込みサービスに対して受け渡しを行なうセグメントレジスタの内容。
		割り込みサービスが ds, es を変更した場合、sr の内容も変更される。
	
	ri, ro, sr に無効なポインタを渡すことはできない。
	

戻り値
		割り込みサービスから復帰したときの ax レジスタの値。
		また、ro 内の全内容、sr 内の ds, es エントリ値が変更される
		可能性がある。
		ro->x.cflag はキャリーフラグの状態を表す。割り込みサービスからの
		復帰時にキャリーフラグがセットされていた場合は 0 以外の値になる。
		キャリーフラグがリセットされていれば 0 になる。


farcall86x2

宣言
	int farcall86x2(unsigned off, unsigned seg, union REGS2 *ri, union REGS2 *ro, struct SREGS *sr)

引数
	off
		far call エントリのオフセット値 
	seg
		far call エントリのセグメント値 
	ri, ro, sr
		int86x2 と同じ

戻り値
	int86x2 と同じ


_readflags

宣言
	unsigned _readflags(void)
	（inline assembler の場合がある）

戻り値
	現在の flags レジスタの内容


*/


#ifndef INT86X2_H__
#define INT86X2_H__

#include <dos.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__WATCOMC__)
#define INT86X2CALL __cdecl
#define INT86X2FAR __far
#define INT86X2NEAR __near
#elif defined(__TURBOC__)
#define INT86X2CALL cdecl
#define INT86X2FAR far
#define INT86X2NEAR near
#elif defined(__SC__) || defined(__DMC__)
#define INT86X2CALL __cdecl
#define INT86X2FAR __far
#define INT86X2NEAR __near
#elif defined(_MSC_VER)
# if _MSC_VER <= 700
# define INT86X2CALL _cdecl
# define INT86X2FAR _far
# define INT86X2NEAR _near
# else
# define INT86X2CALL __cdecl
# define INT86X2FAR __far
# define INT86X2NEAR __near
# endif
#endif


#if defined(__WATCOMC__)
#pragma pack(push, 1)
#elif defined(__SC__) || defined(__DMC__) || defined(_MSC_VER)
#pragma pack(1)
#elif defined(__TURBOC__) || defined(__BORLANDC__)
#pragma option -a-
#endif

struct _WORDREGS2 {
	unsigned short  ax, bx, cx, dx, si, di, cflag, flags, bp;
};

struct _BYTEREGS2 {
	unsigned char  al,ah, bl, bh, cl, ch, dl, dh, sil,sih, dil,dih, cflag,cflagh, flagsl,flagsh, bpl,bph;
};

union REGS2 {
	struct _BYTEREGS2  h;
	struct _WORDREGS2  x;
};

#if defined(__WATCOMC__)
#pragma pack(pop)
#elif defined(__SC__) || defined(__DMC__) || defined(_MSC_VER)
#pragma pack()
#elif defined(__TURBOC__) || defined(__BORLANDC__)
#pragma option -a.
#endif


#if defined(LSI_C)

int  int86x2_nn(void near *, unsigned, unsigned, unsigned, \
		int, union REGS2 near *, union REGS2 near *, struct SREGS near*);
int  int86x2_nf(unsigned, unsigned, unsigned, unsigned, \
		int, union REGS2 far *, union REGS2 far *, struct SREGS far*);
int  farcall86x2_nn(unsigned, unsigned, unsigned, unsigned, \
		unsigned, unsigned , union REGS2 near *, union REGS2 near *, struct SREGS near*);
int  carcall86x2_nf(unsigned, unsigned, unsigned, unsigned, \
		unsigned, unsigned, union REGS2 far *, union REGS2 far *, struct SREGS far*);

#define int86x2(n, r1, r2, sr)	int86x2_nn(0,0,0,0, n, r1, r2, sr)
#define _fint86x2(n, r1, r2, sr)	int86x2_nf(0,0,0,0, n, r1, r2, sr)
#define farcall86x2(o, s, r1, r2, sr)	farcall86x2_nn(0,0,0,0, o, s, r1, r2, sr)
#define _ffarcall86x2(o, s, r1, r2, sr)	farcall86x2_nf(0,0,0,0, o, s, r1, r2, sr)

unsigned _asm_c_readflags(char *);
#define _readflags()	_asm_c_readflags("\n\t" "pushf" "\n\t" "pop\tax" "\n")

#else

int  INT86X2CALL INT86X2NEAR int86x2_nn(int, union REGS2 INT86X2NEAR *, union REGS2 INT86X2NEAR *, struct SREGS INT86X2NEAR *);
int  INT86X2CALL INT86X2NEAR int86x2_nf(int, union REGS2 INT86X2FAR *, union REGS2 INT86X2FAR *, struct SREGS INT86X2FAR *);
int  INT86X2CALL INT86X2FAR int86x2_fn(int, union REGS2 INT86X2NEAR *, union REGS2 INT86X2NEAR *, struct SREGS INT86X2NEAR *);
int  INT86X2CALL INT86X2FAR int86x2_ff(int, union REGS2 INT86X2FAR *, union REGS2 INT86X2FAR *, struct SREGS INT86X2FAR *);

int  INT86X2CALL INT86X2NEAR farcall86x2_nn(unsigned, unsigned, union REGS2 INT86X2NEAR *, union REGS2 INT86X2NEAR *, struct SREGS INT86X2NEAR *);
int  INT86X2CALL INT86X2NEAR farcall86x2_nf(unsigned, unsigned, union REGS2 INT86X2FAR *, union REGS2 INT86X2FAR *, struct SREGS INT86X2FAR *);
int  INT86X2CALL INT86X2FAR farcall86x2_fn(unsigned, unsigned, union REGS2 INT86X2NEAR *, union REGS2 INT86X2NEAR *, struct SREGS INT86X2NEAR *);
int  INT86X2CALL INT86X2FAR farcall86x2_ff(unsigned, unsigned, union REGS2 INT86X2FAR *, union REGS2 INT86X2FAR *, struct SREGS INT86X2FAR *);

unsigned INT86X2CALL INT86X2NEAR _readflags_n(void);
unsigned INT86X2CALL INT86X2FAR _readflags_f(void);

#if defined(__TINY__) || defined(__SMALL__)
#define int86x2(n, r1, r2, sr)	int86x2_nn(n, r1, r2, sr)
#define _fint86x2(n, r1, r2, sr)	int86x2_nf(n, r1, r2, sr)
#define farcall86x2(o, s, r1, r2, sr)	farcall86x2_nn(o, s, r1, r2, sr)
#define _ffarcall86x2(o, s, r1, r2, sr)	farcall86x2_nf(o, s, r1, r2, sr)
#define _readflags	_readflags_n
#elif defined(__MEDIUM__)
#define int86x2(n, r1, r2, sr)	int86x2_fn(n, r1, r2, sr)
#define _fint86x2(n, r1, r2, sr)	int86x2_ff(n, r1, r2, sr)
#define farcall86x2(o, s, r1, r2, sr)	farcall86x2_fn(o, s, r1, r2, sr)
#define _ffarcall86x2(o, s, r1, r2, sr)	farcall86x2_ff(o, s, r1, r2, sr)
#define _readflags	_readflags_f
#elif defined(__COMPACT__)
#define int86x2(n, r1, r2, sr)	int86x2_nf(n, r1, r2, sr)
#define _fint86x2(n, r1, r2, sr)	int86x2_nf(n, r1, r2, sr)
#define farcall86x2(o, s, r1, r2, sr)	farcall86x2_nf(o, s, r1, r2, sr)
#define _ffarcall86x2(o, s, r1, r2, sr)	farcall86x2_nf(o, s, r1, r2, sr)
#define _readflags	_readflags_n
#elif defined(__LARGE__) || defined(__HUGE__)
#define int86x2(n, r1, r2, sr)	int86x2_ff(n, r1, r2, sr)
#define _fint86x2(n, r1, r2, sr)	int86x2_ff(n, r1, r2, sr)
#define farcall86x2(o, s, r1, r2, sr)	farcall86x2_ff(o, s, r1, r2, sr)
#define _ffarcall86x2(o, s, r1, r2, sr)	farcall86x2_ff(o, s, r1, r2, sr)
#define _readflags	_readflags_f
#endif

#endif

#ifdef __cplusplus
}
#endif

#endif
