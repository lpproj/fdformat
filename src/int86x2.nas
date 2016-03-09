; int86x2.nas

	%if 0
COMMENT #
-------------------------------------------------------------------------------

int86x2, farcall86x2 support
use nasm (the Netweide Assembler) to assemble

target:
	16bit DOS (Microsoft|Borland|Watcom|DigitalMars C/C++)

optional symbols:
	FARCODE		for MEDIUM/LARGE/HUGE model
	FARDATAPTR	for COMPACT/LARGE/HUGE model
	FARCALL86X	produce farcal86x2 function if defined
	USE_186		use pusha/popa instruction
	LSIC_SPECIAL	for LSI-C


memo:
	・スタック上に int n; retf のコードを生成し、そこに retf する。
	　pushf して割り込みベクタのアドレスを far call したほうが楽な
	　気もするが、仮想 86 モードだと微妙に意味合いが違ってきたりするので 
	　まじめに int 命令で呼び出してみることにした。
	　スタック上に命令を書いたのは割り込みハンドラ内での再入を考慮して。
	・メモリ上のデータに対して直接 push/pop するよりいったんレジスタで
	　受けたほうが（486 では）速いみたいなんだけど…。

-------------------------------------------------------------------------------
#
	%endif


	BITS 16


%macro _pusha 0
	%ifdef USE_186
	pusha
	%else
	push	ax
	push	cx
	push	dx
	push	bx
	push	sp
	push	bp
	push	si
	push	di
	%endif
%endmacro

%macro _popa 0
	%ifdef USE_186
	popa
	%else
	pop	di
	pop	si
	pop	bp
	pop	ax	; drop sp on the stack
	pop	bx
	pop	dx
	pop	cx
	pop	ax
	%endif
%endmacro


%ifdef FARCODE
%define RESCODEPTR	resd
%define RETFUNC		retf
%else
%define RESCODEPTR	resw
%define RETFUNC		retn
%endif

%ifdef FARDATAPTR
%define RESPTR		resd
%define _ldptr		lds
%else
%define RESPTR		resw
%define _ldptr		mov
%endif

%macro FUNCDEF 1
%ifdef LSIC_SPECIAL
%ifdef FARCODE
  %ifdef FARDATAPTR
	GLOBAL %1_ff_
%1_ff_:
  %else
	GLOBAL %1_fn_
%1_fn_:
  %endif
%else
  %ifdef FARDATAPTR
	GLOBAL %1_nf_
%1_nf_:
  %else
	GLOBAL %1_nn_
%1_nn_:
  %endif
%endif
%else
%ifdef FARCODE
  %ifdef FARDATAPTR
	GLOBAL _%1_ff
_%1_ff:
  %else
	GLOBAL _%1_fn
_%1_fn:
  %endif
%else
  %ifdef FARDATAPTR
	GLOBAL _%1_nf
_%1_nf:
  %else
	GLOBAL _%1_nn
_%1_nn:
  %endif
%endif
%endif
%endmacro

%ifdef FARCODE
%define BEGIN_CODE_SECTION	SEGMENT INT86X_TEXT PUBLIC USE16 CLASS=CODE
%else
  %ifdef LSIC_SPECIAL
%define BEGIN_CODE_SECTION	SEGMENT TEXT PUBLIC USE16 CLASS=CODE
  %else
%define BEGIN_CODE_SECTION	SEGMENT _TEXT PUBLIC USE16 CLASS=CODE
  %endif
%endif

%macro SetupCallerFrame 0
.retptr:	RESCODEPTR 1
%ifdef FARCALL86X
.p_off		resw 1
.p_seg		resw 1
%else
.p0:		resw 1
%endif
.p_ri:		RESPTR 1
.p_ro:		RESPTR 1
.p_sr:		RESPTR 1
%endmacro

%macro SetupAsmFrame 1
.%1_es:		resw 1
.%1_ds:		resw 1
.%1_di:		resw 1
.%1_si:		resw 1
.%1_bp:		resw 1
.%1_sp:		resw 1
.%1_bx:		resw 1
.%1_dx:		resw 1
.%1_cx:		resw 1
.%1_ax:		resw 1
.%1_flags:	resw 1
%endmacro

	STRUC	af
	SetupAsmFrame r
	SetupCallerFrame
	ENDSTRUC

	STRUC	afdrop
	SetupAsmFrame r1_drop
%ifndef FARCALL86X
.codedummy	resw 2
%endif
	ENDSTRUC

	STRUC	af2
	SetupAsmFrame r1
%ifndef FARCALL86X
.codedummy	resw 2
%endif
	SetupAsmFrame r2
	SetupCallerFrame
	ENDSTRUC

%macro _enter_asm_frame 0
	pushf
	_pusha
	push	ds
	push	es
	mov	bp, sp
%endmacro

%macro _leave_asm_frame 0
	pop	es
	pop	ds
	_popa
	popf
%endmacro

	STRUC	REGS
.x_ax:		resw 1
.x_bx:		resw 1
.x_cx:		resw 1
.x_dx:		resw 1
.x_si:		resw 1
.x_di:		resw 1
.x_cflag:	resw 1
.x_flags:	resw 1
.x_bp:		resw 1
	ENDSTRUC

	STRUC	SREGS
.sr_es		resw 1
.sr_cs		resw 1
.sr_ss		resw 1
.sr_ds		resw 1
	ENDSTRUC


	BEGIN_CODE_SECTION

%ifdef FARCALL86X
FUNCDEF	farcall86x2
%else
FUNCDEF	int86x2
%endif

	_enter_asm_frame
	;
%ifndef FARCALL86X
	mov	ax, 90cbh		; CB 90 (retf; nop)
	push	ax
	mov	al, 0cdh		; CD n (int n)
	mov	ah, [bp + af.p0]
	push	ax
	mov	dx, sp
%endif
	push	cs
	mov	ax, .loc_2
	push	ax
%ifdef FARCALL86X
	mov	ax, [bp + af.p_seg]
	push	ax
	mov	ax, [bp + af.p_off]
	push	ax
%else
	push	ss
	push	dx
%endif
	;
	; setup registers
	;
	_ldptr	bx, [bp + af.p_ri]
	mov	ax, [bx + REGS.x_flags]
	push	ax
	mov	ax, [bx + REGS.x_ax]
	push	ax
	mov	ax, [bx + REGS.x_cx]
	push	ax
	mov	ax, [bx + REGS.x_dx]
	push	ax
	mov	ax, [bx + REGS.x_bx]
	push	ax
	push	ax			; dummy value (sp)
	mov	ax, [bx + REGS.x_bp]
	push	ax
	mov	ax, [bx + REGS.x_si]
	push	ax
	mov	ax, [bx + REGS.x_di]
	push	ax
	_ldptr	bx, [bp + af.p_sr]
	mov	ax, [bx + SREGS.sr_ds]
	push	ax
	mov	ax, [bx + SREGS.sr_es]
	push	ax
	_leave_asm_frame
	retf				; invoke int n; retf (to .loc_2)

.loc_2:
	; restore registers
	_enter_asm_frame
	
	%ifndef FARDATAPTR
	mov	ds, [bp + af2.r2_ds]	; reload oridinal DS
	%endif
	
	_ldptr	bx, [bp + af2.p_sr]
	mov	ax, [bp + af2.r1_ds]
	mov	[bx + SREGS.sr_ds], ax
	mov	ax, [bp + af2.r1_es]
	mov	[bx + SREGS.sr_es], ax
	
	_ldptr	bx, [bp + af2.p_ro]
	mov	ax, [bp + af2.r1_ax]
	mov	[bx + REGS.x_ax], ax
	mov	ax, [bp + af2.r1_bx]
	mov	[bx + REGS.x_bx], ax
	mov	ax, [bp + af2.r1_cx]
	mov	[bx + REGS.x_cx], ax
	mov	ax, [bp + af2.r1_dx]
	mov	[bx + REGS.x_dx], ax
	mov	ax, [bp + af2.r1_si]
	mov	[bx + REGS.x_si], ax
	mov	ax, [bp + af2.r1_di]
	mov	[bx + REGS.x_di], ax
	mov	ax, [bp + af2.r1_bp]
	mov	[bx + REGS.x_bp], ax
	mov	ax, [bp + af2.r1_flags]
	mov	[bx + REGS.x_flags], ax
	and	ax, 1
	mov	[bx + REGS.x_cflag], ax
	
	add	sp, BYTE afdrop_size
	_leave_asm_frame
	RETFUNC
