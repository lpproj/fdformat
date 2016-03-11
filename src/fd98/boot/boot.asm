;
; File:
;                            boot.asm
; Description:
;                           DOS-C boot
;
;                       Copyright (c) 1997;
;                           Svante Frey
;                       All Rights Reserved
;
; This file is part of DOS-C.
;
; DOS-C is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version
; 2, or (at your option) any later version.
;
; DOS-C is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
; the GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public
; License along with DOS-C; see the file COPYING.  If not,
; write to the Free Software Foundation, 675 Mass Ave,
; Cambridge, MA 02139, USA.
;


LOADSEG		equ	0060h
FATBUF		equ	2000h	; offset of temporary buffer for FAT chain

_SS		equ	1400h
_SP		equ	((1f00h - _SS) << 4) - 1
DISK_BOOT	equ	0584h	; seg=0000h
BOOTPART_SCRATCHPAD	equ	03feh;

;-----------------------------------------------------------------------

segment .text
	align	1
	org	0

bsJump:		jmp	short real_start
		nop
bsOemName	db	'FreeDOS '	; OEM label
; beginning of BPB
bsBytesPerSec	dw	0		; bytes/sector (Logical)
bsSecPerClust	db	0		; sectors/allocation unit
bsResSectors	dw	0		; # reserved sectors
bsFATs		db	0		; # of fats
bsRootDirEnts	dw	0		; # of root dir entries
bsSectors	dw	0		; # sectors total in image (0; see bsHugeSectors)
bsMedia		db	0		; media descrip: fd=2side9sec, etc...
bsFATsecs	dw	0		; # sectors in a fat
bsSecPerTrack	dw	0		; # sectors/track
bsHeads		dw	0		; # heads
bsHiddenSecs	dd	0		; # hidden sectors
bsHugeSectors	dd	0		; # sectors if > 65536
; end of BPB
bsDriveNumber	db	0		; drive number
bsReserved1	db	0
bsBootSignature	db	0		; extended boot signature
; beginning of extended BPB
bsVolumeID	dd	0		; (midSerialNum)
bsVolumeLabel	times 11 db 0		; (midVolLabel)
bsFileSysType	times 8  db 0		; (midFileSysType)
; end of extended BPB

%if 1
sysPartStart	dd	0		; (nec98 DOS5+ HD) first sector of the partition (same as bsHiddenSecs)
sysDataOffset	dw	0		; (nec98 DOS5+ HD) offset of system data (IO.SYS) by physical sector (from sysPartStart)
sysPhysicalBPS	dw	0		; (nec98 DOS5+ HD) bytes/sector (Physical)
		db	0		; (nec98 DOS5+ HD) unknown
; specific of FreeDOS(in boot.asm/sys.c)
sysRootDirStart	dd	0		; first root directory sector (logical sector, started from the partition)
sysRootDirSecs	dw 0		; count of logical sectors of root directory
sysFatStart	dd	0		; first FAT sector (logical sector, started from the partition)
sysDataStart	dd	0		; first data sector (logical sector, started from the partition)
tempbuf_dw:
tempbuf_off	dw	0
tempbuf_seg	dw	LOADSEG
%else
; specific of FreeDOS(in boot.asm/sys.c)
tempbuf		dw	LOADSEG
sysRootDirSecs	dw	0		; # of sectors root dir uses
sysFatStart	dd	0		; first FAT sector
sysRootDirStart	dd	0		; first root directory sector
sysDataStart	dd	0		; first data sector
sysPhysicalBPS	dw	0		; bytes/sector (Physical)
%endif

;-----------------------------------------------------------------------

;	+--------+ 1F80:0200
;	|BOOT IPL|
;	|--------| 1F80:0000
;	|        |
;	|--------| 1F00:0000
;	|CLUSTER |
;	|LIST    |
;	|--------| 1F00:0000
;	|        |
;	|--------|
;	|KERNEL  | also used as max 128k FAT buffer
;	|LOADED  | before kernel loading starts
;	|--------| 0060:0000
;	|        |
;	+--------+

real_start:
;		mov	ah, 11h	; view cursor
;		int	18h

		cld
		mov	ax, _SS
		mov	ss, ax
		mov	sp, _SP
		xor	ax, ax
		mov	ds, ax
		mov	al, [DISK_BOOT]	; DA/UA
		mov [BOOTPART_SCRATCHPAD], si		; preserve boot partition (HDD)
		push	cs
		pop	ds
		mov	[bsDriveNumber], al

%if 1
		call	print
		db	"FreeDOS(98)",0
%endif

%ifdef NEC98FDD
		; calc sector length
		; (0 = 128, 1 = 256, 2 = 512, 3 = 1024bytes per sector)
		mov	ax, [sysPhysicalBPS]
.calc_sl:
		inc	byte [readDisk.rd_sl]
		shr	ax, 1
		ja	.calc_sl	; loop if cy=0 and zf=0
;		jbe	short .calc_sl2	; jmp if cy=1 or z=1
;		inc	al
;		jmp	short .calc_sl
;.calc_sl2:
;		mov	[readDisk.rd_sl], al
%endif


;       FINDFILE: Searches for the file in the root directory.
;
;       Returns:
;                               AX = first cluster of file

                ; First, read the whole root directory
                ; into the temporary buffer.

                mov     ax, word [sysRootDirStart]
                mov     dx, word [sysRootDirStart + 2]
                mov     di, word [sysRootDirSecs]
                les bx, [tempbuf_dw]									; ES:BX = LOADSEG:0000
                push es
                push bx
                call    readDisk
                pop di																; di = bx (0)
                pop ax																; ax = es (LOADSEG)
                jc      jmp_boot_error

		; Search for KERNEL.SYS file name, and find start cluster.

	.next_entry:
                mov es, ax
		mov     cx, 11
                mov     si, filename
                push    di
                repe    cmpsb
                pop     di
                je      .ffDone

                add ax, 2		; go to next directory entry
                cmp     byte [es:di], 0		; if the first byte of the name is 0,
                jnz     .next_entry		; there is no more files in the directory

		jmp	short boot_error; fail if not found
	.ffDone:
                mov     ax, [es:di + 1ah]	; get cluster number from directory entry
                push    ax			; store first cluster number

                call    print
                db      " FAT",0



;       GETFATCHAIN:
;
;       Reads the FAT chain and stores it in a temporary buffer in the first
;       64 kb.  The FAT chain is stored an array of 16-bit cluster numbers,
;       ending with 0.
;
;       The file must fit in conventional memory, so it can't be larger than
;       640 kb. The sector size must be at least 512 bytes, so the FAT chain
;       can't be larger than around 3 kb.
;
;       Call with:      AX = first cluster in chain

                ; Load the complete FAT into memory. The FAT can't be larger
                ; than 128 kb, so it should fit in the temporary buffer.

	%if 0
                mov     es, [tempbuf]
                xor     bx, bx
                mov     di, [bsFATsecs]
                mov     ax, word [sysFatStart]
                mov     dx, word [sysFatStart + 2]
	%else
		les	ax, [sysFatStart]
		push	es
		pop	dx
    les bx, [tempbuf_dw]
		mov	di, [bsFATsecs]
	%endif
                call    readDisk
                pop     ax                      ; restore first cluster number
jmp_boot_error: jc      boot_error

                ; Set ES:DI to the temporary storage for the FAT chain.
                push    ds
                push    es
                pop     ds
                pop     es
;		push	es
;		pop	ds
;		mov	es, 1f00h
                mov     di, FATBUF

next_clust:     stosw                           ; store cluster number
                mov     si, ax                  ; SI = cluster number

%ifdef ISFAT12
                ; This is a FAT-12 disk.

fat_12:         add     si, si          ; multiply cluster number by 3...
                add     si, ax
                shr     si, 1           ; ...and divide by 2
                lodsw

                ; If the cluster number was even, the cluster value is now in
                ; bits 0-11 of AX. If the cluster number was odd, the cluster
                ; value is in bits 4-15, and must be shifted right 4 bits. If
                ; the number was odd, CF was set in the last shift instruction.

                jnc     .fat_even
                mov     cl, 4
                shr     ax, cl          ; shift the cluster number

	.fat_even:
		and     ah, 0fh		; mask off the highest 4 bits
                cmp     ax, 0ff8h	; check for EOF
                jb      next_clust      ; continue if not EOF

%endif
%ifdef ISFAT16
                ; This is a FAT-16 disk. The maximal size of a 16-bit FAT
                ; is 128 kb, so it may not fit within a single 64 kb segment.

fat_16:         mov     dx, [cs:tempbuf_seg]
                add     si, si          ; multiply cluster number by two
                jnc     .first_half	; if overflow...
                add     dh, 10h		; ...add 64 kb to segment value

	.first_half:
		mov     ds, dx          ; DS:SI = pointer to next cluster
                lodsw                   ; AX = next cluster

                cmp     ax, 0fff8h	; >= FFF8 = 16-bit EOF
                jb      next_clust      ; continue if not EOF
%endif

finished:       ; Mark end of FAT chain with 0, so we have a single
                ; EOF marker for both FAT-12 and FAT-16 systems.

                xor     ax, ax
                stosw

                push    cs
                pop     ds

                call    print
                db      " Kernel",0


;       loadFile: Loads the file into memory, one cluster at a time.

                les bx, [tempbuf_dw]    ; set ES:BX to load address

                mov     si, FATBUF      ; set DS:SI to the FAT chain

cluster_next:   lodsw                           ; AX = next cluster to read
                or      ax, ax                  ; if EOF...
                je      boot_success            ; ...boot was successful

                dec     ax                      ; cluster numbers start with 2
                dec     ax

                mov     di, word [bsSecPerClust]
                and     di, 0ffh		; DI = sectors per cluster
                mul     di
                add     ax, [sysDataStart]
                adc     dx, [sysDataStart + 2]	; DX:AX = first sector to read
                call    readDisk
                jnc     cluster_next


boot_error:	call	print
		db	10, "err!",0

		xor	ah, ah
		int	18h			; wait for a key
		jmp	word 0ffffh:0		; reboot the machine

boot_success:
	%ifdef HD_AS_BOOTDRIVE
		mov ah, 8eh			; SASI/IDE HDD `half-height' mode
		int 1bh
		xor ax, ax
		mov ds, ax
		mov [BOOTPART_SCRATCHPAD], ax
		mov bl, 80h			; boot device <- SASI(IDE) #1
		mov byte [DISK_BOOT], bl
	%else
		call	print
		db	" GO! ",0
		;mov	bl, [bsDriveNumber]
	%endif
		jmp	word LOADSEG:0


; prints text after call to this function.

print:
		pop	si
		push	di
		push	es
		les	di, [.vram_off]
	.loop:
		xor	ah, ah
		lodsb
		or	al, al
		jz	.end
		cmp	al, 10	; LF
		jz	.lf
		stosw
		jmp	short .loop
	.lf:
		mov	ax, di
		mov	dl, 80 * 2
		div	dl
		inc	ax
		mul	dl
		mov	di, ax
		jmp	short .loop
	.end:
		mov	[.vram_off], di
		mov	dx, di
		inc	dx
		mov	ah, 13h	; set cursor position
		int	18h
		pop	es
		pop	di
		push	si
		ret

.vram_off	dw	0
.vram_seg	dw	0xa000


;       readDisk:       Reads a number of sectors into memory.
;
;       Call with:      DX:AX = 32-bit DOS sector number
;                       DI = number of sectors to read
;                       ES:BX = destination buffer
;                       ES must be 64k aligned (1000h, 2000h etc).
;
;       Returns:        CF set on error
;                       ES:BX points one byte after the last byte read.

readDisk:

%ifdef NEC98FDD
	push	cx
	push	dx
	;add	ax, [bsHiddenSecs]
	;adc	dx, [bsHiddenSecs + 2]
	
	mov	cx, di
	
.rd_loop:
	call	.read_onesec
	jc	.rd_exit
	add	ax, byte 1
	adc	dx, byte 0
	;
	add	bx, [sysPhysicalBPS]
	jnc	.rd_loop2
	push	es
	pop	di
	add	di, 1000h
	push	di
	pop	es
.rd_loop2:
	loop	.rd_loop
.rd_exit:
	;mov	di, cx
	pop	dx
	pop	cx
	ret
	
.read_onesec:
	; input lba...dx:ax
	;
	; lba  HDD (DA/UA = 0xh, 2xh)
	;   LBA  DX:CX
	; chs  HDD (DA/UA = 8xh)
	;     C  CX
	;     H  DH
	;     S  DL (beginning at 0)
	; chs  FDD
	;     C  CL
	;     H  DH
	;     S  DL (beginning at 1)
	; other  CH sector size 0(128), 1(256), 2(512) or 3(1024)
	
	
	push	ax
	push	cx
	push	dx
	push	bx
	push	bp
	;mov	bp, sp
	push	ax
	mov	al, byte [bsHeads]
	mov	cx, [bsSecPerTrack]
	mul	cl
	mov	bp, ax		; bx = s * h
	pop	ax
	div	bp
	mov	[.rd_c], al
	mov	ax, dx
	div	cl
	mov	dh, al
	mov	dl, ah
	mov	ah, 46h			; READ DATA: MFM, err-retry
	mov	cx, [.rd_c]
	cmp	cl, [.rd_cp]
	je	short .rd1s_2
	or	ah, 10h			; with SEEK
	mov	[.rd_cp], cl
.rd1s_2:
	;mov	dx, [.rd_s]
	inc	dl
	mov	bp, [sysPhysicalBPS]
	xchg	bx, bp
	;mov	bx, [sysPhysicalBPS]
	;mov	bp, [bp + 2]		; prev_bx
.rd1s_2x:
	mov	al, [bsDriveNumber]	; DA/UA
	int	1bh
%if 0
	jnc	.rd1s_exit	; Init Device when error.
	mov	ah, 3
	int	1bh
	mov	ah, 56h
	jmp	short .rd1s_2x
.rd1s_exit:
%endif
	pop	bp
	pop	bx
	pop	dx
	pop	cx
	pop	ax
	ret

.rd_cp		db	0
.rd_c		db	0
.rd_sl		db	(256 - 7 - 1)
;.rd_s		db	0
;.rd_h		db	0
%endif
%ifdef NEC98HDD
		push	dx
		push	ax
		xor	dx, dx
		mov	ax, [bsBytesPerSec]
		div	word [sysPhysicalBPS]
		mov	cx, ax
		mul	di
		mov	di, ax
		pop	ax
		pop	dx
		mul	cx

	.read_next:
		push	dx
		push	ax
		push	bp

		mov	bp, bx			; buffer offset
		add	ax, [bsHiddenSecs]
		adc	dx, [bsHiddenSecs + 2]
		mov	cx, ax			; sector number (low)
		mov	bx, [sysPhysicalBPS]	; bx = trans size

		mov	ah, 06h			; read data
		mov	al, [bsDriveNumber]	; DA/UA
		and	al, 7fh
		int	1bh
		jnc	.read_ok

		mov	ah, 03h			; reset
		int	1bh
		pop	bp
		pop	ax
		pop	dx
		jmp	short .read_next

	.read_ok:
		add	bp, bx
		jnc	.no_incr_es		; if overflow...

		mov	ax, es
		add	ah, 10h			; ...add 1000h to ES
		mov	es, ax

	.no_incr_es:
		mov	bx, bp
		pop	bp
		pop	ax
		pop	dx			; DX:AX = last sector number

		add	ax, 1
		adc	dx, 0			; DX:AX = next sector to read
		dec	di			; if there is anything left to read,
		jnz	.read_next		; continue

		clc
		ret
%else
		stc
		ret
%endif

filename	db	"KERNEL  SYS"

		times	512 - $ + $$ db 0

