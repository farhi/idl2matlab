; $Id: read_x11_bitmap.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

;+
; NAME:
;	READ_X11_BITMAP
;
; PURPOSE:
;	Read bitmaps stored in the X11 format.
;
;	The X Windows bitmap(1) program produces a C header file
;	containing the definition of a bitmap produced by that program.
;	This procedure reads such a file and creates an IDL byte array
;	containing the bitmap.
;
;	This procedure is used primarily to read bitmaps to be used as
;	IDL widget button labels.
;
; CATEGORY:
;	Bitmaps, X Windows, widgets.
;
; CALLING SEQUENCE:
;	READ_X11_BITMAP, File, Bitmap [, X, Y]
;
; INPUTS:
;	File: The name of the file containing the bitmap.
;
; KEYWORD PARAMETERS:
;
;	EXPAND_TO_BYTES: return a 2-D array which has one bit per byte
;		(0 for a 0 bit), (255 for a 1 bit) instead. (See example)
;
; OUTPUTS:
;	Bitmap: The variable in which to store the bitmap.  This variable
;		is returned as a byte array.
;
; OPTIONAL OUTPUT PARAMETERS:
;	X:	The width of the bitmap is returned in this variable.
;
;	Y:	The height of the bitmap is returned in this variable.
;
; COMMON BLOCKS:
;	None.
;
; EXAMPLE:
;	To open and read the X11 bitmap file named "my.x11" in the current 
;	directory, store the bitmap in the variable BITMAP1, and the width
;	and height in the variables X and Y, enter:
;
;		READ_X11_BITMAP, "my.x11", BITMAP1, X, Y
;
;	To display the new bitmap, enter:
;
;		READ_X11_BITMAP, "my.x11", Image, /EXPAND_TO_BYTES
;		TV, Image, /ORDER
;
; MODIFICATION HISTORY:
;	10 January 1991, AB
;	1 Apr, 1992, CF fixed bug with bitmaps larger than 32k bytes.
;	24 March 1993, JWG fixed EXPAND_TO_BYTES option
;-
;

pro READ_X11_BITMAP, FILE, BITMAP, X, Y, EXPAND_TO_BYTES=EXPAND

on_error, 2		; On error, return to caller

expand	= keyword_set(expand)

; This array, indexed by an ASCII character, returns the hex value associated
; with that character in the least significant 4 bits of the byte.
lo_ascii=bytarr(256, /nozero)
lo_ascii((byte('0'))(0):(byte('9'))(0)) = bindgen(10)
lo_ascii((byte('A'))(0):(byte('F'))(0)) = bindgen(6)+10B
lo_ascii((byte('a'))(0):(byte('f'))(0)) = bindgen(6)+10B

; This array, indexed by an ASCII character, returns the hex value associated
; with that character in the most significant 4 bits of the byte.
hi_ascii=bytarr(256, /nozero)
hi_ascii((byte('0'))(0):(byte('9'))(0)) = bindgen(10)*16B
hi_ascii((byte('A'))(0):(byte('F'))(0)) = (bindgen(6)+10)*16B
hi_ascii((byte('a'))(0):(byte('f'))(0)) = (bindgen(6)+10)*16B


openr, unit, file, /get_lun

; The width
line = ''
while (((p=strpos(line, '_width'))) eq -1) do begin
  if (eof(unit)) then message,'Not an X11 bitmap file: ' + FILE
  readf, unit, line
endwhile
x = long(strmid(line, p+6, 32767))

; The height
readf, unit, line
while (((p=strpos(line, '_height'))) eq -1) do readf, unit, line
y = long(strmid(line, p+7, 32767))

; The result array
  x_bytes = (x + 7) / 8
  BITMAP=bytarr(X_BYTES, Y, /nozero)

; Get to the start of the data
while (((p=strpos(line, '[]'))) eq -1) do readf, unit, line

; Read it
readf, unit, line
pos=0
for i = 0L, x_bytes * y - 1 do begin
  pos = strpos(line, '0x', pos)
  if (pos eq -1) then begin
    readf, unit, line
    pos = strpos(line, '0x')
  endif
  tmp = byte(strmid(line, pos+2, 2))
  pos=pos+4
  bitmap(i) = hi_ascii(tmp(0)) + lo_ascii(tmp(1))
endfor

  if (expand) then begin
    tmp=bitmap
    bitmap=bytarr(X, Y)
    for i=0,X-1 do begin
	byte = i / 8
	mask = 2 ^ (i mod 8)
	ind = where( tmp(byte,*) and mask, count)
	if count ne 0 then bitmap(i, ind) = 255
    endfor
  endif
free_lun, unit
end
