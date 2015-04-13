; $Id: write_srf.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO WRITE_SRF, FILE, IMG, R, G, B, WRITE_32 = write_32, ORDER = ORDER
;+
; NAME:
;	WRITE_SRF
;
; PURPOSE:
;	Write an IDL image and color table vectors to a
;	Sun rasterfile.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;	WRITE_SRF, File		;Write contents of current window. 
;
;	WRITE_SRF, File, Image  ;Write a given array.
;
;	WRITE_SRF, File, Image, R, G, B  ;Write array with given color tables.
;
; INPUTS:
;	File:	Scalar string giving the name of the rasterfile to write.
;
;	Image:	The 2D array to be output.  If Image is dimensioned (3,n,m),
;		a 24-bit Sun Raster File is written.  If Image is omitted,
;		the entire current window is read into an array and written
;		to the SRF file.  IMAGE should be of byte type, and in top
;		to bottom scan line order.
;
; OPTIONAL INPUT PARAMETERS:
;      R, G, B:	The Red, Green, and Blue color vectors to be written
;		with Image.
;
; KEYWORD PARAMETERS:
;	ORDER:	If specified, the image is written from the top down instead
;		of bottom up.  This only has effect
;		when writing a file from the current IDL window instead of 
;		an image passed as a parameter.
;
;     WRITE_32:	If the input image is a true color image, dimensioned (3,n,m), 
;		it is normally written as a 24-bit raster file.  Set this 
;		keyword to write the result as a 32-bit file.
;
; OUTPUTS:
;	FILE contains the image in rasterfile format. If color vectors
;	were supplied, they are used. Otherwise, the last color tables
;	established by LOADCT are used (If LOADCT hasn't been used
;	to establish color tables yet it is used to load the B/W tables.).
;
;	See the file /usr/include/rasterfile.h for the structure of
;	Sun rasterfiles.
;
; COMMON BLOCKS:
;	COLORS
;
; SIDE EFFECTS:
;	If R, G, and B aren't supplied and LOADCT hasn't been called yet,
;	this routine uses LOADCT to load the B/W tables.
;
; RESTRICTIONS:
;	This routine only writes 32, 24, & 8-bit deep rasterfiles of
;	type RT_STANDARD.  Use the Unix command rasfilter8to1(1) to convert 
;	these files to 1-bit deep files.
;
; MODIFICATION HISTORY:
;	Written 26 June 1988, AB.
;
;	Added 24 bit color, March 1990, DMS.
;
;	Added 32 bit color, July, 1990, DMS.
;
;	Changed to use CURRENT, rather than ORIGINAL colortables, if
;	the color parameter is not provided.  Made sure
;	that colortables were written as bytes.  April, 1991.
;		
;	Fixed bug that misordered the colors when writing a 24 bit
;	image.  Jan, 1992.
;-
; Copyright (c) 1990, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

; Check the arguments
on_error, 1		;Return to main level if error
n_params = n_params();

if n_params eq 1 then begin
	n_params = 2		;Fake 2 param call
	if n_elements(order) ne 1 then order = 1  ;Set default
	img = tvrd(0,0,!d.x_vsize, !d.y_vsize, ORDER = order)
	endif

IF ((n_params NE 2) AND (n_params NE 5))THEN $
  message, "usage: WRITE_SRF, image, [r, g, b]'

; Does image have the required attributes?
img_size = SIZE(img)
IF (img_size(0) NE 2) and (img_size(0) ne 3) THEN  $
	message, 'Image must be a matrix.'

if (img_size(0) eq 3) and (img_size(1) ne 3) then $
	message, '24 or 32 Bit images must be dimensioned (3,n,m)'


if img_size(0) eq 3 then begin
	if keyword_set(write_32) then depth = 32L else depth = 24L
	istart = 1
	cols = img_size(2)
	rows = img_size(3)
  endif else begin
	depth = 8L
	istart = 0
	cols = img_size(1)
	rows = img_size(2)
 endelse

; If any color vectors are supplied, do they have right attributes ?
IF (n_params EQ 5) THEN BEGIN
	r_size = SIZE(r)
	g_size = SIZE(g)
	b_size = SIZE(b)
	IF ((r_size(0) + g_size(0) + b_size(0)) NE 3) THEN $
		message, "R, G, & B must all be 1D vectors."
	IF ((r_size(1) NE g_size(1)) OR (r_size(1) NE b_size(1)) ) THEN $
		message, "R, G, & B must all have the same length."
	map_len = r_size(1) * 3L
    ENDIF ELSE BEGIN
	IF (n_elements(r_curr) EQ 0) THEN LOADCT, 0	; Load B/W tables
	map_len = n_elements(r_curr) * 3
    ENDELSE

; Write the result
OPENW, unit, file, /STREAM, /GET_LUN
a =  { rasterfile, $		;Make the header
	magic:'59a66a95'XL, $
	width: cols, $
	height: rows, $
	depth: depth, $
	length: rows * cols, $
	type:1L, $
	maptype: 1L, $
	maplength: map_len}

test = byte(1L,0,4)  ;Get the byte order of this machine

if test(0) eq 1b then begin  ;I386 order?
	byteorder, a, /htonl		;To network order
	endif

WRITEU, unit,a				;Write header
IF (n_params EQ 5) THEN BEGIN
	WRITEU, unit, BYTE(r)		;Write out color tables
	WRITEU, unit, BYTE(g)
	WRITEU, unit, BYTE(b)
	ENDIF ELSE WRITEU, unit, BYTE(r_curr), BYTE(g_curr), BYTE(b_curr)

if depth eq 32 then begin		;Pad out 24 to 32 bits
	for i=0, rows-1 do $
		writeu, unit, byte(img([2,2,1,0],*,i)) ; 3 bytes/pixel to 4
	FREE_LUN, unit
	return
endif

if (cols and 1) ne 0 then begin ;Odd number of columns?
	message, 'Warning, image width should be even, adding padding.',/info
	if depth eq 8 then $
		for i=0,rows-1 do $  ;Each row
		   writeu, unit, byte(img(*,i)), 0b $
	else for i=0, rows-1 do $
		writeu, unit, byte(img(*,*,i)), [0b,0b,0b]
  endif else WRITEU, unit, BYTE(img)

FREE_LUN, unit			; Close file and free unit

end
