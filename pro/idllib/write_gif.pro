; $Id: write_gif.pro,v 1.2 1993/08/23 16:38:19 steve Exp $

PRO WRITE_GIF, FILE, IMG, R, G, B
;+
; NAME:
;	WRITE_GIF
;
; PURPOSE:
;	Write an IDL image and color table vectors to a
;	GIF (graphics interchange format) file.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
;	WRITE_GIF, File, Image  ;Write a given array.
;
;	WRITE_GIF, File, Image, R, G, B  ;Write array with given color tables.
;
;
; INPUTS:
;	Image:	The 2D array to be output.
;
; OPTIONAL INPUT PARAMETERS:
;      R, G, B:	The Red, Green, and Blue color vectors to be written
;		with Image.
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	If R, G, B values are not provided, the last color table
;	established using LOADCT is saved. The table is padded to
;	256 entries. If LOADCT has never been called, we call it with
;	the gray scale entry.
;
;
; COMMON BLOCKS:
;	COLORS
;
; SIDE EFFECTS:
;	If R, G, and B aren't supplied and LOADCT hasn't been called yet,
;	this routine uses LOADCT to load the B/W tables.
;
; RESTRICTIONS:
;	This routine only writes 8-bit deep GIF files of the standard
;	type: (non-interlaced, global colormap, 1 image, no local colormap)
;
; MODIFICATION HISTORY:
;	Written 9 June 1992, JWG.
;-
; Copyright (c) 1992, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

; Check the arguments
ON_ERROR, 1		;Return to main level if error
n_params = N_PARAMS();

IF ((n_params NE 2) AND (n_params NE 5))THEN $
  message, "usage: WRITE_GIF, file, image, [r, g, b]'

; Is the image a 2-D array of bytes?

img_size	= SIZE(img)
IF img_size(0) NE 2 OR img_size(3) NE 1 THEN	$
	message, 'Image must be a byte matrix.'

cols	= img_size(1)
rows	= img_size(2)

; If any color vectors are supplied, do they have right attributes ?
IF (n_params EQ 2) THEN BEGIN
	IF (n_elements(r_curr) EQ 0) THEN LOADCT, 0	; Load B/W tables
	r	= r_curr
	g	= g_curr
	b	= b_curr
ENDIF

r_size	= SIZE(r)
g_size	= SIZE(g)
b_size	= SIZE(b)
IF ((r_size(0) + g_size(0) + b_size(0)) NE 3) THEN $
	message, "R, G, & B must all be 1D vectors."
IF ((r_size(1) NE g_size(1)) OR (r_size(1) NE b_size(1)) ) THEN $
	message, "R, G, & B must all have the same length."

;	Pad color arrays

clrmap	= BYTARR(3,256)

tbl_size		= r_size(1)-1
clrmap(0,0:tbl_size)	= r
clrmap(0,tbl_size:*)	= r(tbl_size)
clrmap(1,0:tbl_size)	= g
clrmap(1,tbl_size:*)	= g(tbl_size)
clrmap(2,0:tbl_size)	= b
clrmap(2,tbl_size:*)	= b(tbl_size)

; Write the result
; MACTYPE find me
if (!version.os EQ 'MacOS') then begin
  OPENW, unit, file, /STREAM, /GET_LUN, MACTYPE = "GIFf"
endif else begin 
  OPENW, unit, file, /STREAM, /GET_LUN
endelse

hdr	=  { giffile, $		;Make the header
	magic:'GIF87a', 		$
	width_lo:0b, width_hi:0b,	$
	height_lo:0b, height_hi:0b,	$
	global_info: BYTE('F7'X),	$	; global map, 8 bits color
	background:0b, reserved:0b }		; 8 bits/pixel

hdr.width_lo	= cols AND 255
hdr.width_hi	= cols / 256
hdr.height_lo	= rows AND 255
hdr.height_hi	= rows / 256

ihdr	= { 	imagic: BYTE('2C'X),		$	; BYTE(',')
		left:0, top: 0,			$
		width_lo:0b, width_hi:0b,	$
		height_lo:0b, height_hi:0b,	$
		image_info:7b }

ihdr.width_lo	= cols AND 255
ihdr.width_hi	= cols / 256
ihdr.height_lo	= rows AND 255
ihdr.height_hi	= rows / 256

WRITEU, unit, hdr				;Write header
WRITEU, unit, clrmap				;Write color map

WRITEU, unit, ihdr

ENCODE_GIF, unit, img

FREE_LUN, unit			; Close file and free unit


END
