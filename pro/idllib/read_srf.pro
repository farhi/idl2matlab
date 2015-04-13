; $Id: read_srf.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO READ_SRF, FILE, IMAGE, R, G, B
;
;+
; NAME:
;	READ_SRF
;
; PURPOSE:
;	Read the contents of a Sun rasterfile and return the image and
;	color table vectors (if present) in the form of IDL variables.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	READ_SRF, File, Image [, R, G, B]
;
; INPUTS:
;	File:	Scalar string giving the name of the rasterfile to read
;
; OUTPUTS:
;	Image:	The 2D byte array to contain the image.
;
;
; OPTIONAL OUTPUT PARAMETERS:
;     R, G, B:	The variables to contain the Red, Green, and Blue color vectors
;		if the rasterfile containes colormaps.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	This routine only handles 1, 8, 24, and 32-bit rasterfiles of type
;	RT_OLD and RT_STANDARD.  See the file /usr/include/rasterfile.h
;	for the structure of Sun rasterfiles.
;
; EXAMPLE:
;	To open and read the Sun rasterfile named "sun.srf" in the current
;	directory, store the image in the variable IMAGE1, and store the color
;	vectors in the variables R, G, and B, enter:
;
;		READ_SRF, "sun.srf", IMAGE1, R, G, B
;
;	To load the new color table and display the image, enter:
;
;		TVLCT, R, G, B
;		TV, IMAGE1
; 
; MODIFICATION HISTORY:
;	Written fall 1897, AB
;	3/1/90, Added 24 bit images, DMS.
;	7/8/90, Added 32 bit images, DMS.
;	1/22/92, the colors within 24 bit images were not ordered
;		correctly, DMS.
;-
; Copyright (c) 1990, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

; Define the Sun Raster File structure
a = {rasterfile, magic:0L, width:0L, height:0L, depth: 0L, $
	length:0L, type:0L, maptype:0L, maplength:0L}

OPENR, unit, file, /GET_LUN, /BLOCK

READU, unit, a
; Check the magic number
if (a.magic EQ '956aa659'XL) then begin
	byteorder, a, /ntohl		;Back to our order
	endif
	
IF (a.magic NE '59a66a95'X) THEN begin
	message, /CONTINUE, 'File ' + file + $
		' does not have a SUN rasterfile magic number.'
	GOTO, done
	ENDIF

; Only know how to do RT_OLD and RT_STANDARD style rasterfiles.
IF ((a.type NE 0) AND (a.type NE 1)) THEN BEGIN
	message, /CONTINUE, $
		'Don''t know how to handle this style of rasterfile, sorry...'
	GOTO, done
	ENDIF

; Only know how to handle 1 bit and 8 bit images
IF (a.depth NE 1) AND (a.depth NE 8) AND (a.depth ne 24) AND $
   (a.depth ne 32) then begin
	message, /CONTINUE, 'Can''t handle ' + string(a.depth) + ' bit images.'
	GOTO, done
	ENDIF

; Recompute length, since rasterfile.h says you have to for old files...
a.length = (a.width * a.height * a.depth) / 8
image = 0		;Delete original value of image var

; If image has color tables, read them
IF ( (a.maptype EQ 1) AND (a.maplength NE 0) ) THEN BEGIN
	maplen = a.maplength / 3
	r = BYTARR(maplen)
	g = BYTARR(maplen)
	b = BYTARR(maplen)
	READU, unit, r, g, b
	ENDIF

; Get a byte array for the image

; If the image is 1 bit, read it and decode into bytes
case a.depth of
1: begin
	ncols = ((a.width + 15)/16)*16  ;# of columns padded to short
	image = BYTARR(ncols, a.height) ;make the array
	temp = intarr(a.length/2)	; Get the packed shorts
	READU, unit, temp
        mask = ROTATE((2 ^ indgen(16)),2) ;Array of 16 bits
	FOR i = 0,15 DO BEGIN		;Unpack each bit
		ind = WHERE(temp AND mask(i))
		s = size(ind)
		if (s(0) ne 0) THEN image(ind * 16 + i) = 255
		ENDFOR
ENDCASE

8: BEGIN	; If the image is eight bit, read it
	ncols = ((a.width + 1)/2)*2  ;Pad to even number
	image = BYTARR(ncols, a.height)
	READU, unit, image
ENDCASE

24: BEGIN		;24 bit
	ncols = ((a.width + 1)/2)*2  ;Pad to even number
	image = BYTARR(3, ncols, a.height)
	readu, unit, image
ENDCASE

32:	BEGIN		;Read 32 bit into a (3,n,m) image
	image = BYTARR(3,a.width, a.height)
	row = bytarr(4,a.width)	;Read a row at a time
	indices = [3,2,1] # replicate(1,a.width*3) + $
		(4 * (indgen(a.width*3)/3))
	for i=0,a.height-1 do begin
		readu, unit, row
		image(0,0,i) = reform(row(indices),3,a.width,/over)
		endfor
ENDCASE
ENDCASE
done:
	FREE_LUN, unit
	END
