; $Id: read_ppm.pro,v 1.1 1994/11/18 23:28:57 dave Exp $
; Copyright (c) 1994. Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;
Function READ_PPM_NEXT_LINE, unit
cr = string(13b)
repeat begin
    a = ''
    readf, unit, a
    l = strpos(a, '#')   ;Strip comments
    if l ge 0 then a = strmid(a,0,l)
    if strmid(a,0,1) eq cr then a = strmid(a,1,1000)  ;Leading <CR>
    if strmid(a,strlen(a)-1,1) eq cr then a = strmid(a,0,strlen(a)-1)
    a = strtrim(a,2)		;Remove leading & trailing blanks.
    a = strcompress(a)		;Compress white space.
endrep until strlen(a) gt 0
return, a
end

Function READ_PPM_NEXT_TOKEN, unit, buffer
if strlen(buffer) le 0 then buffer = READ_PPM_NEXT_LINE(unit)
white = strpos(buffer, ' ')
if white eq -1 then begin	;No blanks?
    result = buffer
    buffer = ''
endif else begin		;Strip leading token.
    result = strmid(buffer, 0, white)
    buffer = strmid(buffer, white+1, 1000)
endelse
return, result
end



PRO READ_PPM, FILE, IMAGE, MAXVAL = maxval
;
;+
; NAME:
;	READ_PPM
;
; PURPOSE:
;	Read the contents of a PGM (gray scale) or PPM (portable pixmap
;	for color) format image file and return the image in the form
;	of an IDL variable.
;	PPM/PGM format is supported by the PMBPLUS and Netpbm packages.
;
;	PBMPLUS is a toolkit for converting various image formats to and from
;	portable formats, and therefore to and from each other.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	READ_PPM, File, Image 
;
; INPUTS:
;	File:	Scalar string giving the name of the PGM or PPM file.
;
; OUTPUTS:
;	Image:	The 2D byte array to contain the image.  In the case
;		of a PPM file, a (3, n, m) array is returned.
;
; KEYWORD Parameters:
;	MAXVAL = returned maximum pixel value.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Should adhere to the PGM/PPM "standard".
;	Accepts: P2 = graymap ASCII, P5 graymap RAWBITS, P3 true-color
;	ASCII pixmaps, and P6 true-color RAWBITS pixmaps.
;	Maximum pixel values are limited to 255.
;	Images are always stored with the top row first. (ORDER=1)
;
; EXAMPLE:
;	To open and read the PGM image file named "foo.pgm" in the current
;	directory, store the image in the variable IMAGE1 enter:
;
;		READ_PPM, "foo.gif", IMAGE1
;
; MODIFICATION HISTORY:
;	Written Nov, 1994, DMS.
;-
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

ON_IOERROR, bad_io
ON_ERROR, 1

OPENR, unit, file, /GET_LUN, /BLOCK
image = 0
buffer = ''		;Read using strings
magic = READ_PPM_NEXT_TOKEN(unit, buffer)
if strmid(magic,0,1) ne 'P' then begin
Not_pgm: MESSAGE, 'File "'+file+'" is not a PGM/PPM file.'
    return
    endif

type = strmid(magic,1,1)

width = long(READ_PPM_NEXT_TOKEN(unit, buffer))
height = long(READ_PPM_NEXT_TOKEN(unit, buffer))
maxval = long(READ_PPM_NEXT_TOKEN(unit, buffer))
case type of
'2' : BEGIN
	image = bytarr(width, height, /nozero)
	readf, unit, image
      ENDCASE
'3' : BEGIN
	image = bytarr(3, width, height, /nozero)
	readf, unit, image
      ENDCASE
'5' : BEGIN
	image = bytarr(width, height, /nozero)
	readu, unit, image
      ENDCASE
'6' : BEGIN
	image = bytarr(3, width, height, /nozero)
	readu, unit, image
      ENDCASE
else :	goto, Not_pgm
ENDCASE

free_lun, unit
return
BAD_IO: Message, 'Error occured accessing PGM/PPM file:' + file
end

