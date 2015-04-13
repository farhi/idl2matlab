; $Id: write_ppm.pro,v 1.1 1994/11/18 23:28:57 dave Exp $
; Copyright (c) 1994. Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.

PRO WRITE_PPM, FILE, Image, ASCII = ascii
;+
; NAME:
;	WRITE_PPM
;
; PURPOSE:
;	Write an image to a PPM (true-color) or PGM (gray scale) file.
;	PPM/PGM format is supported by the PMBPLUS and Netpbm packages.
;
;	PBMPLUS is a toolkit for converting various image formats to and from
;	portable formats, and therefore to and from each other.
;
; CATEGORY:
;	Input/Output.
;	
; CALLING SEQUENCE:
;
;	WRITE_PPM, File, Image  ;Write a given array.
;
; INPUTS:
;	Image:	The 2D (gray scale) or 3D (true-color) array to be output.
;
; KEYWORD PARAMETERS:
;	ASCII = if set, formatted ASCII IO is used to write the image data.
;		If omitted, or set to zero, the far more efficient
;		binary IO (RAWBITS) format is used to write the image data.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A file is written.
;
; RESTRICTIONS:
;	This routine only writes 8-bit deep PGM/PPM files of the standard
;	type.
;	Images should be ordered so that the first row is the top row.
;	If your image is not, use WRITE_PPM, File, REVERSE(Image, 2)
;
; MODIFICATION HISTORY:
;	Written Nov, 1994, DMS.
;-
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

; Check the arguments
ON_IOERROR, bad_io
ON_ERROR, 1

; Is the image a 2-D array of bytes?
img_size	= SIZE(image)
maxval = max(image)
if maxval gt 255 then message, $
		'Data larger than 255 not allowed'
IF img_size(0) eq 2 then begin
    cols = img_size(1)
    rows = img_size(2)
    type = 5 - 3 * keyword_set(ascii)
endif else if img_size(0) eq 3 then begin
    if img_size(1) ne 3 then MESSAGE, 'True-color images must be (3,n,m)'
    cols = img_size(2)
    rows = img_size(3)
    type = 6 - 3 * keyword_set(ascii)
endif else message, 'IMAGE parameter must be dimensioned (n,m) or (3,n,m)'


OPENW, unit, file, /GET_LUN, /BLOCK
printf, unit, 'P'+strtrim(type,2)
printf, unit, cols, rows, byte(maxval)
if keyword_set(ascii) then printf, unit, byte(image) $
else writeu, unit, byte(image)
FREE_LUN, unit
return

BAD_IO: Message, 'Error occured accessing PGM/PPM file:' + file
end
