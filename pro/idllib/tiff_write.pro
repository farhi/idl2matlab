; $Id: tiff_write.pro,v 1.6 1995/02/02 18:31:47 dave Exp $

; Copyright (c) 1991-1993. Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	TIFF_WRITE
;
; PURPOSE:
;	Write 8-bit or 24-bit images in TIFF format.
;
; CATEGORY:
;	Input/output.
;
; CALLING SEQUENCE:
;	TIFF_WRITE, Filename, Array [, Orientation], $
;		    [RED = R, GREEN = G, BLUE = B, PLANARCONFIG = pc, $
;		    XRESOL = xresol, YRESOL = yresol]
;
; INPUTS:
;     Filename:	A string containing the name of file to create. 
;
;	Array:	The image data to be written.  If not already a byte array,
;		it is made a byte array.  Array may be either an
;		(n, m) array for Grayscale or Palette classes, or
;		a (3, n, m) array for RGB full color, interleaved
;		by image.  If the PLANARCONFIG keyword (see below) is set
;		to 2 then the Array parameter is ignored (and may be
;		omitted). See PROCEDURE below for more information on
;		TIFF classes. 
;
; OPTIONAL INPUT PARAMETERS:
; Orientation:	This parameter should be 0 if the image is stored from bottom 
;		to top (the default).  For images stored from top to bottom, 
;		this parameter should be 1.  
;
;		WARNING:  Not all TIFF readers are capable of reversing the 
;		scan line order.  If in doubt, first convert the image
;		to top to bottom order (use the IDL REVERSE() function), and 
;		set Orientation to 1.
;
; OPTIONAL KEYWORD PARAMETERS:
; RED, GREEN, BLUE:
;		The color table vectors, scaled from 0 to 255 in the case of 
;		a Class P, Palette color image.  If, PlanarConfig is 2, these 
;		parameters must contain the 3 color component image parameters.
;
; PLANARCONFIG:	Set this parameter to 2 if writing an RGB image that is 
;		contained in three separate images (color planes), specified
;		in the RED, GREEN, and BLUE parameters.  Otherwise, omit
;		this parameter (or set it to 1).
;
;	XRESOL:	The horizontal resolution, in pixels per inch.  The default
;		is 100.
; 
;	YRESOL:	The vertical resolution, in pixels per inch.  The default
;		is 100.
;
; OUTPUTS:
;	No explicit inputs.
;
; COMMON BLOCKS:
;	TIFF_COM.  Only for internal use.
;
; SIDE EFFECTS:
;	A file is created and written.
;
; RESTRICTIONS:
;	This procedure writes images in a single strip, or 3 strips when 
;	PLANARCONFIG is set to 2.  This procedure may cause readers with 
;	memory limitations problems.
;
; PROCEDURE/EXAMPLES:
;	Four types of TIFF files can be written:
;
;	TIFF Class G, Grayscale.  Array contains the 8-bit image array.
;	A value of 0 is black, 255 is white.  The Red, Green, and Blue
;	keywords are omitted.  Example:
;		TIFF_WRITE, 'a.tiff', Array
;
;	TIFF Class P, Palette Color.  Array contains the 8-bit image array.  
;	The keyword parameters RED, GREEN, and BLUE contain the color tables, 
;	which can have up to 256 elements, scaled from 0 to 255.  Example:
;		TIFF_WRITE, 'a.tiff', Array, RED = r, GREEN = g, BLUE = b
;
;	TIFF Class R, RGB Full Color, color interleaved by pixel.
;	Array contains the byte data, and is dimensioned (3, cols, rows).
;	Example:
;		TIFF_WRITE, 'a.tiff', Array
;
;	TIFF Class R, RGB Full Color, color interleaved by image.
;	Input is three separate images, provided in the keyword
;	parameters RED, GREEN, and BLUE.  The input parameter "Array"
;	is ignored.  The keyword PLANARCONFIG must be set to 2 in this case.
;	Example:
;		TIFF_WRITE, 'a.tiff', RED = r, GREEN = g, BLUE = b, PLAN = 2
;
; MODIFICATION HISTORY:
;	DMS, Written for VMS in 1985.
;
;	DMS, April, 1991.  Rewrote and added class R and P images.
;	DJC, Nov, 1993.  Fixed doc header.
;-
pro tiff_add_tag, lun, tag, value  ;Add a tag to the Image File Directory (IFD)
common tiff_com, order, ifd, count

s = size(value)		;Determine type from parameter
typ = s(s(0)+1)		;IDL type code
tiff_typ = ([ 0, 1, 3, 4, 5, 0, 0, 2])(typ)  ;Tiff types vs IDL
TypeLen = ([0, 1, 1, 2, 4, 8])(tiff_typ)

n = s(s(0)+2)		; # of elements
offset = count * 12 + 2	; Offset into ifd
ifd(offset) =   byte(fix(tag),0,2)	;integer tag
ifd(offset+2) = byte(tiff_typ, 0, 2)	;data type
ifd(offset+4) = byte(n,0,4)		;count
nbytes = n * TypeLen

if nbytes le 4 then begin	;Simple case
	ifd(offset+8) = byte(value,0,nbytes)
endif else begin		;Array, written to file
	point_lun, -lun, pos   ;Get file posit
	ifd(offset+8) = byte(pos, 0, 4)  ;Set IFD ^ pointer
	if typ ne 4 then writeu, lun, value $	;Write the data
	else begin		;Write floating
		s = lonarr(n * 2)
		s(indgen(n)*2) = value * 10000.  ;Arbritrary scale of 10000
		s(indgen(n)*2+1) = 10000
		writeu,lun, s
	endelse
endelse
count = count + 1
end


pro tiff_write, filename, array, orientation, $
	Red=red, Green=green, Blue=blue, PlanarConfig = PlanarConfig, $
	Xresol = Xresol, Yresol = Yresol
common tiff_com, order, ifd, count

on_error,2                      ;Return to caller if an error occurs

if n_elements(array) gt 0 then array = byte(array) ;Make sure it's byte
s = size(array)
if n_elements(PlanarConfig) le 0 then PlanarConfig = 1

color = 0			;True if palette color with tables

if s(0) eq 3 then begin		;True color image?
	photo = 2
	if s(1) ne 3 then message,'For true-color, image must be (3,n,m)'
	cols = s(2)
	rows = s(3)
	samples = 3		;3 samples / pixel
endif else if PlanarConfig eq 2 then begin   ;RGB with separate sample planes
	photo = 2
	s = size(red)		;Take image param from r,g,b
	if s(0) ne 2 then message, 'Parameter must be 2D'
	cols = s(1)
	rows = s(2)
	samples = 3
	if (n_elements(red) ne n_elements(green)) or $
	    (n_elements(red) ne n_elements(blue)) then $
		message,'Image components must have same size'
endif else begin		;Assume must be palette
	if s(0) ne 2 then message, 'Parameter must be 2D'
	cols = s(1)
	rows = s(2)
	samples = 1
	if (n_elements(red) ne n_elements(green)) or $
	    (n_elements(red) ne n_elements(blue)) then $
			message,'Color tables must have same size'
	color = N_elements(red) GT 0
	if color then photo = 3 else photo = 1
endelse

if n_elements(orientation) eq 0 then orientation = 0

if (!version.os EQ 'MacOS') then begin
openw, lun, filename, /BLOCK, /GET_LUN, MACTYPE = "TIFF"
endif else begin
openw, lun, filename, /BLOCK, /GET_LUN
endelse
header = bytarr(8)		;The Tiff header

tst = byte(1,0,2)		;Which endian???
if tst(0) eq 1 then header(0) = byte("II") $	;Intel order
   else header(0) = byte("MM")	;Motorola order

header(2) = byte(42,0,2)	;Version = 42

writeu, lun, header

ifd = bytarr(512)		;Image file directory
count = 0			;# of tags

tiff_add_tag, lun, 254, 0L		;New Subfile type
tiff_add_tag, lun, 256, long(cols)	;Image width
tiff_add_tag, lun, 257, long(rows)	;Image height

tiff_add_tag, lun, 258, replicate(8,samples)  ;bit/sample
tiff_add_tag, lun, 259, 1		;No compression
tiff_add_tag, lun, 262, photo	;Photometric Interpretation
nbytes = rows * cols		;Bytes / plane

; Write image data......
point_lun, -lun, faddr		;Get current file position
if PlanarConfig eq 2 then begin ; write R,G,B in separate planes
	tiff_add_tag, lun, 273, faddr + [0,1,2] * nbytes + 12  ;Strip offsets
	writeu,lun, byte(red)
	writeu,lun, byte(green)
	writeu,lun, byte(blue)
endif else begin		;Write image as one chunk
	tiff_add_tag, lun,273, faddr  ;Strip offset
	writeu, lun, byte(array)
endelse

tiff_add_tag, lun, 274, fix(4 - 3 * (orientation and 1)) ;Orientation  
tiff_add_tag, lun, 277, samples	;Samples / pixel
tiff_add_tag, lun, 278, rows		;Rows / strip

if PlanarConfig eq 2 then  t = replicate(nbytes, samples) $ ;Strip byte cnts
else t = samples * nbytes
tiff_add_tag, lun, 279,  t  ;Strip byte counts 

if n_elements(xresol) le 0 then xresol = 100.
if n_elements(yresol) le 0 then yresol = 100.
tiff_add_tag, lun, 282, float(xresol)		;Xresolution
tiff_add_tag, lun, 283, float(yresol)		;... and Yresolution
tiff_add_tag, lun, 284, PlanarConfig ;PlanarConfig

IF (photo EQ 3) THEN BEGIN	;Add colormap?
	rgb_array = intarr(768)	;Make the color maps
	rgb_array(0) = ishft(fix(red),8)  ;Scale up to 65K max
	rgb_array(256) = ishft(fix(green), 8)
	rgb_array(512) = ishft(fix(blue),8)
	tiff_add_tag, lun, 320, rgb_array
	ENDIF


point_lun, -lun, faddr		;Write IFD at and, get addr
ifd(0) = byte(count,0,2)	;Insert count
writeu, lun, ifd(0: count*12+5) ;Write IFD followed by 4 zero bytes

point_lun, lun, 0		;Rewind to header
header(4) = byte(faddr,0,4)	;Write ifd offset
writeu, lun, header		;And save it

free_lun,lun			;Done
end
		

