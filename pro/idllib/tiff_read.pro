; $Id: tiff_read.pro,v 1.5 1995/03/14 14:05:39 mattr Exp $

; Copyright (c) 1991-1993. Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	TIFF_READ
;
; PURPOSE:
;	Read 8-bit or 24-bit images in TIFF format.
;
; CATEGORY:
;	Input/output.
;
; CALLING SEQUENCE:
;   Result = TIFF_READ(Filename [,R, G, B], ORDER = order, PLANARCONFIG = pc)
;
; INPUTS:
;    Filename:	A string containing the name of file to read.
;		The default extension is ".TIF".
;
; OUTPUTS:
;	TIFF_READ returns a byte array containing the image data.  The 
;	dimensions of the result are the same as defined in the TIFF 
;	file: (Columns, Rows).
;
;	For TIFF images that are RGB interleaved by pixel, the output 
;	dimensions are (3, Cols, Rows).
;
;	For TIFF images that are RGB interleaved by image, on output
;	Planarconfig is set to 2, and the result is the integer value
;	zero.   In this case, three separate images are returned in
;	the R, G, and B output parameters.
;
; OPTIONAL OUTPUTS:
;     R, G, B:	Variables to hold the Red, Green, and Blue color vectors
;		extracted from TIFF Class P, Palette Color images.
;	For TIFF images that are RGB interleaved by image (Planarconfig
;	returned as 2) the R, G, and B variables each hold an image
;	with the dimensions (Columns, Rows).
;
; KEYWORDS:
;	The following keywords are used for output parameters only:
;
;	ORDER:	The order parameter from the TIFF File.  This parameter is
;		returned as 0 for images written bottom to top, and 1 for
;		images written top to bottom.  If the Orientation parameter 
;		does not appear in the TIFF file, an order of 1 is returned.
;
; PLANARCONFIG:	This parameter is returned as 1 for TIFF files that are
;		GrayScale, Palette, or RGB color interleaved by pixel.  
;		This parameter is returned as 2 for RGB color TIFF files 
;		interleaved by image.
;
; COMMON BLOCKS:
;	TIFF_COM.  Only for internal use.
;
; SIDE EFFECTS:
;	A file is read.
;
; RESTRICTIONS:
;	Handles TIFF classes G, P, and R.  One image per file.
;
; EXAMPLE:
;	Read the file "my.tiff" in the current directory into the variable
;	IMAGE, and save the color tables in the variables, R, G, and B by
;	entering:
;
;		IMAGE = TIFF_READ("my.tiff", R, G, B)
;
;	To view the image, load the new color table and display the image by
;	entering:
;
;		TVLCT, R, G, B
;		TV, IMAGE 
;
;
; MODIFICATION HISTORY:
;	DMS, Written for VMS in 1985.
;	DMS, April, 1991.  Rewrote and added class R and P images.
;	DMS, Jan, 1992.  Fixed bug for images without a RowsPerStrip field.
;       DJC, Nov, 1993.  Fixed doc header.
;	DMS, Dec, 1994.	 Fixed bug with private tags.
;	MWR, Mar, 1995.  Fixed bug when opening non-existent file.
;-

function tiff_long,a,i,len=len	;return longword(s) from array a(i)
common tiff_com, order, ifd, count

on_error,2              ;Return to caller if an error occurs

   if n_elements(len) le 0 then len = 1
   if len gt 1 then result = long(a,i,len) $
   else result = long(a,i)
   if order then byteorder, result, /lswap
   return, result
end


function tiff_rational,a,i, len = len  	; return rational from array a(i)
common tiff_com, order, ifd, count

on_error,2              ;Return to caller if an error occurs

if n_elements(len) le 0 then len = 1
tmp = tiff_long(a, i, len = 2 * len)	;1st, cvt to longwords
if len gt 1 then begin
	subs = lindgen(len)
	rslt = float(tmp(subs*2)) / tmp(subs*2+1)
endif else rslt = float(tmp(0)) / tmp(1)
return, rslt
end

function tiff_int,a,i, len=len	;return unsigned long int from TIFF short int
common tiff_com, order, ifd, count

on_error,2              ;Return to caller if an error occurs
if n_elements(len) le 0 then len = 1
if len gt 1 then begin	;Array?
	result = fix(a,i,len)
	if order then byteorder, result, /sswap
	result = long(result)
	if min(result) lt 0 then begin	;Convert to unsigned from signed 16bit
	  negs = where(result lt 0)
	  result(negs) = 65535L + result(negs)
	  endif
endif else begin	;Scalar
	result = fix(a,i)
	if order then byteorder, result, /sswap
	if result lt 0 then result = 65535L + result
endelse
return, result
end

function tiff_byte, a,i,len=len	;return bytes from array a(i)
common tiff_com, order, ifd, count

on_error,2              ;Return to caller if an error occurs

   if n_elements(len) le 0 then len = 1
   if len gt 1 then result = a(i:i+len-1) $
   else result = a(i)
   return, result
end

function tiff_read_field, index, tag, lun  ;Return contents of field index
; On output, tag = tiff tag index.
;
common tiff_com, order, ifd, count


on_error,2                      ;Return to caller if an error occurs
TypeLen = [0, 1, 1, 2, 4, 8] ;lengths of tiff types, 0 is null type for indexin

ent = ifd(index * 12: index * 12 + 11)  ;Extract the ifd
tag = tiff_int(ent, 0)		;Tiff tag index
typ = tiff_int(ent, 2)		;Tiff data type
cnt = tiff_long(ent, 4)		;# of elements
nbytes = cnt * TypeLen(typ)	;Size of tag field
IF (nbytes GT 4) THEN BEGIN 	;value size > 4 bytes ?
        offset = tiff_long(ent, 8)	;field has offset to value location
        Point_Lun, lun, offset
        val = BytArr(nbytes) 	;buffer will hold value(s)
        Readu, lun, val
        CASE typ OF		;Ignore bytes, as there is nothing to do
	   1: i = 0		;Dummy
           2: val = String(val)		;tiff ascii type
           3: val = tiff_int(val,0, len = cnt)
	   4: val = tiff_long(val,0, len = cnt)
           5: val = tiff_rational(val,0, len = cnt)
	ENDCASE
ENDIF ELSE BEGIN			;Scalar...
        CASE typ OF
	   1: val = ent(8)
  	   2: val = string(ent(8:8+(cnt>1)-1))
	   3: val = tiff_int(ent,8)
	   4: val = tiff_long(ent,8)
        ENDCASE
     ENDELSE
return, val
end




function tiff_read, file, r, g, b, order = ord, PlanarConfig = PC
common tiff_com, order, ifd, count


on_error,2                      ;Return to caller if an error occurs

openr,lun,file, error = i, /GET_LUN, /BLOCK
if i lt 0 then begin ;OK?
	if keyword_set(lun) then free_lun,lun
	lun = -1
	message, 'Unable to open file: ' + file
	endif

hdr = bytarr(8)			;Read the header
readu, lun, hdr

typ = string(hdr(0:1))		;Either MM or II
if (typ ne 'MM') and (typ ne 'II') then begin
	message,'TIFF_READ: File is not a Tiff file: ' + string(file)
	return,0
	endif
order = typ eq 'MM'  		;1 if Motorola 0 if Intel (LSB first or vax)
endian = byte(1,0,2)		;What endian is this?
endian = endian(0) eq 0		;1 for big endian, 0 for little
order = order xor endian	;1 to swap...

; print,'Tiff File: byte order=',typ, ',  Version = ', tiff_int(hdr,2)

offs = tiff_long(hdr, 4)	;Offset to IFD

point_lun, lun, offs		;Read it

a = bytarr(2)			;Entry count array
readu, lun, a
count = tiff_int(a,0)		;count of entries
; print,count, ' directory entries'
ifd = bytarr(count * 12)	;Array for IFD's
readu, lun, ifd			;read it

;	Insert default values:
compression = 1
bits_sample = 1
ord = 1
samples_pixel = 1L
pc = 1
photo = 1
rows_strip = 'fffffff'xl	;Essentially infinity

for i=0,count-1 do begin	;Print each directory entry
	value = tiff_read_field(i, tag, lun)  ;Get each parameter
	case tag of		;Decode the tag fields, other tags could be added
256:	width = value
257:	length = value
258:	bits_sample = value
259:	compression = value
262:	Photo = value
273:	StripOff = value
274:	Ord = value
277:	samples_pixel = long(value)
278:	Rows_strip = value
279:	Strip_bytes = value
284:	PC = value
320:	ColorMap = value
else:   value = 0		;Throw it away
	endcase
endfor	

;	Do a cursory amount of checking:
	if bits_sample(0) ne 8 then $
		message,'TIFF_READ: bits_sample must be 8'
	if compression ne 1 then $
		message,'TIFF_READ: Images must be un-compressed'
;	if photo ge 4 then $
;		message,'TIFF_READ: Photometric Interpretation must be 0 to 3.'
	if (pc eq 2) and (samples_pixel ne 3) then $
		message,'TIFF_READ: RGB data must have 3 SamplesPerPlane'
	
nbytes = width * long(length)
strips_image = (length + rows_strip -1) / rows_strip


if pc eq 1 then begin	;Planar Config...., simple
	if samples_pixel eq 1 then image = bytarr(width, length, /nozero) $
	else image = bytarr(samples_pixel, width, length, /nozero)
	if strips_image eq 1 then begin	;Quick way?
		point_lun, lun, stripoff(0)   ;1st image data
		readu, lun, image 	;Yes....
	endif else begin		;1 strip at a time....
		tmp = 0b
		for i=0L, strips_image-1 do begin
			point_lun, lun, stripoff(i)
			if n_elements(tmp) ne Strip_bytes(i) Then $
			    tmp = bytarr(Strip_bytes(i), /nozero)
			readu, lun, tmp
			image(samples_pixel * width * i * rows_strip) = tmp
			endfor
	endelse
	if n_elements(ColorMap) gt 0 then begin	;Color map present?
	   if n_elements(ColorMap) eq 768 then begin
		r = ishft(ColorMap(0:255), -8) ;Remove and scale
		g = ishft(ColorMap(256:511), -8)
		b = ishft(ColorMap(512:767), -8)
	    endif else message,'TIFF_READ: color map has wrong # of elements'
	endif
endif else begin			;PC = 2, = interleaved by image
	l = 0
	tmp = 0b
	for band = 0,2 do begin		;Read each image
		image = bytarr(width, length, /nozero)
		for i=0L, strips_image-1 do begin
			point_lun, lun, stripoff(l)
			if n_elements(tmp) ne Strip_bytes(l) then $
			  tmp = bytarr(strip_bytes(l), /nozero)
			readu, lun, tmp
			image(width * i * rows_strip) = tmp
			l = l + 1
			endfor		;Each strip
		case band of
		0: r = image
		1: g = image
		2: b = image
		endcase
		image = 0
	endfor				;Each band
endelse					;PC = 2
	
free_lun, lun
return, image
end
