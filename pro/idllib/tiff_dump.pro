; $Id: tiff_dump.pro,v 1.3 1995/02/02 18:31:47 dave Exp $

; Copyright (c) 1991-1993. Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;
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



pro tiff_basic		;Just load the stuff....
a=1
end

pro tiff_dump_field, index, lun	;Return contents of field index
common tiff_com, order, ifd, count

on_error,2                      ;Return to caller if an error occurs
TypeLen = [0, 1, 1, 2, 4, 8] ;lengths of tiff types, 0 is null type for indexin
TypeName = [ 'Undefined', 'Byte', 'Ascii', 'Short', 'Long', 'Rational' ]
TagIndex = [258,320, 301, 259,291,290,257,256,254,262,284,$
	317,296,278,277,279,273,282,283,315,306,316,270,271,272,305,274,255]
TagName = ['BitsPerSample', 'ColorMap', 'ColorResponseCurves','Compression',$
	'GrayResponseCurve','GrayResponseUnit','ImageLength','ImageWidth', $
	'NewSubfileType','PhotometricInterpretation','PlanarConfiguration',$
	'Predictor','ResolutionUnit','RowsPerStrip','SamplesPerPixel',$
	'StripByteCounts','StripOffsets','XResol','Yresol',$
	'Artist','DateTime','HostComputer','ImageDescription','Make','Model',$
	'Software','Orientation','SubfileType' ]

ent = ifd(index * 12: index * 12 + 11)  ;Extract the ifd
tag = tiff_int(ent, 0)
typ = tiff_int(ent, 2)
tname = TypeName(typ)
cnt = tiff_long(ent, 4)
if tag ge 32768L then begin		;Private tag?
  tag = 65536L + tag		;Unsigned long
  name = '<PrivateTag>'
endif else begin
  i = where(tag eq TagIndex, j)	;Look up name...
  if j gt 0 then name = TagName(i(0)) else name = '<NoName>'
  if (typ le 0) or (typ gt 5) then $
	message,'Illegal type code, ifd = '+string(index)
endelse

print,'*** ',name, ', tag = ', tag, ', ', tname, ', Count = ',cnt

nbytes = cnt * TypeLen(typ)
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
	print, val(0: (cnt-1) < 15)
ENDIF ELSE BEGIN			;Scalar
        CASE typ OF
	   1: val = ent(8)
  	   2: val = string(ent(8:8+(cnt>1)-1))
	   3: val = tiff_int(ent,8)
	   4: val = tiff_long(ent,8)
        ENDCASE
	print,'  ', val
     ENDELSE
end


pro tiff_dump, file
;+
; NAME:
;	TIFF_DUMP
;
; PURPOSE:
;	Dump the Image File Directories of a TIFF File.  This procedure is
;	used mainly for debugging.
;
; CATEGORY:
;	Input/output.
;
; CALLING SEQUENCE:
;	TIFF_DUMP, Filename
;
; INPUTS:
;    Filename:	string containing the name of file to read.
;		The default extension is ".TIF".
;
; OUTPUTS:
;	All output is to the terminal.  Each TIFF Image File Directory
;	entry is printed.
;
; COMMON BLOCKS:
;	TIFF_COM.  Only for internal use.
;
; SIDE EFFECTS:
;	A file is read.
;
; RESTRICTIONS:
;	Not all of the tags have names encoded.
;	In particular, Facsimile, Document Storage and Retrieval,
;	and most no-longer recommended fields are not encoded.
;
; PROCEDURE:
;	The TIFF file Header and the IFD (Image File Directory) are read
;	and listed.
;
; MODIFICATION HISTORY:
;	DMS, Apr, 1991.  Extracted from TIFF_READ.
;	DMS, Dec, 1994.	 Added private tags
;
;-

common tiff_com, order, ifd, count

;on_error,2                      ;Return to caller if an error occurs

openr,lun,file, error = i, /GET_LUN, /BLOCK

if i lt 0 then begin ;OK?
	if lun gt 0 then free_lun,lun
	lun = -1
	message, 'Error opening ' + file
	endif

hdr = bytarr(8)			;Read the header
readu, lun, hdr

typ = string(hdr(0:1))		;Either MM or II
if (typ ne 'MM') and (typ ne 'II') then begin
	print,'TIFF_READ: File is not a Tiff file: ', file
	return
	endif
order = typ eq 'MM'  		;1 if Motorola 0 if Intel (LSB first or vax)
endian = byte(1,0,2)		;What endian is this?
endian = endian(0) eq 0		;1 for big endian, 0 for little
order = order xor endian	;1 to swap...

print,'Tiff File: byte order=',typ, ',  Version = ', tiff_int(hdr,2)

offs = tiff_long(hdr, 4)	;Offset to IFD

point_lun, lun, offs		;Read it

a = bytarr(2)			;Entry count array
readu, lun, a
count = tiff_int(a,0)		;count of entries
print,count, ' directory entries'
ifd = bytarr(count * 12)	;Array for IFD's
readu,lun, ifd			;read it

old_tag = 0			;Prev tag...
for i=0,count-1 do begin	;Print each directory entry
	tag = tiff_int(ifd, i*12)
	if tag lt old_tag then $
		print,'Error in TIFF file, Directory entries out of order ****'
	old_tag = tag
	tiff_dump_field, i, lun
	endfor	

free_lun, lun
lun = -1
end
