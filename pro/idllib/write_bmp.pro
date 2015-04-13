; $Id: write_bmp.pro,v 1.6 1994/10/31 16:09:58 dave Exp $

PRO WRITE_BMP, File, Image, Red, Green, Blue, $
    FOUR_BIT = four_bit, Ihdr = Ihdr, HEADER_DEFINE = h
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	WRITE_BMP
;
; PURPOSE:
;   	This procedure writes a Microsoft Windows Version 3 device
;	independent bitmap file (.BMP).
;
; CATEGORY:
;   	Input/Output.
;
; CALLING SEQUENCE:
;   	WRITE_BMP, File, Image [, R, G, B]
;
; INPUTS:
;   	File:	   The full path name of the bitmap file to write.
;   	Image:	   The array to write into the new bitmap file. The array
;	 	   should be scaled into a range of bytes for 8 and 24
;		   bit deep images. Scale to 0-15 for 4 bit deep images.
;       	   If the image has 3 dimensions and the first dimension
;		   is 3, a 24 bit deep bitmap file is created.
;		   NOTE: for 24 bit images, color interleaving is blue,
;		   green, red: image(0,i,j) = blue, image(1,i,j) = green, etc.
;
; OPTIONAL INPUTS:
;   	R, G, B:   Color tables. If omitted, the colors loaded in the
;		   COLORS common block are used.
;
; KEYWORD PARAMETERS:
;   	FOUR_BITS: Set this keyword to write as a four bit device
;	  	   independent bitmap. If omitted or zero, an eight bit
;		   deep map is written.
;   	IHDR:	   { BITMAPINFOHEADER } structure containing the file header
;       	   fields that are not obtained from the image parameter.
;       	   (The only fields that the user can set are:
;		   bi{XY}PelsPerMeter, biClrUsed, and biClrImportant.)
;
; OUTPUTS:
;   	No explicit outputs.
;
; KEYWORD OUTPUT PARAMETERS:
;   	HEADER_DEFNIE: Returns an empty BITMAPINFOHEADER structure,
;		       containing zeroes. No other actions are performed.
;		       This structure may be then modified with the
;		       pertinent fields and then passed in via the Ihdr
;       	       keyword parameter. See the Microsoft Windows
;		       Programmers Reference Guide for a description of
;		       each field in the structure. NOTE: this parameter
;		       must be defined before the call.  e.g.:
;           		h = 0
;           		WRITE_BMP, HEADER_DEFINE = h
;
; COMMON BLOCKS:
;   	COLORS:    Used with 4- and 8-bit images if no colors are specified.
;
; SIDE EFFECTS:
;   	IO is performed.
;
; RESTRICTIONS:
;   	Does not handle 1-bit images or compressed images.
;   	Is not fast for 4-bit images. Works best on images where the
;   	number of bytes in each scan line is evenly divisible by 4.
;
; PROCEDURE:
;   	Straightforward. Will work on both big endian and little endian
;	machines.
;
; EXAMPLES:
;       Pseudo screen dump from the current window:
;         WRITE_BMP, 'test.bmp', TVRD()
;
;       Scale an image to 0-15, and then write a four bit BMP file,
;       using a gray scale color table:
;         r = BYTSCL(INDGEN(16))   ;Ramp from 0 to 255.
;         WRITE_BMP, 'test.bmp', BYTSCL(Image, MAX=15), r, r, r, /FOUR
;
; MODIFICATION HISTORY:
;   DMS, RSI.   March 1993.	Original version.
;   DMS, RSI.   May, 1993.	Now works on all machines...
;-

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr


on_ioerror, bad
on_error, 2         ;Return on error


if n_elements(ihdr) eq 0 then $         ;Define our header?
  ihdr = { BITMAPINFOHEADER, $
    bisize: 0L, $
    biwidth: 0L, $
    biheight: 0L, $
    biplanes: 0, $
    bibitcount: 0, $
    bicompression: 0L, $
    bisizeimage: 0L, $
    bixpelspermeter: 0L, $
    biypelspermeter: 0L, $
    biclrused: 0L, $
    biclrimportant: 0L $
  } $
else if tag_names(ihdr, /STRUCTURE_NAME) ne "BITMAPINFOHEADER" then $
    message, 'IHDR must contain a BITMAPINFOHEADER structure'

if n_elements(h) gt 0 then begin
    h = ihdr
    return
    endif

fhdr = { BITMAPFILEHEADER, $
    bftype: bytarr(2), $        ;A two char string
    bfsize: 0L, $
    bfreserved1: 0, $
    bfreserved2: 0, $
    bfoffbits: 0L $
  }
s = size(image)
if s(0) lt 2 then message,'Image parameter must have 2 or 3 dimensions.'
nx = s(1)
ny = s(2)
if keyword_set(four_bit) then begin     ;4 bit image
    ihdr.bibitcount = 4 
    nc = 16
    bperl = (nx + 1)/2                  ;bytes / line
endif else if (s(0) eq 3) and (s(1) eq 3) then begin  ;True color
    ihdr.bibitcount = 24
    nx = ny
    ny = s(3)
    nc = 0
    bperl = 3 * nx
endif else begin                ;Plain 8 bit image
    ihdr.bibitcount = 8
    nc = 256
    bperl = nx
endelse

padded = (bperl + 3) and (not 3)   ;padded length
if padded ne bperl then pad = bytarr(padded - bperl)

fhdr.bftype = byte("BM")
ihdr.bisize = 40        ;Init some fields, size of info header

;                filehdr   ihdr          colors  
fhdr.bfoffbits = 14L +     ihdr.bisize + 4 * nc   ;Data start
ihdr.bisizeimage = padded * ny              ;bytes in image part
fhdr.bfsize = fhdr.bfoffbits + ihdr.bisizeimage  ;Total bytes in file
ihdr.biwidth = nx
ihdr.biheight = ny
ihdr.biplanes = 1
ihdr.bicompression = 0          ;For BI_RGB

openw, unit, file, /GET_LUN, /BLOCK
if (byte(1,0,2))(0) eq 0b then $	;Big endian machine
  writeu, unit, swap_endian(fhdr), swap_endian(ihdr) $  ;Swap bytes
else writeu, unit, fhdr, ihdr           ;Write the file and info headers

if nc ne 0 then begin               ;Pseudo color?
    colors = bytarr(nc, 4)          ;Transposed color array
    if n_elements(red) le 0 then begin  ;Get current color table?
        if n_elements(r_curr) eq 0 then loadct,0, /silent  ;Fake it
        n = (nc < n_elements(r_curr))-1     ;# of colors to take
        colors(0,2) = r_curr(0:n)
        colors(0,1) = g_curr(0:n)
        colors(0,0) = b_curr(0:n)
    endif else begin                ;Parameters passed in
        n = (nc < n_elements(red)) -1  ;# of colors to take
        colors(0,2) = red(0:n)
        colors(0,1) = green(0:n)
        colors(0,0) = blue(0:n)
    endelse
    writeu, unit, transpose(colors)  ;Write colors
    endif

if ihdr.bibitcount eq 4 then begin  ;4 bits/pixel?
    if padded ne bperl then pad = bytarr(padded - bperl)
    even = lindgen(nx/2) * 2
    odd = even + 1
    for i=0, ny-1 do begin
        buff = ishft(byte(image(even, i)), 4) + $
		(byte(image(odd,i)) and 15b) ;combine
        if (nx and 7) eq 0 then writeu, unit, buff $  ;No messing?
        else if nx and 1 then begin         ;Odd # of columns?
            t = ishft(byte(image(nx-1, i)), 4)       ;Last byte
            if n_elements(pad) ne 0 then writeu, unit, buff, t, pad $
            else writeu, unit, buff, t
        endif else writeu, unit, buff, pad  ;Even, but add padding
        endfor
endif else if ihdr.bibitcount eq 8 then begin          ;8 bits/pixel?
    if n_elements(pad) eq 0 then writeu, unit, byte(image) $     ;Slam dunk it
    else begin                      ;Must write line by line...
       for i=0, ny-1 do writeu, unit, byte(image(*,i)), pad  ;Write each line
    endelse
endif else begin                    ;24 bits / pixel....
    if n_elements(pad) eq 0 then writeu, unit, byte(image) $  ;Again, dunk it.
    else begin
        for i=0, ny-1 do writeu, unit, byte(image(*,*,i)), pad
    endelse
endelse

free_lun, unit                  ;All done
return

bad:  if n_elements(unit) gt 0 then free_lun, unit
Message, 'Error writing BMP file: ' + file
return
end

