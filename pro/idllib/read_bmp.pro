; $Id: read_bmp.pro,v 1.4 1994/10/10 15:38:44 dave Exp $

FUNCTION READ_BMP, File, Red, Green, Blue, Ihdr
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	READ_BMP
;
; PURPOSE:
; 	This function reads a Microsoft Windows Version 3 device
;	independent bitmap file (.BMP).
;
; CATEGORY:
;   	Input/Output
;
; CALLING SEQUENCE:
;   	Result = READ_BMP(File [, R, G, B [, IHDR]])
;
; INPUTS:
; 	File: The full path name of the bitmap file to read.
;
; OUTPUTS:
;	This function returns a byte array containing the image
;	from the bitmap file. In the case of 4-bit or 8-bit images,
;	the dimensions of the resulting array are (biWidth, biHeight);
;	for 24-bit images the dimensions are (3, biWidth, biHeight).
;	Dimensions are taken from the BITMAPINFOHEADER of the file.
;	NOTE: for 24 bit images, color interleaving is blue, green, red;
;	i.e. result(0,i,j) = blue, result(1,i,j) = green, etc.
;
; OPTIONAL OUTPUTS:
;   	R, G, B:  Color tables from the file. There 16 elements each for
;		  4 bit images, 256 elements each for 8 bit images. Not
;		  defined or used for 24 bit images.
;  	Ihdr:	  A structure containing BITMAPINFOHEADER from file.
;		  Tag names are as defined in the MS Windows Programmer's
;		  Reference Manual, Chapter 7.
;
; SIDE EFFECTS:
;   	IO is performed.
;
; RESTRICTIONS:
;   	DOES NOT HANDLE: 1 bit deep images, or compressed images.
;   	Is not fast for 4 bit images. Works best on images where the
;   	number of bytes in each scan-line is evenly divisible by 4.
;
; PROCEDURE:
;   	Straightforward. Will work on both big endian and little endian
;	machines.
;
; EXAMPLE:
;   	TV, READ_BMP('c:\windows\party.bmp', r, g, b) 	;Read & display image
;   	TVLCT, r, g, b              			;Load it's colors
;
; MODIFICATION HISTORY:
;   DMS, RSI.   March 1993.   	Original version.
;   DMS, RSI.   May, 1993.	Now works on all machines...
;-

on_ioerror, bad
on_error, 2         ;Return on error

openr, unit, file, /GET_LUN, /BLOCK
fhdr = { BITMAPFILEHEADER, $
    bftype: bytarr(2), $        ;A two char string
    bfsize: 0L, $
    bfreserved1: 0, $
    bfreserved2: 0, $
    bfoffbits: 0L $
  }
readu, unit, fhdr           ;Read the bitmapfileheader
if string(fhdr.bftype) ne "BM" then $
    message, 'File '+file+' is not in bitmap file format'

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
  }

readu, unit, ihdr

if (byte(1,0,2))(0) eq 0b then begin	;Big endian machine?
    fhdr = swap_endian(fhdr)		;Yes, swap it
    ihdr = swap_endian(ihdr)
    endif

if ihdr.bibitcount eq 1 then $
     message, 'Can''t handle monochrome images'
if ihdr.bicompression ne 0 then $
    message, 'Can''t handle compressed images'

if ihdr.bibitcount ne 24 then begin ;Pseudo color?
    colors = bytarr(4, 2^ihdr.bibitcount)
    readu, unit, colors             ;Read colors
    red = reform(colors(2, *))        ;Decommutate colors
    green = reform(colors(1, *))
    blue = reform(colors(0, *))
    endif

nx = ihdr.biwidth
ny = ihdr.biheight

point_lun, unit, fhdr.bfoffbits     ;Point to data...

if ihdr.bibitcount eq 4 then begin  ;4 bits/pixel?
    a = bytarr(nx, ny, /nozero)
    buff = bytarr(nx/2, /nozero)   ;Line buffer
    even = lindgen(nx/2) * 2
    odd = even + 1
    if nx and 1 then pad = 0B       ;interbyte padding
    i = (n_elements(buff) + n_elements(pad)) and 3  ;bytes we have
    if i ne 0 then pad = bytarr(4-i+n_elements(pad))
    for i=0, ny-1 do begin
        if n_elements(pad) ne 0 then readu, unit, buff, pad $
        else readu, unit, buff
        a(even, i) = ishft(buff, -4)
        a(odd, i) = buff and 15b
        if nx and 1 then a(nx-1, i) = ishft(pad(0), -4) ;Last odd byte?
        endfor
endif else if ihdr.bibitcount eq 8 then begin          ;8 bits/pixel?
    a = bytarr(nx, ny, /nozero)
    if (nx and 3) eq 0 then readu, unit, a $          ;Slam dunk it
    else begin                      ;Must read line by line...
       pad = bytarr(4 - (nx and 3))
       buff = bytarr(nx, /nozero)
       for i=0, ny-1 do begin       ;Each line
           readu, unit, buff, pad
           a(0,i) = buff
           endfor
    endelse
endif else begin                    ;24 bits / pixel....
    a = bytarr(3, nx, ny, /nozero)
    if ((3 * nx) and 3) eq 0 then readu, unit, a $  ;Again, dunk it.
    else begin
        pad = bytarr(4 - ((3 * nx) and 3))
        buff = bytarr(3, nx, /nozero)
        for i=0, ny-1 do begin
            readu, unit, buff, pad
            a(0,0, i) = buff            ;Insert line
            endfor
    endelse
endelse

free_lun, unit
return, a

bad:  if n_elements(unit) gt 0 then free_lun, unit
Message, 'Can''t open (or read)' + file
return, 0
end

