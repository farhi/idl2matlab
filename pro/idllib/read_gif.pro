; $Id: read_gif.pro,v 1.3 1995/01/11 21:03:02 dave Exp $

;
;  GifReadByte
;	Read a single byte out of the given file
;
FUNCTION GifReadByte, unit
	ch	= 0b
	READU, unit, ch
	RETURN, ch
END

PRO READ_GIF, FILE, IMAGE, R, G, B
;
;+
; NAME:
;	READ_GIF
;
; PURPOSE:
;	Read the contents of a GIF format image file and return the image
;	and color table vectors (if present) in the form of IDL variables.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	READ_GIF, File, Image [, R, G, B]
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
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	This routine only reads in the first image in a file (the format
;	allows many). Local colormaps are not supported.
;	Only 8 bit images are supported.
;
; EXAMPLE:
;	To open and read the GIF image file named "foo.gif" in the current
;	directory, store the image in the variable IMAGE1, and store the color
;	vectors in the variables R, G, and B, enter:
;
;		READ_GIF, "foo.gif", IMAGE1, R, G, B
;
;	To load the new color table and display the image, enter:
;
;		TVLCT, R, G, B
;		TV, IMAGE1
; 
; MODIFICATION HISTORY:
;	Written June 1992, JWG
;	Added GIF89a and interlaced format, Jan, 1995, DMS.
;	
;-
; Copyright (c) 1992, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

;	Define GIF header (and screen descriptor)

on_error, 2		;Return to caller on errors
h	= { 	magic:bytarr(6),				$
		width_lo:0b, width_hi:0b,			$
		height_lo:0b, height_hi:0b,			$
		screen_info:0b, background:0b, reserved:0b }

OPENR, unit, file, /GET_LUN, /BLOCK, ERROR=i
if i ne 0 then message, 'Error occured opening file: '+file

READU, unit, h		;Read gif header

;	Check Magic in header: GIF87a or GIF89a.
gif	= STRING(h.magic(0:2))
vers = STRING(h.magic(3:5))

IF gif NE 'GIF' OR (vers ne '87a' and vers ne '89a') THEN BEGIN
	MESSAGE, 'File ' + file + ' is not a GIF file'
	FREE_LUN, unit		
	RETURN
ENDIF

; Allocate an array to hold the image in

width	= h.width_hi * 256 + h.width_lo
height	= h.height_hi * 256 + h.height_lo
image	= BYTARR(width, height, /NOZERO)

;	Find out how big the color map is

bits_per_pixel	= (h.screen_info AND 'F'X) + 1
color_map_size	= 2 ^ bits_per_pixel

;	Read in the colormap (optional)

IF (h.screen_info AND '80'X) NE 0 THEN BEGIN
	map	= BYTARR(3,color_map_size, /NOZERO)
	READU, unit, map
	map	= transpose(map)
	r	= map(*,0)
	g	= map(*,1)
	b	= map(*,2)
	ENDIF


;	Image header declaration

ihdr	= { 	left:0, top:0,			$	; we read this but
		iwidth:0, iheight:0,		$	; mostly we ignore
		image_info:0b }				; its content

;	Read the image description

while 1 do begin			;Read till we get a terminator
  cmd = GifReadByte(unit)		;Loop thru commands in file.
  CASE STRING(cmd) OF

';':	BEGIN				; GIF terminator (0x3b)
	FREE_LUN, unit		
	END

',':	BEGIN				; Image description (0x2c)
	READU,unit,ihdr

	; Check for file formats we don't support
	; We don't support local colormaps

	if (ihdr.image_info AND '80'X) NE 0 THEN begin  ;Local color map
	   lcolor_map_size = 2^((ihdr.image_info and 7) + 1)
	   junk = bytarr(3, lcolor_map_size, /NOZERO)
	   readu, unit, junk
	   message,'Local colormaps ignored', /CONTINUE
	   endif

	;	Now call special GIF-LZW routine hidden within IDL
	;	to do the ugly serial bit stream decoding

	DECODE_GIF,unit,image		; magic
	FREE_LUN, unit			; done

	;	Reorder rows in an interlaced image

	if (ihdr.image_info AND '40'X) NE 0 THEN BEGIN
	    l = lindgen(height)	;Row indices...
		;Giff interlace ordering
	    p = [l(where(l mod 8 eq 0)), l(where(l mod 8 eq 4)), $
		 l(where(l mod 4 eq 2)), l(where(l and 1))]
	    image2 = bytarr(width, height, /NOZERO)
	    h1 = height-1
	    for i=0, h1 do image2(0, h1-p(i)) = image(*,h1-i)
	    image = temporary(image2)
	ENDIF

	RETURN
	END

'!':	BEGIN				;Gif Extention block (ignored) (0x21)
	label = GifReadByte(unit)	;toss extension block label
	repeat begin			;read and ignore blkss
	  blk_size = GifReadByte(unit)	;block size
	  if blk_size ne 0 then begin
		junk = BYTARR(blk_size, /NOZERO)
		READU, unit, junk
		endif
	endrep until blk_size eq 0
	ENDCASE

ELSE:	message,'Unknown GIF keyword in ' + file
ENDCASE
endwhile

END
