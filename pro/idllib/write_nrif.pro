; $Id: write_nrif.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO WRITE_NRIF, FILE, IMAGE, R, G, B

;+
; NAME:
;	WRITE_NRIF
;
; PURPOSE:
;	Write an IDL image and color table vectors to an
;	NCAR Raster Interchange Format (NRIF) rasterfile.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	WRITE_NRIF, File, Image, [R, G, B]
;
; INPUTS:
;	File:	A string containing the name of the rasterfile to write.
;
;	Image:	The byte array to be output.  If Image is dimensioned (n,m) an
;		8-bit NCAR Raster File with color tables is output.  If Image 
;		is dimensioned (3,n,m), a 24-bit NCAR Raster File is output, 
;		where each byte triple represents the red, green, and blue 
;		intensities at (n,m) on a scale from 0 to 255.  In either 
;		case, IMAGE must be a byte array.  The NRIF image will be 
;		rendered from bottom to top, in accordance with IDL standards,
;		so the !ORDER variable should not be changed from its default 
;		value of zero.
;
; OPTIONAL INPUT PARAMETERS:
;      R, G, B:	The Red, Green, and Blue color vectors to be used as a color
;		table with 8-bit images.  If color vectors are supplied, they
;		are included in the output (8-bit images only).  If color 
;		vectors are not supplied, the color vectors established by 
;		LOADCT or PALETTE are included in the output.  If LOADCT or 
;		PALETTE have not yet been used to define color vectors, 
;		"LOADCT, 0" is called to load the standard grayscale color
;		table.
;
;		This routine does not recognize color vectors loaded directly 
;		using TVLCT, so if a custom color table is desired and it is 
;		not convenient to use PALETTE, include the R, G, and B vectors
;		that were used to create the color table.
;
; OUTPUTS:
;	No explicit outputs.  The specified File will contain header 
;	information, color vectors (8-bit images only), and the image, in
;	NCAR Raster Interchange Format (NRIF).
;
; COMMON BLOCKS:
;	COLORS:  The IDL color table common block.
;
; SIDE EFFECTS:
;	If R, G, and B aren't supplied and color tables haven't been previously
;	established by LOADCT or PALETTE, this routine calls "LOADCT, 0" to 
;	load the standard gray scale color table.
;
; RESTRICTIONS:
;	This routine only writes 8 or 24-bit deep rasterfiles of types
;	"Indexed Color" (for 8-bit) and "Direct Color integrated" for 24-bit.
;	The color map is included only for 8-bit files.
;
; FURTHER INFORMATION:
;	See the document "NCAR Raster Interchange Format and TAGS Raster
;	Reference Manual", available from the Scientific Computing Division,
;	National Center for Atmospheric Research, Boulder, CO, 80307-3000,
;	for the structure of NCAR Raster Interchange Format (NRIF) files.
;
; MODIFICATION HISTORY:
;	Written February, 1991 by Stan Solomon, LASP, University of Colorado.
;	(Adapted from the WRITE_SRF procedure.)
;-
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

; Check the arguments:
on_error, 1
n_params = n_params()
if ((n_params ne 2) and (n_params ne 5))then $
  message, "usage: WRITE_NRIF, file, image, [r, g, b]"

; Check that image has the required attributes:
img_size = size(image)
if ( (img_size(0) ne 2) and (img_size(0) ne 3) ) then  $
  message, 'Image must be a matrix.'
if ( (img_size(0) eq 3) and (img_size(1) ne 3) ) then $
  message, '24 bit images must be dimensioned (3,n,m)'
if ( (img_size(0) eq 2) and (img_size(3) ne 1) ) then $
  message, 'Image must be a byte array.'
if ( (img_size(0) eq 3) and (img_size(4) ne 1) ) then $
  message, 'Image must be a byte array.'

; Determine if this is an 8-bit or 24-bit image:
if (img_size(0) eq 3) then begin
  depth = 24L
  cols = img_size(2)
  rows = img_size(3)
endif else begin
  depth = 8L
  cols = img_size(1)
  rows = img_size(2)
endelse

; Load color vectors into color map if supplied, otherwise use loadct vectors:
if (n_params eq 5) then begin
  r_size = size(r)
  g_size = size(g)
  b_size = size(b)
  if ((r_size(0) + g_size(0) + b_size(0)) ne 3) then $
    message, "R, G, & B must all be 1D vectors."
  if ( (r_size(1) ne g_size(1)) or (r_size(1) ne b_size(1)) ) then $
    message, "R, G, & B must all have the same length."
  map_len = r_size(1) * 3L
  rmap = byte(r)
  gmap = byte(g)
  bmap = byte(b)
endif else begin
  if (n_elements(r_orig) eq 0) then loadct, 0
  tmp = size(r_orig)
  map_len = tmp(1) * 3L
  rmap = byte(r_orig)
  gmap = byte(g_orig)
  bmap = byte(b_orig)
endelse

; Construct header:
magic= 'NRIF'
flags= 2L
width= cols
height= rows
cmtlen= 0L
dev= 0L
devlen= 0L
ibits= 8L
cbits= 8L
ncolor= map_len/3L
if (depth eq 8) then begin
  encoding= 2L
  enclen= map_len+12L
endif
if (depth eq 24) then begin
  encoding= 4L
  enclen= 4L
endif

; If this is a small-endian machine, reverse the byte order of longword
; integers in the header:
test = byte(1L,0,4)
if (test(0) eq 1b) then begin
  flags = long_reverse(flags)
  width = long_reverse(width)
  height = long_reverse(height)
  cmtlen = long_reverse(cmtlen)
  dev = long_reverse(dev)
  devlen = long_reverse(devlen)
  encoding = long_reverse(encoding)
  enclen = long_reverse(enclen)
  ibits = long_reverse(cbits)
  ncolor = long_reverse(ncolor)
  cbits = long_reverse(cbits)
endif

; Write header:
openw, unit, file, /stream, /get_lun
if (depth eq 8) then begin
  writeu, unit, magic, flags, width, height, cmtlen, dev, devlen, $
    encoding, enclen, ibits, ncolor, cbits
  writeu, unit, rmap, gmap, bmap
endif
if (depth eq 24) then begin
  writeu, unit, magic, flags, width, height, cmtlen, dev, devlen, $
    encoding, enclen, cbits
endif

; Write image:
writeu, unit, image

; Close file and free unit:
free_lun, unit

end

