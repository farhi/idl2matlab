; $Id: read_xwd.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION READ_XWD, FILE_NAME, RED, GREEN, BLUE
;+
; NAME:
;	READ_XWD
;
; PURPOSE:
;	Read the contents of files created by the XWD (X Windows Dump)
;	command and return the image and color table vectors in the form of
;	IDL variables.
;
; CATEGORY:
;	Input/Output.
;
; CALL:
;	Result = READ_XWD(File_Name [, R, G, B])
;
; INPUTS:
;   File_Name:	Scalar string giving the name of the xwd file to read
;
; OUTPUTS:
;	READ_XWD returns a 2D byte array containing the image.  If the file
;	cannot be open or read, the return value is zero.
;
; OPTIONAL OUTPUT PARAMETERS:
;     R, G, B:	The variables to contain the Red, Green, and Blue color
;		vectors if the XWD file contains color tables.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	I/O is performed.
;
; RESTRICTIONS:
;	This function is intended to be used only on files containing
;	8-bit pixmaps.  It is not intended to be used with all XWD files.
;	No guarantees are made that all XWD files will work with this routine.
;	This routine will not work with XWD files with version less than 6.
;
; PROCEDURE:
;	The header is read into a structure and the bytes are reversed
;	if necessary.  Then the colormap and image portions of the
;	file are read into their respective variables.
;
; EXAMPLE:
;	To open and read the X Windows Dump file named "my.xwd" in the current
;	directory, store the image in the variable IMAGE1, and store the color 
;	vectors in the variables, R, G, and B, enter:
;
;		IMAGE1 = READ_XWD("my.xwd", R, G, B)
;
;	To load the new color table and display the image, enter:
;
;		TVLCT, R, G, B
;		TV, IMAGE1
;
;
; MODIFICATION HISTORY:
;	September, 1990 DMS and SMR, Research Systems, Inc.
;-


on_error, 2				; return to caller on error
on_ioerror, error			; goto error section on error
openr,unit, FILE_NAME, /get_lun		; open the file

hdr = { XWD_FILE_HEADER, $		; definition of xwd header struct
	header_size : 0L, $
	file_version : 0L, $
	pixmap_format: 0L, $
	pixmap_depth: 0L, $
	pixmap_width: 0L, $
	pixmap_height: 0L, $
	xoffset: 0L, $
	byte_order: 0L, $
	bitmap_unit: 0L, $
	bitmap_bit_order: 0L, $
	bitmap_pad: 0L, $
	bits_per_pixel: 0L, $
	bytes_per_line: 0L, $
	visual_class: 0L, $
	red_mask: 0L, $
	green_mask: 0L, $
	blue_mask: 0L, $
	bits_per_rgb: 0L, $
	colormap_entries: 0L, $
	ncolors: 0L, $
	window_width: 0L, $
	window_height: 0L, $
	window_x: 0L, $
	window_y: 0L, $
	window_bdrwidth: 0L }

Color = { XWDColor, $,			;The xwd color element structure
	pixel: 0L, $
	red: 0,$
	green: 0, $
	blue: 0, $
	flags: 0B, $
	pad: 0B }	

readu, unit, hdr			; read the header
test = 1L				; do a test to check the system's byte
byteorder, test, /htonl			; order. If needed switch the byte 
if (test ne 1L) then $			; order to correspond to network byte
   byteorder, hdr, /htonl		; order

point_lun, unit, hdr.header_size	; seek to beginning of colormap

colormap = replicate(color, hdr.ncolors)
readu, unit, colormap			; read the colormap entries from file

if hdr.pixmap_format ne 2 then begin
	message,'READ_XWD: can only handle Z format pixmaps.'
	endif


IMAGE = bytarr(hdr.pixmap_width, hdr.pixmap_height)  ;Create the pixmap
line = bytarr(hdr.bytes_per_line)		; get one scan line

for i=0, hdr.pixmap_height-1 do begin	;Read each scan line
	readu, unit, line		
	if hdr.pixmap_width eq hdr.bytes_per_line then $
		IMAGE(0,hdr.pixmap_height - i -1) = line $
	else image(0, hdr.pixmap_height - i - 1) = line(0:hdr.pixmap_width-1)
	endfor

free_lun, unit				; close and free the file

pixel = colormap.pixel			; Extract pixel value separately
if test ne 1L then begin		; switch byte order if needed
	byteorder, colormap, /htons	; Swap whole structure
	byteorder, pixel, /htonl	; & pixel separately
	endif
RED = ishft(colormap.red, -8)
GREEN = ishft(colormap.green, -8)
BLUE = ishft(colormap.blue, -8)

return, IMAGE

;------------ the error message ------------

error: message, "READ_XWD: Error reading file: " + FILE_NAME
end


