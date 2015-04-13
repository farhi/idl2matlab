; $Id: read_pict.pro,v 1.2 1994/07/12 23:39:52 dave Exp $

pro read_pict_item, unit, data
; procedure reverse from big-endian to little endian or vis a versa.
; On entry:
;  DATA should be defined, and items are read from unit if it is > 0.
;  If Unit is < 0, DATA is already read.
;  The common block, write_pict_rev should already be set up.

common write_pict_rev, rev

if unit ge 0 then readu, unit, data		;Read it???
if rev eq 0 then return		;Nothing to do...
s = size(data)			;What type of data?
case s(s(0)+1) of
2:	byteorder, data, /SSWAP	;Swap shorts
3:	byteorder, data, /LSWAP ;longs
4:	byteorder, data, /LSWAP ;Float
5:	BEGIN			;Double
	n = n_elements(data)
	data = byte(data, 0, n*8)
	for i=0L,8*(n-1),8 do for j=0L,7 do data(i+j) = data(i+7-j)
	data = double(data, 0, n)
	ENDCASE
6:	byteorder, data, /LSWAP	;Complex => floats
8:	BEGIN			;Structure...
	for i=0, n_tags(data)-1 do begin	;Do each tag individually.
	   tmp = data.(i)
	   read_pict_item, -1, tmp
	   data.(i) = tmp
	   ENDFOR
	ENDCASE
else:				;Do nothing for bytes & strings
ENDCASE
end	


function UnPackData, width, height, unit

;	Function UnPackData
;	This function is used by the READ_PICT user library routine and
;	it performs the Quickdraw style run length decoding for PICT
;	files.  IMAGE is the unpacked data.  
;	NOPROGRESS is a keyword and when set, it suppressed the display of
;	the progress of the packing.


IMAGE = bytarr(width, height)
for scanline = height-1,0, -1 do begin
  scanlinecount = 0
  packlineindex = 0
  if(width gt 250) then count = 0 else count = 0b
  read_pict_item, unit, count
  packline = bytarr(count)
  readu, unit, packline
  while(scanlinecount lt width) do begin
    packcount = packline(packlineindex)
    packlineindex = packlineindex + 1
    if (packcount ge 128) then begin		; bits are packed
      packcount = 256 - packcount
      repeatedvalue = packline(packlineindex)
      packlineindex = packlineindex + 1
      IMAGE(scanlinecount:scanlinecount + packcount, scanline) = repeatedvalue
    endif else begin
      IMAGE(scanlinecount:scanlinecount + packcount,		$
	       scanline) = packline(packlineindex:packlineindex + packcount)
      packlineindex = packlineindex + packcount + 1
    endelse
    scanlinecount = scanlinecount + packcount + 1
  endwhile
  if(scanlinecount ne width) then begin
    print, "error in reading scanline ",scanline
    stop
  endif
endfor

return, IMAGE

end


PRO READ_PICT, filename, resultimage, r, g, b, DEBUG = DEBUG

;+
; NAME:		READ_PICT
; PURPOSE: 	Reads limited types of image files written in the PICT
;		Version 2 Format.  This format is used by the Apple 
;		Macintosh Computers.
; CATEGORY:	
; CALLING SEQUENCE:
;	READ_PICT, FILE, IMAGE		;Reads PICT file into IMAGE
;	READ_PICT, FILE, IMAGE, R, G, B	;Reads Image and loads color vectors
; INPUTS:
;       FILE = Scalar string giving the name of the PICT file to read.
;       IMAGE = 2D matrix to be input.  
; OPTIONAL INPUT PARAMETERS:
;       R, G, B = The Red, Green, and Blue color vectors to be read
;               with IMAGE.  If not specified, the color table associated
;		with the picture is ignored.
; OUTPUTS:
;	IMAGE - the image that is read in from the file.
;	R, G, B - the color vectors from the PICT file.
; SIDE EFFECTS:
;	A file is opened, UnPackData is called, I/O is performed
; RESTRICTIONS:
;	Only creates Version 2 PICT files.  Not intended to read all PICT
;	files.  Known to work with IDL PICT files written with write_pict
;	routine in IDL.
; PROCEDURE:
;	Read in the header, size, and the following quickdraw opcodes.
;
; MODIFICATION HISTORY:
;	Written 19 November 1990, Steve Richards.
;		19 November 1992, Steve Richards, Fixed a problem where
;			the color vectors returned were integer and not
;			bytes.
;		Jul 1994,  DMS, RSI.  Added code for both big and little
;			endian byte ordering.  Previous version would not
;			work on little endian machines.
;-

common write_pict_rev, rev

ON_ERROR, 2
i  = byte(1,0,2)		;Test byte ordering of this machine
rev = i(0) eq 1b 		;TRUE to reverse for little endian

if keyword_set(DEBUG) then DEBUG = 1 else DEBUG = 0
hdr = bytarr(512)
imagesize = 0
Rect = {rect, top:0, left:0, bottom:0, right:0}

openr, unit, filename, /get_lun
readu, unit, hdr
read_pict_item, unit, imagesize
read_pict_item, unit, Rect
image = bytarr(Rect.right - Rect.left, Rect.bottom - Rect.top)
done = 0
count = 0

while(done eq 0) do begin
  opcode = 0
  read_pict_item, unit, opcode
  count = count + 1
  
  case opcode of
    0: begin					;nop
	 if(DEBUG ne 0) then print, "---NOP Opcode"
       end

    1: begin					;clip region size
	 if(DEBUG ne 0) then print, "---Clip Opcode"
	 regionsize = 0
	 read_pict_item, unit, regionsize
	 if regionsize ne 10 then begin
	   print, "Non rectangular regions not supported"
	   print, "Region is of size ",regionsize
	   done = 1
         endif else begin
	   clipregion = Rect
	   read_pict_item, unit, clipregion
           if(DEBUG ne 0) then print, "Clip Region is ",clipregion
	 endelse
       end

    17:	begin					;version number
	  if(DEBUG ne 0) then print, "---Version Opcode"
	  versionnumber = 0b
	  lowbyte = 0b
	  readu, unit, versionnumber
	  readu, unit, lowbyte
	  if(DEBUG ne 0) then $
	    print, "  Reading PICT file with version number ",versionnumber, $
		   "   and lowbyte", lowbyte
        end

    30: begin					;default highlight operation
	  if(DEBUG ne 0) then print, "---DefHilite Opcode"
	end	  

    152: begin					;packed copybits operation
	   if(DEBUG ne 0) then print, "---PackBitsRect Opcode"
	   pixMap = {pixMapstr,	rowBytes:0,		$
				Boundtop:0,		$
				Boundleft:0,		$
				Boundbottom:0,		$
				Boundright:0,		$
				version:0,		$
				packType:0,		$
				packSize:0L,		$
				hRes:0L,		$
				vRes:0L,		$
				pixelType:0,		$
				pixelSize:0,		$
				cmpCount:0,		$
				cmpSize:0,		$
				planeBytes:0L,		$
				pmTable:0L,		$
				pmReserved:0L}
	   read_pict_item, unit, pixMap
	   if(DEBUG ne 0) then print, "read pixMap"
	   pixMap.rowBytes = pixMap.rowBytes and 32767	;strip high bit
	   coltable = {coltablestr,	ctseed:0L,	$
					transIndex:0,	$
					ctSize:0}
	   read_pict_item, unit, coltable
	   if(DEBUG ne 0) then help, /str, coltable
	   colors = intarr(4, coltable.ctSize + 1)
	   read_pict_item, unit, colors
	   if(DEBUG ne 0) then print, "read colors"
	   colors = byte(colors / 256)
	   r = reform(colors(1,*), n_elements(colors(1,*)), /overwrite)
	   g = reform(colors(2,*), n_elements(colors(2,*)), /overwrite)
	   b = reform(colors(3,*), n_elements(colors(3,*)), /overwrite)
	   srcRect = Rect
	   read_pict_item, unit, srcRect
	   if(DEBUG ne 0) then print, "read srcRect"
	   dstRect = Rect
	   read_pict_item, unit, dstRect
	   if(DEBUG ne 0) then print, "read dstRect"
	   mode = 0
	   read_pict_item, unit, mode
	   if(DEBUG NE 0) THEN PRINT, mode, "is the mode"
	   if(DEBUG ne 0) then print, "read mode"
	   if(pixMap.rowBytes lt 8) then begin	;data is unpacked
	     if(DEBUG ne 0) then print, "Data is unpacked"
	     datasize = pixMap.rowBytes*(pixMap.Boundbottom -	$
					 pixMap.Boundtop)
	     image = bytarr(pixMap.rowBytes*				$
			    (pixMap.Boundbottom - pixMap.BoundTop))
	     readu, unit, image
	   endif else begin			;data is packed
	     if(DEBUG ne 0) then print, "Data is packed"
	     image = UnPackData(pixMap.rowBytes,			$
				pixMap.Boundbottom - pixMap.BoundTop, unit)
	   endelse
	   done = 1
	 end

    154: begin		;reserved apple instruction
 	   if(DEBUG ne 0) then print, "---Reserved Apple Opcode 0x009A"
           datasize = 0
	   read_pict_item, unit, datasize
           if(datasize ne 0) then begin
	     trash = bytarr(datasize)
	     readu, unit, trash
	     if(DEBUG ne 0) then print, "  datasize ",datasize, "      data", $
					trash
	   endif else begin
	     if(DEBUG ne 0) then print, "  datasize = 0"
	   endelse
	 end

    255: begin					;end of pict file opcode
	   if(DEBUG ne 0) then begin
	     print, "---opEndPic Opcode"
 	     stuff = bytarr(16)
	     readu, unit, stuff
	     print, stuff, format = '("  Rest in HEX ",16(" ",Z2.2," "))
	     print, stuff, format = '("  Rest in DEC ",16(I3.3," "))
	   endif	   
	   done = 1
	 end

    3072: begin					;header operation
	    headerdata = bytarr(24)
	    readu, unit, headerdata
	    if(DEBUG ne 0) then print, "---HeaderOp Opcode"
	    if(DEBUG ne 0) then $
	      print, headerdata, $
		     format = '("  Header data is ",3(/"    ",8(Z2.2," ")))'
	  end

    else: begin					;unknown opcode
	    print, opcode, opcode, $
		   format = '("Stopped on unknown opcode $",Z4.4," or ",I)'
	    if(DEBUG ne 0) then begin 
	      print, "Dump follows"
	      opcodes = bytarr(10)
              readu, unit, opcodes
	      print, opcodes, format = '(10(I3,"  "))'
	    endif
	    done = 1
	  end
  endcase

endwhile

free_lun, unit
resultimage = image

end



