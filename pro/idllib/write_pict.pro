; $Id: write_pict.pro,v 1.5 1994/06/24 21:17:16 davee Exp $

pro write_pict_item, unit, data, key
;       procedure write
;       This procedure swaps bytes for short and long words on little endian
;       machines.
common write_pict_rev, rev

   if (key eq 0) or (rev eq 0) then begin
      writeu, unit, data
      return
   endif

   if (key eq 1) then begin
         x = data
         byteorder,x,/sswap
         writeu, unit, x
   endif else if (key eq 2) then begin
         x = data
         byteorder,x,/lswap
         writeu, unit, x
   endif else writeu, unit, data
end

FUNCTION PackData, image

;       Function PackData
;       This function is used by the WRITE_PICT user library routine and
;       it performs the Quickdraw style run length encoding for PICT
;       files.  Image is the unpacked data.  

imagesize = SIZE(image)
IF (imagesize(0) NE 2) THEN MESSAGE, "non two dimensional array passed to pack"
width = FIX(imagesize(1))
height = FIX(imagesize(2))

retval = BYTARR((LONG(width) + 10L) * LONG(height), /NOZERO)
retvalindex = 0L
pack = bytarr(width * 1.5, /NOZERO)
indexarray = lindgen(width)

FOR scanline = height - 1, 0, -1 DO BEGIN

  in = image(*,scanline)
  IF width GT 250 THEN packindex = 2 ELSE packindex = 1

  runstart = WHERE((in EQ in(1:*)) AND (in EQ in(2:*)), found)
  IF (found EQ 0) THEN runstart = width ELSE runstart = [runstart, width]

  inpind = 0
  endrun = -1

  FOR i = 0,N_ELEMENTS(runstart)-1 DO BEGIN     ;For each run
    IF (endrun LT runstart(i)) THEN BEGIN
      WHILE inpind LT runstart(i) DO BEGIN      ;Send out the bytes before it
        packlen = (runstart(i) - inpind) < 128  ;in 128 byte chunks
        pack(packindex) = packlen-1
        pack(packindex+1:packindex+packlen) = in(inpind:inpind+packlen-1)
        packindex = packindex + packlen + 1
        inpind = inpind + packlen
      ENDWHILE
      IF runstart(i) NE width THEN BEGIN
        runinds = WHERE((WHERE(in(runstart(i)) EQ in(runstart(i):*)) EQ $
                      indexarray))
        runsize = N_ELEMENTS(runinds)
        WHILE (runsize NE 0) DO BEGIN
          runlen = runsize < 128
          pack(packindex) = 256 - (runlen - 1)
          pack(packindex+1) = in(runstart(i))
          packindex = packindex + 2
          inpind = inpind + runlen
          endrun = inpind
          runsize = runsize - runlen
        ENDWHILE
      ENDIF
    ENDIF
  ENDFOR

  IF WIDTH GT 250 THEN BEGIN
    pack(0) = (packindex - 2) / 256
    pack(1) = (packindex - 2) mod 256
  ENDIF ELSE BEGIN
    pack(0) = packindex - 1
  ENDELSE

  retval(retvalindex:retvalindex + packindex - 1) = pack(0:packindex-1)
  retvalindex = retvalindex + packindex

ENDFOR

RETURN, retval(0:retvalindex - 1)

END
;------------------------ end of PackData routine --------------------------
  

  

PRO WRITE_PICT, FILE, IMAGE, R, G, B

;+
; NAME:         WRITE_PICT
; PURPOSE:      Writes image files with the current color palette in the PICT
;               Version 2 Format.  This format is used by Apple Macintosh 
;               Computers.
; CATEGORY:     
; CALLING SEQUENCE:
;       WRITE_PICT, FILE                ;Writes contents of current window
;       WRITE_PICT, FILE, IMAGE         ;Writes given array
;       WRITE_PICT, FILE, IMAGE, R, G, B  ;Writes array w/given color table
; INPUTS:
;       FILE = Scalar string giving the name of the PICT file to write.
;       IMAGE = 2D matrix to be output.  If IMAGE is omitted,
;         the entire current window is read into an array and written
;         to the PICT file.
; OPTIONAL INPUT PARAMETERS:
;       R, G, B = The Red, Green, and Blue color vectors to be written
;               with IMAGE.  If not specified, the current color table is used
; OUTPUTS:
;       FILE contains the image in a PICT version 2 file format.  If color
;       vectors were supplied, they are used. Otherwise, the last color tables
;       established by LOADCT are used (If LOADCT hasn't been used
;       to establish color tables yet it is used to load the B/W tables.).
; SIDE EFFECTS:
;       If R, G, and B aren't supplied and LOADCT hasn't been called yet,
;       this routine uses LOADCT to load the B/W tables.
; RESTRICTIONS:
;       Only creates Version 2 PICT files.  Only works with 8-bit displays
; PROCEDURE:
;       Write out the header, size, and the following quickdraw opcodes:
;               Version, HeaderOp, DefHilite, Clip, and PackBitsRect
;       Then pack the image data using the QUICKDRAW PackBits
;       run length encoding algorithm.
;       Packing method:
;               Each line is preceeded by a byte if the image width
;               is less than 250 or a integer otherwise.  This prefix
;               tells how many bytes are in the packed line to follow.
;               The line following this length descriptor is made up of
;               a series of runs and data as follows:
;       - Runs
;               If there is a run, the high bit of the first byte is 
;               set and the other seven bits tell how many elements are
;               in the run.  The next byte is then the value of the run.
;               Runs can only be 128 bytes in length so longer runs are
;               broken up into smaller runs if they exceed 128 bytes.
;               The smallest run is three bytes.
;       - Data
;               If there are a series of image values that differ at least
;               every two bytes, they are written out after a byte that
;               describes how many dissimilar data bytes are to follow.
;               As with runs, the length of a run of data can not be 
;               longer than 128 without setting the high bit of the
;               length descriptor so long strings of data are broken up
;               into chunks 128 bytes or smaller.
; MODIFICATION HISTORY:
;       Written 16 November 1990, Steve Richards.
;       SMR, Aug 25, '92        Rewrote the packing routine and fixed bugs.
;       JIY, Mar 30, '92        added fix to work on Ultrix and VMS.
;       SMR, Oct 12, '93        Added changes suggested by Joe Gurman that
;                               prevented the clobbering of color vectors on
;                               exit from the routine.
;       SMR, Jan 12, '94        Added a case for OSF byte ordering, suggested
;                               by Joe Gurman.
;       DMS, Jun 24, 1994       Fixed byte ordering logic.
;                               Added code to clip image to # of colors-1
;-

common write_pict_rev, rev

i  = byte(1,0,2)                        ;Test byte ordering of this machine
rev = i(0) eq 1b                        ;TRUE to reverse for little endian

ON_ERROR, 2                             ;Return to main level if error

n_params = N_PARAMS();                  ;Check the arguments

IF (n_params EQ 1) THEN BEGIN           ;if no image passed in,
  n_params = 2                          ;Fake 2 param call
  IMAGE = TVRD()                        ;Read screen
ENDIF

IF ((n_params NE 2) AND (n_params NE 5)) THEN $         ;return error if args
  MESSAGE, "usage: WRITE_PICT, file, [IMAGE], [r,g,b]"  ;were incorrect

; If any color vectors are supplied, do they have right attributes ?
IF (n_params EQ 5) THEN BEGIN
  r_size = SIZE(r)
  g_size = SIZE(g)
  b_size = SIZE(b)
  IF ( (r_size(0) + g_size(0) + b_size(0)) NE 3) THEN $
    MESSAGE, "R, G, & B must all be 1D vectors."
  IF ( (r_size(1) NE g_size(1)) OR (r_size(1) NE b_size(1)) ) THEN $
    MESSAGE, "R, G, & B must all have the same length."
ENDIF ELSE BEGIN
  tvlct, r,g,b,/GET
ENDELSE

r_mac = long(r) * 256L          ;macs use ints for
g_mac = long(g) * 256L          ;color values so move
b_mac = long(b) * 256L          ;the values up

arraysize = fix(SIZE(IMAGE))    ;make sure correct dimensions of image
IF(arraysize(0) NE 2) THEN $    ;were used
  MESSAGE, "IMAGE must be a two dimensional matrix."

if (!version.os EQ 'MacOS') then begin
        OPENW, unit, FILE, /GET_LUN,/STREAM, $
            MACTYPE = "PICT"            ;open the file for writing
endif else begin
        OPENW, unit, FILE, /GET_LUN,/STREAM     ;open the file for writing
endelse

hdr = BYTARR(512)               ;pad the file with a 512 byte IMAGE
write_pict_item, unit, hdr, 0   ;that contains nothing important

imagesize = 0                   ;integer padding for IMAGE size which
write_pict_item, unit, imagesize,1      ;is ignored by version 2 PICT files

Rect = {rect, top:0, left:0, bottom:arraysize(2), right:arraysize(1)}
write_pict_item, unit, Rect, 1

opcode = 17                             ;Version Opcode
version = 2b                            ;This is a version 2 file
lowbyte = 255b                          ;being written
  write_pict_item, unit, opcode, 1
  write_pict_item, unit, version, 0
  write_pict_item, unit, lowbyte, 0

opcode = 3072                           ;HeaderOp Opcode
  headerdata = BYTE([[255,255,255,255],bytarr(20)])
  write_pict_item, unit, opcode, 1
  write_pict_item, unit, headerdata, 0

opcode = 30                             ;DefHilite Opcode
  write_pict_item, unit, opcode, 1

opcode = 1                              ;Clip Opcode
  regionsize = 10
  clipregion = Rect
  write_pict_item, unit, opcode, 1
  write_pict_item, unit, regionsize, 1
  write_pict_item, unit, clipregion, 1

opcode = 152                            ;PackBitsRect Opcode
  pixMap = {pixMapstr,  $
                rowBytes:fix(32768 + Rect.right),$      ;set high bit
                Boundtop:0,                     $
                Boundleft:0,                    $
                Boundbottom:Rect.bottom,        $
                Boundright:Rect.right,          $
                version:0,                      $
                packType:0,                     $
                packSize:0L,                    $
                hRes:4718592L,                  $
                vRes:4718592L,                  $
                pixelType:0,                    $
                pixelSize:8,                    $
                cmpCount:1,                     $
                cmpSize:8,                      $
                planeBytes:0L,                  $
                pmTable:0L,                     $
                pmReserved:0L}

  colorlistsize = n_elements(r_mac)
  colorTable = {colorTablestr,  ctseed:1038L,           $
                                transIndex:0,           $
                                ctSize:FIX(colorlistsize)-1}

  colors = INTARR(4, colorlistsize)
  colors(0,*) = INDGEN(colorlistsize)
  colors(1,*) = r_mac
  colors(2,*) = g_mac
  colors(3,*) = b_mac

  srcRect = Rect
  dstRect = Rect

  mode = 0

  imagedata = byte(IMAGE) < byte(colorlistsize-1)  ;Clip it
  IF(Rect.right GE 8) THEN BEGIN                ;pack data
    imagedata = PackData(imagedata)
  ENDIF

  write_pict_item, unit, opcode, 1
  write_pict_item, unit, pixmap.rowbytes, 1
  write_pict_item, unit, pixmap.boundtop, 1
  write_pict_item, unit, pixmap.boundleft, 1
  write_pict_item, unit, pixmap.boundbottom, 1
  write_pict_item, unit, pixmap.boundright, 1
  write_pict_item, unit, pixmap.version, 1
  write_pict_item, unit, pixmap.packtype, 1
  write_pict_item, unit, pixmap.packsize, 1
  write_pict_item, unit, pixmap.hres, 2
  write_pict_item, unit, pixmap.vres, 2
  write_pict_item, unit, pixmap.pixeltype, 1
  write_pict_item, unit, pixmap.pixelsize, 1
  write_pict_item, unit, pixmap.cmpcount, 1
  write_pict_item, unit, pixmap.cmpsize, 1
  write_pict_item, unit, pixmap.planebytes, 2
  write_pict_item, unit, pixmap.pmtable, 2
  write_pict_item, unit, pixmap.pmreserved, 2
  write_pict_item, unit, colortable.ctseed, 2
  write_pict_item, unit, colortable.transindex, 1
  write_pict_item, unit, colortable.ctsize, 1
  write_pict_item, unit, colors, 1
  write_pict_item, unit, srcrect, 1
  write_pict_item, unit, dstrect, 1
  write_pict_item, unit, mode, 1
  write_pict_item, unit, imagedata, 0 

  IF ((N_ELEMENTS(imagedata) MOD 2) NE 0) THEN $
    write_pict_item, unit, 0B, 0

  write_pict_item, unit, 255, 1         ;EOF Opcode

  FREE_LUN, unit

END
