; $Id: modifyct.pro,v 1.2 1995/01/25 23:00:25 billo Exp $

PRO MODIFYCT, ITAB, NAME, R, G, B, FILE=file	;MODIFY COLOR TABLE IN FILE
;+
; NAME:
;	MODIFYCT 
;
; PURPOSE:
;	Update the distribution color table file "colors1.tbl" or the
;	user-supplied file with a new table.
;
; CATEGORY:
;	Z4 - Image processing, color table manipulation.
;
; CALLING SEQUENCE:
;	MODIFYCT, Itab, Name, R, G, B
;
; INPUTS:
;	Itab:	The table to be updated, numbered from 0 to 255.  If the
;		table entry is greater than the next available location
;		in the table, then the entry will be added to the table
;		in the available location rather than the index specified
;		by Itab.  On return, Itab will contain the index for the
;		location that was modified or extended.  The table 
;		can be loaded with the IDL command:  LOADCT, Itab.
;
;	Name:	A string up to 32 characters long that contains the name for 
;		the new color table.
;
;	R:	A 256-element vector that contains the values for the red
;		color gun.
;
;	G:	A 256-element vector that contains the values for the green
;		color gun.
;
;	B:	A 256-element vector that contains the values for the blue
;		color gun.
;
; KEYWORD PARAMETERS:
;	FILE:	If this keyword is set, the file by the given name is used
;		instead of the file colors1.tbl in the IDL directory.  This
;		allows multiple IDL users to have their own color table file.
;		The file specified must be a copy of the colors1.tbl file.
;		The file must exist.
;
; OUTPUTS:
;	Itab:	The index of the entry which was updated, 0 to 255.  This
;		may be different from the input value of Itab if the
;		input value was greater than the next available location
;		in the table.  If this was the case the entry was added to
;		the table in the next available location instead of leaving
;		a gap in the table.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The distribution file "colors.tbl1" or the user-supplied file is
;	modified with the new table.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	Aug, 1982, DMS, Written.
;	Unix version, 1987, DMS.
;	ACY, 9/92, Update for new color table structure, Add FILE keyword.
;		   Allow extending table.
;	WSO, 1/95, Updated for new directory structure
;	
;-
  ON_ERROR,2                    ;Return to caller if an error occurs
  IF (ITAB LT 0)OR(ITAB GT 255) THEN message, $
		'Color table number out of range.'
  
  IF (N_ELEMENTS(file) GT 0) THEN filename = file $
  ELSE filename = FILEPATH('colors1.tbl', subdir=['resource', 'colors'])

  GET_LUN,IUNIT		;GET A LOGICAL UNIT
  OPENU,IUNIT, filename, /BLOCK  ;OPEN FILE
  ntables = 0b
  readu, IUNIT, ntables

  if (ITAB LT ntables) then begin	; Update an existing record
     AA=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1)	;UPDATE NAME RECORD.
     a = aa(0)
     a(*,ITAB) = 32B			;blank out old name
     a(0:strlen(name)-1,ITAB) = byte(name)
     aa(0)=a				;Write names out

     AA=ASSOC(IUNIT,BYTARR(256),1)	;UPDATE VECTORS. SKIP PAST COUNT
     AA(ITAB*3)   = BYTE(R)		;PUT IN RED. GUARANTEE BYTE
     AA(ITAB*3+1) = BYTE(G)		;GREEN IN 2ND BLOCK
     AA(ITAB*3+2) = BYTE(B)		;BLUE IN 3RD BLOCK

  endif else begin			; Add a new record at the end of table
     ITAB = ntables
     ; Add new vectors.  First, read names, then insert vectors
     AA=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1) ;UPDATE NAME RECORD.
     a = aa(0)
     ; Skip past old vectors
     AA=ASSOC(IUNIT,BYTARR(256),ntables*768L+1)      ;UPDATE VECTORS
     AA(0) = BYTE(R)             ;PUT IN RED. GUARANTEE BYTE
     AA(1) = BYTE(G)             ;GREEN IN 2ND BLOCK
     AA(2) = BYTE(B)             ;BLUE IN 3RD BLOCK

     ; Skip past new vector to put in names
     AA=ASSOC(IUNIT,BYTARR(32,ntables+1), (ntables+1)*768L+1)
     ; Add new name to end
     temp=bytarr(32)+32B
     temp(0:strlen(name)-1)=byte(name)
     allnames=bytarr(32,ntables+1)
     allnames(*,0:ntables-1) = a
     allnames(*,ntables)=temp
     AA(0) = allnames		; write the names out
          
     ; Update count
     AA=ASSOC(IUNIT,BYTARR(1))
     AA(0) = [ntables+1B]
  endelse

  FREE_LUN,IUNIT
  RETURN
END
