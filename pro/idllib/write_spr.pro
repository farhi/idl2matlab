;$Id: write_spr.pro,v 1.4 1994/03/02 15:42:06 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       WRITE_SPR
;
; PURPOSE:
;       This procedure writes a row-indexed sparse matrix stucture to a 
;	specified file.  A  row-indexed sparse matrix is created by the
;	Numerical Recipes routine NR_SPRSIN.
;
; CATEGORY:
;       Sparse Matrix File I/O 
;
; CALLING SEQUENCE:
;       WRITE_SPR, AS, 'Filename' 
;
; INPUTS:
;       AS:  row indexed sparse matrix created by NR_SPRSIN
;	Filename:  Name of file to contain AS.
;
; KEYWORDS:
;	NONE
;
; OUTPUTS:
;	NONE
;
; EXAMPLE:
;	a = [[3.,0., 1., 0., 0.],$
;	     [0.,4., 0., 0., 0.],$
;     	     [0.,7., 5., 9., 0.],$
;     	     [0.,0., 0., 0., 2.],$
;     	     [0.,0., 0., 6., 5.]]
;
;	as = NR_SPRSIN(transpose(a))
;
;	WRITE_SPR, as, 'sprs.as'
;
; MODIFICATION HISTORY:
;       Written by:     BMH, 1/94.
;-
 
PRO WRITE_SPR, as, filename

; as structure format = {sa:FLTARR(nmax) or sa:DBLARR(nmax),  - value array
;			 ija:LONARR(nmax)}                    - index array
;

ON_IOERROR, BADFILE
info = SIZE(as.(0))   ;Access type and size information for the sa array 

nmax = info(1) ;sa and ija vectors are of equal length. 
type = info(2) ;Type of matrix value vector (sa)

OPENW, fileLUN, filename, /GET_LUN

;Store type and size info for file read 
WRITEU, fileLUN, nmax, type, as  

FREE_LUN, fileLUN


RETURN

BADFILE:
IF (N_Elements(fileLUN) GT 0L) THEN $
   FREE_LUN, fileLUN
MESSAGE, 'Error writing to sparse matrix file: ' + filename
 

END



