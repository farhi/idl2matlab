FUNCTION num2reg3d,num1,num2,FILENAME=filnam
;+
; Variante of d20reg for 3D-workspaces ... 
; Automatic reading of numors
; Created 27 July 1998 by Th. Hansen after demand of X. Turrillas
;-
@lamp.cbk
datp=0
TAKE_DATP,datp,W=one
IF N_ELEMENTS(datp) LT 1 THEN datp=0
w=RDAND(num1,num2,datp=datp)
IF NOT KEYWORD_SET(filnam) THEN BEGIN
  filnam=STRCOMPRESS(num1,/REMOVE_ALL)+'_'+STRCOMPRESS(num2,/REMOVE_ALL)
ENDIF
help,datp
help,datp.x
reg3d,w,filnam,DATP=datp
GIVE_DATP,datp
RETURN,w
END
