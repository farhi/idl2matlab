FUNCTION user
;+
; PURPOSE:
;  This function without input gives the name of the user using IDL/LAMP.
;-
if (sys_dep('MACHINE') eq 'mac') then BEGIN
   u='root'
ENDIF ELSE BEGIN
  SPAWN, 'who am i',u
ENDELSE
RETURN, u
END
