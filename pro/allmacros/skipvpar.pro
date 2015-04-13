pro skipvpar ,vpb,vpara,in
;** ********
;**
IF vpb gt 0 THEN BEGIN deco=''
		 FOR I=0,vpb-1 DO BEGIN
			 READF,in,deco
			 READF,in,deco     & SKIPLINE,in,deco,vpb
			 vpara=fltarr(vpb) & READF,in,vpara
		 ENDFOR
ENDIF
RETURN
END

