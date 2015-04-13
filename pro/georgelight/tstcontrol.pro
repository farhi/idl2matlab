function tstnewvalue, TYPE=type, SETVALUE=setvalue, COMMENT=comment
;******* ***********
;**
common gogo, mono1,mono2,mono3,myval,curmon,monot

if n_elements(type) eq 1 then if type eq 'status' then return, 'Idle' $
                         else if type eq 'wave'   then return, 4.14
return,myval
end



function tstcontrol , cmd, CHECK=check
;******* **********
;**
common gogo, mono1,mono2,mono3,myval,curmon,monot

if n_elements(mono1) le 1 then begin 
   hw1  =15. ;Half width
   h    =10.  ;Hight
   n1   =200.
   mono1= h*exp(-(((findgen(n1)-n1/2)/hw1)^2)/2)
   mono1=mono1+sqrt(mono1)*(randomu(s,n1)-.5)*2./10.
   mono1=shift(mono1,-n1/2)
   hw1  =19. ;Half width
   h    =20.  ;Hight
   n1   =200.
   mono2= h*exp(-(((findgen(n1)-n1/2)/hw1)^2)/2)
   mono2=mono2+sqrt(mono2)*(randomu(s,n1)-.5)*2./10.
   mono2=shift(mono2,-n1/2)
   hw1  =22. ;Half width
   h    =30.  ;Hight
   n1   =200.
   mono3= h*exp(-(((findgen(n1)-n1/2)/hw1)^2)/2)
   mono3=mono3+sqrt(mono3)*(randomu(s,n1)-.5)*2./10.
   mono3=shift(mono3,-n1/2)
   
   monot=fltarr(n1) & curmon=0 & myval=0
endif

com=strsplit(cmd,'. ',/extract)
; print, cmd

case com(0) of
'raz':   begin  mono1=0 & curmon=0 & end
'mono1': begin	if curmon ne 1 then begin curmon=1 & monot=monot+mono1 & endif
		pos=long(com(1)) & myval=monot(abs(pos-(3800-2)))
	 end
'mono2': begin	if curmon ne 2 then begin curmon=2 & monot=monot+mono2 & endif
		pos=long(com(1)) & myval=monot(abs(pos-(3960+2)))
	 end
'mono3': begin	if curmon ne 3 then begin curmon=3 & monot=monot+mono3 & endif
		pos=long(com(1)) & myval=monot(abs(pos-(4220-4)))
	 end
'count':
else:
endcase

return,myval
end
