FUNCTION med, w
;********
;**
;** The call is w6=med(...)
Wout=w(*,0)
FOR i=0,N_ELEMENTS(w(0,*)) DO BEGIN
	Wout(i)=median(w(i,*))
ENDFOR
return, Wout
end
