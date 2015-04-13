FUNCTION mkcal  , w2 , w1
;********
;**
;** The call is w6=mkcal(...)
w1=w2
take_datp,datp
a=N_ELEMENTS(w1(0,*))
b=N_ELEMENTS(w1(*,0))
Wout=FLTARR(b)+1.0
w=FLTARR(a+b)
n=w*0.0
FOR i=1,b-1 DO BEGIN
  n(i-1:i+a-2)=n(i-1:i+a-2)+1.0
  w(i-1:i+a-2)=w(i-1:i+a-2)+w1(i-1,0:a-1)
  Wout(i)=total(w(i:i+a-2)/n(i:i+a-2))/total(w1(i,0:a-2))
ENDFOR
Wout=Wout/mean(Wout)
FOR i=0,b-1 DO w1(i,*)=w1(i,*)*Wout(i)
mod_datp,datp,'x_tit','cell'
mod_datp,datp,'e',0
mod_datp,datp,'x',INDGEN(b)
give_datp,datp
RETURN, Wout
end
