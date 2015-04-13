FUNCTION mkcal5,win,i,last,pattern,w,xx,x,error
;********
;**
;** The call is w6=mkcal4(w5,32,1567,w7,w8,x7)
take_datp,datp
w=win
pv=datp.pv
x=datp.x
e=datp.e
window,0
window,1
window,2
j=i+1
pattern=REFORM(w(i,*))
error  =REFORM(e(i,*))
xx     =REFORM(pv(13,*))+x(i)-pv(13,0)
Wout=FLTARR(N_ELEMENTS(w(*,0)))
Eout=Wout
Wout(i)=1.0
FOR j=i+1,last DO BEGIN
  wset,0
  plot,xx,pattern
  oplot,pv(13,*)+x(j)-pv(13,0),w(j,*)
  a=MIN(pv(13,*)+x(j)-pv(13,0))
  b=MAX(xx)
  x0=xx(WHERE(xx gt a))
  ;rounding=10.
  ;x0=[x0,REFORM(x(j,WHERE(x(j,*) lt b)))]
  ;x0=ROUND(x0*rounding)
  ;x0=x0(SORT(x0))
  ;x0=x0(UNIQ(x0))
  ;x0=FLOAT(x0)*rounding
  ;c=INTERPOL(pattern,xx,x0)
  ;c_err=INTERPOL(error,xx,x0)
  c=pattern(WHERE(xx ge a))
  c_err=error(WHERE(xx ge a))
  d=INTERPOL(w(j,*),pv(13,*)+x(j)-pv(13,0),x0)
  d_err=INTERPOL(e(j,*),pv(13,*)+x(j)-pv(13,0),x0)
  oplot,x0,c
  oplot,x0,d
  idx=where(x0 ge 7.5 or x0 le -8.5,points)
  Wout(j)=TOTAL(c(idx)/d(idx)*c_err(idx)*d_err(idx)/d(idx)^2)
  Wout(j)=Wout(j)/TOTAL(c_err(idx)*d_err(idx)/d(idx)^2)
  Eout(j)=TOTAL((c(idx)/d(idx)-wout(j))^2*c_err(idx)*d_err(idx)/d(idx)^2)
  Eout(j)=Eout(j)/TOTAL(c_err(idx)*d_err(idx)/d(idx)^2)
  Eout(j)=sqrt(Eout(j))
  print,j,Wout(j),Eout(j),N_ELEMENTS(x0),a,b,points
  wset,2
  plot,x0(idx),c(idx),psym=7,yr=[min([c(idx),d(idx)]),max([c(idx),d(idx)])]
  plot,x0(idx),d(idx),/noer, yr=[min([c(idx),d(idx)]),max([c(idx),d(idx)])]
  wset,1
  plot,wout(i:j)
  d     =d     *Wout(j)
  d_err =d_err *Wout(j)+d     *Eout(j)
  w(j,*)=w(j,*)*Wout(j)
  e(j,*)=e(j,*)*Wout(j)+w(j,*)*Eout(j)
  d_tmp=c*(SQRT(c)/c_err)^2+d*(SQRT(d)/d_err)^2
  d_div=  (SQRT(c)/c_err)^2+  (SQRT(d)/d_err)^2
  d    =     d_tmp /d_div
  d_err=SQRT(d_tmp)/d_div
  x1=WHERE(xx LT a)
  x2=REFORM(pv(13,*)+x(j)-pv(13,0))
  x3=WHERE(x2 GT b)
  xx=[xx(x1),x0,x2(x3)]
  pattern=[pattern(x1),d,REFORM(w(j,x3))]
  error=[error(x1),d_err,REFORM(e(j,x3))]
ENDFOR
mod_datp,datp,'e',Eout
;help,datp.e,wout
mod_datp,datp,'x',indgen(n_elements(wout))
give_datp,datp
return, Wout
end
