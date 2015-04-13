FUNCTION mkcal3,win,i,last,pattern,w,xx,x
;********
;**
;** The call is w6=mkcal2(w5,32,1567,w7,w8,x7)
take_datp,datp
w=win
pv=datp.pv
x=datp.x
window,0
window,1
window,2
j=i+1
pattern=REFORM(w(i,*))
xx=reform(pv(13,*))+x(i)-x(0)
Wout=FLTARR(N_ELEMENTS(w(*,0)))
Wout(i)=1.0
;;;pattern=smooth(pattern,9)
FOR j=i+1,last DO BEGIN
  ;pattern=smooth(pattern,3)
  wset,0
  plot,xx,pattern
  oplot,pv(13,*)+x(j)-x(0),w(j,*)
  a=MIN(pv(13,*)+x(j)-x(0))
  b=MAX(xx)
  x0=[xx,REFORM(pv(13,*)+x(j)-x(0))]
  x0=x0(SORT(x0))
  x0=x0(UNIQ(x0))
  x0=x0(WHERE(x0 GE a AND x0 LE b))
  x0=FINDGEN((b-a)*10-1)*0.1+a
  c=INTERPOL(pattern,xx,x0)
  ;;;;c=c(where(x0 ge 7 or x0 le -7))
  ;;c=smooth(c,3)
  d=INTERPOL(w(j,*),pv(13,*)+x(j)-x(0),x0)
  ;;;;d=d(where(x0 ge 7 or x0 le -7))
  ;;;d=smooth(d,9)
  oplot,x0,c
  oplot,x0,d
  idx=where(x0 ge 8 or x0 le -8,points)
 PRINT,min(c(idx)),min(d(idx)),points,a,b
  Wout(j)=TOTAL(c(idx)/d(idx)*SQRT((c(idx)+d(idx))/2.))/TOTAL(SQRT((c(idx)+d(idx))/2.))
  Wout(j)=TOTAL(c(idx)/d(idx))/points
  print,j,Wout(j),N_ELEMENTS(x0),N_ELEMENTS(xx),min(x0),max(x0),a,b
wset,2
plot,x0(idx),c(idx),psym=7
oplot,x0(idx),d(idx),psym=1
  wset,1
  plot,wout(i:j)
  d=d*Wout(j)
  w(j,*)=w(j,*)*Wout(j)
  d=(c+d)/2.0
  ;;d=smooth(d,3)
  x1=WHERE(xx LT a)
  x2=REFORM(pv(13,*)+x(j)-x(0))
  x3=WHERE(x2 GT b)
  xx=[xx(x1),x0,x2(x3)]
  pattern=[pattern(x1),d,w(j,x3)]
  ;;pattern=smooth(pattern,3)
ENDFOR
return, Wout
end
