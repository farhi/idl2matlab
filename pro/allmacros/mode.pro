Function mode,x,J,pj=pj,imax=imax,printout=printout,prob=p,e=e,s=s,plot=plot

J=Floor(j)
N=N_ELEMENTS(x)
if (J LT 1) THEN BEGIN
  J=1
endif
if (J GE N) THEN BEGIN
  J=N-1
endif
p=FLTARR(N-J)
e=FLTARR(N-J)
s=FLTARR(N-J)
x=x(Sort (x))
pmax=0
FOR i=0,N-J-1 DO BEGIN
  p(i)=J/(N*(x(i+J)-x(i)))
  e(i)=0.5*(x(i)+x(i+J))
  s(i)=sqrt(J)/(N*2*e(i))
  ;Print, i,i+J,x(i),x(i+J),e(i),p(i)
  if (p(i) GT pmax) THEN BEGIN
    imax=i
    pmax=p(i)
  endif
ENDFOR
if KEYWORD_SET(printout) THEN BEGIN
  Print, e(imax),p(imax),s(imax)	,s(imax)/p(imax),1/sqrt(J)
  i=imax
  x1=(e(i)+e(i-1))/2.
  x2=(e(i)+e(i+1))/2.
  y1=(alog(p(i))-alog(p(i-1)))/(e(i)-e(i-1))
  y2=(alog(p(i+1))-alog(p(i)))/(e(i+1)-e(i))
    print,1./SQRT(-(y2-y1)/(x2-x1)),j,1./SQRT(-(y2-y1)/(x2-x1))/SQRT(j)
endif
if KEYWORD_SET(plot) THEN BEGIN
     Plot, e,p,ps=0
endif
pj=p(imax)


Return, e(imax)
end 

