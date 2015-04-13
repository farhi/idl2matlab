Function maxlike,w,jmax,pj=p,i=i,prob=prob,dx=dx,x=x,sigma=sigma,e=e

window,1
N=n_ELEMENTS(w)
;print,jmax
likelihood=FLTARR((N-1)<jmax)
x=FLTARR(N)
dx=x
p=x
sigma=x
FOR j=3,N-2 DO BEGIN
  x(j)=mode(w,j,pj=pj,i=i,/plot)
  p(j)=pj
  dx(j)=w(i+j)-w(i)
  ;print, j, x(j),p(j), dx(j),i,w(i+j),w(i)
ENDFOR
maxi=0
;print,LONG(3),jmax,N-1
FOR j=LONG(3),(jmax<(N-1))-1 DO BEGIN
  likelihood(j)=1
  FOR k=3, (N-2) DO BEGIN
    if (k NE j) THEN BEGIN
      likelihood(j)=likelihood(j)*igamma(k,j*dx(k)/dx(j))
      ;print ,k,j,dx(k),dx(j),j*dx(k)/dx(j),igamma(k,j*dx(k)/dx(j)),likelihood(j)
    endif
  ENDFOR
   print, j,likelihood(j),x(j) ;,sigma(j)
  if (likelihood(j) GT maxi) THEN BEGIN
    maxi=likelihood(j)
    best=j
  endif
ENDFOR
j=best
x(j)=mode(w,j,pj=pj,i=i,/plot,prob=prob,e=e,s=sigma)
p(j)=pj
dx(j)=w(i+j)-w(i)
print, " "
print, j,likelihood(j),x(j) ;,sigma(j),p(j)
x1=(e(i)+e(i-1))/2.
x2=(e(i)+e(i+1))/2.
y1=(alog(prob(i))-alog(prob(i-1)))/(e(i)-e(i-1))
y2=(alog(prob(i+1))-alog(prob(i)))/(e(i+1)-e(i))
;print,e(i-1),prob(i-1),alog(prob(i-1))
;print,e(i),prob(i),alog(prob(i))
;print,e(i+1),prob(i+1),alog(prob(i+1))
;print,x1,y1
;print,x2,y2
;print,(y2-y1)/(x2-x1)
print,1./SQRT(-(y2-y1)/(x2-x1)),j,1./SQRT(-(y2-y1)/(x2-x1))/SQRT(j)

end 
