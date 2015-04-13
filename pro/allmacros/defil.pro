FUNCTION defil,w1,b,c,d,datp=datpp,x=x1,bg=bg,delta=delta

IF NOT Keyword_set(datpp) THEN take_datp,datp ELSE datp=datpp
IF NOT Keyword_set(x1) THEN x1=datp.x
IF NOT Keyword_set(x1) THEN delta=.5
pv1=datp.pv
p1=datp.p
FOR i=0,N_ELEMENTS(x1(0,*))-1 DO x1(*,i)=x1(*,i)-pv1(14,i)            
a=indgen(N_ELEMENTS(w1(0,*)))
w2=fltarr(100*(max(x1)-min(x1))) 
x2=findgen(100*(max(x1)-min(x1)))/100.+min(x1)            
for i=0,1599 do w2(10*i+a)=w2(10*i+a)+w1(i,a) 
w7=w1*0.
for j=0,N_ELEMENTS(w1(0,*))-1 do w7(*,j)=w1(*,j)*(x1(*,j) le b+delta and x1(*,j) ge b-delta )
w8=w7
c=where(total(w7,2) gt 0) 
cells=N_ELEMENTS(c)
w8=w7(c,*)
x8=0
x8=x1(c,*)   
points=N_ELEMENTS(w8(0,*))
bg=fltarr(cells)
for i=0,cells-1 DO bg(i)=min(w8(i,where(w8(i,*) gt 0)))
bg=min(bg)
w8=(w8-bg)>0
mod_datp,datp,'x',x8
IF NOT Keyword_set(datpp) THEN give_datp,datp ELSE datpp=datp
d=x1(0)+p1(14)
RETURN,w8

END
