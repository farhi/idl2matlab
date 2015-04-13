FUNCTION mix,w

take_datp,datp
x=datp.x
e=datp.e
flag=0*indgen(n_elements(x))
index=0*indgen(n_elements(x))
newx=sort(x)
plot,x,newx
FOR i=0,n_elements(x) DO BEGIN
  j=WHERE (flag eq 0,count)
  k=0
  WHILE NOT x(i) eq newx(j(k)) DO k=k+1
  flag(j(k))=1
  index(i)=j(k)
ENDFOR
FOR i=0,n_elements(w) DO BEGIN
  neww(i) = w(index(i))
  newe(i) = e(index(i))
ENDFOR
datp.e=newe
datp.x=newx
RETURN,neww
give_datp,datp
END
