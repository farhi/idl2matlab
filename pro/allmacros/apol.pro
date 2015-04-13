PRO apol,n1,n2,dnf,df,rnf,rf,qq

dpr=180./!pi
peak=212
gap=11
lam=5.0
l=0
k=0
data_read,n1,datnf,monx,y,mm,q,th,san0,dan0,dett,nx
m1=long(1)
m2=long(1)

z=size(datnf)

df=fltarr(z(1),(n2-n1+1)/2)
dnf=fltarr(z(1),(n2-n1+1)/2)
san=fltarr((n2-n1+1)/2)
dan=fltarr((n2-n1+1)/2)
qq=fltarr((n2-n1+1)/2)
mon=fltarr((n2-n1+1)/2)
print,'size= ',((n2-n1+1)/2),z(1)

for i=0,(n2-n1),2 do begin
  
  print,'iiiiiiiiii',i,l,i+n1
  data_read,n1+i,datnf,m1,y,mm,q,th,s,d,dett,nx
  if (l eq 0) then monnf0=float(m1)
  print,'monitor nf',float(m1),monnf0,total(fix(datnf))
  dnf(*,l)=total(float(datnf),2)*float(m1)/monnf0
  
  data_read,n1+i+1,datf,m2,y,mm,q,th,s,d,dett,nx
  if (l eq 0) then monf0=float(m2)
  df(*,l)=total(float(datf),2)*float(m2)/monf0
  print,'monitor f',float(m2),monf0,total(fix(datf))
  print,'san dan mon',s,d,m2
  san(l)=s & dan(l)=d & mon(l)=m2
  qq(l)= 4.*!pi*sin(san(l)/dpr)/lam   
  l=l+1
endfor


fnf=total(dnf(peak-(gap-1)/2:peak+(gap-1)/2,*),1)
bnf=total(dnf(peak-(gap-1)/2-gap:peak-(gap-1)/2-1,*),1)
ff=total(df(peak-(gap-1)/2:peak+(gap-1)/2,*),1)
bf=total(df(peak-(gap-1)/2-gap:peak-(gap-1)/2-1,*),1)

print,peak-(gap-1)/2-gap,peak-(gap-1)/2-1,peak-(gap-1)/2,peak+(gap-1)/2

rnf=(fnf-bnf)*sin(san0/dpr)/((fnf(0)-bnf(0))*sin(san))
rf=(ff-bf)*sin(san0/dpr)/((ff(0)-bf(0))*sin(san))


end
