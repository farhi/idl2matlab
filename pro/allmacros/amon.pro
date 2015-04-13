PRO amon,n1,n2,water,dnf,rnf,qq

dpr=180./!pi
peak=212
gap=11
lam=5.0
l=0
k=0
data_blead,n1,datnf,monx,y,mm,q,th,san0,dan0,dett,nx,t0
m1=long(1)
m2=long(1)
ymin=61
ymax=214
z=size(datnf)


dnf=fltarr(z(1),(n2-n1+1))
san=fltarr((n2-n1+1))
dan=fltarr((n2-n1+1))
qq=fltarr((n2-n1+1))
mon=fltarr((n2-n1+1))
print,'size= ',((n2-n1+1)),z(1)

for i=0,(n2-n1) do begin
  
  print,'iiiiiiiiii',i,l,i+n1
  data_blead,n1+i,datnf,m1,y,mm,q,th,s,d,dett,nx,time
  if (l eq 0) then monnf0=float(m1)
  print,'total nf',total(float(datnf))

  dnf(*,l)=total(float(datnf(*,*)),2)/(time*water)
  print,'total dnf',total(dnf(*,l))
  print,'san dan time',s,d,time
  san(l)=s & dan(l)=d & mon(l)=m2
  qq(l)= 4.*!pi*sin(san(l)/dpr)/lam   
  l=l+1
endfor


fnf=total(dnf(peak-(gap-1)/2:peak+(gap-1)/2,*),1)
bnf=total(dnf(peak-(gap-1)/2-gap:peak-(gap-1)/2-1,*),1)

print,peak-(gap-1)/2-gap,peak-(gap-1)/2-1,peak-(gap-1)/2,peak+(gap-1)/2

rnf=(fnf-bnf)



end
