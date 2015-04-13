pro wave,n,w1,x1
data_read,n,dat,m,l

rr=7
dbtot=total(dat,1)
w1=dbtot
x1=l
dbtotm=max(dbtot,dbm)
print,'direct beam peak at: ',dbm
f1=dbm-(rr-1)/2
f2=dbm+(rr-1)/2
print,'sum ranges for db: ',f1,f2
;COM for db
numersum=0.
denomsum=0.
for i=f1,f2 do begin
	numer=dbtot(i)*float(i)
	denom=dbtot(i)
	numersum=numer+numersum
	denomsum=denom+denomsum
endfor
print,'db stuff:', numersum,denomsum
dbcom=numersum/denomsum
print,'fitted db peak at: ',(dbcom)

;print,l

a=floor(dbcom)
diff=dbcom-a
print,a
dwdc=l(a+1)-l(a)

wav=l(a)+dwdc*diff

print,'wavelength = ',wav
end
