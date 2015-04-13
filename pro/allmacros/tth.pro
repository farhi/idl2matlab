pro tth,n1,n2

data_read,n1,w1,mon,y,m,q,th,san,d0,det,nx


data_read,n2,w2,mon,y,m,q,th,san,dr,det,nx


a=max(total(w1,2),p0)

b=max(total(w2,2),pr)

step=5

k=0.
kk=0.
print,'com direct :',p0-step,p0+step
for i=p0-step,p0+step do begin
  k=k+total(w1(i,*),2)
  kk=kk+total(w1(i,*),2)*float(i)

endfor

pp0=kk/k


l=0.
ll=0.
for i=pr-step,pr+step do begin
  l=l+total(w2(i,*),2)
  ll=ll+total(w2(i,*),2)*i
endfor
ppr=ll/l


dpr=180./!pi

pcen=135.79/nx
mmpp=1.04*nx

print,'det: ',det,' X grouping: ',nx
print,'detector centre: ',pcen,' mmpp: ',mmpp
print,'reflection dan: ',dr,' direct beam dan: ',d0
print,'reflection pixel:',ppr,' direct pixel:',pp0

print,'ref th= ',(dr+dpr*atan((pcen-ppr)*mmpp/det))/2-(d0+dpr*atan((pcen-pp0)*mmpp/det))/2

return
end
