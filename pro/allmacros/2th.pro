pro 2th,p0,d0,pr,dr,pmin,pmax
dpr=180./!pi
pmin=6.
pmax=63.
pcen=(pmin+pmax)/2.
mmpp=250./(pmax-pmin)
print,'pcen= ',pcen,' mmpp= ',mmpp
print,'ref th= ',(dr+dpr*atan((pcen-pr)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
  
return
end
