pro chores,d,dd,T,p,res,trans,lam

;   macro to calculate the transmission and fractional resolution for a
;given TOF distance D, chopper period T, separation DD and phase angle p




lmin=0.
lmax=39.
nn=300
k=3956.


dl=(lmax-lmin)/nn

res=fltarr(nn)
trans=fltarr(nn)
lam=findgen(nn)*dl+lmin

for i=0,nn-1  do begin
  dt=(dd*(lmin+i*dl)/k)+(p/360.)*T
  res(i)=dt/(d*(lmin+i*dl)/k)
  trans(i)=dt/T
endfor

end





