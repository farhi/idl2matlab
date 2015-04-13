function arrows,u,v,n,length,nsteps=nsteps
on_error,2                      ;Return to caller if an error occurs
su=size(u)
nx=su[1]
ny=su[2]

IF NOT KEYWORD_SET(NSTEPS) THEN NSTEPS=10
lmax=sqrt(max(u^2+v^2))		;Max vector length
lth=1.*length/lmax/nsteps
xt=randomu(seed,n)		;Starting position
yt=randomu(seed,n)
x=fltarr(n,nsteps+3,2)
x[0,0,0]=xt
x[0,0,1]=yt
for i=1,nsteps-1 do begin
 xt[0]=(nx-1)*x[*,i-1,0]
 yt[0]=(ny-1)*x[*,i-1,1]
 ut=vel_mybi(u,xt,yt)
 vt=vel_mybi(v,xt,yt)
 x[0,i,0]=x[*,i-1,0]+ut*lth
 x[0,i,1]=x[*,i-1,1]+vt*lth
end
ARRHEAD,X
return,x<1.0>0.0
end
