pro res


;Read in two Dt/t's and two q's to establish variables
redo:
print,""
read,"Res1,q1,Res2,q2 :>",r1,q1,r2,q2
if r1 eq 9 then begin
	r1=.01
	q1=.05
	r2=.02
	q2=1.3
endif
if (r1 lt 0) or (r2 lt 0) or (q1 lt 0) or (q2 lt 0) then begin
	print,"No negative numbers allowed, please redo"
	goto, redo
endif

print,"Processing... Please Wait" 


;ensure accuracy
r1=float(r1) & r2=float(r2) & q1=float(q1) & q2=float(q2) 


;calculate gradient (m) and intercept (c)
rmin=r1<r2 & rmax=r1>r2 & qmin=q1<q2 & qmax=q1>q2
m=(rmax-rmin)/(qmax-qmin)
c=rmax-m*qmax
print,c,m

;Ranges Of The Variables (min,max) stored on disc
;in ranges.dat
d=[0.,0.]
dis=[0.,0.]
w=[0.,0.]
lamda=[0.,0.]
phi=[0.,0.]
r=0.
openr,1,'ranges.dat'
readf,1,d,dis,w,lamda,phi,r
close,1

;dis=Dis-(d(1)-d(0))/2 ;small correction
k=3956.0346035 ; In A
n=8;no of examples

;obtain dis and d
nd=101; higher means more accurate range
dist=dis(0)+findgen(nd)*(dis(1)-dis(0))/(nd-1)
dist=dist*c
if max((dist lt d(1)) and (dist gt d(0))) then begin
	dist=dist(where(dist lt d(1)))
	dist=dist(where(dist gt d(0)))
	d=[min(dist),max(dist)]; new range
	dis=[d(0)/c,d(1)/c]; new range
endif else begin
	print,"d range out of bounds"
	goto, pass
endelse
	

;obtain omega
nw=101
dist=dis(0)+findgen(nw)*(dis(1)-dis(0))/(nw-1)
w1=2*!pi*k/dist/lamda(1)
if max((w1 lt w(1)) and (min(w1) gt w(0))) then begin
	w1=w1(where(w1 gt w(0)))
	w1=w1(where(w1 lt w(1)))
	w(0)=[min(w1),max(w1)];new range
	dis=rotate(2*!pi*k/lamda(1)/w,2);new range
	d=c*dis;newrange
endif else begin
	print,"w range out of bounds"
	goto, pass
endelse

;obtain chopper angle
phil=2*!pi*m/((1/qmin)-(1/qmax))
phil=phil/!pi*180 ; do i do this line ???????????????????????
if ((phil gt phi(1)) or (phil lt phi(0))) then begin
	print, "Chopper angle invalid"
	goto, pass
endif



;delta x
velchop=w*r
velbeam=k/lamda,2
b=atan(velchop/velbeam)
x=d*sin(b)


;Draw the plot
ng=21 ;No of points in graph
Tt=fltarr(ng) ;Time Resolution
q=fltarr(ng) ;q values
q=qmin+(findgen(ng)*(qmax-qmin)/(ng-1))
Tt=m*q+c
plot,q,Tt,ystyle=1,xstyle=1,xtitle="q (1/m)",ytitle="Dt/t",title="Time Resolution For Double Chopper",$
psym=0


; Print data out
print,"Dist is in the range      :",dis(0),dis(1)
print,"d is in the range         :",d(0),d(1)
print,"Rpms are in the range     :",w(1)*60/2/!pi,w(0)*60/2/!pi
print,"The Chopper angle is      :",phil
print,'Range of Delta x          :',x
print,"Range of velocity of beam :",velbeam

;Generate n values of Dist,d and rpms
dist=dis(0)+findgen(n)*(dis(1)-dis(0))/(n-1)
dt=dist*c
w=2*!pi*k/lamda(1)/dist
print,""
print,"Examples :"
print,"Dist :",dist
print,"d    :",dt
print,"w    :",w*60/2/!pi

pass:
end
