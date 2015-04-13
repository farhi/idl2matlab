; $Id: plot_field.pro,v 1.3 1993/10/08 15:53:31 doug Exp $

function mybi,a,x,y	;Bilinear interpolation
sizea=size(a)
nx=sizea(1)
i=long(x)+nx*long(y)
q=y-long(y)
p=x-long(x)
aint=(1.0-p)*(1.0-q)*a(i)+p*(1.0-q)*a(i+nx)+q*(1.0-p)*a(i+1)+p*q*a(i+nx+1)
return,aint
end



PRO ARRHEAD,X		;Add an arrowhead to a vector
THETA=ATAN(1.0)/4.0
TANT = TAN(THETA)
NP=3.0
SCAL=6.

SX=SIZE(X)
N=SX(2)


BIGL=SQRT((X(*,N-4,0)-X(*,N-5,0))^2+(X(*,N-4,1)-X(*,N-5,1))^2)
wbigl=where(BIGL ne 0.0)
wnbigl=where(bigl eq 0.0)
LL  = SCAL*TANT*BIGL(wbigl)/NP

DX = LL*(X(wbigl,N-4,1)-X(wbigl,N-5,1))/BIGL(wbigl)
DY = LL*(X(wbigl,N-4,0)-X(wbigl,N-5,0))/BIGL(wbigl)

XM = X(wbigl,N-4,0)-(SCAL-1)*(X(wbigl,N-4,0)-X(wbigl,N-5,0))/NP
YM = X(wbigl,N-4,1)-(SCAL-1)*(X(wbigl,N-4,1)-X(wbigl,N-5,1))/NP

X(wbigl,N-3,0) = XM-DX
X(wbigl,N-2,0) = X(wbigl,N-4,0)
X(wbigl,N-1,0) = XM+DX

X(wbigl,N-3,1) = YM+DY
X(wbigl,N-2,1) = X(wbigl,N-4,1)
X(wbigl,N-1,1) = YM-DY

if n_elements(wnbigl) gt 1 then begin
X(wnbigl,N-3,0) = x(wnbigl,n-4,0)
X(wnbigl,N-2,0) = X(wnbigl,n-4,0)
X(wnbigl,N-1,0) = X(wnbigl,n-4,0)

X(wnbigl,N-3,1) = X(wnbigl,N-4,1)
X(wnbigl,N-2,1) = X(wnbigl,N-4,1)
X(wnbigl,N-1,1) = X(wnbigl,N-4,1)
end
return
END



function arrows,u,v,n,length,xmax,lmax
su=size(u)
nx=su(1)
ny=su(2)
if n_params(0) lt 6 then lmax=sqrt(max(u^2+v^2))
if n_params(0) lt 5 then xmax=1.0
lth=0.1*length/lmax
xt=randomu(seed,n)
yt=randomu(seed,n)
x=fltarr(n,13,2)
x(0,0,0)=xt(*)
x(0,0,1)=yt(*)
for i=1,9 do begin
 xt(0)=(nx-1)*x(*,i-1,0)
 yt(0)=(ny-1)*x(*,i-1,1)
 ut=mybi(u,xt,yt)
 vt=mybi(v,xt,yt)
 x(0,i,0)=x(*,i-1,0)+ut*lth
 x(0,i,1)=x(*,i-1,1)+vt*lth
end
ARRHEAD,X
return,x<1.0>0.0
end




PRO Plot_field,U,V,N=N,LENGTH=length,ASPECT=aspect, title= title
;+
; NAME:
;	PLOT_FIELD
;
; PURPOSE:
;	This procedure plots a 2-dimensional field.
;
; CATEGORY:
;	Plotting, two-dimensional.
;
; CALLING SEQUENCE:
;	PLOT_FIELD, U, V
;
; INPUTS:
;	U:	The 2-dimensional array giving the field vector at each
;		point in the U(X) direction.
;
;	V:	The 2-dimensional array giving the field vector at each
;		point in the V(Y) direction.
;
; KEYWORD PARAMETERS:
;	N:	The number of arrows to draw. The default is 200.
;
;	LENGTH:	The length of the longest field vector expressed as a fraction
;		of the plotting area. The default is 0.1.
;
;	ASPECT:	The aspect ratio of the plot (i.e., the ratio of the X size 
;		to Y size). The default is 1.0.
;
;	TITLE:	The title of plot. The default is "Velocity Field".
;
; OUTPUTS:
;	No explicit outputs.
;
; SIDE EFFECTS:
;	A new plot is drawn to the current output device.
;
; PROCEDURE:
;	N random points are picked, and from each point a path is traced
;	along the field. The length of the path is proportional to "LENGTH" 
;	and the field vector magnitude.
;
; EXAMPLE:
;	X = FINDGEN(20, 20)		; Create array X
;	Y = FINDGEN(20, 20)*3		; Create array Y
;	PLOT_FIELD, X, Y		; Create plot
;	
; MODIFICATION HISTORY:
;	Jan., 1988	Neal Hurlburt, University of Colorado.
;-


if n_elements(n) le 0 then n=200
if n_elements(length) le 0 then length=.1
if n_elements(aspect) le 0 then aspect=1.0
if n_elements(title) le 0 then title = 'Velocity Field'

X=ARROWS(U,V,N,LENGTH)
IF ASPECT GT 1. THEN position=[0.20,(0.5-0.30/ASPECT),0.90,(0.5+0.40/ASPECT)]$
 else position=[(0.5-0.30*ASPECT),0.20,(0.5+0.40*ASPECT),0.90]
plot,[0,1,1,0,0],[0,0,1,1,0],title=title,/nodata, pos = position
FOR I=0,N-1 DO OPLOT,X(I,*,0),X(I,*,1)
RETURN
end

