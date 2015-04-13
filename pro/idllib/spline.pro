; $Id: spline.pro,v 1.3 1993/10/06 17:29:55 doug Exp $

function spline,x,y,t,sigma
;+
; NAME:
;	SPLINE
;
; PURPOSE:
;	This function performs cubic spline interpolation.
;
; CATEGORY:
;	Interpolation - E1.
;
; CALLING SEQUENCE:
;	Result = SPLINE(X, Y, T [, Sigma])
;
; INPUTS:
;	X:	The abcissa vector. Values MUST be monotonically increasing.
;
;	Y:	The vector of ordinate values corresponding to X.
;
;	T:	The vector of abcissae values for which the ordinate is 
;		desired. The values of T MUST be monotonically increasing.
;
; OPTIONAL INPUT PARAMETERS:
;	Sigma:	The amount of "tension" that is applied to the curve. The 
;		default value is 1.0. If sigma is close to 0, (e.g., .01),
;		then effectively there is a cubic spline fit. If sigma
;		is large, (e.g., greater than 10), then the fit will be like
;		a polynomial interpolation.
;
; OUTPUTS:
;	SPLINE returns a vector of interpolated ordinates.
;	Result(i) = value of the function at T(i).
;
; RESTRICTIONS:
;	Abcissa values must be monotonically increasing.
;
; EXAMPLE:
;	The commands below show a typical use of SPLINE:
;
;		X = [2.,3.,4.]  	;X values of original function
;		Y = (X-3)^2     	;Make a quadratic
;		T = FINDGEN(20)/10.+2 	;Values for interpolated points.
;					;twenty values from 2 to 3.9.
;		Z = SPLINE(X,Y,T) 	;Do the interpolation.
;
;
;
; MODIFICATION HISTORY:
;	Author:	Walter W. Jones, Naval Research Laboratory, Sept 26, 1976.
;	Reviewer: Sidney Prahl, Texas Instruments.
;	Adapted for IDL: DMS, Research Systems, March, 1983.
;
;-
;
on_error,2                      ;Return to caller if an error occurs
if n_params(0) lt 4 then sigma = 1.0 else sigma = sigma > .001	;in range?
n = n_elements(x) < n_elements(y)
;
if n le 1 then message, 'X and Y must be arrays.'

;
xx = x * 1.			;Make X values floating if not.
yp = fltarr(n*2)		;temp storage
delx1 = xx(1)-xx(0)		;1st incr
dx1=(y(1)-y(0))/delx1
;
nm1 = n-1L
np1 = n+1L
if (n eq 2) then begin
	yp(0)=0.
	yp(1)=0.
   end else begin
	delx2 = xx(2)-xx(1)
	delx12 = xx(2)-xx(0)
	c1 = -(delx12+delx1)/delx12/delx1
	c2 = delx12/delx1/delx2
	c3 = -delx1/delx12/delx2
;
	slpp1 = c1*y(0)+c2*y(1)+c3*y(2)
	deln = xx(nm1)-xx(nm1-1)
	delnm1 = xx(nm1-1)-xx(nm1-2)
	delnn = xx(nm1)-xx(nm1-2)
	c1=(delnn+deln)/delnn/deln
	c2=-delnn/deln/delnm1
	c3=deln/delnn/delnm1
	slppn = c3*y(nm1-2)+c2*y(nm1-1)+c1*y(nm1)
   endelse
;
	sigmap = sigma*nm1/(xx(nm1)-xx(0))
	dels = sigmap*delx1
	exps = exp(dels)
	sinhs = .5d0*(exps-1./exps)
	sinhin=1./(delx1*sinhs)
	diag1 = sinhin*(dels*0.5d0*(exps+1./exps)-sinhs)
	diagin = 1./diag1
	yp(0)=diagin*(dx1-slpp1)
	spdiag = sinhin*(sinhs-dels)
	yp(n)=diagin*spdiag
;
	if  n gt 2 then for i=1L,nm1-1 do begin
		delx2 = xx(i+1)-xx(i)
		dx2=(y(i+1)-y(i))/delx2
		dels = sigmap*delx2
		exps = exp(dels)
		sinhs = .5d00 *(exps-1./exps)
		sinhin=1./(delx2*sinhs)
		diag2 = sinhin*(dels*(.5*(exps+1./exps))-sinhs)
		diagin = 1./(diag1+diag2-spdiag*yp(n+i-1))
		yp(i)=diagin*(dx2-dx1-spdiag*yp(i-1))
		spdiag=sinhin*(sinhs-dels)
		yp(i+n)=diagin*spdiag
		dx1=dx2
		diag1=diag2
	   endfor
;

	diagin=1./(diag1-spdiag*yp(n+nm1-1))
	yp(nm1)=diagin*(slppn-dx2-spdiag*yp(nm1-1))
	for i=n-2,0,-1 do yp(i)=yp(i)-yp(i+n)*yp(i+1)		
;
;
	m = n_elements(t)
	subs = replicate(long(nm1),m) ;subscripts
	s = xx(nm1)-xx(0)
	sigmap = sigma*nm1/s
	j=0L
	for i=1L,nm1 do $ ;find subscript where xx(subs) > t(j) > xx(subs-1)
		while xx(i) gt t(j) do begin
			subs(j)=i
			j=j+1
			if j eq m then goto,done
			endwhile
	
done:	subs1 = subs-1
	del1 = t-xx(subs1)
	del2 = xx(subs)-t
	dels = xx(subs)-xx(subs1)
	exps1=exp(sigmap*del1)
	sinhd1 = .5*(exps1-1./exps1)
	exps=exp(sigmap*del2)
	sinhd2=.5*(exps-1./exps)
	exps = exps1*exps
	sinhs=.5*(exps-1./exps)
	spl=(yp(subs)*sinhd1+yp(subs1)*sinhd2)/sinhs+ $
		((y(subs)-yp(subs))*del1+(y(subs1)-yp(subs1))*del2)/dels
	if m eq 1 then return,spl(0) else return,spl
end
