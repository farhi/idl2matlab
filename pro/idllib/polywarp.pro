; $Id: polywarp.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro POLYWARP, XI, YI, XO, YO, DEGREE, KX, KY
;+
; NAME:
;	POLYWARP
;
; PURPOSE:
;	Perform polynomial spatial warping.
;
;	Using least squares estimation, determine the coefficients Kx(i,j) 
;	and Ky(i,j) of the polynomial functions:
;		Xi = sum over i and j of:  Kx(i,j) * Xo^j * Yo^i
;		Yi = sum over i and j of:  Ky(i,j) * Xo^j * Yo^i
;
;	Kx and Ky can be used as inputs P and Q to the built-in function
;	POLY_2D.
;	
; CATEGORY:
;	Image processing.
;
; CALLING SEQUENCE:
;	POLYWARP, Xi, Yi, Xo, Yo, Degree, Kx, Ky
;
; INPUTS:
;	Xi, Yi:	The vectors of x,y coordinates to be fit as a function 
;		of Xo and Yo.
;
;	Xo, Yo:	The vectors of x,y independent coordinates.  These vectors 
;		must have the same number of elements as Xi and Yi.
;
;	Degree:	The degree of the fit.  The number of coordinate pairs must be
;		greater than or equal to (Degree+1)^2.
;
; OUTPUTS:
;	Kx:	The array of coefficients for Xi as a function of (xo,yo).  
;		This parameter is returned as a (Degree+1) by (Degree+1) 
;		element array.
;
;	Ky:	The array of coefficients for yi.  This parameter is returned
;		as a (Degree+1) by (Degree+1) element array.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	See:	Computer Image Processing and Recognition, Ernest L. Hall,
;		Academic Press, 1979, Pages 186-188.
;
;	Xi and Yi are expressed as polynomials of Xo, Yo:
;		Xi = Kx(i,j) * Xo^j * Yo^i   Summed for i,j = 0 to degree.
;	And
;		Yi = Ky(i,j) * Xo^j * Yo^i.
;
;	This coordinate transformation may be then used to
;	map from Xo, Yo coordinates into Xi, Yi coordinates.
;
; EXAMPLE:
;	The following example shows how to display an image and warp it
;	using the POLYWARP and POLY_2D routines.
;
;	Create and display the original image by entering:
;
;		A = BYTSCL(SIN(DIST(250)))
;		TVSCL, A
;
;	Now set up the Xi's and Yi's.  Enter:
;
;		XI = [24, 35, 102, 92]
;		YI = [81, 24, 25, 92]
;
;	Enter the Xo's and Yo's:
;
;		XO = [61, 62, 143, 133]
;		YO = [89, 34, 38, 105]
;
;	Run POLYWARP to obtain a Kx and Ky:
;
;		POLYWARP, XI, YI, XO, YO, 1, KX, KY
;
;	Create a warped image based on Kx and Ky with POLY_2D:
;
;		B = POLY_2D(A, KX, KY)
;
;	Display the new image:
;
;		TV, B
;
; MODIFICATION HISTORY:
;	DMS, RSI, Dec, 1983.
;-
;
on_error,2                      ;Return to caller if an error occurs
m = n_elements(xi)		;# of points..
if (m ne n_elements(yi)) or (n_elements(xo) ne n_elements(yo)) $
	or (m ne n_elements(xo)) then begin
		message,'Inconsistent number of elements.'
		endif
;
n = degree		;use halls notation
n2=(n+1)^2
if n2 gt m then message, '# of points must be ge (degree+1)^2.'
;
x = dblarr(2,m)		;x array
u = x
x = double([transpose(xi(*)),transpose(yi(*))])
u = double([transpose(xo(*)),transpose(yo(*))])
;
ut = dblarr(n2,m)	;transpose of U
u2i = dblarr(n+1)	;[1,u2i,u2i^2,...]
for i=0L,m-1 do begin
	u2i(0)=1.	;init u2i
	zz = u(1,i)
	for j=1,n do u2i(j)=u2i(j-1)*zz
	ut(0,i)= u2i	;evaluate 0 th power separately
	for j=1,n do ut(j*(n+1),i)=u2i*u(0,i)^j ;fill ut=u0i^j * U2i
	endfor
;
uu = transpose(ut)	;big u
kk = invert(ut#uu)#ut	;solve equation
kx = fltarr(n+1,n+1) + float(kk # transpose(x(0,*)))	;g1, make 2d square
ky = fltarr(n+1,n+1) + float(kk # transpose(x(1,*)))	;g2
return
end
