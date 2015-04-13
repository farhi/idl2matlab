; 
; Copyright (c) 1993, Research Systems, Inc. All rights reserved. 
;	Unauthorized reproduction prohibited. 

FUNCTION Krig_expon, d, t       ;Return Exponential Covariance Fcn
r = t(2) * exp((-3./t(0)) * d)
z = where(d eq 0.0, count)
if count gt 0 then r(z) = t(1) + t(2)
return, r
end

FUNCTION Krig_sphere, d, t      ;Return Spherical Covariance Fcn
r = d/t(0) < 1.0                ;Normalized distance
r = t(2) * (1. - 1.5 * r + 0.5 * r^3)
z = where(d eq 0.0, count)
if count gt 0 then r(z) = t(1) + t(2)
return, r
end

FUNCTION krig2d, z, x, y, REGULAR = regular, XGRID=xgrid, $
	XVALUES = xvalues, YGRID = ygrid, YVALUES = yvalues, $
	GS = gs, BOUNDS = bounds, NX = nx0, NY = ny0, EXPONENTIAL = ex, $
	SPHERICAL = sp, NESTED = nest, C0 = C0

;+ 
; NAME: 
;	KRIG_2D 
; 
; PURPOSE: 
;	This function interpolates a regularly or irregularly gridded
;	set of points Z = F(X,Y) using kriging.
;
; CATEGORY: 
;	Interpolation, Surface Fitting 
;
; CALLING SEQUENCE: 
;	Result = KRIG2D(Z [, X, Y]) 
;
; INPUTS: 
;	X, Y, Z:  arrays containing the X, Y, and Z coordinates of the 
;		  data points on the surface. Points need not be 
;		  regularly gridded. For regularly gridded input data, 
;		  X and Y are not used: the grid spacing is specified 
;		  via the XGRID and YGRID (or XVALUES and YVALUES) 
;		  keywords, and Z must be a two dimensional array. 
;		  For irregular grids, all three parameters must be
;		  present and have the same number of elements. 
;
; KEYWORD PARAMETERS: 
;   Model Parameters:
;	EXPONENTIAL: if set (with parameters [A, C0, C1]), use an exponential
;		     semivariogram model.
;	SPHERICAL:   if set (with parameters [A, C0, C1]), use a spherical
;		     semivariogram model.
;
;   Both models use the following parameters:
;	A:	  the range. At distances beyond A, the semivariogram 
;		  or covariance remains essentialy constant. 
;		  See the definition of the functions below. 
;	C0:	  the "nugget," which provides a discontinuity at the
;		  origin. 
;	C1:	  the covariance value for a zero distance, and the variance
;		  of the random sample Z variable. If only a two element
;		  vector is supplied, C1 is set to the sample variance.
;		  (C0 + C1) = the "sill," which is the variogram value for
;		  very large distances.
;
;  Input grid description:
;	REGULAR:  if set, the Z parameter is a two dimensional array 
;		  of dimensions (N,M), containing measurements over a 
;		  regular grid. If any of XGRID, YGRID, XVALUES, YVALUES 
;		  are specified, REGULAR is implied. REGULAR is also 
;		  implied if there is only one parameter, Z. If REGULAR is 
;		  set, and no grid (_VALUE or _GRID) specifications are 
;		  present, the respective grid is set to (0, 1, 2, ...). 
;	XGRID:    contains a two element array, [xstart, xspacing], 
;		  defining the input grid in the X direction. Do not
;		  specify both XGRID and XVALUES. 
;	XVALUES:  if present, XVALUES(i) contains the X location 
;		  of Z(i,j). XVALUES must be dimensioned with N elements. 
;	YGRID:    contains a two element array, [ystart, yspacing], 
;		  defining the input grid in the Y direction. Do not
;		  specify both YGRID and YVALUES. 
;	YVALUES:  if present, YVALUES(i) contains the Y location 
;		  of Z(i,j). YVALUES must be dimensioned with N elements. 
;
;  Output grid description:
;	GS:	  If present, GS must be a two-element vector [XS, YS],
;		  where XS is the horizontal spacing between grid points
;		  and YS is the vertical spacing. The default is based on
;		  the extents of X and Y. If the grid starts at X value
;		  Xmin and ends at Xmax, then the default horizontal
;		  spacing is (Xmax - Xmin)/(NX-1). YS is computed in the
;		  same way. The default grid size, if neither NX or NY
;		  are specified, is 26 by 26. 
;	BOUNDS:   If present, BOUNDS must be a four element array containing
;		  the grid limits in X and Y of the output grid:
;		  [Xmin, Ymin, Xmax, Ymax]. If not specified, the grid
;		  limits are set to the extent of X and Y. 
;	NX:       The output grid size in the X direction. NX need not
;	  	  be specified if the size can be inferred from GS and
;		  BOUNDS. The default value is 26.
;	NY:       The output grid size in the Y direction. See NX. 
; 
; OUTPUTS: 
;	This function returns a two dimensional floating point array
;	containing the interpolated surface, sampled at the grid points.
;
; RESTRICTIONS:
;	The accuracy of this function is limited by the single precision
;	floating point accuracy of the machine.
;
;		SAMPLE EXECUTION TIMES (measured on a Sun IPX)
;	# of input points	# of output points	Seconds
;	10			676			1.1
;	20			676			1.5
;	40			676			2.6
;	80			676			7.8
;	10			1024			1.6
;	10			4096			5.9
;	10			16384			23
;
; PROCEDURE: 
;	Ordinary kriging is used to fit the surface described by the
;	data points X,Y, and Z. See: Isaaks and Srivastava,
;	"An Introduction to Applied Geostatistics," Oxford University
;	Press, 1989, Chapter 12.
;
;	The parameters of the data model, the range, nugget, and
;	sill, are highly dependent upon the degree and type of spatial
;	variation of your data, and should be determined statistically.
;	Experimentation, or preferrably rigorus analysis, is required.
;
;	For N data points, a system of N+1 simultaneous
;	equations are solved for the coefficients of the 
;	surface. For any interpolation point, the interpolated value 
;	is: 
;          F(x,y) = Sum( w(i) * C(x(i),y(i), x, y)
;
;	Formulas used to model the variogram functions:
;		d(i,j) = distance from point i to point j.
;		V = variance of samples.
;		C(i,j) = Covariance of sample i with sample j.
;               C(x0,y0,x1,y1) = Covariance of point (x0,y0) with (x1,y1).
;
;       Exponential covar: C(d) = C1 * EXP(-3*d/A)   if d ne 0.
;                               = C1 + C0          if d eq 0.
;
;       Spherical covar:   C(d) = (1.0 - 1.5 * d/a + 0.5 * (d/a)^3)
;                               = C1 + C0           if d eq 0.
;                               = 0                 if d > a.
;
; EXAMPLES:
; Example 1: Irregularly gridded cases 
;	Make a random set of points that lie on a gaussian: 
;	  n = 15		;# random points
;	  x = RANDOMU(seed, n) 
;	  y = RANDOMU(seed, n) 
;	  z = exp(-2 * ((x-.5)^2 + (y-.5)^2))	;The gaussian 
;
; 	get a 26 by 26 grid over the rectangle bounding x and y: 
;	  e = [ 0.25, 0.0]	;Range and nugget are 0.25, and 0.
;				;(These numbers are dependent upon
;				;your data model.)
;	  r = krig2d(z, x, y, EXPON = e)	;Get the surface. 
;
; 	Or: get a surface over the unit square, with spacing of 0.05: 
;	  r = krig2d(z, x, y, EXPON=e, GS=[0.05, 0.05], BOUNDS=[0,0,1,1])
;
; 	Or: get a 10 by 10 surface over the rectangle bounding x and y: 
;	  r = krig2d(z, x, y, EXPON=e, NX=10, NY=10) 
; 
; Example 2: Regularly gridded cases 
;	  s = [ 10., 0.2]			;Range and sill, data dependent.
;	  z = randomu(seed, 5, 6)		;Make some random data

;	interpolate to a 26 x 26 grid: 
;	  CONTOUR, krig2d(z, /REGULAR, SPHERICAL = s)
; 
; MODIFICATION HISTORY: 
;	DMS, RSI, March, 1993. Written.
;-

on_error, 2

s = size(z)		;Assume 2D
nx = s(1)
ny = s(2)

reg = keyword_set(regular) or (n_params() eq 1)

if n_elements(xgrid) eq 2 then begin
	x = findgen(nx) * xgrid(1) + xgrid(0)
	reg = 1
endif else if n_elements(xvalues) gt 0 then begin
	if n_elements(xvalues) ne nx then $
		message,'Xvalues must have '+string(nx)+' elements.'
	x = xvalues
	reg = 1
endif

if n_elements(ygrid) eq 2 then begin
	y = findgen(ny) * ygrid(1) + ygrid(0)
	reg = 1
endif else if n_elements(yvalues) gt 0 then begin
	if n_elements(yvalues) ne ny then $
		message,'Yvalues must have '+string(ny)+' elements.'
	y = yvalues
	reg = 1
endif

if reg then begin
	if s(0) ne 2 then message,'Z array must be 2D for regular grids'
	if n_elements(x) ne nx then x = findgen(nx)
	if n_elements(y) ne ny then y = findgen(ny)
	x = x # replicate(1., ny)	;Expand to full arrays.
	y = replicate(1.,nx) # y
	endif

n = n_elements(x)
if n ne n_elements(y) or n ne n_elements(z) then $
	message,'x, y, and z must have same number of elements.'

if keyword_set(ex) then begin       ;Get model params
    t = ex
    fname = 'KRIG_EXPON'
endif else if keyword_set(sp) then begin
    t = sp
    fname = 'KRIG_SPHERE'
endif else MESSAGE,'Either EXPONENTIAL or SPHERICAL model must be selected.'

if n_elements(t) eq 2 then begin    ;Default value for variance?
    mz = total(z) / n		;Mean of z
    var = total((z - mz)^2)/n	;Variance of Z
    t = [t, var-t(1)]	;Default value for C1
    endif

m = n + 1			;# of eqns to solve
a = fltarr(m, m)

for i=0, n-2 do for j=i,n-1 do begin  ;Only upper diagonal elements
    d = (x(i)-x(j))^2 + (y(i)-y(j))^2  ;Distance squared
    a(i,j) = d & a(j,i) = d             ;symmetric
    endfor

a = call_function(fname, sqrt(a), t)        ;Get coefficient matrix
a(n,*) = 1.0            ;Fill edges
a(*,n) = 1.0
a(n,n) = 0.0

; c = invert(a)           ;Solution using inverse
ludcmp, a, indx, even_odd       ;Solution using LU decomposition

if n_elements(nx0) le 0 then nx0 = 26	;Defaults for nx and ny
if n_elements(ny0) le 0 then ny0 = 26

xmin = min(x, max = xmax)		;Make the grid...
ymin = min(y, max = ymax)

if n_elements(gs) lt 2 then $
	gs = [(xmax-xmin)/(nx0-1.), (ymax-ymin)/(ny0-1.)]
if n_elements(bounds) lt 4 then bounds = [xmin, ymin, xmax, ymax]

nx = ceil((bounds(2)-bounds(0))/gs(0))+1	;# of elements
ny = ceil((bounds(3)-bounds(1))/gs(1))+1

d = fltarr(m)                       ;One extra for lagrange constranint
r = fltarr(nx,ny,/nozero)           ;Result

for j=0,ny-1 do begin               ;Each output point
  y0 = bounds(1) + gs(1) * j
  for i=0,nx-1 do begin
    x0 = bounds(0) + gs(0) * i
    d(0) = sqrt((x-x0)^2 + (y-y0)^2) ;distance
    d = call_function(fname, d, t)          ;Get rhs
    d(n) = 1.0                              ;lagrange constr
    lubksb, a, indx, d
    r(i,j) = total(d * z)
  endfor
endfor
return, r
end
