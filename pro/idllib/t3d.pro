; $Id: t3d.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro t3d, translate = trans, scale = scale, rotate=rota, $
	reset = reset, perspective = pers, oblique = oblique, $
	xyexch = xyexch, xzexch = xzexch, yzexch = yzexch
;+
; NAME:
;	T3D
;
; PURPOSE:
;	Implement three-dimensional transforms.
;
;	This routine accumulates one or more sequences of translation,
;	scaling, rotation, perspective, and oblique transformations
;	and stores the result in !P.T, the 3D transformation system variable.
;	All the IDL graphic routines use this (4,4) matrix for output.
;
; CATEGORY:
;	Graphics.
;
; CALLING SEQUENCE:
;	T3D [, /RESET, TRANSLATE = T, SCALE = S, ROTATE = R, ... ]
;
; INPUTS:
;	No non-keyword inputs.
;
; KEYWORDS:
;	All inputs to T3D are in the form of keywords.  Any, all, or none of 
;	the following keywords can be present in a call to T3D.  The order of 
;	the input parameters does not matter.
;
;	The transformation specified by each keyword is performed in the
;	order of their descriptions below (e.g., if both TRANSLATE and
;	SCALE are specified, the translation is done first):
;
;	RESET:	Set this keyword to reset the transformation to the default 
;		identity matrix.
;
;    TRANSLATE:	A three-element vector of the translations in the X, Y, and Z 
;		directions.
;
;	SCALE:	A three-element vector of scale factors for the X, Y, and Z 
;		axes.
;
;       ROTATE:	A three-element vector of the rotations, in DEGREES, 
;		about the X, Y, and Z axes.  Rotations are performed in the
;		order of X, Y, and then Z.
;
;  PERSPECTIVE:	Perspective transformation.  This parameter is a scalar (p) 
;		that indicates the Z distance of the center of the projection.
;		Objects are projected into the XY plane at Z=0, and the "eye" 
;		is at point (0,0,p).
;
;      OBLIQUE:	A two-element vector of oblique projection parameters.
;		Points are projected onto the XY plane at Z=0 as follows:
;			x' = x + z(d COS(a)), and y' = y + z(d SIN(a)).
;		where OBLIQUE(0) = d, and OBLIQUE(1) = a.
;
;	XYEXCH:	Exchange the X and Y axes.
;
;	XZEXCH:	Exchange the X and Z axes.
;
;	YZEXCH:	Exchange the Y and Z axes.
;
; OUTPUTS:
;	The 4 by 4 transformation matrix !P.T is updated with the
;	resulting transformation.  !P.T3D is NOT set, so for the 
;	transformations to have effect you must set !P.T3D = 1 (or set
;	the T3D keyword in subsequent calls to graphics routines).
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	!P.T is changed.
;
; RESTRICTIONS:
;	This routine implements general rotations about the three axes.
;	The routines SURFACE and SHADE_SURF may only be used in conjunction 
;	with T3D rotations that project the Z axis in 3 dimensions to
;	a line parallel to the Y axis in 2 dimensions.
;
; PROCEDURE:
;	This program follows that of Foley & Van Dam, "Fundamentals of
;	Interactive Computer Graphics", Chapter 8, "Viewing in Three
;	Dimensions".
;
;	The matrix notation is reversed from the normal IDL sense,
;	i.e., here, the first subscript is the column, the second is the row,
;	in order to conform with the above reference.
;
;	A right-handed system is used.  Positive rotations are COUNTER-
;	CLOCKWISE when looking from a positive axis to the origin.
;
; EXAMPLES:
;	To reset the transformation, rotate 30 degs about the X axis
;	and do perspective transformation with the center of the projection
;	at Z = -1, X=0, and Y=0, enter:
;
;		T3D, /RESET, ROT = [ 30,0,0], PERS = 1.
;
;	Transformations may be cascaded, for example:
;
;		T3D, /RESET, TRANS = [-.5,-.5,0], ROT = [0,0,45]
;		T3D, TRANS = [.5,.5,0]
;
;	The first command resets, translates the point (.5,.5,0) to the 
;	center of the viewport, then rotates 45 degrees counterclockwise 
;	about the Z axis.  The second call to T3D moves the origin back to 
;	the center of the viewport.
;
; MODIFICATION HISTORY:
;	DMS, Nov, 1987.
;
;	DMS, June 1990.	Fixed bug that didn't scale or translate
;			matrices with perspective properly.
;-

on_error,2              ;Return to caller if an error occurs
id = fltarr(4,4)	;Make identity matrix
for i=0,3 do id(i,i)= 1.0

if n_elements(reset) eq 0 then reset = 0
if reset then a = id else a = !p.t ;Use either !P.T or identity

if n_elements(trans) ne 0 then begin	;Translate?
	ri = id
	for i=0,2 do ri(3,i) = trans(i)
	a = a # ri			;Apply translation
	endif

if n_elements(scale) ne 0 then begin	;Scale
	ri = id
	for i=0,2 do ri(i,i) = scale(i)
	a = a # ri			;Apply scale
	endif

if n_elements(rota) ne 0 then begin	;Rotate?
	ri = id(0:2,0:2)		;Use 3 by 3's
	r = ri
	sx = sin(rota/!radeg) & cx = cos(rota/!radeg)

	if rota(0) ne 0.0 then begin	;X Angle
		r(1,1) = cx(0) & r(1,2) = sx(0)
		r(2,1) = -sx(0) & r(2,2) = cx(0)
		endif
	if rota(1) ne 0 then begin	;Y angle
		rr = ri
		rr(0,0) = cx(1) & rr(0,2) = -sx(1)
		rr(2,0) = sx(1) & rr(2,2) = cx(1)
		r =  r # rr
		endif
	if rota(2) ne 0 then begin	;Z angle
		rr = ri
		rr(0,0)= cx(2) & rr(0,1) = sx(2)
		rr(1,0) = -sx(2) & rr(1,1) = cx(2)
		r =  r # rr
		endif
	rr = fltarr(4,4) 
	rr(0,0) = r & rr(3,3)=1.0
	a = a # rr			;Apply cumulative rot transforms
	endif

if n_elements(pers) ne 0 then begin	;Perspective?
	r = id
	r(2,3) = -1./pers
	a = a # r
	endif

if n_elements(oblique) ne 0 then begin  ;Oblique projection?
	r = id
	r(2,2)=0.0
	r(2,0) = oblique(0) * cos(oblique(1)/ !radeg)
	r(2,1) = oblique(0) * sin(oblique(1)/ !radeg)
	a = a # r
	endif

if keyword_set(xyexch) then exch = [0,1]	;Code to exchange axes.
if keyword_set(xzexch) then exch = [0,2]
if keyword_set(yzexch) then exch = [1,2]
if n_elements(exch) ne 0 then begin	;Exchange axes.
	t = a(exch(0),*)
	a(exch(0),0) = a(exch(1),*)
	a(exch(1),0) = t
	endif

!p.t =  a	;Save final projection
end
