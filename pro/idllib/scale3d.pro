; $Id: scale3d.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro scale3d		;Scale !p.t to bring unit cube into viewing area
;+
; NAME:
;	SCALE3D
;
; PURPOSE:
;	Scale the 3D unit cube (a cube with the length of each side equal
;	to 1) into the viewing area.
;
; CATEGORY:
;	Graphics, 3D.
;
; CALLING SEQUENCE:
;	SCALE3D
;
; INPUTS:
;	No explicit inputs.  !P.T is an implicit input.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	No explicit outputs.  !P.T is an implicit output.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	!P.T is modified.
;
; RESTRICTIONS:
;	Doesn't work for all forms of perspective transformations.
;
; PROCEDURE:
;	Eight, 3D data points are created at the vertices of the 3D
;	unit cube.  They are transformed by !P.T.  The system
;	is translated to bring the minimum (x,y,z) point to the origin, 
;	and then scaled to make each coordinates maximum value equal to 1.
;
; MODIFICATION HISTORY:
;	DMS, May, 1988.
;-

on_error,2              ;Return to caller if an error occurs
for i = 0,7 do begin	;Find 8 corners
	p = [ i and 1, (i/2) and 1, ((i/4) and 1),1] # !p.t
	p = p / p(3)	;normalize homogenous coords
	if i eq 0 then begin
		pmin = p & pmax = p
	  endif else begin
		pmin = pmin < p & pmax = pmax > p
	  endelse
	endfor
;
;print,'PMAX: ',transpose(pmax)
;print,'PMIN: ',transpose(pmin)
;	avoid dividing by 0 if there is no Z transformation.
if pmax(2) eq pmin(2) then pmax(2) = pmin(2)+1
t3d,tr = [ -pmin(0), -pmin(1), -pmin(2)], sc=1./(pmax(0:2)-pmin(0:2))
end
