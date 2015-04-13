; $Id: coord2to3.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION COORD2TO3, MX, MY, DIM, D0, PTI
;+
; NAME:
;	COORD2TO3
;
; PURPOSE:
;	Return 3D data coordinates given the normalized X and Y screen
;	coordinates and one of the three data coordinates.
;
; CATEGORY:
;	Geometry / Graphics.
;
; CALLING SEQUENCE:
;	Result = COORD2TO3(Mx, My, Dim, D0 [, PTI])
;
; INPUTS:
;	Mx, My: The normalized X and Y screen coordinates.
;
;	Dim:  	A parameter used to specify which data coordinate is fixed.
;		Use 0 for a fixed X data coordinate, 1 for a fixed Y data 
;		coordinate, or 2 for a fixed Z data coordinate.
;
;	D0:	The value of the fixed data coordinate.
;
; OPTIONAL INPUT PARAMETERS:
;	PTI:  	The inverse of !P.T.  If this parameter is not supplied, 
;		or set to 0, COORD2TO3 computes the inverse.  If this routine
;		is to be used in a loop, the caller should supply PTI for
;		highest efficiency.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	Returns a 3-element vector containing the 3D data coordinates.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	A valid 3D transform must exist in !P.T or PTI.
;	The axis scaling variables, !X.S, !Y.S and !Z.S must be valid.
;
; PROCEDURE:
;	The intersection of the plane determined by data coordinates
;	Dim and D0 and the screen coordinate line (Mx, My, 0),
;	(Mx, My, 1) is computed.
;
; EXAMPLE:
;	To return the data coordinates of the mouse, fixing the
;	data Z value at 10, enter the commands:
;		CURSOR, X, Y, /NORM	;Get the normalized mouse coords.
;	  	P = COORD2TO3(X, Y, 2, 10.0)
;
; MODIFICATION HISTORY:
;	DMS, 9/91.
;-


if n_elements(pti) le 0 then pti = invert(!p.t) $
   else if total(pti) eq 0. then pti = invert(!p.t)

vs = [[!x.s], [!y.s], [!z.s]]
a = d0 * vs(1,dim) + vs(0,dim)  ;To normalized data coords

;	Solve for normalized screen Z
az = (a - mx * pti(0,dim) - my * pti(1,dim) - pti(3,dim)) / pti(2,dim)
p = [ mx, my, az, 1]		;Normalized screen coords
p = p # pti			;to normalized 3d coords
return, (p(0:2) - vs(0,*)) / vs(1,*) ;to 3d data coords.
end
