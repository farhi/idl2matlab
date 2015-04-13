; $Id: sph_4pnt.pro,v 1.6 1994/11/29 20:42:40 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       SPH_4PNT
;
; PURPOSE:
;       Given four 3-dimensional points, this procedure returns the
;       center and radius necessary to define the unique sphere passing
;       through those points.
;
; CATEGORY:
;       Analytic Geometry.
;
; CALLING SEQUENCE:
;       SPH_4PNT, X, Y, Z, Xc, Yc, Zc, R
;
; INPUTS:
;       X: A 4-element vector containing the X coordinates of the points.
;       Y: A 4-element vector containing the Y coordinates of the points.
;       Z: A 4-element vector containing the Z coordinates of the points.
;
;	Note: X, Y, and Z should be floating-point or double-precision
;	      vectors.
;
; OUTPUTS:
;       Xc: The sphere's center x-coordinate. 
;       Yc: The sphere's center y-coordinate.
;       Zc: The sphere's center z-coordinate.
;       R:  The sphere's radius.
;
; RESTRICTIONS:
;       Points may not coincide.
;
; EXAMPLE:
;       Find the center and radius of the unique sphere passing through
;       the points: (1, 1, 0), (2, 1, 2), (1, 0, 3), (1, 0, 1)
;       
;       Define the floating-point vectors containing the x, y and z 
;       coordinates of the points. 
;         X = [1, 2, 1, 1] + 0.0
;	  Y = [1, 1, 0, 0] + 0.0
;	  Z = [0, 2, 3, 1] + 0.0
;
;       Compute the sphere's center and radius.
;         SPH_4PNT, X, Y, Z, Xc, Yc, Zc, R
;
;       Print the results.
;         PRINT, Xc, Yc, Zc, R
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, Jan 1993
;       Modified:    GGS, RSI, March 1994
;                    Rewrote documentation header.
;                    Uses the new Numerical Recipes NR_LUDCMP/NR_LUBKSB.
;       Modified:    GGS, RSI, November 1994
;                    Changed internal array from column major to row major.
;                    Changed NR_LUDCMP/NR_LUBKSB to LUDC/LUSOL
;-

PRO SPH_4PNT, x, y, z, xc, yc, zc, r

;Define the relationships between x, y and z as the transposed
;linear system.
  a = fltarr(3, 3)
  for k = 0, 2 do begin
    a(0, k) = x(k) - x(k+1)
    a(1, k) = y(k) - y(k+1)
    a(2, k) = z(k) - z(k+1)
  endfor

;Define right-side of linear system.
  q = x^2 + y^2 + z^2
  c = 0.5 * (q(0:2) - q(1:3))

;Solve the linear system Ay = c where y = (Xc, Yc, Zc)
  ludc, a, index
;Solution y is stored in C
  c = lusol(a, index, c)

;The sphere's center x-coordinate.
  xc = c(0)

;The sphere's center y-coordinate.
  yc = c(1)

;The sphere's center z-coordinate.
  zc = c(2)

;The sphere's radius.
  r = sqrt(q(0) - 2*(x(0)*xc + y(0)*yc + z(0)*zc) + xc^2 + yc^2 + zc^2)

end
