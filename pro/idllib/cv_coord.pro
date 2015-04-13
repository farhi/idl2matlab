; $Id: cv_coord.pro,v 1.1 1994/04/05 17:22:55 dan Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CV_COORD
;
; PURPOSE:
;       Converts 2-D and 3-D coordinates between the RECTANGULAR, POLAR,
;       CYLINDRICAL, and SPHERICAL coordinate systems.
;
; CATEGORY:
;       Graphics
;
; CALLING SEQUENCE:
;       Coord = CV_COORD()
;
; KEYWORD PARAMETERS:
;
;       FROM_RECT:
;                  A vector of the form [x, y] or [x, y, z], or a (2, n) or
;                  (3, n) array containing rectangular coordinates to convert.
;
;       FROM_POLAR:
;                  A vector of the form [angle, radius], or a (2, n) array of
;                  polar coordinates to convert.
;
;       FROM_CYLIN:
;                  A vector of the form [angle, radius, z], or a (3, n) array
;                  of cylindrical coordinates to convert.
;
;       FROM_SPHERE:
;                  A vector of the form [longitude, latitude, radius], or a
;                  (3, n) array of spherical coordinates to convert.
;
;       TO_RECT:   If set, then rectangular coordinates are returned.
;
;       TO_POLAR:  If set, then polar coordinates are returned.
;
;       TO_CYLIN:  If set, then cylindrical coordinates are returned.
;
;       TO_SPHERE: If set, then spherical coordinates are returned.
;
;       DEGREES:   If set, then the input (and output) coordinates are in
;                  degrees (where applicable). Otherwise, the angles are
;                  in radians.
;
; OUTPUTS:
;       This function returns the converted coordinate(s) based on which of
;       the "TO_" keywords is used :
;
;          TO_RECT   : If the input coordinates were polar, then a vector
;                      of the form [x, y] or a (2, n) array is returned.
;                      Otherwise, a vector of the form [x, y, z], or a
;                      (3, n) array is returned.
;          TO_POLAR  : A vector of the form [angle, radius], or a (2, n)
;                      array is returned.
;          TO_CYLIN  : A vector of the form [angle, radius, z], or a (3, n)
;                      array is returned.
;          TO_SPHERE : A vector of the form [longitude, latitude, radius],
;                      or a (3, n) array is returned.
;
;       If the value passed to the "FROM_" keyword is double precision, then
;       all calculations are performed in double precision and the returned
;       value is double precision. Otherwise, single precision is used.
;
;       If none of the "FROM_" keywords are specified then 0 is returned.
;       If none of the "TO_" keywords are specified then the input coordinates
;       are returned.
;
; PROCEDURE:
;       When converting from spherical to polar coordinates, the points
;       are first projected along the z axis to the x-y plane to get 2-D
;       rectangular coordinates. The 2-D rectangular coordinates are
;       then converted to polar.
;
; EXAMPLE:
;       ; Convert from spherical to cylindrical coordinates.
;
;       sphere_coord = [[45.0, -60.0, 10.0], [0.0, 0.0, 0.0]]
;       rect_coord = CV_COORD(From_Sphere=sphere_coord, /To_Cylin, /Degrees)
;
;       ; Convert from rectangular to polar coordinates.
;
;       rect_coord = [10.0, 10.0]
;       polar_coord = CV_COORD(From_Rect=rect_coord, /To_Polar)
;
; MODIFICATION HISTORY:
;       Written by:     Daniel Carr, Thu Mar 31 14:42:58 MST 1994
;-

FUNCTION CV_COORD, From_Rect=from_rect, From_Polar=from_polar, $
                   From_Cylin=from_cylin, From_Sphere=from_sphere, $
                   To_Rect=to_rect, To_Polar=to_polar, $
                   To_Cylin=to_cylin, To_Sphere=to_sphere, $
                   Degrees=degrees

IF (N_Elements(from_rect) GT 0L) THEN BEGIN ; Convert from rectangular.
   sz_from = Size(from_rect)
   IF (sz_from(0) EQ 1L) THEN $
      sz_from = [2L, sz_from(1), 1L, sz_from(2), sz_from(3)]

   IF (sz_from(sz_from(0) + 2L) EQ 5L) THEN BEGIN ; Double precision.
      IF (Keyword_Set(degrees)) THEN ang_out = 180.0D/!DPI ELSE ang_out = 1.0D
      zero = 0.0D
   ENDIF ELSE BEGIN ; Single precision
      IF (Keyword_Set(degrees)) THEN ang_out = !Radeg ELSE ang_out = 1.0
      zero = 0.0
   ENDELSE

   IF (Keyword_Set(to_polar)) THEN BEGIN
      ang = Replicate(zero, 1L, sz_from(2))
      rad = Sqrt(from_rect(0, *)^2 + from_rect(1, *)^2)
      non_zero_ind = Where(rad NE zero)
      IF (non_zero_ind(0) GE 0L) THEN $
         ang(non_zero_ind) = ang_out * $
            Atan(from_rect(1, non_zero_ind), from_rect(0, non_zero_ind))
      RETURN, [ang, rad]
   ENDIF

   IF (Keyword_Set(to_cylin)) THEN BEGIN
      ang = Replicate(zero, 1L, sz_from(2))
      rad = Sqrt(from_rect(0, *)^2 + from_rect(1, *)^2)
      non_zero_ind = Where(rad NE zero)
      IF (non_zero_ind(0) GE 0L) THEN $
         ang(non_zero_ind) = ang_out * $
            Atan(from_rect(1, non_zero_ind), from_rect(0, non_zero_ind))
      IF (sz_from(1) GE 3L) THEN RETURN, [ang, rad, from_rect(2, *)] $
      ELSE RETURN, [ang, rad, Replicate(zero, 1L, sz_from(2))]
   ENDIF

   IF (Keyword_Set(to_sphere)) THEN BEGIN
      ang1 = Replicate(zero, 1L, sz_from(2))
      ang2 = Replicate(zero, 1L, sz_from(2))
      IF (sz_from(1) LT 3L) THEN z = Replicate(zero, 1L, sz_from(2)) $
      ELSE z = from_rect(2, *)
      rad = Sqrt(from_rect(0, *)^2 + from_rect(1, *)^2 + z^2)
      non_zero_ind = Where(rad GT zero)
      IF (non_zero_ind(0) GE 0L) THEN BEGIN
         ang1(non_zero_ind) = ang_out * $
            Atan(from_rect(1, non_zero_ind), from_rect(0, non_zero_ind))
         ang2(non_zero_ind) = ang_out * Atan(z(0, non_zero_ind), $
            Sqrt(from_rect(0, non_zero_ind)^2 + from_rect(1, non_zero_ind)^2))
      ENDIF
      RETURN, [ang1, ang2, rad]
   ENDIF

   RETURN, from_rect
ENDIF

IF (N_Elements(from_polar) GT 0L) THEN BEGIN ; Convert from polar.
   sz_from = Size(from_polar)
   IF (sz_from(0) EQ 1L) THEN $
      sz_from = [2L, sz_from(1), 1L, sz_from(2), sz_from(3)]

   IF (sz_from(sz_from(0) + 2L) EQ 5L) THEN BEGIN ; Double precision.
      IF (Keyword_Set(degrees)) THEN ang_in = !DPI/180.0D ELSE ang_in = 1.0D
      zero = 0.0D
   ENDIF ELSE BEGIN ; Single precision
      IF (Keyword_Set(degrees)) THEN ang_in = !Dtor ELSE ang_in = 1.0
      zero = 0.0
   ENDELSE

   IF (Keyword_Set(to_rect)) THEN $
      RETURN, [from_polar(1, *) * Cos(ang_in * from_polar(0, *)), $
               from_polar(1, *) * Sin(ang_in * from_polar(0, *))]

   IF (Keyword_Set(to_cylin)) THEN $
      RETURN, [from_polar(0, *), from_polar(1, *), $
               Replicate(zero, 1, sz_from(2))]

   IF (Keyword_Set(to_sphere)) THEN $
      RETURN, [from_polar(0, *), Replicate(zero, 1, sz_from(2)), $
               from_polar(1, *)]

   RETURN, from_polar
ENDIF

IF (N_Elements(from_cylin) GT 0L) THEN BEGIN ; Convert from cylindrical.
   sz_from = Size(from_cylin)
   IF (sz_from(0) EQ 1L) THEN $
      sz_from = [2L, sz_from(1), 1L, sz_from(2), sz_from(3)]

   IF (sz_from(sz_from(0) + 2L) EQ 5L) THEN BEGIN ; Double precision.
      IF (Keyword_Set(degrees)) THEN BEGIN
         ang_in = !DPI/180.0D
         ang_out = 180.0D/!DPI
      ENDIF ELSE BEGIN
         ang_in = 1.0D
         ang_out = 1.0D
      ENDELSE
      zero = 0.0D
   ENDIF ELSE BEGIN ; Single precision
      IF (Keyword_Set(degrees)) THEN BEGIN
         ang_in = !Dtor
         ang_out = !Radeg
      ENDIF ELSE BEGIN
         ang_in = 1.0
         ang_out = 1.0
      ENDELSE
      zero = 0.0
   ENDELSE

   IF (Keyword_Set(to_rect)) THEN $
      RETURN, [from_cylin(1, *) * Cos(ang_in * from_cylin(0, *)), $
               from_cylin(1, *) * Sin(ang_in * from_cylin(0, *)), $
               from_cylin(2, *)]

   IF (Keyword_Set(to_polar)) THEN RETURN, [from_cylin(0, *), from_cylin(1, *)]

   IF (Keyword_Set(to_sphere)) THEN BEGIN
      ang1 = from_cylin(0, *)
      ang2 = Replicate(zero, 1L, sz_from(2))
      rad = Sqrt(from_cylin(1, *)^2 + from_cylin(2, *)^2)
      non_zero_ind = Where(rad GT zero)
      IF (non_zero_ind(0) GE 0L) THEN $
         ang2(non_zero_ind) = ang_out * Atan(from_cylin(2, non_zero_ind), $
                                             from_cylin(1, non_zero_ind))
      RETURN, [ang1, ang2, rad]
   ENDIF

   RETURN, from_cylin
ENDIF

IF (N_Elements(from_sphere) GT 0L) THEN BEGIN ; Convert from spherical.
   sz_from = Size(from_sphere)
   IF (sz_from(0) EQ 1L) THEN $
      sz_from = [2L, sz_from(1), 1L, sz_from(2), sz_from(3)]

   IF (sz_from(sz_from(0) + 2L) EQ 5L) THEN BEGIN ; Double precision.
      IF (Keyword_Set(degrees)) THEN ang_in = !DPI/180.0D ELSE ang_in = 1.0D
      zero = 0.0D
   ENDIF ELSE BEGIN ; Single precision
      IF (Keyword_Set(degrees)) THEN ang_in = !Dtor ELSE ang_in = 1.0
      zero = 0.0
   ENDELSE

   IF (Keyword_Set(to_rect)) THEN BEGIN
      RETURN, [ $
          from_sphere(2, *) * Cos(ang_in * from_sphere(0, *)) * $
                                     Cos(ang_in * from_sphere(1, *)), $
          from_sphere(2, *) * Sin(ang_in * from_sphere(0, *)) * $
                                     Cos(ang_in * from_sphere(1, *)), $
          from_sphere(2, *) * Sin(ang_in * from_sphere(1, *))]
   ENDIF

   IF (Keyword_Set(to_polar)) THEN $
      RETURN, [from_sphere(0, *), $
               from_sphere(2, *) * Cos(ang_in * from_sphere(1, *))]

   IF (Keyword_Set(to_cylin)) THEN $
      RETURN, [from_sphere(0, *), $
               from_sphere(2, *) * Cos(ang_in * from_sphere(1, *)), $
               from_sphere(2, *) * Sin(ang_in * from_sphere(1, *))]

   RETURN, from_sphere
ENDIF

RETURN, 0
END
