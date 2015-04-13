; $Id: extract_slice.pro,v 1.4 1994/11/21 23:52:33 dan Exp $

; Copyright (c) 1992-1993, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	EXTRACT_SLICE
;
; PURPOSE:
;	This function returns a 2-D planar slice extracted from
;       3-D volumetric data. The slicing plane may be oriented at
;       any angle, and may pass through any desired location in the
;       volume.
;
; CATEGORY:
;	Volume Rendering.
;
; CALLING SEQUENCE:
;       Slice = EXTRACT_SLICE(Vol, X_size, Y_size, X_center, Y_center, $
;                             Z_center, X_rot, Y_rot, Z_rot)
;
; INPUTS:
;       Vol:        The three dimensional volume of data to slice.
;                   Data type : Any 3-D array except string or structure.
;       X_size:     The size of the returned slice in X (The returned
;                   slice will have the dimensions X_size by Y_size).
;                   Data type : Long.
;       Y_size:     The size of the returned slice in Y. To preserve
;                   the correct aspect ratio of the data, Y_size should
;                   equal X_size. For optimal results, set X_size and
;                   Y_size to be greater than or equal to the largest of
;                   the three dimensions of Vol.
;                   Data type : Long.
;       X_center:   The X coordinate (index) within the volume that the
;                   slicing plane passes through. The center of the
;                   slicing plane passes through Vol at the coordinate
;                   (X_center, Y_Center, Z_center).
;                   Data type : Any scalar numeric value (usually Long).
;       Y_center:   The Y coordinate (index) within the volume that the
;                   slicing plane passes through.
;                   Data type : Any scalar numeric value (usually Long).
;       Z_center:   The Z coordinate (index) within the volume that the
;                   slicing plane passes through.
;                   Data type : Any scalar numeric value (usually Long).
;       X_rot:      The orientation (X rotation) of the slicing plane.
;                   Before transformation, the slicing plane is parallel
;                   to the X-Y plane. The slicing plane transformations
;                   are performed in the following order :
;                      1. Rotate Z_rot degrees about the Z axis.
;                      2. Rotate Y_rot degrees about the Y axis.
;                      3. Rotate X_rot degrees about the X axis.
;                      4. Translate the center of the plane to
;                         X_center, Y_center, Z_center.
;                   Data type : Float.
;       Y_rot:      The orientation (Y rotation) of the slicing plane.
;                   Data type : Float.
;       Z_rot:      The orientation (Z rotation) of the slicing plane.
;                   Data type : Float.
;	
; KEYWORD PARAMETERS:
;       CUBIC:      If CUBIC is set, then cubic interpolation is used.
;                   The default is to use tri-linear interpolation.
;                   If the SAMPLE keyword is set, then the CUBIC keyword
;                   is ignored.
;       OUT_VAL:    If OUT_VAL is set, then the portions of the returned
;                   slice that lie outside the original volume are set to
;                   the value passed to OUT_VAL.
;                   Data type : Any scalar numeric value (usually the same
;                               type as Vol).
;       RADIANS:    Set this keyword to a non-zero value to indicate that
;                   X_rot, Y_rot, and Z_rot are in radians. The default
;                   is degrees.
;                   Data type : Int.
;       SAMPLE:     If SAMPLE is set to a non-zero value then nearest
;                   neighbor sampling is used to compute the slice.
;                   Otherwise, tri-linear (or cubic) interpolation is used.
;                   A small reduction in execution time will result if
;                   SAMPLE mode is set and the OUT_VAL keyword is NOT
;                   used.
;
; OUTPUTS:
;       This function returns the planar slice as a two dimensional
;       array with the same data type as Vol. The dimensions of the
;       returned array are X_size by Y_size.
;
; EXAMPLE:
;       Display an oblique slice through volumetric data.
;
;       ; Create some data.
;          vol = RANDOMU(s, 40, 40, 40)
;          FOR i=0, 10 DO vol = SMOOTH(vol, 3)
;          vol = BYTSCL(vol(3:37, 3:37, 3:37))
;
;       ; Extract and display a slice.
;          slice = EXTRACT_SLICE(vol, 40, 40, 17, 17, 17, 30.0, 30.0, 0.0, $
;                                OUT_VAL=0B)
;          TVSCL, REBIN(slice, 400, 400)
;
; MODIFICATION HISTORY:
;       Written by:     Daniel Carr. Wed Sep  2 14:47:07 MDT 1992
;       Modified by:    Daniel Carr. Mon Nov 21 14:59:45 MST 1994
;          Improved speed and added the CUBIC keyword.
;-

FUNCTION Extract_Slice, vol, x_size, y_size, $
                        x_center, y_center, z_center, $
                        x_rot, y_rot, z_rot, Radians=radians, $
                        Out_Val=out_val, Sample=p_sample, $
                        Cubic=cubic

; *** Test inputs

sz_vol = Size(vol)
IF (sz_vol(0) NE 3L) THEN BEGIN
   Print, 'Volume array must have three dimensions'
   STOP
ENDIF
vol_type = sz_vol(sz_vol(0)+1)
IF (vol_type EQ 0L) THEN BEGIN
   Print, 'Volume array must be defined'
   STOP
ENDIF
IF (vol_type EQ 7L) THEN BEGIN
   Print, 'Invalid volume array type (string)'
   STOP
ENDIF
IF (vol_type EQ 8L) THEN BEGIN
   Print, 'Invalid volume array type (structure)'
   STOP
ENDIF

x_size = Long(x_size(0))
IF (x_size LT 2L) THEN BEGIN
   Print, 'X_size must be >= 2'
   STOP
ENDIF
y_size = Long(y_size(0))
IF (y_size LT 2L) THEN BEGIN
   Print, 'Y_size must be >= 2'
   STOP
ENDIF

x_center = Float(x_center(0))
IF ((x_center LT 0.0) OR (x_center GE Float(sz_vol(1)))) THEN BEGIN
   Print, 'X_center must be >= 0 and less than the x dimension of vol'
   STOP
ENDIF
y_center = Float(y_center(0))
IF ((y_center LT 0.0) OR (y_center GE Float(sz_vol(2)))) THEN BEGIN
   Print, 'Y_center must be >= 0 and less than the y dimension of vol'
   STOP
ENDIF
z_center = Float(z_center(0))
IF ((z_center LT 0.0) OR (z_center GE Float(sz_vol(3)))) THEN BEGIN
   Print, 'Z_center must be >= 0 and less than the z dimension of vol'
   STOP
ENDIF

x_rot = Float(x_rot(0))
y_rot = Float(y_rot(0))
z_rot = Float(z_rot(0))

IF (N_Elements(radians) GT 0L) THEN BEGIN
   IF (radians(0) NE 0) THEN BEGIN
      x_rot = x_rot * !RADEG
      y_rot = y_rot * !RADEG
      z_rot = z_rot * !RADEG
   ENDIF
ENDIF

; *** Set up the required variables

IF (N_Elements(out_val) GT 0L) THEN set_out = 1B ELSE set_out = 0B

sample = 0B
IF (N_Elements(p_sample) GT 0L) THEN sample = Byte(p_sample(0))

vol_ind = [[Reform((Findgen(x_size) # Replicate(1.0, y_size)), (x_size * y_size))], $
           [Reform((Replicate(1.0, x_size) # Findgen(y_size)), (x_size * y_size))], $
           [Replicate(0.0, (x_size * y_size))], [Replicate(1.0, (x_size * y_size))]]

; *** Extract the slice

save_pt = !P.T
T3d, /Reset
T3d, Translate=[-(Float(x_size-1L)/2.0), -(Float(y_size-1L)/2.0), 0.0]
T3d, Rotate=[0.0, 0.0, z_rot]
T3d, Rotate=[0.0, y_rot, 0.0]
T3d, Rotate=[x_rot, 0.0, 0.0]
T3d, Translate=Float([x_center, y_center, z_center])
vol_ind = vol_ind # !P.T
!P.T = save_pt

IF (sample) THEN BEGIN
   slice = Reform((vol(0>vol_ind(*, 0)<(sz_vol(1)-1), $
                  0>vol_ind(*, 1)<(sz_vol(2)-1), $
                  0>vol_ind(*, 2)<(sz_vol(3)-1))), x_size, y_size)
   IF (set_out) THEN BEGIN
      out_v = Where((((vol_ind(*, 0) LT 0.0) OR $
                      (vol_ind(*, 0) GE sz_vol(1))) OR $
                     ((vol_ind(*, 1) LT 0.0) OR $
                      (vol_ind(*, 1) GE sz_vol(2)))) OR $
                     ((vol_ind(*, 2) LT 0.0) OR (vol_ind(*, 2) GE sz_vol(3))))
      IF (out_v(0) GE 0L) THEN slice(out_v) = out_val
   ENDIF
ENDIF ELSE BEGIN
   IF (set_out) THEN BEGIN
      slice = $
         Reform((Interpolate(vol, $
            vol_ind(*, 0), vol_ind(*, 1), vol_ind(*, 2), $
            Missing=out_val, Cubic=Keyword_Set(cubic))), x_size, y_size)
   ENDIF ELSE BEGIN
      slice = Reform((Interpolate(vol, $
         vol_ind(*, 0), vol_ind(*, 1), vol_ind(*, 2), Cubic=Keyword_Set(cubic))), $
         x_size, y_size)
   ENDELSE
ENDELSE

RETURN, slice
END
