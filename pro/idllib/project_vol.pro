; $Id: project_vol.pro,v 1.4 1994/11/18 22:03:25 dan Exp $

; Copyright (c) 1992-1993, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	PROJECT_VOL
;
; PURPOSE:
;       This function returns a two dimensional image that is the
;       projection of a 3-D volume of data onto a plane (similar to an
;       X-ray). The returned image is a translucent rendering of the
;       volume (the highest data values within the volume show up as the
;       brightest regions in the returned image). Depth queing and
;       opacity may be used to affect the image. The volume is
;       projected using a 4x4 matrix, so any type of projection may
;       be used including perspective. Typically the system viewing
;       matrix (!P.T) is used as the 4x4 matrix.
;
;	Also, see the intrinsic procedure VOXEL_PROJ which performs many
;	of the same functions as this routine.   VOXEL_PROJ provides the
;       RGBO rendering method.   VOXEL_PROJ is faster, but it does not
;       allow for perspective projections.
;
;       PROJECT_VOL can combine the contents of the Z-buffer with the
;       projection when the Z_BUFFER keyword is set.   PROJECT_VOL will not,
;       however, modify the contents of the Z-buffer.
;
; CATEGORY:
;	Volume Rendering.
;
; CALLING SEQUENCE:
;       Image = PROJECT_VOL(Vol, X_sample, Y_sample, Z_sample)
;
; INPUTS:
;       Vol:        The three dimensional volume of data to project.
;                   Data type : Any 3-D array except string or structure.
;       X_sample:   The number of rays to project along the X dimension
;                   of the image. (The returned image will have the
;                   dimensions X_sample by Y_sample).
;                   Data type : Long.
;       Y_sample:   The number of rays to project along the Y dimension
;                   of the image.
;                   Data type : Long.
;       Z_sample:   The number of samples to take along each ray.
;                   Higher values for X_sample, Y_sample, and Z_sample
;                   increase the image resolution as well as execution time.
;                   Data type : Long.
;
; KEYWORD PARAMETERS:
;       AVG_INTENSITY:
;                   If set, then the average intensity method of projection
;                   is used.   The default is a maximum intensity projection.
;                   This keyword only has effect when NOT using the Z-buffer.
;       CUBIC:      If set, then the cubic method of interpolation is used.
;                   The default is a bilinear interpolation.
;       DEPTH_Q:    Set this keyword to indicate that the image should be
;                   created using depth queing. The depth queing should
;                   be a single floating point value (between 0.0 and 1.0).
;                   This value specifies the brightness of the farthest
;                   regions of the volume relative to the closest regions
;                   of the volume. A value of 0.0 will cause the back
;                   side of the volume to be completely blacked out,
;                   while a value of 1.0 indicates that the back side
;                   will show up just as bright as the front side.
;                   The default is 1.0 (indicating no depth queing).
;                   Data type : Float.
;       OPAQUE:     A 3-D array with the same size and dimensions as Vol.
;                   This array specifies the opacity of each cell in the
;                   volume. Opaque values of 0 allow all light to
;                   pass through. Opaque values are cumulative.
;                   For example, if a ray eminates from a data value of 50,
;                   and then passes through 10 opaque cells (each with a
;                   data value of 0 and an opacity value of 5) then that
;                   ray would be completely blocked out (the cell with the
;                   data value of 50 would be invisible on the returned
;                   image).   The default is no opacity.
;                   Data type : Any 3-D array except string or structure
;                               (usually the same type as Vol).
;       TRANS:      A 4x4 floating point array to use as the
;                   transformation matrix when projecting the volume.
;                   The default is to use the system viewing matrix (!P.T).
;                   Data type : Fltarr(4, 4).
;       XSIZE:      The X size of the image to return.   Congrid is used to
;                   resize the final image to be XSIZE by YSIZE.   The default
;                   is the X size of the current window (or the X size of the
;                   Z-buffer).   If there is no current window then the
;                   default is X_sample.
;                   Data type: Int or Long.
;       YSIZE:      The Y size of the image to return.   Congrid is used to
;                   resize the final image to be XSIZE by YSIZE.   The default
;                   is the Y size of the current window (or the Y size of the
;                   Z-buffer).   If there is no current window then the
;                   default is Y_sample.
;                   Data type: Int or Long.
;       Z_BUFFER:   If set, then the projection is combined with the contents
;                   of the Z-buffer.   The default is to not use the Z-buffer
;                   contents.
;
; OUTPUTS:
;       This function returns the projected volume as a two dimensional
;       array with the same data type as Vol. The dimensions of the
;       returned array are XSIZE by YSIZE.
;
; EXAMPLE:
;       Use "T3D" to set up a viewing projection and render a volume of
;       data using "PROJECT_VOL" :
;
;       ; Create some data.
;          vol = RANDOMU(s, 40, 40, 40)
;          FOR i=0, 10 DO vol = SMOOTH(vol, 3)
;          vol = BYTSCL(vol(3:37, 3:37, 3:37))
;          opaque = RANDOMU(s, 40, 40, 40)
;          FOR i=0, 10 DO opaque = SMOOTH(opaque, 3)
;          opaque = BYTSCL(opaque(3:37, 3:37, 3:37), TOP=25B)
;
;       ; Set up the view.
;          xmin = 0 & ymin = 0 & zmin = 0
;          xmax = 34 & ymax = 34 & zmax = 34
;          !X.S = [-xmin, 1.0] / (xmax - xmin)
;          !Y.S = [-ymin, 1.0] / (ymax - ymin)
;          !Z.S = [-zmin, 1.0] / (zmax - zmin)
;          T3D, /RESET
;          T3D, TRANSLATE=[-0.5, -0.5, -0.5]
;          T3D, SCALE=[0.7, 0.7, 0.7]
;          T3D, ROTATE=[30, -30, 60]
;          T3D, TRANSLATE=[0.5, 0.5, 0.5]
;          window, 0, xsize=512, ysize=512
;
;       ; Generate and display the image.
;          img = PROJECT_VOL(vol, 64, 64, 64, DEPTH_Q=0.7, $
;                OPAQUE=opaque, TRANS=(!P.T))
;          TVSCL, img
;
; MODIFICATION HISTORY:
; 	Written by:	Daniel Carr. Tue Sep  1 17:52:06 MDT 1992
;
;       Modified to increase speed.   Also modified
;       to use current data->normal coordinate conversion.   Added
;       CUBIC, AVG_INTENSITY, XSIZE, YSIZE, and Z_BUFFER keywords.
;                       Daniel Carr. Tue Nov 15 16:03:15 MST 1994
;-

FUNCTION Project_Vol, vol, x_sample, y_sample, z_sample, $
         Depth_q=depth_q, Opaque=opaque, Trans=trans, Cubic=cubic, $
         Xsize=xsize, Ysize=ysize, Avg_Intensity=avg_intensity, $
         Z_Buffer=z_buffer

; *** Test inputs.

size_vol = Size(vol)
vol_type = size_vol(size_vol(0)+1)

x_sample = Long(x_sample(0))
y_sample = Long(y_sample(0))
xy_sample = x_sample * y_sample
z_sample = Long(z_sample(0))
zf_sample = Float(z_sample)
zf_sample_m1 = zf_sample - 1.0
z_sample_m1 = z_sample - 1L
z_max = Float(z_sample - 1L)

IF (n_elements(xsize) LE 0L) THEN BEGIN
   IF (!D.Window GE 0L) THEN xsize = !D.X_Size ELSE xsize = x_sample
ENDIF

IF (n_elements(ysize) LE 0L) THEN BEGIN
   IF (!D.Window GE 0L) THEN ysize = !D.Y_Size ELSE ysize = y_sample
ENDIF

IF (N_ELEMENTS(depth_q) GT 0) THEN depth_q = (Float(depth_q(0)) > 0.0) < 1.0 $
ELSE depth_q = 1.0
depth_q = 1.0 - depth_q

block_out = 0B
IF (N_Elements(opaque) GT 0L) THEN BEGIN
   IF (N_Elements(opaque) EQ N_Elements(vol)) THEN BEGIN
      opaque = Reform(opaque, size_vol(1), size_vol(2), size_vol(3))
      block_out = 1B
   ENDIF ELSE BEGIN
      Print, 'Opaque array must be the same size as volume array.'
      RETURN, Bytarr(xsize, ysize)
   ENDELSE
ENDIF

IF (N_Elements(trans) GT 0) THEN BEGIN
   IF (N_Elements(trans) NE 16) THEN BEGIN
      Print, 'Incorrect number of elements in Trans.'
      RETURN, Bytarr(xsize, ysize)
   ENDIF ELSE BEGIN
      trans = Float(Reform(trans, 4, 4))
   ENDELSE
ENDIF ELSE BEGIN
   trans = !P.T
ENDELSE
trans = Invert(trans, status)
IF (status NE 0) THEN BEGIN
   Print, 'Unable to invert transformation matrix.'
   RETURN, Bytarr(xsize, ysize)
ENDIF

; *** Set up the required variables.

x_ind = (((Float(size_vol(1)) * Findgen(x_sample) / Float(x_sample - 1L)) * $
   !X.S(1)) + !X.S(0)) # Replicate(1.0, y_sample)
y_ind = Replicate(1.0, x_sample) # $
        (((Float(size_vol(2)) * Findgen(y_sample) / Float(y_sample - 1L)) * $
   !Y.S(1)) + !Y.S(0))

max_vol = Float(Max(vol))

IF Keyword_Set(z_buffer) THEN BEGIN ; *** Use Z-Buffer info.
   save_win = !D.Window
   save_name = !D.Name
   Set_Plot, 'Z'
   img = Congrid(Tvrd(), x_sample, y_sample, /Interp, /Minus_One)
   min_img = 0B
   depth_fac = 0B
   depth = Congrid(Tvrd(Channel=1, /Words), x_sample, y_sample, $
      /Interp, /Minus_one)
   xsize = !D.X_size
   ysize = !D.Y_size
   Set_Plot, save_name
   IF (save_win GE 0L) THEN Wset, save_win

   ; *** Do the projection.

   FOR k=0L, z_sample_m1 DO BEGIN
      kf = Float(k)
      z_pos = (kf * !Z.S(1)) + !Z.S(0)

      index = [[x_ind(*)], [y_ind(*)], $
         [replicate(z_pos, xy_sample)], [replicate(1.0, xy_sample)]] # trans

      indx = Float(size_vol(1)) * index(*, 0) / index(*, 3)
      indy = Float(size_vol(2)) * index(*, 1) / index(*, 3)
      indz = Float(size_vol(3)) * index(*, 2) / index(*, 3)

      z_pos = 2.0 * ((((z_pos - 0.5) > (-0.5)) < 0.5) * 32765.0)
      z_ind = Where(z_pos GT depth, count)

      IF (count EQ (xsize * ysize)) THEN BEGIN ; *** Nothing in the way.
         IF (block_out) THEN $
            img = img - $
               (Interpolate(opaque, indx, indy, indz, Missing=min_img, $
                            cubic=Keyword_Set(cubic)) < img)

         depth_fac(0) = ((1.0 - (kf / z_max)) * depth_q * max_vol)
         img = img > ((Interpolate(vol, indx, indy, indz, Missing=min_img, $
                                   cubic=Keyword_Set(cubic)) > $
            depth_fac(0)) - depth_fac(0))
      ENDIF ELSE BEGIN ; *** Something in the way.
         IF (count GE 1L) THEN BEGIN ; *** Something new in front.
            IF (block_out) THEN BEGIN
               temp_img = $
                  Interpolate(opaque, indx, indy, indz, Missing=min_img, $
                              cubic=Keyword_Set(cubic)) < img
               img(z_ind) = img(z_ind) - temp_img(z_ind)
            ENDIF

            depth_fac(0) = ((1.0 - (kf / z_max)) * depth_q * max_vol)
            temp_img = $
               (Interpolate(vol, indx, indy, indz, Missing=min_img, $
                            cubic=Keyword_Set(cubic)) > $
               depth_fac(0)) - depth_fac(0)
            img(z_ind) = img(z_ind) > temp_img(z_ind)
         ENDIF ; *** Nothing new in front.
      ENDELSE
   ENDFOR
ENDIF ELSE BEGIN ; *** Don't use Z-Buffer.
   IF Keyword_Set(avg_intensity) THEN img = Fltarr(x_sample, y_sample) $
   ELSE BEGIN
      CASE vol_type OF
         2: img = Intarr(x_sample, y_sample)
         3: img = Lonarr(x_sample, y_sample)
         4: img = Dblarr(x_sample, y_sample)
         5: img = Fltarr(x_sample, y_sample)
         6: img = Complexarr(x_sample, y_sample)
      ELSE: img = Bytarr(x_sample, y_sample)
      ENDCASE
   ENDELSE

   min_img = img(0)
   depth_fac = img(0)

   ; *** Do the projection.

   FOR k=0L, z_sample_m1 DO BEGIN
      kf = Float(k)
      z_pos = (kf * !Z.S(1)) + !Z.S(0)

      index = [[x_ind(*)], [y_ind(*)], $
         [replicate(z_pos, xy_sample)], [replicate(1.0, xy_sample)]] # trans

      indx = Float(size_vol(1)) * index(*, 0) / index(*, 3)
      indy = Float(size_vol(2)) * index(*, 1) / index(*, 3)
      indz = Float(size_vol(3)) * index(*, 2) / index(*, 3)

      IF (block_out) THEN $
         img = img - $
         (Interpolate(opaque, indx, indy, indz, Missing=min_img, $
                      cubic=Keyword_Set(cubic)) < img)

      depth_fac(0) = ((1.0 - (kf / z_max)) * depth_q * max_vol)
      IF Keyword_Set(avg_intensity) THEN $
         img = img + (((Interpolate(vol, indx, indy, indz, Missing=min_img, $
                        cubic=Keyword_Set(cubic)) > $
            depth_fac(0)) - depth_fac(0)) / zf_sample) $
      ELSE $
         img = img > ((Interpolate(vol, indx, indy, indz, Missing=min_img, $
                       cubic=Keyword_Set(cubic)) > $
            depth_fac(0)) - depth_fac(0))
   ENDFOR
   IF Keyword_Set(avg_intensity) THEN BEGIN
      CASE vol_type OF
         2: img = Fix(img)
         3: img = Long(img)
         4: img = Double(img)
         5: img = Float(img)
         6: img = Complex(img)
      ELSE: img = Byte(img)
      ENDCASE
   ENDIF
ENDELSE

IF ((xsize NE x_sample) OR (ysize NE y_sample)) THEN $
  img = Congrid(img, xsize, ysize, /Interp, /Minus_One)

RETURN, img
END
