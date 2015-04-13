; $Id: recon3.pro,v 1.8 1994/11/23 02:31:47 dan Exp $

;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;
;+
; NAME:
;	RECON3
;
; PURPOSE:
;	This function can reconstruct a 3-dimensional data array from
;       two or more images (or projections) of an object.   For example,
;       if you placed a dark object in front of a white background and
;       then photographed it three times (each time rotating the object a
;       known amount) then these three images could be used with RECON3
;       to approximate a 3-D volumetric representation of the object.
;       RECON3 also works with translucent projections of an object.
;       RECON3 returns a 3-D byte array.   RECON3 uses the back-projection
;       method.   In medical imaging and other applications, a method
;       known as "Filtered Backprojection" is often desired.   This may
;       be accomplished here by first filtering the images as desired,
;       and then using the filtered images for the reconstruction.
;
; CATEGORY:
;	Volume Reconstruction
;
; CALLING SEQUENCE:
;       vol = RECON3(Images, Obj_Rot, Obj_Pos, Focal, Dist, $
;                    Vol_Pos, Img_Ref, Img_Mag, Vol_Size)
;
; INPUTS:
;       Images:   The images to use to reconstruct the volume.   Execution
;                 time increases linearly with more images.
;                 Data Type: 8-bit (byte) array with dimensions (x, y, n)
;                 where x is the horizontal image dimension, y is the vertical
;                 image dimension, and n is the number of images.
;
;       Obj_Rot:  The the amount the object is rotated to make it appear as
;                 it does in each image.   The object is first rotated
;                 about the X axis, then about the Y axis, and finally
;                 about the Z axis (with the object's reference point at the
;                 origin.
;                 Data Type: (3, n) Float array where Obj_Rot(0, *) is the X
;                 rotation for each image, Obj_Rot(1, *) is the Y rotation, 
;                 and Obj_Rot(2, *) is the Z rotation.
;                 
;       Obj_Pos:  The position of the the object's reference point RELATIVE to
;                 the camera lens.   The camera lens is located at the
;                 coordinate origin and points in the negative Z direction
;                 (the view up vector points in the positive Y direction).
;                 Obj_Pos should be expressed in this coordinate system.
;                 The values for Obj_Pos, Focal, Dist, and Vol_Pos should all
;                 be expressed in the same units (mm, cm, m, in, ft, etc.).
;                 Data Type: (3, n) Float array where Obj_Pos(0, *) is the X
;                 position for each image, Obj_Pos(1, *) is the Y position, 
;                 and Obj_Pos(2, *) is the Z position.   All the values in
;                 Obj_Pos(2, *) should be less than zero.
;
;       Focal:    The focal length of the lens for each image.   Focal may be
;                 set to zero to indicate a parallel image projection
;                 (infinite focal length).
;                 Data Type: Float array with n elements.
;
;       Dist:     The distance from the camera lens to the image plane (film)
;                 for each image.   Dist should be greater than Focal.
;                 Data Type: Float array with n elements.
;
;       Vol_Pos:  The two opposite corners of a cube that surrounds the object.
;                 Vol_Pos should be expressed in the object's coordinate system
;                 RELATIVE to the object's reference point.
;                 Data Type: (3, 2) Float array where Vol_Pos(*, 0) specifies
;                 one corner and Vol_Pos(*, 1) specifies the opposite corner.
;
;       Img_Ref:  The pixel location at which the object's reference point
;                 appears in each of the images.
;                 Data Type: (2, n) Int or Float array where Img_Ref(0, *) is
;                 the X coordinate for each image and Img_Ref(1, *) is the Y
;                 coordinate.
;
;       Img_Mag:  The magnification factor for each image.   This number is
;                 actually the length (in pixels) that a test object would
;                 appear in an image if it were N units long and N units
;                 distant from the camera lens.
;                 Data Type: (2, n) Int or float array where Img_Mag(0, *) is
;                 the X dimension (in pixels) of a test object for each image,
;                 and Img_Mag(1, *) is the Y dimension.   All elements in
;                 Img_Mag should be greater than or equal to 1.
;
;       Vol_Size: The size of the volume to return.   The returned volume will
;                 be a 3-D byte array with dimensions equal to Vol_Size.
;                 Execution time (and resolution) increases exponentially with
;                 larger values for Vol_Size.
;                 Data Type: Int array with 3 elements where Vol_Size(0)
;                 specifies the X dimension of the volume, Vol_Size(1) specifies
;                 the Y dimension, and Vol_Size(2) specifies the Z dimension.
;
; KEYWORD PARAMETERS:
;       CUBIC:    If set, then cubic interpolation is used.   The default is
;                 to use tri-linear interpolation, which is slightly faster.
;
;       MISSING:  The value for cells in the 3-D volume that do not map to
;                 any of the supplied images.   The Missing value is passed
;                 to the IDL "INTERPOLATE" function.
;                 Data Type: Byte.
;                 Default : 0B
;
;       MODE:     If Mode is less than zero then each cell in the 3-D volume
;                 is the MINIMUM of the corresponding pixels in the images.
;                 If Mode is greater than zero then each cell in the 3-D volume
;                 is the MAXIMUM of the corresponding pixels in the images.
;                 If Mode is equal to zero then each cell in the 3-D volume
;                 is the AVERAGE of the corresponding pixels in the images.
;                 Mode should usually be (-1) when the images contain a bright
;                 object in front of a dark background.   Mode should usually
;                 be (+1) when the images contain a dark object in front of a
;                 light background.   AVERAGE mode requires more memory since
;                 the volume array must temporarily be kept as an INT array
;                 instead of a BYTE array.
;                 Data Type: Int
;                 Default : 0 (average cells)
;
; OUTPUTS:
;	RECON3 returns a 3-D byte array containing the reconstructed object.
;
;       If the images contain low (dark) values where the object is and high
;       (bright) values where the object isn't, then Mode should be set to (+1).
;       If the above is true then the returned volume will have low values
;       where the object is, and high values where the object isn't.
;
;       If the images contain high (bright) values where the object is and low
;       (dark) values where the object isn't, then Mode should be set to (-1).
;       If the above is true then the returned volume will have high values
;       where the object is, and low values where the object isn't.
;
; RESTRICTIONS:
;       In general, the object must be CONVEX for a good reconstruction to be
;       possible.   Concave regions are not easily reconstructed.
;       An empty coffee cup, for example, would be reconstructed as if it
;       were full.
;
;	The images should show strong light/dark contrast between the object
;       and the background.
;
;       The more images the better.   Images from many different angles will
;       improve the quality of the reconstruction.   It is also important to
;       supply images that are parallel and perpendicular to any axes of
;       symmetry.   Using the coffee cup as an example, at least one image
;       should be looking through the opening in the handle.
;
;       Telephoto images are also better for reconstruction purposes than
;       wide angle images.
;
; PROCEDURE:
;	A 4x4 transformation matrix is created for each image based upon the
;       parameters Obj_Rot, Obj_Pos, Focal, Dist, and Img_Ref.   Each cell in
;       the volume is assigned a 3-D coordinate based upon the parameters
;       Vol_Pos and Vol_Size.   These coordinates are multiplied by the
;       transformation matricies to produce x,y image coordinates.   Each cell
;       in the volume is assigned a value that is the AVERAGE, MINIMUM, or
;       MAXIMUM of the image values at the x,y position (depending on Mode).
;
; EXAMPLE:
; ------------------------------------------------------------------------------
;       ; Assumptions for this example :
;       ; The object's major axis is parallel to the Z axis.
;       ; The object's reference point is at its center.
;       ; The camera lens is pointed directly at this reference point.
;       ; The reference point is 5000 mm in front of the camera lens.
;       ; The focal length of the camera lens is 200 mm.
;
;       ; If the camera is focused on the reference point, then the
;       ; distance from the lens to the camera's image plane must be
;       ;    dist = (d * f) / (d - f) =
;       ;    (5000 * 200) / (5000 - 200) = (1000000 / 4800) = 208.333 mm
;
;       ; The object is roughly 600 mm wide and 600 mm high.
;       ; The reference point appears in the exact center of each image.
;
;       ;  If the object is 600 mm high and 5000 mm distant from the camera
;       ;  lens, then the object image height must be
;       ;     hi = (h * f) / (d - f)  =
;       ;     (600 * 200) / (5000 - 200) = (120000 / 4800) = 25.0 mm
;       ;  The object image appears 200 pixels high so the final magnification
;       ;  factor is
;       ;     img_mag = (200 / 25) = 8.0
; 
;
;       imgy = 256
;       frames = 3
; 
;       images = Bytarr(imgx, imgy, frames, /Nozero)
;       obj_rot = Fltarr(3, frames)
;       obj_pos = Fltarr(3, frames)
;       focal = Fltarr(frames)
;       dist = Fltarr(frames)
;       vol_pos = Fltarr(3, 2)
;       img_ref = Fltarr(2, frames)
;       img_mag = Fltarr(2, frames)
; 
;       vol_size = [40, 40, 40]
; 
;       ; The object is 5000 mm directly in front of the camera.
;       obj_pos(0, *) =     0.0
;       obj_pos(1, *) =     0.0
;       obj_pos(2, *) = -5000.0
; 
;       ; The focal length of the lens is constant for all the images.
;       focal(*) = 200.0
; 
;       ; The distance from the lens to the image plane is also constant.
;       dist(*) = 208.333
; 
;       ; The cube surrounding the object is 600 mm X 600 mm.
;       vol_pos(*, 0) = [-300.0, -300.0, -300.0]
;       vol_pos(*, 1) = [ 300.0,  300.0,  300.0]
; 
;       ; The image reference point appears at the center of all the images.
;       img_ref(0, *) = imgx / 2
;       img_ref(1, *) = imgy / 2
; 
;       ; The image magnification factor is constant for all images.
;       ; (The images haven't been cropped or resized).
;       img_mag(*, *) = 8.0
; 
;       ; Only the object rotation changes from one image to the next.
;       ; Note that the object is rotated about the X axis first, then Y,
;       ; and then Z.
; 
;       ; Create some fake images for this example.
;       images(30:160, 20:230, 0) = 255
;       images(110:180, 160:180, 0) = 180
;       obj_rot(*, 0) = [-90.0, 0.0, 0.0]
; 
;       images(70:140, 100:130, 1) = 255
;       obj_rot(*, 1) = [-70.0, 75.0, 0.0]
; 
;       images(10:140, 70:170, 2) = 255
;       images(80:90, 170:240, 2) = 150
;       obj_rot(*, 1) = [-130.0, 215.0, 0.0]
; 
;       ; Reconstruct the volume.
;       vol = RECON3(images, obj_rot, obj_pos, focal, dist, vol_pos, img_ref, $
;                    img_mag, vol_size, Missing=255B, Mode=(-1))
; ------------------------------------------------------------------------------
;
; MODIFICATION HISTORY:
; 	Written by:	Daniel Carr	Thu Feb  4 02:54:29 MST 1993
;	KDB - 23 Dec., 1993 - Variable dist had a conflict with Userlib
;			      function DIST and could cause compile errors.
;			      Renamed variable dist to distance.
;       Modified by:    Daniel Carr     Mon Nov 21 14:21:57 MST 1994
;          Improved performance and added CUBIC keyword.
;       Modified by:    Daniel Carr     Tue Nov 22 12:18:15 MST 1994
;          Fixed bug which affected small focal length images.
;          Improved performance again.
;-

FUNCTION RECON3, images, obj_rot, obj_pos, focal, distance, vol_pos, img_ref, $
                 img_mag, vol_size, Missing=missing, Mode=mode, Cubic=cubic

; *** Check inputs.

sz_images = Size(images)
IF (sz_images(0) NE 3) THEN $
   Message, 'Image array must have 3 dimensions.'
frames = sz_images(3)
ysize = sz_images(2)
xsize = sz_images(1)

sz_obj_rot = Size(obj_rot)
IF (sz_obj_rot(0) NE 2) THEN $
   Message, 'Obj_Rot must be a (3, n) array.'
IF (sz_obj_rot(1) NE 3) THEN $
   Message, 'Obj_Rot must be a (3, n) array.'
IF (sz_obj_rot(2) NE frames) THEN $
   Message, 'Obj_Rot must be a (3, n) array, where n is the number of images.'
obj_rot1 = Float(obj_rot)

sz_obj_pos = Size(obj_pos)
IF (sz_obj_pos(0) NE 2) THEN $
   Message, 'Obj_Pos must be a (3, n) array.'
IF (sz_obj_pos(1) NE 3) THEN $
   Message, 'Obj_Pos must be a (3, n) array.'
IF (sz_obj_pos(2) NE frames) THEN $
   Message, 'Obj_Pos must be a (3, n) array, where n is the number of images.'
ind = Where(obj_pos(2, *) GE 0.0)
IF (ind(0) GE 0) THEN $
   Message, 'The object Z position must be < 0 for all images.'
obj_pos1 = Float(obj_pos)

IF (N_Elements(focal) NE frames) THEN $
   Message, $
      'Focal must contain the same number of elements as there are images.'
ind = Where(focal(*) LE 0.0)
IF (ind(0) GE 0) THEN $
   Message, 'Focal must be > 0 for all images.'
focal1 = Float(focal(*))

IF (N_Elements(distance) NE frames) THEN $
   Message, 'Len must contain the same number of elements as Focal.'
ind = Where(distance(*) LE focal(*))
IF (ind(0) GE 0) THEN $
   Message, 'Len must be greater than Focal for all images.'
dist1 = Float(distance(*))

sz_vol_pos = Size(vol_pos)
IF (sz_vol_pos(0) NE 2) THEN $
   Message, 'Vol_Pos must be a (3, 2) array.'
IF (sz_vol_pos(1) NE 3) THEN $
   Message, 'Vol_Pos must be a (3, 2) array.'
IF (sz_vol_pos(2) NE 2) THEN $
   Message, 'Vol_Pos must be a (3, 2) array.'
vol_pos1 = Float(vol_pos)
IF (vol_pos(0, 0) EQ vol_pos(0, 1)) THEN $
   Message, 'Vol_Pos contains invalid X coordinates.'
IF (vol_pos(1, 0) EQ vol_pos(1, 1)) THEN $
   Message, 'Vol_Pos contains invalid Y coordinates.'
IF (vol_pos(2, 0) EQ vol_pos(2, 1)) THEN $
   Message, 'Vol_Pos contains invalid Z coordinates.'
vpx1 = vol_pos(0, 0) < vol_pos(0, 1)
vpy1 = vol_pos(1, 0) < vol_pos(1, 1)
vpz1 = vol_pos(2, 0) < vol_pos(2, 1)
vpx2 = vol_pos(0, 0) > vol_pos(0, 1)
vpy2 = vol_pos(1, 0) > vol_pos(1, 1)
vpz2 = vol_pos(2, 0) > vol_pos(2, 1)

sz_img_ref = Size(img_ref)
IF (sz_img_ref(0) NE 2) THEN $
   Message, 'Img_Ref must be a (2, n) array.'
IF (sz_img_ref(1) NE 2) THEN $
   Message, 'Img_Ref must be a (2, n) array.'
IF (sz_img_ref(2) NE frames) THEN $
   Message, 'Img_Ref must be a (2, n) array, where n is the number of images.'
img_ref1 = Float(img_ref)

sz_img_mag = Size(img_mag)
IF (sz_img_mag(0) NE 2) THEN $
   Message, 'Img_Mag must be a (2, n) array.'
IF (sz_img_mag(1) NE 2) THEN $
   Message, 'Img_Mag must be a (2, n) array.'
IF (sz_img_mag(2) NE frames) THEN $
   Message, 'Img_Mag must be a (2, n) array, where n is the number of images.'
ind = Where(img_mag(*) LT 1)
IF (ind(0) GE 0L) THEN $
   Message, 'All elements in Img_Mag must be >= 1.'
img_mag1 = Float(img_mag)

IF (N_Elements(vol_size) NE 3) THEN $
   Message, 'Vol_size must contain 3 elements.'
vol_size1 = Long(Abs(vol_size(*)))

; *** Check keywords.

miss = 0B
IF (N_Elements(missing) GT 0L) THEN miss = Byte(missing(0))

eval_mode = 0
IF (N_Elements(mode) GT 0L) THEN eval_mode = Fix(mode(0))

; *** Set up variables.

vx = vol_size1(0)
vy = vol_size1(1)
vz = vol_size1(2)
vxm1 = vx - 1
vym1 = vy - 1
vzm1 = vz - 1

; *** Cell coordinates.
rx = ((vpx2 - vpx1) * Findgen(vx) / Float(vxm1)) + vpx1
ry = ((vpy2 - vpy1) * Findgen(vy) / Float(vym1)) + vpy1
rz = ((vpz2 - vpz1) * Findgen(vz) / Float(vzm1)) + vpz1
xplane = Reform((Temporary(rx) # Replicate(1.0, vy)), (vx * vy))
yplane = Reform((Replicate(1.0, vx) # Temporary(ry)), (vx * vy))
coords = [[Temporary(xplane)], [Temporary(yplane)], $
          [Fltarr((vx * vy), /Nozero)], [Replicate(1.0, (vx * vy))]]

; *** Save current view matrix.
save_t3d = !P.T

; *** Create the volume.
IF (eval_mode GT 0) THEN vol = Bytarr(vx, vy, vz)
IF (eval_mode EQ 0) THEN vol = Intarr(vx, vy, vz)
IF (eval_mode LT 0) THEN vol = Replicate(255B, vx, vy, vz)

FOR j=0, (frames-1) DO BEGIN
   ; *** Set up transform.
   T3d, /Reset
   T3d, Rotate=[obj_rot1(0, j), 0.0, 0.0]
   T3d, Rotate=[0.0, obj_rot1(1, j), 0.0]
   T3d, Rotate=[0.0, 0.0, obj_rot1(2, j)]
   T3d, Translate=obj_pos1(*, j)
   IF (focal1(j) GT 0.0) THEN BEGIN
      T3d, Translate=[0.0, 0.0, -dist1(j)]
      T3d, Perspective=$
           ((focal1(j) / obj_pos1(2, j)) * (obj_pos1(2, j) - dist1(j)))
   ENDIF
   T3d, Scale=[img_mag1(0, j), img_mag1(1, j), 1.0]
   T3d, Translate=[img_ref1(0, j), img_ref1(1, j), 0.0]

   ; *** Fill in the volume one plane at a time.
   FOR i=0, vzm1 DO BEGIN
      coords(0, 2) = Replicate(rz(i), (vx * vy))
      plane_index = coords # !P.T
      vol_x  = plane_index(*, 0) / plane_index(*, 3)
      vol_y  = plane_index(*, 1) / plane_index(*, 3)

      IF (eval_mode LT 0) THEN $
         vol(0, 0, i) = vol(*, *, i) < $
            Interpolate(images(*, *, j), vol_x, vol_y, Missing=miss, $
                        Cubic=Keyword_Set(cubic))

      IF (eval_mode GT 0) THEN $
         vol(0, 0, i) = vol(*, *, i) > $
            Interpolate(images(*, *, j), vol_x, vol_y, Missing=miss, $
                        Cubic=Keyword_Set(cubic))

      IF (eval_mode EQ 0) THEN $
         vol(0, 0, i) = vol(*, *, i) + $
            Fix(Interpolate(images(*, *, j), vol_x, vol_y, Missing=miss, $
                            Cubic=Keyword_Set(cubic)))

   ENDFOR
   Print, 'Completed image ' + String(j+1)
ENDFOR

IF (eval_mode EQ 0) THEN vol = Byte(vol / frames)

; *** Restore view matrix.
!P.T = save_t3d

RETURN, vol
END
