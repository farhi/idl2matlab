; $Id: create_view.pro,v 1.2 1993/10/04 20:31:27 doug Exp $

; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	CREATE_VIEW
;
; PURPOSE:
;	This procedure sets the various system variables required to
;       define a coordinate system and a 3-D view.   This procedure
;       builds the system viewing matrix (!P.T) in such a way that the
;       correct aspect ratio of the data is maintained even if the
;       display window is not square.
;       CREATE_VIEW also sets the "Data" to "Normal" coordinate
;       conversion factors (!X.S, !Y.S, and !Z.S) so that center of
;       the unit cube will be located at the center of the display
;       window.
;
; CATEGORY:
;	Viewing.
;
; CALLING SEQUENCE:
;       CREATE_VIEW
;	
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;       XMIN:       The minimum data value on the X axis.
;                   The default is (0.0).
;                   Data type : Any scalar numeric value.
;       XMAX:       The maximum data value on the X axis.
;                   The default is (1.0).
;                   Data type : Any scalar numeric value.
;       YMIN:       The minimum data value on the Y axis.
;                   The default is (0.0).
;                   Data type : Any scalar numeric value.
;       YMAX:       The maximum data value on the Y axis.
;                   Data type : Any scalar numeric value.
;                   The default is (1.0).
;       ZMIN:       The minimum data value on the Z axis.
;                   The default is (0.0).
;                   Data type : Any scalar numeric value.
;       ZMAX:       The maximum data value on the Z axis.
;                   The default is (1.0).
;                   Data type : Any scalar numeric value.
;       AX:         The orientation (X rotation) of the view.
;                   The default is (0.0).
;                   Data type : Float.
;       AY:         The orientation (Y rotation) of the view.
;                   The default is (0.0).
;                   Data type : Float.
;       AZ:         The orientation (Z rotation) of the view.
;                   The default is (0.0).
;                   Data type : Float.
;       WINX:       The X size, in pixels, of the window that the
;                   view is being set up for.
;                   The default is (640).
;                   Data type : Long.
;       WINY:       The Y size, in pixels, of the window that the
;                   view is being set up for.
;                   The default is (512).
;                   Data type : Long.
;       ZOOM:       The view zoom factor.   If zoom is a single
;                   value then the view will be zoomed equally in
;                   all 3 dimensions.   If zoom is a 3 element vector
;                   then the view will be scaled zoom(0) in X,
;                   zoom(1) in Y, and zoom(2) in Z.
;                   The default is (1.0).
;                   Data type : Float or Fltarr(3).
;       ZFAC:       Use this keyword to expand or contract the view
;                   in the Z dimension.
;                   The default is (1.0).
;                   Data type : Float.
;       PERSP:      The perspective projection distance.   A value of
;                   (0.0) indicates an isometric projection (NO per-
;                   spective).
;                   The default is (0.0).
;                   Data type : Float.
;       RADIANS:    Set this keyword to a non-zero value if the values
;                   passed to AX, AY, and AZ are in radians.
;                   The default is degrees.
;                   Data type : Int.
;
; SIDE EFFECTS:
;	This procedure sets the following IDL system variables :
;
;          !P.T, !P.T3D, !P.Position, !P.Clip, !P.Region
;          !X.S, !X.Style, !X.Range, !X.Margin
;          !Y.S, !Y.Style, !Y.Range, !Y.Margin
;          !Z.S, !Z.Style, !Z.Range, !Z.Margin
;
; PROCEDURE:
;       This procedure sets the 4x4 system viewing matrix (!P.T) by
;       calling T3D with the following parameters :
;
;       ; Reset (!P.T) to the identity matrix.
;          T3D, /RESET
;       ; Translate the center of the unit cube to the origin.
;          T3D, TRANSLATE=[(-0.5), (-0.5), (-0.5)]
;       ; Zoom the view.
;          T3D, SCALE=ZOOM
;       ; Scale the view to preserve the correct aspect ratio.
;          xrange = xmax - xmin
;          yrange = ymax - ymin
;          zrange = (zmax - zmin) * zfac
;          max_range = xrange > yrange > zrange
;          T3D, SCALE=([xrange, yrange, zrange] / max_range)
;       ; Rotate the view.
;          T3D, ROTATE=[0.0, 0.0, AZ]
;          T3D, ROTATE=[0.0, AY, 0.0]
;          T3D, ROTATE=[AX, 0.0, 0.0]
;       ; Define a perspective projection (if any).
;          IF (p_proj) THEN T3D, PERSPECTIVE=PERSP
;       ; Compensate for the aspect ratio of the display window.
;          T3D, SCALE=[xfac, yfac, 1.0]
;       ; Translate the unit cube back to its starting point.
;          T3D, TRANSLATE=[(0.5), (0.5), (0.5)]
;
; EXAMPLE:
;       Set up a view to display an iso-surface from volumetric data.
;
;       ; Create some data.
;          vol = FLTARR(40, 50, 30)
;          vol(3:36, 3:46, 3:26) = RANDOMU(s, 34, 44, 24)
;          FOR i=0, 10 DO vol = SMOOTH(vol, 3)
;
;       ; Generate the iso-surface.
;          SHADE_VOLUME, vol, 0.2, polygon_list, vertex_list, /LOW
;
;       ; Set up the view.
;       ; Note that the subscripts into the Vol array range from
;       ; 0 to 39 in X, 0 to 49 in Y, and 0 to 29 in Z.   As such,
;       ; the 3-D coordinates of the iso-surface (vertex_list) may
;       ; range from 0.0 to 39.0 in X, 0.0 to 49.0 in Y,
;       ; and 0.0 to 29.0 in Z.   Set XMIN, YMIN, and ZMIN to
;       ; zero (the default), and set XMAX=39, YMAX=49, and ZMAX=29.
;          WINDOW, XSIZE=600, YSIZE=400
;          CREATE_VIEW, XMAX=39, YMAX=49, ZMAX=29, AX=(-60.0), AZ=(30.0), $
;                       WINX=600, WINY=400, ZOOM=(0.7), PERSP=(1.0)
;
;       ; Display the iso surface in the specified view.
;          img = POLYSHADE(polygon_list, vertex_list, /DATA, /T3D)
;          TVSCL, img
;
; MODIFICATION HISTORY:
; 	Written by:	Daniel Carr. Wed Sep  2 16:40:47 MDT 1992
;       Modified the way the view is compensated for the data aspect ratio.
;                       Daniel Carr. Tue Dec  8 17:53:54 MST 1992
;-

PRO Create_View, Xmin=p_xmin, Xmax=p_xmax, Ymin=p_ymin, Ymax=p_ymax, $
                 Zmin=p_zmin, Zmax=p_zmax, Az=p_az, Ay=p_ay, Ax=p_ax, $
                 Winx=p_winx, Winy=p_winy, Zoom=p_zoom, Zfac=p_zfac, $
                 Persp=p_persp, Radians=p_radians

; *** Test inputs

xmin = 0.0
IF (N_Elements(p_xmin) GT 0) THEN xmin = Float(p_xmin(0))
xmax = 1.0
IF (N_Elements(p_xmax) GT 0) THEN xmax = Float(p_xmax(0))
IF (xmax LE xmin) THEN BEGIN
   Print, 'Xmax must be larger than Xmin'
   STOP
ENDIF

ymin = 0.0
IF (N_Elements(p_ymin) GT 0) THEN ymin = Float(p_ymin(0))
ymax = 1.0
IF (N_Elements(p_ymax) GT 0) THEN ymax = Float(p_ymax(0))
IF (ymax LE ymin) THEN BEGIN
   Print, 'Ymax must be larger than Ymin'
   STOP
ENDIF

zmin = 0.0
IF (N_Elements(p_zmin) GT 0) THEN zmin = Float(p_zmin(0))
zmax = 1.0
IF (N_Elements(p_zmax) GT 0) THEN zmax = Float(p_zmax(0))
IF (zmax LE zmin) THEN BEGIN
   Print, 'Zmax must be larger than Zmin'
   STOP
ENDIF

radians = 0B
IF (N_Elements(p_radians) GT 0) THEN radians = Byte(p_radians(0))

az = 0.0
IF (N_Elements(p_az) GT 0) THEN az = Float(p_az(0))
IF (radians GE 1B) THEN az = az * 180.0 / !PI
ay = 0.0
IF (N_Elements(p_ay) GT 0) THEN ay = Float(p_ay(0))
IF (radians GE 1B) THEN ay = ay * 180.0 / !PI
ax = 0.0
IF (N_Elements(p_ax) GT 0) THEN ax = Float(p_ax(0))
IF (radians GE 1B) THEN ax = ax * 180.0 / !PI

winx = 640.0
IF (N_Elements(p_winx) GT 0) THEN winx = Float(p_winx(0))
IF (winx LT 2.0) THEN BEGIN
   Print, 'Window X size must be >= 2'
   STOP
ENDIF
winy = 512.0
IF (N_Elements(p_winy) GT 0) THEN winy = Float(p_winy(0))
IF (winy LT 2.0) THEN BEGIN
   Print, 'Window Y size must be >= 2'
   STOP
ENDIF

zoom = [1.0, 1.0, 1.0]
IF (N_Elements(p_zoom) GT 0) THEN zoom = $
   [Float(p_zoom(0)), Float(p_zoom(0)), Float(p_zoom(0))]
IF (N_Elements(p_zoom) GE 3) THEN zoom = $
   [Float(p_zoom(0)), Float(p_zoom(1)), Float(p_zoom(2))]
IF (Min(zoom) LE 0.0) THEN BEGIN
   Print, 'Zoom factor must be > 0'
   STOP
ENDIF

zfac = 1.0
IF (N_Elements(p_zfac) GT 0) THEN zfac = Float(p_zfac(0))
IF (zfac LT 0.0) THEN BEGIN
   Print, 'Zfac must be >= 0'
   STOP
ENDIF

persp = 0.0
IF (N_Elements(p_persp) GT 0) THEN persp = Float(p_persp(0))
p_proj = 0B
IF (persp GT 0.0) THEN p_proj = 1B

; *** Start setting up view

!X.Style = 1
!X.Range = [xmin, xmax]
!X.Margin = [0, 0]
!Y.Style = 1
!Y.Range = [ymin, ymax]
!Y.Margin = [0, 0]
!Z.Style = 1
!Z.Range = [zmin, zmax]
!Z.Margin = [0, 0]

!X.S = [(-xmin), 1.0] / (xmax - xmin)
!Y.S = [(-ymin), 1.0] / (ymax - ymin)
!Z.S = [(-zmin), 1.0] / (zmax - zmin)

!P.Position = [0.0, 0.0, 1.0, 1.0]
!P.Clip = [0, 0, (winx-1), (winy-1), 0, 0]
!P.Region = [0.0, 0.0, 1.0, 1.0]
!P.T3D = 1

xfac = 1.0
yfac = 1.0
IF (winx GT winy) THEN xfac = winy / winx
IF (winy GT winx) THEN yfac = winx / winy

xrange = xmax - xmin
yrange = ymax - ymin
zrange = (zmax - zmin) * zfac
max_range = xrange > yrange > zrange
xyz_fac = [xrange, yrange, zrange] / max_range

T3d, /Reset
T3d, Translate=[(-0.5), (-0.5), (-0.5)]
T3d, Scale=zoom
T3d, Scale=xyz_fac
T3d, Rotate=[0.0, 0.0, az]
T3d, Rotate=[0.0, ay, 0.0]
T3d, Rotate=[ax, 0.0, 0.0]
IF (p_proj) THEN T3d, Perspective=persp
T3d, Scale=[xfac, yfac, 1.0]
T3d, Translate=[(0.5), (0.5), (0.5)]

RETURN
END
