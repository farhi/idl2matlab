pro dial_tornade_macro ,D
;** ******************
;**
common	c_torn, pix_win,pix_sav,see_win,winx,winy,$
	c_back,c_ground,c_clouds,c_torn,c_spot,$
	clouds_pts,ground_pts,tornado_pts,arc_pts,trans,ident4,$
	max_torn_pt,max_torn_pt_m1,max_torn_pt_m2,ramp,clouds_ang,ground_ang,$
	pos_clouds_x ,pos_clouds_y ,pos_clouds_z ,vel_clouds_x ,vel_clouds_y ,vel_clouds_z,$
	pos_ground_x ,pos_ground_y ,pos_ground_z ,vel_ground_x ,vel_ground_y ,vel_ground_z,$
	pos_tornado_x,pos_tornado_y,pos_tornado_z,vel_tornado_x,vel_tornado_y,vel_tornado_z,$
	twist_ang,spin_pts,plane_pts,spin_tornado,spin_change,speed_tornado,$
	time_inc,float_vel,grav_time,count

IF not D.init then begin D.init=1
	c_back   =   5
	c_ground =  60
	c_clouds = 100
	c_torn   = 190
	c_spot   = 250
	IF n_elements(pix_sav) eq 0 THEN pix_sav=0
	IF n_elements(pix_win) eq 0 THEN pix_win=0
	DialWset   &  see_win =!d.Window  &  winx=!d.x_size  &  winy=!d.y_size
	
   clouds_pts  = 200
   ground_pts  = 200
   tornado_pts = 25
   arc_pts     = 7

   clouds_center_x = 0.5
   clouds_center_y = 0.5

   trans = Fltarr(4, 4)
   ident4 = trans
   ident4([0, 5, 10, 15]) = 1.0

   max_torn_pt = tornado_pts - 1
   max_torn_pt_m1 = max_torn_pt - 1
   max_torn_pt_m2 = max_torn_pt - 2

   arc_ones = Replicate(1.0, arc_pts)
   zeroes = Fltarr(tornado_pts)
   ramp = 1.0 - (Findgen(tornado_pts) / Float(tornado_pts - 1))
   clouds_ang = Fltarr(clouds_pts)
   ground_ang = Fltarr(ground_pts)

   temp = Randomu(s, 1)

   pos_clouds_x = (Randomu(s, clouds_pts) * 2.0) - 0.5
   pos_clouds_y = (Randomu(s, clouds_pts) * 2.0) - 0.5
   pos_clouds_z = Replicate(1.0, clouds_pts)

   vel_clouds_x = Randomu(s, clouds_pts) /  100.0
   vel_clouds_y = Randomu(s, clouds_pts) /  100.0
   vel_clouds_z = Randomu(s, clouds_pts) / 1000.0

   pos_ground_x = (Randomu(s, ground_pts) * 2.0) - 0.5
   pos_ground_y = (Randomu(s, ground_pts) * 2.0) - 0.5
   pos_ground_z = Fltarr(ground_pts)

   vel_ground_x = Fltarr(ground_pts)
   vel_ground_y = Fltarr(ground_pts)
   vel_ground_z = Fltarr(ground_pts)

   pos_tornado_x = Replicate(clouds_center_x, tornado_pts)
   pos_tornado_y = Replicate(clouds_center_y, tornado_pts)
   pos_tornado_z = Replicate(1.0, tornado_pts)

   vel_tornado_x = Fltarr(tornado_pts)
   vel_tornado_y = Fltarr(tornado_pts)
   vel_tornado_z = Replicate((-0.01), tornado_pts) * ramp

   twist_ang = Randomu(s, tornado_pts) * 180.0
   ang_inc = 360.0 / Float(arc_pts)

   spin_pts = Fltarr(3, arc_pts, tornado_pts)

   plane_pts = Fltarr(3, arc_pts)
   ang = 0.0
   FOR i=0, (arc_pts - 1) DO BEGIN
      plane_pts(0, i) = Cos(ang * !DTOR)
      plane_pts(1, i) = Sin(ang * !DTOR)
      ang = ang + ang_inc
   ENDFOR

   spin_tornado = Fltarr(tornado_pts)
   spin_change = Replicate(0.1, tornado_pts)
   speed_tornado = Fltarr(tornado_pts)

   time_inc = 0.025
   float_vel = 0.1
   grav = 1.0
   grav_time = grav * time_inc
   pi2 = !PI * 2.0

   count = 0.0
ENDIF

kp_p=!P & kp_x=!X & kp_y=!Y & kp_z=!Z

IF pix_win eq 0 THEN BEGIN Window,/Free,/Pixmap,Xsize=winx,Ysize=winy,Retain=1 & pix_win=!D.Window & endif
IF pix_sav eq 0 THEN BEGIN Window,/Free,/Pixmap,Xsize=winx,Ysize=winy,Retain=1 & pix_sav=!D.Window
                           Device, Copy=[0, 0, winx, winy, 0, 0, see_win]      & endif

;THE LOOP
      Create_View, Winx=winx, Winy=winy, Az=30, Ay=0, Ax=-90, Zfac=1., Zoom=0.6, Persp=2.
      count = count + 1.0

      vel_clouds_z  = Temporary(vel_clouds_z) + (float_vel / 20.0)

      z_pos_ramp    = 1.0 - pos_tornado_z

      vel_tornado_x = Temporary(vel_tornado_x) + (ramp * z_pos_ramp^2 * (Randomu(s, tornado_pts) - 0.5) / 4.0)

      vel_tornado_y = Temporary(vel_tornado_y) + (ramp * z_pos_ramp^2 * (Randomu(s, tornado_pts) - 0.5) / 4.0)

      vel_tornado_z = Temporary(vel_tornado_z) - (ramp * float_vel * (Randomu(s, tornado_pts) - 0.5))

      IF (pos_tornado_z(0) GE 0.25) THEN vel_tornado_z(0) = vel_tornado_z(0) < vel_tornado_z(1) < (0.0)

      vel_tornado_x(max_torn_pt) = 0.0
      vel_tornado_y(max_torn_pt) = 0.0
      vel_tornado_z(max_torn_pt) = 0.0

      vel_tornado_x = Smooth(Temporary(vel_tornado_x),  3); , /Edge_Truncate)
      vel_tornado_y = Smooth(Temporary(vel_tornado_y),  3); , /Edge_Truncate)
      vel_tornado_z = Smooth(Temporary(vel_tornado_z),  3); , /Edge_Truncate)

      pos_tornado_x = Temporary(pos_tornado_x) + (vel_tornado_x * time_inc)
      pos_tornado_y = Temporary(pos_tornado_y) + (vel_tornado_y * time_inc)
      pos_tornado_z = Temporary(pos_tornado_z) + (vel_tornado_z * time_inc)

;     IF (pos_tornado_z(0) GT 0.0) THEN BEGIN
;        index      = Where((pos_tornado_z(1:*)-pos_tornado_z(0:max_torn_pt_m1)) LE 0.0)
;        IF (index(0) GT 0L) THEN pos_tornado_z(index) = pos_tornado_z(index) - (0.005)
;     ENDIF

      pos_tornado_x(0) = pos_tornado_x(1)
      pos_tornado_x(max_torn_pt) = pos_tornado_x(max_torn_pt_m1)
      pos_tornado_x    = Smooth(Temporary(pos_tornado_x),  3); , /Edge_Truncate)

      pos_tornado_y(0) = pos_tornado_y(1)
      pos_tornado_y(max_torn_pt) = pos_tornado_y(max_torn_pt_m1)
      pos_tornado_y    = Smooth(Temporary(pos_tornado_y),  3); , /Edge_Truncate)

      pos_tornado_z(0) = pos_tornado_z(0) < (pos_tornado_z(1) - 0.01)
      pos_tornado_z(max_torn_pt) = 1.0
      pos_tornado_z    = Smooth(Temporary(pos_tornado_z),  3)

      index = Where((pos_tornado_x LE 0.0) OR (pos_tornado_x GE 1.0))
      IF (index(0) GE 0L) THEN vel_tornado_x(index) = 0.0
      index = Where((pos_tornado_y LE 0.0) OR (pos_tornado_y GE 1.0))
      IF (index(0) GE 0L) THEN vel_tornado_y(index) = 0.0
      index = Where((pos_tornado_z LE 0.0) OR (pos_tornado_z GE 1.0))
      IF (index(0) GE 0L) THEN vel_tornado_z(index) = 0.0
      
      pos_tornado_x = (Temporary(pos_tornado_x) > 0.0) < 1.0
      pos_tornado_y = (Temporary(pos_tornado_y) > 0.0) < 1.0
      pos_tornado_z = (Temporary(pos_tornado_z) > 0.0) < 1.0

;     speed_tornado =  ((1.5 + ramp) * time_inc * 20.0 * (Sqrt(spin_tornado) + 0.001))
      speed_tornado =  ((0.5 + ramp) * time_inc * 50.0 * (Sqrt(spin_tornado) + 0.001))

      spin_change   = Temporary(spin_change) + $
         Smooth(((Randomu(s, tornado_pts) - 0.5) / 10.0), 3); , /Edge_Truncate)
      spin_change   = Smooth(Temporary(spin_change), 3); , /Edge_Truncate)

      spin_change   =  (Temporary(spin_change) > (-0.125)) < (0.125)
      spin_tornado  = ((Temporary(spin_tornado) + (spin_change * time_inc)) > 0.0) < 0.15
      spin_tornado(max_torn_pt) = spin_tornado(max_torn_pt_m1) + ((0.001 * count)  < 0.15)

      spin_tornado  = Smooth(Temporary(spin_tornado),  3); , /Edge_Truncate)
      spin_tornado  = Smooth(Temporary(spin_tornado), (tornado_pts/4)); , /Edge_Truncate)
      spin_tornado(0) = spin_tornado(1) * (1.0 - pos_tornado_z(0))^4

      twist_ang     = Temporary(twist_ang) + (10.0 * time_inc * speed_tornado)
      index         = Where(twist_ang GE 360.0)
      IF (index(0) GE 0L) THEN twist_ang(index) = twist_ang(index) - 360.0

      ground_dx = pos_ground_x - pos_tornado_x(0)
      ground_dy = pos_ground_y - pos_tornado_y(0)
      ground_dz = pos_ground_z - pos_tornado_z(0)
      ground_dist = Sqrt(ground_dx^2 + ground_dy^2)
      index = Where(ground_dist GT 0.0)
      ground_ang(*) = 0.0
      ground_ang(index) = Atan(ground_dy(index), ground_dx(index))

      ground_xyz_dist   = Sqrt(ground_dx^2 + ground_dy^2 + ground_dz^2)

      vect_x   = pos_tornado_x(2) - pos_tornado_x(0)
      vect_y   = pos_tornado_y(2) - pos_tornado_y(0)
      pt_dz    = Sqrt(vect_x^2 + vect_y^2)
      IF (pt_dz GT 0.0) THEN pt_ang = Atan(vect_y, vect_x) ELSE pt_ang = 0.0
      ang_diff = pt_ang - ground_ang
      tilt_fac = (pt_dz * (-Cos(ang_diff))) + (Randomu(s, ground_pts) / 2.0)

      vel_ground_z =   Temporary(vel_ground_z) - (grav_time)

      pos_ground_x = ((Temporary(pos_ground_x) + (vel_ground_x * time_inc)) > (-1.0)) < 2.0
      pos_ground_y = ((Temporary(pos_ground_y) + (vel_ground_y * time_inc)) > (-1.0)) < 2.0
      pos_ground_z =  (Temporary(pos_ground_z) + (vel_ground_z * time_inc)) > 0.0
      index = Where(pos_ground_z LE 0.0)
      IF (index(0) GE 0L) THEN BEGIN
         vel_ground_x(index) = 0.0
         vel_ground_y(index) = 0.0
         vel_ground_z(index) = time_inc * spin_tornado(1) * 10.0 * tilt_fac(index) / $
            (ground_xyz_dist(index) + 0.25)^6
      ENDIF

      index = Where(pos_ground_z GT 0.0)
      IF (index(0) GE 0L) THEN BEGIN
         vel_ground_x(index) = vel_ground_x(index) - $
            (Sin(ground_ang(index)) * 4.0 * spin_tornado(1) / (1.5*(ground_xyz_dist(index) + 0.5))^6)
         vel_ground_y(index) = vel_ground_y(index) + $
            (Cos(ground_ang(index)) * 4.0 * spin_tornado(1) / (1.5*(ground_xyz_dist(index) + 0.5))^6)
      ENDIF

      vel_ground_x = Temporary(vel_ground_x) * 0.85
      vel_ground_y = Temporary(vel_ground_y) * 0.85
      vel_ground_z = Temporary(vel_ground_z) * 0.95

      clouds_dx    = pos_clouds_x - pos_tornado_x(max_torn_pt)
      clouds_dy    = pos_clouds_y - pos_tornado_y(max_torn_pt)
      clouds_dist  = Sqrt(clouds_dx^2 + clouds_dy^2)
      index        = Where(clouds_dist GT 0.0)
      clouds_ang(*)= 0.0
      clouds_ang(index) = Atan(clouds_dy(index), clouds_dx(index))
      vel_clouds_x = Temporary(vel_clouds_x) - $
         (Sin(clouds_ang) * 2.0 * spin_tornado(max_torn_pt) / (1.5*(clouds_dist + 0.5))^4)
      vel_clouds_y = Temporary(vel_clouds_y) + $
         (Cos(clouds_ang) * 2.0 * spin_tornado(max_torn_pt) / (1.5*(clouds_dist + 0.5))^4)
      vel_clouds_x = Temporary(vel_clouds_x) + (0.075 * (Randomu(s, clouds_pts) - 0.5))
      vel_clouds_y = Temporary(vel_clouds_y) + (0.075 * (Randomu(s, clouds_pts) - 0.5))
      vel_clouds_x = Temporary(vel_clouds_x) * 0.75
      vel_clouds_y = Temporary(vel_clouds_y) * 0.75

      vel_clouds_x = Temporary(vel_clouds_x) - (0.03 * Cos(clouds_ang) / (1.0 + (10.0 * clouds_dist)))
      vel_clouds_y = Temporary(vel_clouds_y) - (0.03 * Sin(clouds_ang) / (1.0 + (10.0 * clouds_dist)))

      vect_x   = pos_tornado_x(max_torn_pt) - pos_tornado_x(max_torn_pt_m2)
      vect_y   = pos_tornado_y(max_torn_pt) - pos_tornado_y(max_torn_pt_m2)
      pt_dz    = Sqrt(vect_x^2 + vect_y^2)
      IF (pt_dz GT 0.0) THEN pt_ang = Atan(vect_y, vect_x) ELSE pt_ang = 0.0
      ang_diff = pt_ang - clouds_ang
      tilt_fac = pt_dz * (-Cos(ang_diff)) * 5.0

      clouds_dz    = pos_clouds_z - pos_tornado_z(max_torn_pt)
      clouds_dist  = (clouds_dx^2 + clouds_dy^2 + clouds_dz^2)

      vel_clouds_z = Temporary(vel_clouds_z) - (spin_tornado(max_torn_pt) * tilt_fac / (clouds_dist > 0.01))

      vel_clouds_z = Temporary(vel_clouds_z) + ((Randomu(s, clouds_pts) - 0.5) * float_vel / 40.0)

      vel_clouds_z =(Temporary(vel_clouds_z) > (-float_vel)) < float_vel

      pos_clouds_x =   Temporary(pos_clouds_x) + (vel_clouds_x * time_inc)
      pos_clouds_y =   Temporary(pos_clouds_y) + (vel_clouds_y * time_inc)
      pos_clouds_z = ((Temporary(pos_clouds_z) + (vel_clouds_z * time_inc)) > 0.0) < 1.0

      index = Where(Sqrt((pos_clouds_x - 0.5)^2 + (pos_clouds_y - 0.5)^2) GT 1.0)
      IF (index(0) GE 0L) THEN BEGIN
        vel_clouds_x(index) = 0.0
        vel_clouds_y(index) = 0.0
      ENDIF


      Wset, pix_win
      Erase, c_back

      FOR i=0, max_torn_pt DO BEGIN

         prev_pt = (i - 1) > 0
         next_pt = (i + 1) < max_torn_pt

         vect_x  =  pos_tornado_x(next_pt) - pos_tornado_x(prev_pt)
         vect_y  =  pos_tornado_y(next_pt) - pos_tornado_y(prev_pt)
         vect_z  = (pos_tornado_z(next_pt) - pos_tornado_z(prev_pt)) > 0.001 > $
                                           (0.01 * Float(i) / Float(max_torn_pt))
         xy_len  = Sqrt(vect_x^2 + vect_y^2)

         IF (xy_len GT 0.0) THEN BEGIN
            ang_z = Atan(vect_y, vect_x)
            ang_y = (!PI / 2.0) - (Atan(vect_z, xy_len))
         ENDIF ELSE BEGIN
            ang_z = 0.0
            ang_y = 0.0
         ENDELSE

         trans = ident4

         sf    = (spin_tornado(i)) * 1.25
         m4x4  = ident4
         m4x4([0, 5, 10]) = [sf, sf, 1.0]
         trans = Temporary(trans) # m4x4

         m4x4  = ident4
         s_ang = Sin(twist_ang(i)-ang_z)
         c_ang = Cos(twist_ang(i)-ang_z)
         m4x4(0,0) = c_ang
         m4x4(0,1) = s_ang
         m4x4(1,0) = (-s_ang)
         m4x4(1,1) = c_ang
         trans = Temporary(trans) # m4x4

         m4x4  = ident4
         s_ang = Sin(ang_y)
         c_ang = Cos(ang_y)
         m4x4(0,0) = c_ang
         m4x4(0,2) = (-s_ang)
         m4x4(2,0) = s_ang
         m4x4(2,2) = c_ang
         trans = Temporary(trans) # m4x4

         m4x4  = ident4
         s_ang = Sin(ang_z)
         c_ang = Cos(ang_z)
         m4x4(0,0) = c_ang
         m4x4(0,1) = s_ang
         m4x4(1,0) = (-s_ang)
         m4x4(1,1) = c_ang
         trans = Temporary(trans) # m4x4

         m4x4  = ident4
         m4x4([3,7,11]) = [pos_tornado_x(i), pos_tornado_y(i), pos_tornado_z(i)]
         trans = Temporary(trans) # m4x4

         new_pts = Vert_T3d(plane_pts, Matrix=trans)
         new_pts(2, *) = (new_pts(2, *) > 0.0) < 1.0
         spin_pts(0, 0, i) = new_pts

      ENDFOR

      Plots, pos_ground_x, pos_ground_y, pos_ground_z, Psym=4, Symsize=1.5, $
         /Normal, /T3d, Color=c_ground

      Plots, pos_clouds_x, pos_clouds_y, pos_clouds_z, Psym=1, Symsize=1.0, $
         /Normal, /T3d, Color=c_clouds

      Plots, Reform(spin_pts, 3, arc_pts*tornado_pts), $
         Psym=4, Symsize=0.5, /Normal, /T3d, Color=c_torn

;     Plots, pos_tornado_x, pos_tornado_y, pos_tornado_z, $
;       /Normal, /T3d, Color=c_spot

      Wset, see_win
      Device, Copy=[0, 0, winx, winy, 0, 0, pix_win]
;     Empty

      !P=kp_p & !X=kp_x & !Y=kp_y & !Z=kp_z

      R=DialOn()
      ;evv=WIDGET_EVENT(/nowait) ; if evv.id gt 0 
      IF  R eq 0 THEN BEGIN
	DialStop
	Device, Copy=[0, 0, winx, winy, 0, 0, pix_sav]
	Wdelete,pix_win & pix_win=0
	Wdelete,pix_sav & pix_sav=0
      ENDIF
END

function dial_tornade
;******* ************
;**
return, {INIT:0,DURATION:900,FREQUENCY:.06,PLOT:-2,WUPDATE:-1}
end
