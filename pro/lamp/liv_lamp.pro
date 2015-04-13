pro live_lamp_dens, liveT, xx,yy,zz,w, thresh=thresh, box=cdbox, ax=rx,az=rz, xrange=bx, yrange=by, zrange=bz
;** **************
;**
if thresh ne -99 then begin & endif ;thresh already done !!!

if liveT ne 3 then $
PLOT_3DBOX, xx,yy,zz,psym=2, ax=rx,az=rz, xrange=bx, yrange=by, zrange=bz, $
		/solid_walls,color=80,background=255, gridstyle=0 $
else $
PLOT_3DBOX, xx,yy,zz,psym=2, ax=rx,az=rz, xrange=bx, yrange=by, zrange=bz, $
		/solid_walls,color=80,background=255, gridstyle=0, $
		/xy_plane, /xz_plane, /yz_plane
end

pro live_lamp_plot, liveT, xx,ww, xrange=bx, yrange=by
;** **************
;**
X=xx
W=ww
if n_elements(bx) ne 2 then begin mini=min(x ,max=maxi) & bx=[mini,maxi]
endif			else begin id=where((x ge bx(0)) and (x le bx(1))) & X=X(id) & w=w(id,*) & endelse
if n_elements(by) ne 2 then begin mini=min(w ,max=maxi) & by=[mini,maxi] & endif

z=size(w) & if z(0) eq 2 then k=(z(2)-1)<24 else k=0
if k eq 0 then $
  res=execute('LIVE_PLOT, W  , independent=X, xrange=bx, yrange=by, /indexed_color, error=bidon') $
else begin s=''
  for i=0,k do s=s+',w(*,'+string(i)+')'
  res=execute('LIVE_PLOT'+s+', independent=X, xrange=bx, yrange=by, /indexed_color, error=bidon')
endelse
end

pro live_lamp_img, liveT, w, xrange=bx, yrange=by
;** *************
;**
res=execute('LIVE_IMAGE, w')
end

pro live_lamp_surf, liveT, xx,yy,w, ax=rx,az=rz ,rrr=rrr, style=styles
;** **************
;** rrr=1 image+surface		rrr=2 image  +contour
;** rrr=3 image+surface+contour	rrr=4 surface+contour
;** rrr=5 contour			rrr=6 surface
;** rrr=7 vrml

if (size(x))(0) gt 1 then x=xx(*,0)		else x=xx
if (size(y))(0) gt 1 then y=reform(yy(0,*))	else y=yy

if (rrr eq 2) or (rrr eq 3) or (rrr eq 5) then $
	res=execute('LIVE_CONTOUR, w, Xindependent=X, Yindependent=Y, /indexed_color, error=bidon') $ 
else	res=execute('LIVE_SURFACE, w, Xindependent=X, Yindependent=Y, /indexed_color, error=bidon')
end

pro live_lamp_cont, w,GROUP=group, TIT=tit, XTIT=xtit, YTIT=ytit, XX=xx, YY=yy ,WI=idn
;** **************
;**
    ii=execute("livc_lamp, w,GROUP=group, TIT=tit, XTIT=xtit, YTIT=ytit, XX=xx, YY=yy ,WI=idn")
end

pro live_lamp_vol , liveT, w, ax=rx,az=rz ,rrr=rrr, thresh=thresh, name=str
;** **************
;**
pointr=ptr_new(w)
help,pointr
    ii=execute("slicer3, pointr,data_names=str")
end

pro live_lamp_Anim, w, TITLE=tit, GROUP=group, smoo=smoo, surf=did_repr, az=rz,ax=rx ,regul=regul
;** **************
;**
surfc=0 & regul=0
if n_elements(smoo)     ne 1  then smoo=0
if n_elements(did_repr) gt 6  then begin surfc=did_repr(2) & regul=did_repr(6)
	;Care trap_current if surface mode !!!!!
endif
siz =size(w)  & sx=siz(1) & sy=siz(2) & nf=siz(3)
mini=min(w, max=maxi)
				winx=320	   &   winy=320
				xi= winx/siz(1)    &   yi=winy/siz(2)
				if (xi eq 0) then xi= -(float(siz(1))/winx) 
				if (yi eq 0) then yi= -(float(siz(2))/winy)
				if  xi le -1 then fx= -1./xi else fx=xi
				if  yi le -1 then fy= -1./yi else fy=yi

				fm=min([fx,fy])    & fx=fm & fy=fm

				xi= fix(siz(1)*fx) & yi= fix(siz(2)*fy)

				if xi/yi gt 4 then yi=xi/4 else if yi/xi gt 4 then xi=yi/4

				if winx lt xi then xi=winx   &  if winy lt yi then yi=winy

				ff=fix((float(xi)/sx)*(float(yi)/sy)/2)*smoo

loadct,4,/silent

Xinteranimate,set=[xi,yi,nf], /cycle, mpeg_file="lamp.mpg", $
              /showload, /track, title=tit

ff=fix((float(xi)/sx)*(float(yi)/sy)/2)*smoo

for i=0,nf-1 do $
if (xi eq sx) and (yi eq sy) then $
     Xinteranimate, frame=i, image=bytscl(w(*,*,i), max=maxi, min=mini) $
else begin
     if ff lt 3 then $
     Xinteranimate, frame=i, image=bytscl(congrid(w(*,*,i),xi,yi), max=maxi, min=mini) else $
     Xinteranimate, frame=i, image=bytscl(smooth(congrid(w(*,*,i),xi,yi),ff<6), max=maxi, min=mini)
endelse

Xinteranimate, GROUP=group
end

pro liv_lamp
;** *********
end
