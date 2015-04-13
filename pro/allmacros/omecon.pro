
function omecon, win ,dlt_omega
;*******
;**
;** Transform rectangular coordinates to radial for Omega scan on D7.
;** Theta=xin
;** Omega=yin
@lamp.cbk

wout=0
wk  =strtrim(string(two),2)
xin =0
yin =0
lamd=0
ii  =execute('xin =x'+wk)
ii  =execute('yin =y'+wk)
ii  =execute('lamd=p'+wk+'(27)')

sw=size(win)
sx=size(xin) & sx=sx(1)
sy=size(yin) & sy=sy(1)

if (sw(1) eq sx) and (sw(2) eq sy) then begin

	iout  =fltarr(sx,sy)
	if n_elements(dlt_omega) eq 0 then dlt_omega=1

	for  j=0,sy-1 do $
	for  i=0,sx-1 do iout(i,j)=xin(i)/2+yin(j) + dlt_omega

	iout  = iout*!pi/180.

	k = 2*!pi/lamd
	fact  = 2   * k * sin(xin/2*!pi/180)

	jout  = cos(iout)
	iout  =-sin(iout)

	for  j=0,sy-1 do begin
		iout(*,j)=iout(*,j)*fact
		jout(*,j)=jout(*,j)*fact
	endfor

	triangulate ,iout,jout    ,triangles
	wout=trigrid(iout,jout,win,triangles)

	x_tit(one)='Qx'
	y_tit(one)='Qy'
	w_tit(one)=w_tit(two)

	DRAWIND
	nv=6  &  col=(indgen(nv)+1)*10+50
;	lv=[2700,3000,3500,4700]
;	contour,win,iout,jout,levels=[10000,20000]
	contour,win,iout,jout,nlevels=nv,/fill,c_colors=col,$
			 title=w_tit(one),xtitle=x_tit(one),ytitle=y_tit(one)
;	contour,win,iout,jout,c_colors=col,nlevels=nv

	sw  =size(wout)
	xin =findgen(sw(1))-sw(1)/2 & xin=-xin/xin(0)*2*k
	yin =findgen(sw(2))-sw(2)/2 & yin=-yin/yin(0)*2*k
	wk  =strtrim(string(one),2)
	ii  =execute('x'+wk+'=xin')
	ii  =execute('y'+wk+'=yin')
endif

return,wout
end
