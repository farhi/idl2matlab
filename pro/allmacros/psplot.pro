PRO psplot,w,shade=shade,wmax=wmax,wmin=wmin,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,$
             xrange=xrange,yrange=yrange,wrange=wrange,eps=eps,noprint=noprint,printer=printer,$
             error=error,contour=contour,noscreen=noscreen,ax=ax,az=az,help=help,$
             notitle=notitle,noaxis=noaxis,nozaxis=nozaxis,noxaxis=noxaxis,noyaxis=noyaxis,$
             vector=vector,noxtitle=noxtitle,noytitle=noytitle,$
             xformat=xformat,yformat=yformat,wformat=wformat,wset=wset,window=win,$
             xticks=xticks,yticks=yticks,wticks=wticks,random=random,psym=psym

COMMON C_LAMP_W
COMMON C_LAMP_INFO

!p.position=[0.1,0.15,.95,.95]
IF KEYWORD_SET(help) THEN BEGIN
  PRINT,'     modification 01 December  1997 by Thomas Hansen: More options for 3D-surface plots (thermodiffractometries)'
  PRINT,'Last modification 07 September 1998 by Thomas Hansen: window and wset'
ENDIF
print,'T'
print,'PSPLOT'
take_datp,datp
IF KEYWORD_SET(win)  THEN window,win
IF NOT KEYWORD_SET(wset)  THEN wset=0
IF NOT KEYWORD_SET(landscape)  THEN landscape=0
IF NOT KEYWORD_SET(vector)  THEN vector=0
IF NOT KEYWORD_SET(noxaxis) THEN xaxis=1 ELSE xaxis=0
IF NOT KEYWORD_SET(noyaxis) THEN yaxis=1 ELSE yaxis=0
IF NOT KEYWORD_SET(noxaxis) THEN noxaxis=0
IF NOT KEYWORD_SET(noyaxis) THEN noyaxis=0
IF NOT KEYWORD_SET(nozaxis) THEN nozaxis=0
IF NOT KEYWORD_SET(ax)      THEN ax=20
IF NOT KEYWORD_SET(az)      THEN az=20
IF NOT KEYWORD_SET(xmax)    THEN xmax=max(datp.x)
IF NOT KEYWORD_SET(xmin)    THEN xmin=min(datp.x)
IF NOT KEYWORD_SET(xrange)  THEN xrange=[xmin,xmax]
xmax=max(xrange)
xmin=min(xrange)
IF NOT KEYWORD_SET(wmax)    THEN wmax=max(w(WHERE(datp.x GE xrange(0) AND datp.x LE xrange(1),count),*))
IF NOT KEYWORD_SET(wmin)    THEN wmin=min(w(WHERE(datp.x GE xrange(0) AND datp.x LE xrange(1),count),*))
IF NOT KEYWORD_SET(wrange)  THEN wrange=[wmin,wmax]
IF NOT KEYWORD_SET(wmax)    THEN wmax=max(wrange)
IF NOT KEYWORD_SET(wmin)    THEN wmin=min(wrange)
IF NOT KEYWORD_SET(ymax)    THEN ymax=max(datp.y)
IF NOT KEYWORD_SET(ymin)    THEN ymin=min(datp.y)
IF NOT KEYWORD_SET(yrange)  THEN yrange=[ymin,ymax]
IF NOT KEYWORD_SET(psym)    THEN psym=0
ymax=max(yrange)
ymin=min(yrange)
IF NOT KEYWORD_SET(xformat) THEN IF (xmax LT 1000000) AND (xmax-xmin GT 6) THEN xformat='(I6)' ELSE xformat=0
IF NOT KEYWORD_SET(yformat) THEN IF (ymax LT 1000000) AND (ymax-ymin GT 6) THEN yformat='(I6)' ELSE yformat=0
IF NOT KEYWORD_SET(wformat) THEN IF (wmax LT 1000000) AND (wmax-wmin GT 6) THEN wformat='(I6)' ELSE wformat=0
IF NOT KEYWORD_SET(xticks)  THEN xticks=0
IF NOT KEYWORD_SET(yticks)  THEN yticks=0
IF NOT KEYWORD_SET(wticks)  THEN wticks=0

pp=datp.p
help,pp
ww=w
IF N_ELEMENTS(ww(0,0,*)) GT 1 THEN z_tit='counts' ELSE z_tit=datp.z_tit

FOR ii=0,N_ELEMENTS(w(0,0,*))-1 DO BEGIN
pv=datp.pv(*,0,ii)
IF N_ELEMENTS(ww(0,0,*)) GT 1 THEN subtitle=datp.other_tit+'-'+strcompress(string(ii)+' '+datp.par_txt(10+ROUND(pp(0))/10)+string(pv(10+ROUND(pp(0))/10))) ELSE subtitle=datp.other_tit
print,subtitle
IF NOT KEYWORD_SET(notitle)  THEN title=datp.w_tit ELSE title=''
IF NOT KEYWORD_SET(noxtitle) THEN x_tit=datp.x_tit ELSE x_tit=''
IF NOT KEYWORD_SET(noytitle) THEN y_tit=datp.y_tit ELSE y_tit=''
IF     KEYWORD_SET(noztitle) THEN                       z_tit=''
IF     KEYWORD_SET(notitle)  THEN                    subtitle=''
IF     KEYWORD_SET(nozaxis)  THEN zaxis=-1 ELSE zaxis=3
w=ww(*,*,ii)
IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN x =datp.x (*,ii*N_ELEMENTS(ww(0,*,0)):(ii+1)*N_ELEMENTS(ww(0,*,0))-1) ELSE x =datp.x
y=datp.y
SET_PLOT,'ps'
IF N_ELEMENTS(datp.p) GT 29 THEN BEGIN
  filename=strcompress(string(ROUND(datp.p(29))),/remove_all)+'W'+strcompress(string(alone),/remove_all)
ENDIF ELSE BEGIN
  filename=strcompress(string(W_NUMOR(alone)),/remove_all)+'W'+strcompress(string(alone),/remove_all)
ENDELSE
;PRINT,filename,alone,W_NUMOr
IF KEYWORD_SET(random) THEN filename=strcompress(string(round(systime(1) mod 1000)),/remove_all)
IF N_ELEMENTS(ww(0,0,*)) GT 1 THEN filename=filename+'_'+strcompress(string(ii),/remove_all)
filename=filename+'.ps'
IF KEYWORD_SET(eps) THEN eps=1 ELSE eps=0

DEVICE,FILENAME=filename,$
  /LANDSCAPE,XSIZE=25,YSIZE=15,YOFFSET=25,XOFFSET=5,$
  encapsulated=eps,BITS_PER_PIXEL=256,/color
index=WHERE((x LE xmax) AND (x GE xmin))
x=x(index)
w=w(index,*,ii)
IF N_ELEMENTS(w(0,*,0)) GT 1 THEN BEGIN
  index=WHERE((y LE ymax) AND (y GE ymin))
  y=y(index)
  w=w(*,index,ii)
  IF KEYWORD_SET(shade) THEN $
    SHADE_SURF,w,x,y        ,$
          zRANGE  = wrange,ax=ax,az=az, $
          xRANGE  = xrange, $
          CHARSIZE=0.5,CHARTHICK=0.7,xCHARSIZE=1.5*xaxis,yCHARSIZE=1.5*yaxis,zCHARSIZE=1.5,$
          xSTYLE     = 1+noxaxis*4,$
          ySTYLE     = 1+noyaxis*4,$
          zSTYLE     = 1+nozaxis*4,$
          xTICKFORMAT= xformat,$
          yTICKFORMAT= yformat,$
          zTICKFORMAT= wformat,$
          xTICKS     = xticks,$
          yTICKS     = yticks,$
          zTICKS     = wticks,$
          yRANGE  = [ymin,ymax], $
          TITLE   =title+'  '+subtitle    ,$
          xTITLE  =x_tit,$
          yTITLE  =y_tit,$
          zTITLE  =z_tit 
  IF KEYWORD_SET(contour) THEN $
    CONTOUR,w,x,y        ,$
          zRANGE  = wrange, $
          xRANGE  = xrange, $
          yRANGE  = [ymin,ymax], $
          xSTYLE     = 1+noxaxis*4,$
          ySTYLE     = 1+noyaxis*4,$
          xTICKFORMAT= xformat,$
          yTICKFORMAT= yformat,$
          xTICKS     = xticks,$
          yTICKS     = yticks,$
          TITLE   =title    ,$
          SUBTITLE=subtitle,$
          xTITLE  =x_tit,$
          yTITLE  =y_tit,$
          zTITLE  =z_tit 
  IF NOT (KEYWORD_SET(contour) OR KEYWORD_SET(shade)) THEN $
    SURFACE,w,x,y        ,$
          zRANGE     = wrange,ax=ax,az=az, $
          xRANGE     = xrange, $
          yRANGE     = [ymin,ymax], $
          CHARSIZE   = 0.5,CHARTHICK=0.7,xCHARSIZE=1.5*xaxis,yCHARSIZE=1.5*yaxis,zCHARSIZE=1.5,$
          xSTYLE     = 1+noxaxis*4,$
          ySTYLE     = 1+noyaxis*4,$
          zSTYLE     = 1+nozaxis*4,$
          xTICKFORMAT= xformat,$
          yTICKFORMAT= yformat,$
          zTICKFORMAT= wformat,$
          xTICKS     = xticks,$
          yTICKS     = yticks,$
          zTICKS     = wticks,$
          HORIZONTAL = vector,$
          zAXIS      = zaxis,$
          TITLE      = title+'  '+subtitle    ,$
          xTITLE     = x_tit,$
          yTITLE     = y_tit,$
          zTITLE     = z_tit 
ENDIF ELSE BEGIN
  PLOT,x,w,$
       yRANGE  = wrange, psym=psym,$
       xRANGE  = xrange, $
       TITLE=datp.w_tit,$
       SUBTITLE=subtitle,$
       xTITLE=datp.x_tit,$
       yTITLE=datp.y_tit
  IF KEYWORD_SET(error) THEN OPLOTERR,x,w,datp.e,3 
ENDELSE
DEVICE,/CLOSE
IF NOT KEYWORD_SET(printer) THEN line='$lp -dlj1_d20 '+filename ELSE line='$lp -d'+printer+' '+filename
if NOT keyword_set(noprint) THEN  BEGIN
  PRINT,line
  XICUTE,line 
ENDIF
SET_PLOT,'X'
IF NOT KEYWORD_SET(noscreen) THEN BEGIN
  IF N_ELEMENTS(w(0,*)) GT 1 THEN BEGIN
  IF KEYWORD_SET(shade) THEN $
  IF KEYWORD_SET(shade) THEN $
    SHADE_SURF,w,x,y        ,$
          zRANGE  = wrange,ax=ax,az=az, $
          xRANGE  = xrange, $
          CHARSIZE=0.5,CHARTHICK=0.7,xCHARSIZE=1.5*xaxis,yCHARSIZE=1.5*yaxis,zCHARSIZE=1.5,$
          xSTYLE     = 1+noxaxis*4,$
          ySTYLE     = 1+noyaxis*4,$
          zSTYLE     = 1+nozaxis*4,$
          xTICKFORMAT= xformat,$
          yTICKFORMAT= yformat,$
          zTICKFORMAT= wformat,$
          xTICKS     = xticks,$
          yTICKS     = yticks,$
          zTICKS     = wticks,$
          yRANGE  = [ymin,ymax], $
          TITLE   =title+'  '+subtitle    ,$
          xTITLE  =x_tit,$
          yTITLE  =y_tit,$
          zTITLE  =z_tit 
  IF KEYWORD_SET(contour) THEN $
    CONTOUR,w,x,y        ,$
          zRANGE  = wrange, $
          xRANGE  = xrange, $
          yRANGE  = [ymin,ymax], $
          xSTYLE     = 1+noxaxis*4,$
          ySTYLE     = 1+noyaxis*4,$
          xTICKFORMAT= xformat,$
          yTICKFORMAT= yformat,$
          xTICKS     = xticks,$
          yTICKS     = yticks,$
          TITLE   =title    ,$
          SUBTITLE=subtitle,$
          xTITLE  =x_tit,$
          yTITLE  =y_tit,$
          zTITLE  =z_tit 
  IF NOT (KEYWORD_SET(contour) OR KEYWORD_SET(shade)) THEN $
    SURFACE,w,x,y        ,$
          zRANGE     = wrange,ax=ax,az=az, $
          xRANGE     = xrange, $
          yRANGE     = [ymin,ymax], $
          CHARSIZE   = 0.5,CHARTHICK=0.7,xCHARSIZE=1.5*xaxis,yCHARSIZE=1.5*yaxis,zCHARSIZE=1.5,$
          xSTYLE     = 1+noxaxis*4,$
          ySTYLE     = 1+noyaxis*4,$
          zSTYLE     = 1+nozaxis*4,$
          xTICKFORMAT= xformat,$
          yTICKFORMAT= yformat,$
          zTICKFORMAT= wformat,$
          xTICKS     = xticks,$
          yTICKS     = yticks,$
          zTICKS     = wticks,$
          HORIZONTAL = vector,$
          zAXIS      = zaxis,$
          TITLE      = title+'  '+subtitle    ,$
          xTITLE     = x_tit,$
          yTITLE     = y_tit,$
          zTITLE     = z_tit 
ENDIF ELSE BEGIN
  PLOT,x,w,$
       yRANGE  = wrange, psym=psym,$
       xRANGE  = xrange, $
       TITLE=datp.w_tit,$
       SUBTITLE=datp.other_tit,$
       xTITLE=datp.x_tit,$
       yTITLE=datp.y_tit
  IF KEYWORD_SET(error) THEN OPLOTERR,x,w,datp.e,3 
ENDELSE
ENDIF
ENDFOR
w=ww
;give_datp,datp
END
