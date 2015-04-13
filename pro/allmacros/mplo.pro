PRO mplo,wn,$
     xrange=xr,wrange=wr,TITLE=tit,SUBTITLE=sub,XTITLE=xtit,YTITLE=ytit,$
     ps=ps,landscape=landscape,portrait=portrait,noprint=noprint,printer=printer
@lamp.cbk
wn=ROUND(wn)
wn=wn>1
wn=wn<20
wn=wn(SORT(wn))
wn=wn(UNIQ(wn))
IF NOT KEYWORD_SET(portrait)  THEN portrait =0
IF NOT KEYWORD_SET(landscape) THEN landscape=0
IF NOT KEYWORD_SET(landscape) AND NOT  KEYWORD_SET(portrait) THEN BEGIN
  IF KEYWORD_SET(eps) THEN portrait=1 ELSE landscape=1
ENDIF
TVLCT,[0,255,0,0],[0,0,255,0],[0,0,0,255]
colors=['black', 'red  ', 'green', 'blue ']
nw=N_ELEMENTS(wn)
info=''
FOR i=0,nw-1 DO BEGIN 
  take_datp,datp,w=wn(i)
  take_w,w,w=wn(i)
  PRINT,i,' : Workspace ',wn(i),', Numor:',ROUND(datp.p(29<(N_ELEMENTS(datp.p)-1))),' ',his(wn(i))
  PRINT,i,' ',colors(i mod 4),min(datp.x),'<x<',max(datp.x),min(w),'<w<',max(w),' ',W_NUMOR(wn(i))
  info=info+', W'+STRCOMPRESS(wn(i),/REMOVE_ALL)+':'+STRCOMPRESS(colors(i mod 4),/REMOVE_ALL)+'='+STRCOMPRESS(his(wn(i)))
  IF i EQ 0 THEN BEGIN
    xmin=min(datp.x)
    xmax=max(datp.x)
    wmin=min(w)
    wmax=max(w)
  ENDIF ELSE BEGIN
    xmin=min(datp.x)<xmin
    xmax=max(datp.x)>xmax
    wmin=min(w)<wmin
    wmax=max(w)>wmax
  ENDELSE
ENDFOR
i=nw-1
IF STRLEN(info) GE 190 THEN info=STRMID(info,0,190)
IF NOT KEYWORD_SET(xr)  THEN xr=[xmin,xmax]
IF NOT KEYWORD_SET(wr)  THEN wr=[wmin,wmax]
IF NOT KEYWORD_SET(sub) THEN sub=info
IF NOT KEYWORD_SET(tit) THEN tit='W'+STRCOMPRESS(wn(i),/REMOVE_ALL)+' '+STRCOMPRESS(colors(i mod 4),/REMOVE_ALL)+datp.w_tit+datp.other_tit
IF NOT KEYWORD_SET(xtit) THEN xtit=datp.x_tit
IF NOT KEYWORD_SET(ytit) THEN ytit=datp.y_tit
print,STRLEN(info)
PLOT,xr,[0,0], XSTYLE=1, YSTYLE=1, XTIT=xtit, YTIT=ytit, CHARSIZE=0.6,$
               TITLE=tit, SUBTITLE=sub,xr=xr,yr=wr,background=255,color=0

FOR i=0,nw-1 DO BEGIN 
  take_datp,datp,w=wn(i)
  take_w,w,w=wn(i)
  ns=N_ELEMENTS(w(0,*))
  FOR j=0,ns-1 DO BEGIN
    oplot,datp.x(*,j<(N_ELEMENTS(datp.x(0,*))-1)),w(*,j),color=i mod 4,line=j
  ENDFOR
ENDFOR

IF keyword_set(ps) THEN BEGIN
    IF KEYWORD_SET(eps) THEN extension='.eps' ELSE  extension='.ps' 
    mydevice=!D.NAME
    set_plot,'ps'
    psnam='mplo.ps'
    device,portrait=portrait,landscape=landscape,/color,fil=psnam

    PLOT,xr,[0,0], XSTYLE=1, YSTYLE=1, XTIT=xtit, YTIT=ytit, CHARSIZE=0.6,$
                   TITLE=tit, SUBTITLE=sub,xr=xr,yr=wr,background=255,color=0

    FOR i=0,nw-1 DO BEGIN 
      take_datp,datp,w=wn(i)
      take_w,w,w=wn(i)
      ns=N_ELEMENTS(w(0,*))
      FOR j=0,ns-1 DO BEGIN
        oplot,datp.x(*,j<(N_ELEMENTS(datp.x(0,*))-1)),w(*,j),color=i mod 4,line=j
      ENDFOR
    ENDFOR

    device,/close
    set_plot,mydevice
    IF NOT keyword_set(noprint) THEN BEGIN
      IF NOT keyword_set(printer) THEN printer='dj1_d20'
      spawn,'lp -d'+printer+' '+psnam
    ENDIF
ENDIF

END
