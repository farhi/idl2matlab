PRO psplot32,w,xrange,yrange,comment,noprint=noprint,printer=printer,box=box,points=points
take_datp,datp
SET_PLOT,'ps'
x=findgen(N_ELEMENTS(w))
IF NOT KEYWORD_SET(points) THEN points=0 
IF N_ELEMENTS(xrange) NE 2 THEN xrange=[min(x),max(x)]
IF N_ELEMENTS(yrange) NE 2 THEN yrange=[min(w),max(w)]
IF N_ELEMENTS(comment) LT 1 THEN comment='cell '
filename=strcompress(string(round(xrange(0))),/remove_all)+'.ps'
IF KEYWORD_SET(eps) THEN eps=1 ELSE eps=0
DEVICE,FILENAME=filename,$
  /LANDSCAPE,XSIZE=25,YSIZE=15,YOFFSET=25,XOFFSET=5,$
  BITS_PER_PIXEL=256,/COLOR,encapsulated=eps
IF NOT KEYWORD_SET(wmax) THEN wmax=max(w)
IF NOT KEYWORD_SET(wmin) THEN wmin=min(w)
xx=x(where((x GE xrange(0)) AND (x LE xrange(1))))
tickv=xx(where((xx mod 32) LT 0.05))
tickv=float(tickv)-0.5
tickformat='(F6.1)'
IF KEYWORD_SET(box) THEN BEGIN
  x=float(x)/32.
  tickv=tickv/32. 
  tickformat='(I1)'
  xrange=float(xrange)/32.
ENDIF ELSE IF N_ELEMENTS(tickv) GE 30 THEN tickv=xx(where((xx mod 64) LT 0.05))
ticks=N_ELEMENTS(tickv)-1
PLOT,x,w,$
       PSYM  = points, $
       yRANGE  = yrange, $
       ygridstyle=1,$
       xRANGE  = xrange, $
       xtickv=tickv,$
       xtickFORMAT=tickformat,$
       xgridstyle=1,$
       xticklen=1.,$
       yticklen=1.,$
       xticks=ticks,$
       TITLE=datp.w_tit,$
       SUBTITLE=datp.other_tit,$
       xTITLE=comment,$
       yTITLE=datp.y_tit
IF KEYWORD_SET(box) THEN BEGIN
  PLOT,x,w,/noerase,$
       PSYM  = points, $
       yRANGE  = yrange, $
       ygridstyle=1,$
       xRANGE  = xrange, $
       xtickv=tickv+0.5,$
       xtickFORMAT='(I2)',$
       yticklen=1.,$
       xticks=ticks,$
       TITLE=datp.w_tit,$
       SUBTITLE=datp.other_tit,$
       xTITLE=comment,$
       yTITLE=datp.y_tit
ENDIF
DEVICE,/CLOSE
IF NOT KEYWORD_SET(noprint) THEN BEGIN
  IF KEYWORD_SET(printer) THEN line='$lp -d'+printer+' '+filename ELSE line='$lp '+filename
  print,line
  XICUTE,line
ENDIF
SET_PLOT,'X'
PLOT,x,w,$
       PSYM  = points, $
       yRANGE  =yrange, $
       ygridstyle=1,$
       xRANGE  = xrange, $
       xtickv=tickv,$
       xtickFORMAT=tickformat,$
       xgridstyle=1,$
       xticklen=1.,$
       yticklen=1.,$
       xticks=ticks,$
       TITLE=datp.w_tit,$
       SUBTITLE=datp.other_tit,$
       xTITLE=comment,$
       yTITLE=datp.y_tit
IF KEYWORD_SET(box) THEN BEGIN
  PLOT,x,w,/noerase,$
       PSYM  = points, $
       yRANGE  = yrange, $
       ygridstyle=1,$
       xRANGE  = xrange, $
       xtickv=tickv+0.5,$
       xtickFORMAT='(I2)',$
       yticklen=1.,$
       xticks=ticks,$
       TITLE=datp.w_tit,$
       SUBTITLE=datp.other_tit,$
       xTITLE=comment,$
       yTITLE=datp.y_tit
ENDIF
give_datp,datp
END
