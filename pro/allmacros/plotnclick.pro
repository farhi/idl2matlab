PRO plotnclick,win,x=x,xsize=xsize,ysize=ysize,xrange=xrange,yrange=yrange

IF NOT KEYWORD_SET(x) THEN BEGIN
  take_datp,datp
  x=datp.x
ENDIF
w=win(*,0)
help,x,w
IF N_ELEMENTS(x) NE N_ELEMENTS(w) THEN x=INDGEN(N_ELEMENTS(w))
colors,color
IF NOT KEYWORD_SET(xsize) THEN xsize=1400
IF NOT KEYWORD_SET(ysize) THEN ysize=900
IF NOT KEYWORD_SET(xrange) THEN xrange=[min(x),max(x)]
IF NOT KEYWORD_SET(yrange) THEN yrange=[min(w),max(w)]
Window,1,xsize=xsize,ysize=ysize
range=WHERE(x GE xrange(0) AND X LE xrange(1))
xx=x(range)
w=w(range)
PLOT,xx,w,xstyle=1,ystyle=1,back=color(4),col=color(0),yrange=yrange
x1=!x.crange(0)
x2=!x.crange(1)
y1=!y.crange(0)
y2=!y.crange(1)
x=x1
y=y1
PRINT,'Click outside the plotrange to stop!'
first=1
REPEAT BEGIN 
  CURSOR,x,y,/DOWN 
  IF x GE x1 AND x LE x2 AND y GE y1 AND y LE y2 THEN BEGIN 
    bid=MIN(ABS(xx-x),i)
    PLOTS,[x,x],[y,w(i)],COLOR=COLOR(2)
    ;xyouts,x,y,STRCOMPRESS(x)+STRCOMPRESS(w(i)),$
    ;           COLOR=COLOR(1),CHARSIZE=.6 
    IF first EQ 0 THEN PLOTS,[oldx,x],[oldw,w(i)],COLOR=COLOR(5) ELSE first=0
    PRINT,x,w(i)
    oldx=x
    oldw=w(i)
  ENDIF
ENDREP UNTIL x LT x1 OR x GT x2 OR y LT y1 OR y GT y2 

END
