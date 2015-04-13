PRO click,w,xrange=xrange,yrange=yrange

take_datp,datp
IF NOT KEYWORD_SET(xrange) THEN xrange=[min(datp.x),max(datp.x)]
IF NOT KEYWORD_SET(yrange) THEN yrange=[min(w),max(w)]
colors,color
Window,1,title='Click outside the plotrange to stop!'
;help,datp.x,w
plot,datp.x,w,xstyle=1,ystyle=1,back=color(4),col=color(0),xr=xrange,yr=yrange
x1=!x.crange(0)
x2=!x.crange(1)
y1=!y.crange(0)
y2=!y.crange(1)
x=x1
y=y1
REPEAT BEGIN 
  CURSOR,x,y,/DOWN 
  IF x GE x1 AND x LE x2 AND y GE y1 AND y LE y2 THEN BEGIN
    print,x,w(WHERE(round(10.*x) EQ round(10.*datp.x)))
    PLOTS,[x,x],[y,w(WHERE(round(10.*x) EQ round(10.*datp.x)))],COLOR=COLOR(2)
    xyouts,x,y,STRCOMPRESS(x)+STRCOMPRESS(w(WHERE(round(10.*x) EQ round(10.*datp.x)))),COLOR=COLOR(1),CHARSIZE=.8 
  ENDIF
ENDREP UNTIL x LT x1 OR x GT x2 OR y LT y1 OR y GT y2 

END
