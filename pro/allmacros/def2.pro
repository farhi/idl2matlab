pro def2,w3,pos,threshold,$
         noprint=noprint,printer=printer,delta=delta,scale=scale,max=max,datp=datp

IF NOT KEYWORD_SET(datp) THEN take_datp,datp 
IF N_ELEMENTS(threshold) ne 1600 THEN threshold=0
IF NOT KEYWORD_SET(delta) THEN delta=0
IF N_ELEMENTS(scale) NE N_ELEMENTS(pos) THEN scale=FLTARR(N_ELEMENTS(pos))+1.
WINDOW,0
x3=datp.x
xx=x3
FOR i=0,N_ELEMENTS(pos)-1 do begin
  b=pos(i)
  x3=xx
  mod_datp,datp,'x',xx
  w8=defil(w3,b,d,e,datp=datp,x=x3,bg=bg,delta=delta)
  w8=w8/scale(i)
  plotd,w8,d,b,e,datp=datp,threshold,bg=bg,max=max
  IF NOT keyword_set(noprint) THEN BEGIN
    mydevice=!D.NAME
    set_plot,'ps'
    device,/landscape,/color,filen=strcompress(ABS(b),/re)+'_'+strcompress(ABS(datp.p(14)),/re)+'.ps'
    plotd,w8,d,b,e,datp=datp,threshold,bg=bg,max=max
    device,/close
    set_plot,mydevice
    IF NOT keyword_set(printer) THEN printer='dj1_d20'
    spawn,'lp -d'+printer+' '+strcompress(ABS(b),/re)+'_'+strcompress(ABS(datp.p(14)),/re)+'.ps'
  ENDIF
ENDFOR

END
