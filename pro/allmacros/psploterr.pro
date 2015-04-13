PRO psploterr,w
;,shade=shade,wmax=wmax,wmin=wmin,$
;             eps=eps,noprint=noprint,printer=printer
take_datp,datp
SET_PLOT,'ps'
filename=strcompress(string(round(datp.p(0))),/remove_all)+'.ps'
IF KEYWORD_SET(eps) THEN eps=1 ELSE eps=0
DEVICE,FILENAME=filename,$
  /LANDSCAPE,XSIZE=25,YSIZE=15,YOFFSET=25,XOFFSET=5,$
  BITS_PER_PIXEL=256,/COLOR,encapsulated=eps
IF NOT KEYWORD_SET(wmax) THEN wmax=max(w)
IF NOT KEYWORD_SET(wmin) THEN wmin=min(w)
IF N_ELEMENTS(w(0,*)) GT 1 THEN $
  IF KEYWORD_SET(shade) THEN $
  SHADE_SURF,w,datp.x,datp.y        ,$
          zRANGE  = [wmin,wmax], $
          TITLE   =datp.w_tit    ,$
          SUBTITLE=datp.other_tit,$
          xTITLE  =datp.x_tit,$
          yTITLE  =datp.y_tit,$
          zTITLE  =datp.z_tit $
  ELSE $
  SURFACE,w,datp.x,datp.y        ,$
          zRANGE  = [wmin,wmax], $
          TITLE   =datp.w_tit    ,$
          SUBTITLE=datp.other_tit,$
          xTITLE  =datp.x_tit,$
          yTITLE  =datp.y_tit,$
          zTITLE  =datp.z_tit $
ELSE $
  PLOT,datp.x,w,$
       yRANGE  = [wmin,wmax], $
       TITLE=datp.w_tit,$
       SUBTITLE=datp.other_tit,$
       xTITLE=datp.x_tit,$
       yTITLE=datp.y_tit
OPLOTERR,datp.x,w,datp.e,3 
;IF KEYWORD_SET(err) THEN ERRPLOT,datp.x,w-datp.e,w+datp.e 
;IF KEYWORD_SET(err) THEN ERRPLOT,datp.x,w,datp.e,PSYM=3
DEVICE,/CLOSE
  IF NOT KEYWORD_SET(printer) THEN line='$lp '+filename ELSE line='$lp -d'+printer+' '+filename
;if NOT keyword_set(noprint) THEN  PRINT,line 
if NOT keyword_set(noprint) THEN  XICUTE,line 
SET_PLOT,'X'
give_datp,datp
END
