FUNCTION bacon,w,wmax=wmax,wmin=wmin,xmin=xmin,xmax=xmax,xrange=xrange,wrange=wrange,$
             eps=eps,noprint=noprint,printer=printer,error=error,contour=contour,$
             noscreen=noscreen,par=par,nofit=nofit,color=color,nobg=nobg

;+
; NAME:
;	BACON
;
; PURPOSE:
;	Data output (plot, print, fit) for Prof. G. Bacon.
;
; CATEGORY:
;	Special user
;
; CALLING SEQUENCE:
;	BACON, W
;
; INPUTS:
;	W:	Workspace.
;
; OPTIONAL INPUTS:
;	none.
;	
; KEYWORD PARAMETERS: See also psplot!
;	NOPLOT :	no plot.
;	WR     :	workspace value range, e.g., [min(w),max(w)]
;       PAR    : 	parameter indices.
;
; OUTPUTS:
;	hardcopy of plot, fit and data
;
; OPTIONAL OUTPUTS:
;	none
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;	none.
;
; EXAMPLE:
;
;  W6 = RDRUN(9135)
;  W7 = W6 & BACON, W7, /NOP, WR = [17, 24],PAR = [17, 18]
;
; MODIFICATION HISTORY:
; 	Written by:	Thomas Hansen, June 1997.
;	May,  1998	Documentation
;	July, 1998	Anti-bug and slight changes
;	April,2001	Trying to make it work again ....
;-

take_datp,datp
	

mon=TOTAL(datp.n(0,0,*))/N_ELEMENTS(datp.n(0,0,*))
ner=1/(datp.n(0,0,*)/ SQRT(datp.n(0,0,*) + 1.))^2
FOR i=0,N_ELEMENTS(datp.n(0,0,*))-1 DO BEGIN
 	w(*,i)   =      w(*,i)/datp.n(0,0,i)*mon
	;help,datp.e,datp.n,w,mon,ner
	datp.e(*,i)   = datp.e(*,i)/datp.n(0,0,i)*mon + w(*,i)*ner(i)
	datp.n(0,0,i)= mon
ENDFOR

IF N_ELEMENTS(w(0,*)) GT datp.p(1)  THEN BEGIN
  ww=fltarr(N_ELEMENTS(w(*,0)),datp.p(1))
  xx=fltarr(N_ELEMENTS(w(*,0)),datp.p(1))
  ee=fltarr(N_ELEMENTS(w(*,0)),datp.p(1))
  nn=fltarr(datp.p(1))
  yy=fltarr(datp.p(1))
  pv=fltarr(N_ELEMENTS(datp.pv(*,0)),datp.p(1))
  FOR i=0,datp.p(1)-1 DO BEGIN
    ww(*,i)=TOTAL     (w  (*,WHERE(datp.pv(2,*) EQ i+1,count)),2)
    ee(*,i)=TOTAL(datp.e  (*,WHERE(datp.pv(2,*) EQ i+1,count)),2)/SQRT(count)
    nn(  i)=TOTAL(datp.n    (WHERE(datp.pv(2,*) EQ i+1,count)))
    index=WHERE(datp.pv(2,*) EQ i+1,count)
    yy(  i)=      datp.y  (index(0))
    xx(*,i)=      datp.x(*,index(0))
    pv(*,i)=      datp.pv(*,index(0))
    pv(5,i)= TOTAL(datp.pv(5,(WHERE(datp.pv(2,*) EQ i+1,count))))   
  ENDFOR

  w=ww
  mod_datp,datp,'e',ee  	
  mod_datp,datp,'x',xx  	
  mod_datp,datp,'y',yy  	
  mod_datp,datp,'n',nn  	
  mod_datp,datp,'pv',pv  
ENDIF	

IF NOT KEYWORD_SET(xmax)   THEN xmax=max(datp.x)
IF NOT KEYWORD_SET(xmin)   THEN xmin=min(datp.x)
IF NOT KEYWORD_SET(xrange) THEN xrange=[xmin,xmax]
index=WHERE(datp.x(*,0) GE xrange(0) AND datp.x(*,0) LE xrange(1),count)
w=w(index,*)
mod_datp,datp,'x',datp.x(index,*)
mod_datp,datp,'e',datp.e(index,*)
IF NOT KEYWORD_SET(wmax)   THEN wmax=max(w)
IF NOT KEYWORD_SET(wmin)   THEN wmin=0.
IF NOT KEYWORD_SET(wmin)   THEN IF  KEYWORD_SET(nobg) THEN wmin=min(w)
IF NOT KEYWORD_SET(wrange) THEN wrange=[wmin,wmax]

SET_PLOT,'ps'
filename=strcompress(string(round(datp.p(29))),/remove_all)+'_multi.ps'
IF KEYWORD_SET(eps) THEN eps=1 ELSE eps=0
DEVICE,FILENAME=filename,$
  /LANDSCAPE,XSIZE=25,YSIZE=15,YOFFSET=25,XOFFSET=5;,$
  ;BITS_PER_PIXEL=256;,/COLOR,encapsulated=eps
IF N_ELEMENTS(w(0,*)) GT 1 THEN BEGIN
    IF NOT KEYWORD_SET(color) THEN BEGIN
      SURFACE,w,datp.x,datp.y        ,$
          zRANGE  = wrange, $
          xRANGE  = xrange, $
          TITLE   =datp.w_tit+' '+datp.other_tit ,CHARSIZE=0.7
    ENDIF ELSE BEGIN
      SHADE_SURF,w,datp.x,datp.y        ,$
          zRANGE  = wrange, $
          xRANGE  = xrange, $
          TITLE   =datp.w_tit +' '+datp.other_tit,CHARSIZE=0.7 
   ENDELSE
ENDIF 
DEVICE,/CLOSE
IF NOT KEYWORD_SET(color) THEN BEGIN
  IF NOT KEYWORD_SET(printer) THEN line='$lp '+filename ELSE line='$lp -d'+printer+' '+filename
ENDIF ELSE line='$lp -d'+color+' '+filename
if NOT keyword_set(noprint) THEN  BEGIN
  PRINT,line
  XICUTE,line 
ENDIF


SET_PLOT,'X'
f=newfit(w,xx=datp.x,np=2,nbg=3,int=int,width=width,pos=pos,bg=bg,/pv,/print,initialit=10,iterations=5,/plot)


SET_PLOT,'ps'
!P.LINESTYLE=0
!p.thick=1
!p.psym=0
!p.symsize=0.6
filename=strcompress(string(round(datp.p(29))),/remove_all)+'_single.ps'
DEVICE,FILENAME=filename,$
  /LANDSCAPE,XSIZE=25,YSIZE=15,YOFFSET=25,XOFFSET=5;,$
  ;BITS_PER_PIXEL=256;,/COLOR,encapsulated=eps
FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
  subtitle=datp.other_tit+'-'+strcompress(i,/remove_all)
  IF KEYWORD_SET(par) THEN FOR j=0,N_ELEMENTS(par)-1 DO BEGIN
    subtitle=subtitle+', '+strmid(datp.par_txt(par(j)),0,4)+strmid(strcompress(string(datp.pv(par(j),i))),0,6)
  ENDFOR
    IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN BEGIN
       PLOT,datp.x(*,i),w(*,i),$
         yRANGE     = wrange, $
         xRANGE     = xrange, $
         TITLE      =datp.w_tit,$
         SUBTITLE   =subtitle,$
         xTITLE     =datp.x_tit,$
         yTITLE     =datp.z_tit,symsize=0.6,psym=7
   ENDIF ELSE BEGIN
       PLOT,datp.x,w(*,i),$
       yRANGE     = wrange, $
       xRANGE     = xrange, $
       TITLE      =datp.w_tit,$
       SUBTITLE   =subtitle,$
       xTITLE     =datp.x_tit,$
       yTITLE     =datp.z_tit,symsize=0.6,psym=7
   ENDELSE
   IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN BEGIN
       PLOT,datp.x(*,i),f(*,i),$
         yRANGE     = wrange, $
         xRANGE     = xrange, $
         TITLE      =datp.w_tit,$
         SUBTITLE   =subtitle,$
         xTITLE     =datp.x_tit,$
         yTITLE     =datp.z_tit,symsize=0.6,/noerase
   ENDIF ELSE BEGIN
       PLOT,datp.x,f(*,i),$
       yRANGE     = wrange, $
       xRANGE     = xrange, $
       TITLE      =datp.w_tit,$
       SUBTITLE   =subtitle,$
       xTITLE     =datp.x_tit,$
       yTITLE     =datp.z_tit,symsize=0.6,/noerase
   ENDELSE
  IF KEYWORD_SET(error) THEN OPLOTERR,datp.x(*,i<(N_ELEMENTS(datp.x(0,*))-1)),w(*,i),datp.e(*,i),3 
ENDFOR
DEVICE,/CLOSE
IF NOT KEYWORD_SET(printer) THEN line='$lp '+filename ELSE line='$lp -d'+printer+' '+filename
if NOT keyword_set(noprint) THEN  BEGIN
  PRINT,line
  XICUTE,line 
ENDIF

SET_PLOT,'X'
IF NOT KEYWORD_SET(noscreen) THEN BEGIN
  IF N_ELEMENTS(w(0,*)) GT 1 THEN BEGIN
    SURFACE,w,datp.x,datp.y,$
          zRANGE  = wrange, $
          xRANGE  = xrange, $
          TITLE   =datp.other_tit
  ENDIF 
  FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
    subtitle=datp.other_tit+'-'+strcompress(i,/remove_all)
    IF KEYWORD_SET(par) THEN FOR j=0,N_ELEMENTS(par)-1 DO BEGIN
      subtitle=subtitle+', '+strmid(datp.par_txt(par(j)),0,4)+strmid(strcompress(string(datp.pv(par(j),i))),0,6)
    ENDFOR
    IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN BEGIN
       PLOT,datp.x(*,i),w(*,i),$
         yRANGE     = wrange, $
         xRANGE     = xrange, $
         TITLE      =datp.w_tit,$
         SUBTITLE   =subtitle,$
         xTITLE     =datp.x_tit,$
         yTITLE     =datp.z_tit,symsize=0.6,psym=7
   ENDIF ELSE BEGIN
       PLOT,datp.x,w(*,i),$
       yRANGE     = wrange, $
       xRANGE     = xrange, $
       TITLE      =datp.w_tit,$
       SUBTITLE   =subtitle,$
       xTITLE     =datp.x_tit,$
       yTITLE     =datp.z_tit,symsize=0.6,psym=7
   ENDELSE
   IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN BEGIN
       PLOT,datp.x(*,i),f(*,i),$
         yRANGE     = wrange, $
         xRANGE     = xrange, $
         TITLE      =datp.w_tit,$
         SUBTITLE   =subtitle,$
         xTITLE     =datp.x_tit,$
         yTITLE     =datp.z_tit,symsize=0.6,/noerase
   ENDIF ELSE BEGIN
       PLOT,datp.x,f(*,i),$
       yRANGE     = wrange, $
       xRANGE     = xrange, $
       TITLE      =datp.w_tit,$
       SUBTITLE   =subtitle,$
       xTITLE     =datp.x_tit,$
       yTITLE     =datp.z_tit,symsize=0.6,/noerase
   ENDELSE
   IF KEYWORD_SET(error) THEN OPLOTERR,datp.x(*,i<(N_ELEMENTS(datp.x(0,*))-1)),w(*,i),datp.e(*,i),3 
  ENDFOR
ENDIF


filename=strcompress(string(round(datp.p(29))),/remove_all)+'.fit'
OPENW,fit,filename,/get_lun
PRINTF,fit,datp.w_tit
PRINT,datp.w_tit
printf,fit,datp.other_tit
print,datp.other_tit
help,datp.pv,datp.n
Printf,fit,'Time/step (sec):',datp.pv(5,0),'     Monitor/step (counts):',datp.n(0)
Print,'Time/step (sec):',datp.pv(5,0),'     Monitor/step (counts):',datp.n(0)
Printf,fit
Printf,fit,'Pos1/deg   Area1   FWHM1   Pos2/deg   Area2   FWHM2    A1/A2   BGmin   BGmax'
Print,'Pos1/deg Area1(cnts/deg) FWHM1 Pos2/deg Area2(cnts/deg) FWHM2  A1/A2  BG'
FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
    FWHM1=width(i,0);SQRT(2.*ALOG(2.)*a(2,0,i))
    FWHM2=width(i,1);SQRT(2.*ALOG(2.)*a(2,1,i))
    Area1=int(i,0);a(0,0,i)*FWHM1
    Area2=int(i,1);a(0,1,i)*FWHM2
    IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN BEGIN
      printf,fit,format='(f8.3,f10.1,f7.3,f8.3,f10.1,f7.3,f10.4,f9.1,f9.1)',$
	pos(i,0),Area1,FWHM1,pos(i,1),Area2,FWHM2,Area1/Area2,$
        MIN(bg(i,0)+bg(i,1)*datp.x(*,i)+bg(i,2)*datp.x(*,i)^2),$
        MAX(bg(i,0)+bg(i,1)*datp.x(*,i)+bg(i,2)*datp.x(*,i)^2)
      print,format='(f7.3,f9.1,f6.3,f7.3,f9.1,f6.3,f6.2,f9.1)',pos(i,0),Area1,$
	FWHM1,pos(i,1),Area2,FWHM2,Area1/Area2,$
        MIN(bg(i,0)+bg(i,1)*datp.x(*)+bg(i,2)*datp.x(*)^2),$
        MAX(bg(i,0)+bg(i,1)*datp.x(*)+bg(i,2)*datp.x(*)^2)
    ENDIF ELSE BEGIN
      printf,fit,format='(f8.3,f10.1,f7.3,f8.3,f10.1,f7.3,f10.4,f9.1,f9.1)',$
	pos(i,0),Area1,FWHM1,pos(i,1),Area2,FWHM2,Area1/Area2,$
        MIN(bg(i,0)+bg(i,1)*datp.x(*)+bg(i,2)*datp.x(*)^2),$
        MAX(bg(i,0)+bg(i,1)*datp.x(*)+bg(i,2)*datp.x(*)^2)
      print,format='(f7.3,f9.1,f6.3,f7.3,f9.1,f6.3,f6.2,f9.1)',pos(i,0),Area1,$
	FWHM1,pos(i,1),Area2,FWHM2,Area1/Area2,$
        MIN(bg(i,0)+bg(i,1)*datp.x(*)+bg(i,2)*datp.x(*)^2),$
        MAX(bg(i,0)+bg(i,1)*datp.x(*)+bg(i,2)*datp.x(*)^2)
    ENDELSE 

ENDFOR
flush,fit
close,fit
free_lun,fit

IF NOT KEYWORD_SET(printer) THEN line='$lp '+filename ELSE line='$lp -d'+printer+' '+filename
if NOT keyword_set(noprint) THEN  BEGIN
  PRINT,line
  XICUTE,line 
ENDIF

FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
  filename=strcompress(string(round(datp.p(29))),/remove_all)+'_'+strcompress(string(i),/remove_all)+'.dat'
  OPENW,dat,filename,/get_lun
  subtitle=datp.other_tit+'-'+strcompress(i,/remove_all)
  IF KEYWORD_SET(par) THEN FOR j=0,N_ELEMENTS(par)-1 DO BEGIN
    subtitle=subtitle+', '+strmid(datp.par_txt(par(j)),0,4)+strmid(strcompress(string(datp.pv(par(j),i))),0,6)
  ENDFOR
  FOR j=0,N_ELEMENTS(w(*,0))-1 DO BEGIN
    IF (j MOD 60) EQ 0 THEN BEGIN ; lj1_d20
      PRINTF,dat,datp.w_tit
      PRINTF,dat,subtitle
      Printf,dat,'Time:',datp.pv(5,i)*1e6,' sec     Monitor:',datp.n(i)
      Printf,dat
      Printf,dat,'      2*Theta      Counts       Error'  
    ENDIF 
    
    IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN BEGIN
      PRINTF,dat,datp.x(j,i),w(j,i),datp.e(j,i)
    ENDIF ELSE BEGIN
      PRINTF,dat,datp.x(j),w(j,i),datp.e(j,i)
    ENDELSE 
  ENDFOR
  flush,dat
  close,dat
  free_lun,dat
  PRINT,filename,' created'
  if NOT keyword_set(noprint) THEN  BEGIN
	PRINT,'Printer cooldown loop'
  	FOR j=0.,2000000. DO jj=sin(j)
  	IF NOT KEYWORD_SET(printer) THEN line='$lp '+filename ELSE line='$lp -d'+printer+' '+filename
  	PRINT,line
  	XICUTE,line 
  ENDIF
ENDFOR

give_datp,datp
RETURN,f
END
