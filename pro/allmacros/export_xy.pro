pro export_xy, FileNameInit , Data , XC=x, YC=y , ZC=z , E=e  , N=n   $
                               , PR=p, PV=pv, PAR_TXT=p_txt       $
                               , W_tit=wt  , X_tit=xt , Y_TIT=yt  $
                               , Z_tit=zt  , OTHER_TIT=ot $
                               ,datp=datp $
                               ;, nolamp=nolamp;
                               ,par=par,comment=comment $
                               , bad=bad
;** *********
;**
;**	Standard call for a data-write procedure called by LAMP.
		  
;**	Keywords:		  
;**		  XC       =  vector of x coordinates.
;**		  YC       = [vector of y coordinates.]
;**		  ZC       = [vector of z coordinates.]
;**		  W_TIT    =   main title
;**		  X_TIT    = x axis title
;**		  Y_TIT    = y axis title
;**		  Z_TIT    = z axis title
;**		  OTHER_TIT=    sub title
;**		  N        = monitors
;**		  PR       = vector of parameter values
;**		  PAR_TXT  = string array of text associated to PR (same size)
;**		  PV       = an array of any dimensions containing other parameter values
;**		  E        = the errors associated to DATA (same size) or 0
;
;+
; Export of Fullprof data format INSTR=10
;
;                       X,Y,Sigma format with header lines.
;                       In all cases the first 6 lines are considered
;                       as comments.
;                       If in the first line (left ajusted) appears the
;                       keyword XYDATA, then the following 5 lines are
;                       considered as the heading of the file. Among
;                       these 5 lines the following keywords and values
;                       have a meaning to the program:
;                        -> INTER  fac_x  fac_y  Interpol  Stepin
;                        -> TEMP  tsamp
;                        fac_x  internal multiplier of X-values
;                        fac_y  internal multiplier of Y and Sigma-values
;                        Interpol=0 data are given at constant step
;                                   or variable step is used in the program
;                                =1 The variable step data are interpolated
;                                   internally to the constant step Stepin.
;
;                       If no sigma values are provided the program assumes
;                       that sigma(Y)=sqrt(Y).
;                       You can add comments to the data file if they
;                       start with the character ! in the first position
;                       of the line. These lines are ignored by the program.
;-
if !version.release ge '5.0' then ii=execute('FORWARD_FUNCTION sys_dep')

CATCH,stat & IF stat ne 0 then begin print,!err_string & return & endif
ON_IOERROR,mis

;IF NOT KEYWORD_SET(nolamp) THEN BEGIN
;    XiCute,'@lamp.cbk'
;ENDIF
Filename=FilenameInit
IF KEYWORD_SET(x) AND NOT KEYWORD_SET(e) THEN e=SQRT(data)
IF NOT (KEYWORD_SET(x) AND KEYWORD_SET(e) AND KEYWORD_SET(pv)) THEN BEGIN
  IF NOT KEYWORD_SET(datp) THEN BEGIN
    IF N_ELEMENTS(FilenameInit) GT 1 THEN BEGIN
      IF N_PARAMS() EQ 1 THEN data='data'
      tmp=FilenameInit
      Filename=data
      data=tmp
      flag=1
    ENDIF 
    TAKE_DATP,datpp
    datp=datpp
  ENDIF ELSE IF N_ELEMENTS(data) EQ 1 THEN BEGIN
    tmp=FilenameInit
    Filename=data
    data=tmp
  ENDIF 
  x=datp.x
  n=datp.n
  y=datp.y
  z=datp.z
  e=datp.e
  p=datp.p
  par_txt=datp.par_txt
  pv=datp.pv
  p_txt=datp.par_txt
  wt=datp.w_tit
  xt=datp.x_tit
  yt=datp.y_tit
  zt=datp.z_tit
  ot=datp.other_tit
ENDIF 
help,filename,data
subs=N_ELEMENTS(data(0,*))
newpos=0
flag=0
REPEAT BEGIN
  pos=newpos
  ii=execute("newpos=STRPOS(FILENAME,SYS_DEP('DIVIDER'),pos+1)")
  IF newpos GE 0 THEN flag=1
ENDREP UNTIL newpos LT 0
IF NOT flag THEN pos=-1
IF subs GT 1 THEN BEGIN
  FILENAME=FILENAME+'_'
;  FILENAME=STRMID(FILENAME,pos+1,3)
  PRINT,subs,' subsets'
  IF (pv(29,subs-1)-pv(29,0)) EQ (subs-1) THEN numflag=1 ELSE numflag=0
ENDIF
FOR k=0,subs-1 DO BEGIN
  sub=''
  IF subs GT 1 THEN BEGIN
    digits=STRLEN(STRCOMPRESS(subs,/RE))
    sub=STRMID(STRCOMPRESS(10^digits+k,/RE),0,digits+1)
    IF numflag THEN sub=STRCOMPRESS(ROUND(pv(29,k)),/RE)
  ENDIF
  OPENW ,unit, STRCOMPRESS(FileName)+sub+'.xy', /get_lun
  PRINT,STRCOMPRESS(FileName,/RE)+sub+'.xy'
  PRINTF,unit,'# XYDATA ',strmid(STRCOMPRESS(wt) ,0,73)  
  PRINTF,unit,'# INTER ',1,1,0,0.1
  PRINTF,unit,'# TEMP ',pv([12,11,10],k)
  PRINTF,unit,'# ',strmid(ot ,0,76)   
  PRINTF,unit,'# ',strmid(ot  ,0,76)   
  PRINTF,unit,'# Monitor/counts, Counting time/sec: ',n(0,0,0),n(0,1<(N_ELEMENTS(n(0,*,0))-1),0)
  FOR i=0,N_ELEMENTS(data(*,k))-1 DO BEGIN
    IF x(i,k<(N_ELEMENTS(x(0,*))-1)) GE 0 THEN PRINTF,unit,FORMAT='(F9.4,F18.3,F12.3)',(x(i,k<(N_ELEMENTS(x(0,*))-1)))<(180.),(data(i,k))<(1.0E12),(e(i,k))<(1.0E6)
 ENDFOR

  FREE_LUN,unit

ENDFOR

return

mis:print,!err_string
end
