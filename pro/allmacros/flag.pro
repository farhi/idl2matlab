pro flag,bad=bad,nobad=nobad,int=int,noint=noint,ang=ang,noang=noang,$
         wav=wav,nowav=nowav,nor=nor,nonor=nonor,flp=flp,noflp=noflp,eff=eff,noeff=noeff,$
									old=old,new=new,noprint=noprint,cor=cor,nocor=nocor,soft=soft

;if !version.release ge '5.0' then ii=execute('FORWARD_FUNCTION P_LAMBDA')

;+
; Macro written by Thomas Hansen in 1997 to set some options for the import of D20 raw data
; Modification 2/98 : Monitor normalisation is long integer, otherwise bug ...
; Modification 2/98 : Monitor normalisation is floating point, otherwise bug ...
; Modification 6/98 : Monitor normalisation is floating point, otherwise bug ...
;-

common d20  , bad_d20 ,flag_d20, wav_d20, psd_d20  
common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6

IF NOT KEYWORD_SET(soft) OR N_ELEMENTS(flag_d20) LT 8 THEN BEGIN
    IF  KEYWORD_SET(soft) THEN PRINT,'No information in common flag_d20 (calibration)'
    tmf='rdid.flag'
    tmc=FINDFILE(tmf,count=tmp)
    p_lambda,plambda
    IF tmp EQ 0 THEN begin tmf=PLAMBDA+'/CALIBRATION/rdid.flag'
			   tmc=FINDFILE(tmf,count=tmp) & endif

    bad_flag=0 & interpol_flag=0 & ang_flag=0 & wav_flag=0 & normalize=float(0) & float_flag=0 & eff_flag=0 & cor_flag=0
    IF tmp EQ 1 THEN begin
	 			OPENR,tmp,tmf,/get_lun 
				IF NOT EOF(tmp) THEN READF,tmp,bad_flag
				IF NOT EOF(tmp) THEN READF,tmp,interpol_flag
				IF NOT EOF(tmp) THEN READF,tmp,ang_flag
				IF NOT EOF(tmp) THEN READF,tmp,wav_flag
				IF NOT EOF(tmp) THEN READF,tmp,normalize
				IF NOT EOF(tmp) THEN READF,tmp,float_flag
				IF NOT EOF(tmp) THEN READF,tmp,eff_flag
				IF NOT EOF(tmp) THEN READF,tmp,cor_flag 
				FREE_LUN,tmp
     ENDIF ELSE BEGIN
	cor_flag=0
	print,'rdid.flag not complete'
     ENDELSE
     old=[bad_flag,interpol_flag,ang_flag,wav_flag,normalize,float_flag,eff_flag,cor_flag]
ENDIF ELSE BEGIN
				bad_flag=     flag_d20(0)
				interpol_flag=flag_d20(1)
				ang_flag=     flag_d20(2)
			 wav_flag=     flag_d20(3)
				normalize=    flag_d20(4)
				float_flag=   flag_d20(5)
				eff_flag  =   flag_d20(6)
				cor_flag=     flag_d20(7)
ENDELSE

IF NOT KEYWORD_SET(bad)  THEN bad=bad_flag
IF KEYWORD_SET(nobad)    THEN bad=0
IF NOT KEYWORD_SET(int)  THEN int=interpol_flag
IF KEYWORD_SET(noint)    THEN int=0
IF NOT KEYWORD_SET(ang)  THEN ang=ang_flag
IF KEYWORD_SET(noang)    THEN ang=0
IF NOT KEYWORD_SET(wav)  THEN wav=wav_flag
IF KEYWORD_SET(nowav)    THEN wav=0
IF NOT KEYWORD_SET(nor)  THEN nor=float(normalize)
IF nor EQ 1              THEN nor=float(100000)
IF KEYWORD_SET(nonor)    THEN nor=float(0)
IF NOT KEYWORD_SET(flp)  THEN flp=float_flag
IF KEYWORD_SET(noflp)    THEN flp=0
IF NOT KEYWORD_SET(eff)  THEN eff=eff_flag
IF KEYWORD_SET(noeff)    THEN eff=0
IF NOT KEYWORD_SET(cor)  THEN cor=cor_flag
IF KEYWORD_SET(nocor)    THEN cor=0
IF NOT KEYWORD_SET(new)  THEN BEGIN
  IF NOT KEYWORD_SET(noprint) THEN BEGIN
    PRINT,'      RDID Flags                  old          new'
    PRINT,'BAD cell exclusion          ',bad_flag     ,bad    
    PRINT,'INTerpolation               ',interpol_flag,int
    PRINT,'ANGle calibration           ',ang_flag     ,ang
    PRINT,'WAVelength from *.cal       ',wav_flag     ,wav
    PRINT,'NORmalization to monitor    ',normalize    ,nor
    PRINT,'workspace in FLoating Point ',float_flag   ,flp
    PRINT,'EFFiciency correction       ',eff_flag     ,eff
    PRINT,'CORection of time/monitor   ',cor_flag     ,cor
		ENDIF
  IF NOT KEYWORD_SET(soft) THEN BEGIN 
    OPENW ,tmp,'rdid.flag',/get_lun 
    PRINTF,tmp,bad
    PRINTF,tmp,int
    PRINTF,tmp,ang
    PRINTF,tmp,wav
    PRINTF,tmp,nor
    PRINTF,tmp,flp
    PRINTF,tmp,eff
    PRINTF,tmp,cor
    CLOSE ,tmp
    free_lun ,tmp
		ENDIF
		new=[bad,int,ang,wav,nor,flp,eff,cor]
ENDIF ELSE BEGIN
new=[new,0,0,0,0,float(0),0,0,0]
new=new(0:7)
  IF NOT KEYWORD_SET(noprint) THEN BEGIN
    PRINT,'      RDID Flags                  old          new'
    PRINT,'BAD cell exclusion          ',bad_flag     ,new(0)   
    PRINT,'INTerpolation               ',interpol_flag,new(1)
    PRINT,'ANGle calibration           ',ang_flag     ,new(2)
    PRINT,'WAVelength from *.cal       ',wav_flag     ,new(3)
    PRINT,'NORmalization to monitor    ',normalize    ,new(4)
    PRINT,'workspace in FLoating Point ',float_flag   ,new(5)
    PRINT,'EFFiciency correction       ',eff_flag     ,new(6)
    PRINT,'CORection of time/monitor   ',cor_flag     ,new(7)
		ENDIF
  IF NOT KEYWORD_SET(soft) THEN BEGIN 
    OPENW ,tmp,'rdid.flag',/get_lun 
    for i=0,7 DO PRINTF,tmp,new(i)
    CLOSE ,tmp
    free_lun ,tmp
		ENDIF
ENDELSE
;IF new(6)-eff_flag GE 1 THEN inf_d20='autod20.cal'
flag_d20=new
;print,flag_d20
END

