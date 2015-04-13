FUNCTION tempcat,num1,num2,DDMMYY=DDMMYY,HHMMSS=HHMMSS,reg=reg,t_min=t_min,T_K=T_K

;+
; EXAMPLE :
;   w2=tempcat(76575,78589,DDMMYY=200798,HHMMSS='0',t_min=x1,T_K=w1)
;-

@lamp.cbk
take_datp,datp,w=one
filnam=STRCOMPRESS(num1,/RE)+'_'+STRCOMPRESS(num2,/RE)+'.temp'
shortnam=STRCOMPRESS(num1,/RE)+'_'+STRCOMPRESS(num2,/RE)+'.dat'
OPENW,temp,filnam,/GET_LUN
OPENW,short,shortnam,/GET_LUN
PRINTF,temp,'# TEMPeratureCATalogue : ',num1, ' to ', num2
PRINTF,short,'# TEMPeratureCATalogue : ',num1, ' to ', num2
  DD=0
  MON=0
  YY=0
  HH=0
  MM=0
  SS=0
refday=LONG(0)
IF KEYWORD_SET(DDMMYY) THEN BEGIN
  DDMMYY=STRCOMPRESS(STRING(1000000+(LONG(DDMMYY)<1000000)),/RE)
  day=STRMID(DDMMYY,1,2)
  READS,day,DD
  month=STRMID(DDMMYY,3,2)
  READS,month,MM
  year=STRMID(DDMMYY,5,2)
  READS,year,YY
  PRINTF,temp,'# Starting day for time scale :',DD,MM,YY
  PRINTF,short,'# Starting day for time scale :',DD,MM,YY
  PRINT,'# Starting day for time scale :',DD,MM,YY
  DDMMYY=JULDAY(MM,DD,YY)
ENDIF
IF KEYWORD_SET(HHMMSS) THEN BEGIN
    HHMMSS=STRCOMPRESS(STRING(1000000+(LONG(HHMMSS)<1000000)),/RE)
    hour=STRMID(HHMMSS,1,2)
    READS,hour,HH
    min= STRMID(HHMMSS,3,2)
    READS,min,MM
    sec= STRMID(HHMMSS,5,2)
    READS,sec,SS
    PRINTF,temp,'# Starting hour for time scale :',HH,MM,SS
    PRINTF,short,'# Starting hour for time scale :',HH,MM,SS
    PRINT,'# Starting hour for time scale :',HH,MM,SS
    HHMMSS=HH*60.+MM+SS/60.
ENDIF ELSE HHMMSS=-1.
IF KEYWORD_SET(t_min) AND KEYWORD_SET(T_K) THEN PRINTF,temp,'# Sample Temperature from interpolated worklspace'
IF KEYWORD_SET(t_min) AND KEYWORD_SET(T_K) THEN PRINTF,short,'# Sample Temperature from interpolated worklspace'
PRINTF,short, '# Numor Minutes  SetPoint  Regul.   Sample  Total  Monitor'
PRINTF,temp, '# Numor Minutes  SetPoint  Regul.   Sample  Date    Time     Title'
PRINT,       '# Numor Minutes  SetPoint  Regul.   Sample  Date    Time     Title'
FOR num=num1,num2 DO BEGIN
  w=RDRUN(num,DATP=datp)
  day      = strmid(datp.other_tit,4,2)
		READS,day,DD
  month    = strmid(datp.other_tit,7,3)
  year     = strmid(datp.other_tit,11,2)
		READS,year,YY
  hour     = strmid(datp.other_tit,14,2)
		READS,hour,HH
  min      = strmid(datp.other_tit,17,2)
		READS,min,MM
  sec      = strmid(datp.other_tit,20,2)
		READS,sec,SS
  CASE month OF
     'Jan':  MON=1
     'Feb':  MON=2
     'Mar':  MON=3
     'Apr':  MON=4
     'May':  MON=5
     'Jun':  MON=6
     'Jul':  MON=7
     'Aug':  MON=8
     'Sep':  MON=9
     'Oct':  MON=10
     'Nov':  MON=11
     'Dec':  MON=12
  ENDCASE					
  IF refday EQ 0. THEN BEGIN
    IF HHMMSS GE 0 THEN BEGIN
      refmin=HHMMSS
    ENDIF ELSE refmin=HH*60.+MM+SS/60.
    IF KEYWORD_SET(DDMMYY) THEN BEGIN
        refday=DDMMYY
    ENDIF ELSE refday=FLOAT(JULDAY(MON,DD,YY))
  ENDIF
  day=JULDAY(MON,DD,YY)-refday
  min=day*24.*60.+HH*60.+MM+SS/60.-refmin
  IF KEYWORD_SET(t_min) AND KEYWORD_SET(T_K) THEN BEGIN
    datp.p(12)=INTERPOL(T_K,t_min,min)
  ENDIF
  PRINT,$
        FORMAT="(I6,F12.2,3F9.3,2F15.0)",$
        num,min,$
        datp.p(10:12), TOTAL(w(*,0)), datp.n(0)
  PRINTF,temp,$
        FORMAT="(I6,F12.2,3F9.3,A19,A57)",$
        num,min,$
        datp.p(10:12),$
        strmid(datp.other_tit,3,19),$
        STRMID(STRCOMPRESS(datp.w_tit,REMOVE_ALL=RA),0,57)
  PRINTF,short,$
        FORMAT="(I6,F12.2,3F9.3,2F15.0)",$
        num,min,$
        datp.p(10:12), TOTAL(w(*,0)), datp.n(0)
  IF KEYWORD_SET(reg) THEN Kelvin=datp.p(11) ELSE Kelvin=datp.p(12) 
  IF N_ELEMENTS(result) EQ 0 THEN BEGIN
    result=Kelvin 
    xx=min
  ENDIF ELSE BEGIN
    result=[result,Kelvin]
    xx=[xx,min]
  ENDELSE
ENDFOR
FREE_LUN,temp
FREE_LUN,short
  MOD_DATP,datp,'x',xx
  MOD_DATP,datp,'x_tit','Minutes'
  MOD_DATP,datp,'y_tit','Temperature'
  MOD_DATP,datp,'other_tit',STRCOMPRESS(num1)+' to'+STRCOMPRESS(num2)
  GIVE_datp,datp
RETURN,result
END
