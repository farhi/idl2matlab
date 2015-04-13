;*****************************************************************************
;-----------------------------------------------------------------------------
;
	PRO dial_hytec_data_macro, D
;
; Displays Hytec counters with errors and proper axes
;
;						JRS 10/8/01
;
;*****************************************************************************
;-----------------------------------------------------------------------------

	IF(D.init EQ 0) THEN BEGIN
		D.type='motors'		& bank=DialNewValue() & bank=bank(0:3)
		D.type='wave'		& lambda=DialNewValue()
		IF (lambda LT 4.) THEN zeroshift=1.33 ELSE zeroshift=0.0
		angle=FLTARR(8,4)-zeroshift

;		YIG scan 16/9/99

		angle(*,0)=angle(*,0)+[203.334,197.230,191.938,186.146,180.086,175.015,169.190,163.276]
		angle(*,1)=angle(*,1)+[193.803,188.033,182.300,176.830,170.917,165.057,159.300,153.770]
		angle(*,2)=angle(*,2)+[199.465,194.010,188.467,182.707,177.113,171.570,165.625,160.043]
		angle(*,3)=angle(*,3)+[196.693,190.988,185.062,179.510,173.758,168.194,162.510,156.666]
		D.xvalue=FLTARR(32)
		FOR ibank=0,3 DO D.xvalue(ibank*8:ibank*8+7)=bank(ibank)-angle(*,ibank)
		D.init=1
		D.newtot =0.
		D.holdtot=0.
		DialModValue,lonarr(32)
		DialModValue,lonarr(64) ,tag='NEW'
		DialModValue,lonarr(64) ,tag='HOLD'
	ENDIF

;	GET THE DATA
;	------------
	V=DialNewValue(type='hytec')

;	REFRESHED?
;	---------
	tmp=total(V)
	IF(tmp lt D.newtot) THEN BEGIN	
		D.hold   =D.hold+D.new
		D.holdtot= total(D.hold)
	ENDIF ELSE BEGIN
		D.init=0
	ENDELSE

	D.new   = V
	D.newtot= tmp
	f = D.hold+D.new
	
;	PLOT
;	----
	even=INDGEN(32)*2+1
	yout=f(even)
	yerr=sqrt(yout)
	D.error=yerr
	D.value=yout

	END

;*********************
FUNCTION dial_hytec_data
;*********************
;**
;** The dial initialisation

   ;Dial Variables (optional)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='hytec'     ;when DialNewValue() is used, get the monitor
    ONOFF=0          ;state of the Dial 1=running
    FREQUENCY=2.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE=0          ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=0           ;-2=no plot 1=surface 2=contour n>2 means show vector of last n values of the scalar
    UPPERLIM=100.    ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file .his
    DURATION=0       ;if >0 then Dial is stopped after running duration seconds

   ;Own Variables (optional)
   ;-------------
    HOLD   =0
    NEW    =0
    HOLDTOT=0.
    NEWTOT =0.
    TOFCH  =0L
    XVALUE    =FLTARR(32)
    ERROR = FLTARR(32)
    X_TIT = 'Scattering angle (deg)'
    Y_TIT = 'Counts'

return, {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,holdtot:HOLDTOT,$
	 hold:HOLD,new:NEW,newtot:NEWTOT,init:0L,tofch:TOFCH,plot:PLOT,xvalue:XVALUE,$
	 x_tit:X_TIT,y_tit:Y_TIT,error:ERROR}
end






