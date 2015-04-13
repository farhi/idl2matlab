;*****************************************************************************
;-----------------------------------------------------------------------------
;
	PRO dial_hytec_data_all_macro, D
;
; Displays all Hytec counters with errors and proper axes
;
;						JRS 14/11/01
;
;*****************************************************************************
;-----------------------------------------------------------------------------

	IF(D.init EQ 0) THEN BEGIN
		D.type='motors'		& bank=DialNewValue() & bank=bank(0:3)
		D.type='wave'		& lambda=DialNewValue()
		IF (lambda LT 4.) THEN zeroshift=1.33 ELSE zeroshift=0.0
		angle=FLTARR(16,4)-zeroshift

;		YIG scan 16/9/99

		angle(*,0)=[206.00,203.05,200.32,197.39,194.57,191.73,188.95,186.04, $
			    183.15,180.35,177.50,174.60,171.79,168.94,166.22,163.18]
		angle(*,1)=[196.66,193.78,191.03,188.11,185.26,182.39,179.59,176.73, $
			    173.89,171.04,168.24,165.36,162.51,159.67,156.90,153.98]
		angle(*,2)=[202.71,199.83,196.95,194.19,191.34,188.44,185.66,182.76, $
			    179.90,177.10,174.32,171.34,168.49,165.68,162.88,159.96]
		angle(*,3)=[199.67,196.82,194.04,191.13,188.29,185.45,182.61,179.81, $
			    176.93,174.11,171.35,168.42,165.57,162.73,159.92,156.99]
		D.xvalue=FLTARR(64)
		FOR ibank=0,3 DO D.xvalue(ibank*16:ibank*16+15)=bank(ibank)-angle(*,ibank)
		D.init=1
		D.newtot =0.
		D.holdtot=0.
		DialModValue,lonarr(64)
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
	yout=f
	yerr=sqrt(yout)
	D.error=yerr
	D.value=yout

	END

;*********************
FUNCTION dial_hytec_data_all
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
    HOLD    = 0
    NEW     = 0
    HOLDTOT = 0.
    NEWTOT  = 0.
    TOFCH   = 0L
    XVALUE  = FLTARR(64)
    ERROR   = FLTARR(64)
    X_TIT   = 'Scattering angle (deg)'
    Y_TIT   = 'Counts'

return, {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,holdtot:HOLDTOT,$
	 hold:HOLD,new:NEW,newtot:NEWTOT,init:0L,tofch:TOFCH,plot:PLOT,xvalue:XVALUE,$
	 x_tit:X_TIT,y_tit:Y_TIT,error:ERROR}
end






