;*********************
PRO dial_tof_data_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_tof_data

	IF(D.init EQ 0) THEN BEGIN
		D.init=1
		V=DialNewValue(type='t_para')    
		D.tofch=V.TOF_CHA_RESOL
		D.newtot =0.
		D.holdtot=0.
		DialModValue,lonarr(D.tofch,32)
		DialModValue,lonarr(D.tofch,32) ,tag='NEW'
		DialModValue,lonarr(D.tofch,32) ,tag='HOLD'
		dialinit ,'showx'
		dialstart,'showx' & dialtag,'showx',tag='PATER',set=d.number
ENDIF
;	GET THE DATA
;	------------
	V=DialNewValue(type='pmc')
	e=V(0:(D.tofch*66)-1)
        i=INDGEN(32)*2+1
	e=reform(e,D.tofch,66)
        f=e(*,i)
;	REFRESHED?
;	---------
	tmp=total(f)
	IF(tmp lt D.newtot) THEN BEGIN	D.hold   =D.hold+D.new
					D.holdtot= total(D.hold) & ENDIF
	D.new   = f
	D.newtot= tmp
	D.value = D.hold+D.new
	f=alog10(f>0.0001)
	
	
;	UPDATE OTHER DIALS
;	------------------
	dialtag,'showx',tag='VALUE',set=total(D.value,2)
end



;*********************
FUNCTION dial_tof_data
;*********************
;**
;** The dial initialisation

   ;Dial Variables (optional)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='monitor'   ;when DialNewValue() is used, get the monitor
    ONOFF=0          ;state of the Dial 1=running
    FREQUENCY=5.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
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

return, {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,holdtot:HOLDTOT,$
	 hold:HOLD,new:NEW,newtot:NEWTOT,init:0L,tofch:TOFCH,plot:PLOT}
end






