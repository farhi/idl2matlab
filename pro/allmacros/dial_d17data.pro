;*********************
PRO dial_d17data_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_d17data

    if D.DIMS(0) eq 0 then begin
	V=DialNewValue(TYPE='t_res')
	D.DIMS(0)=V.par_tof(0)
	D.DIMS(1)=V.par_tof(4) - V.par_tof(3) +1
	D.DIMS(2)=V.par_tof(2) - V.par_tof(1) +1
        print,V.par_tof(0),V.par_tof(1),V.par_tof(2),V.par_tof(3),V.par_tof(4),V.par_tof(5),V.par_tof(6)
	print,"Dimensions:",D.DIMS
	DialInit ,'showx'
	DialStart,'showx' & DialTag,'showx',tag='PATER',set=D.number
 	DialInit ,'showy'
	DialStart,'showy' & DialTag,'showy',tag='PATER',set=D.number
   ENDIF

    V=DialNewValue()

	t=D.DIMS(0) & y=D.DIMS(1) & x=D.DIMS(2)
;        print,'xyt',x,y,t

	V=V(0:x*y*t-1)

	if y eq 1 and t gt 1 then begin 
          ;print,'x t data'         
          V=reform(V,x,t)
          px=total(V,2) & if d.logx eq 1 then px=alog10(px>.1)
          DialTag,'showx',tag='VALUE'    ,set=px
          py=total(V,1) & if d.logy eq 1 then py=alog10(py>.1)
  	  DialTag,'showy',tag='VALUE'    ,set=py
          
        endif

        if t eq 1 and y gt 1 then begin
          V=reform(V,y,x)
          V=transpose(V)
          py=total(V,1) & if d.logy eq 1 then py=alog10(py>.1)
  	  DialTag,'showy',tag='VALUE'    ,set=py
          px=total(V,2) & if d.logx eq 1 then px=alog10(px>.1)
          DialTag,'showx',tag='VALUE'    ,set=px

        endif

        if t eq 1 and y eq 1 then begin
;          print,'only X pixel data'
;          V=reform(V,x)
;          V=transpose(V)
          px=V & if d.logx eq 1 then px=alog10(px>.1)
	  DialTag,'showx',tag='VALUE'    ,set=px
          py=px & if d.logy eq 1 then py=alog10(py>.1)
	  DialTag,'showy',tag='VALUE'    ,set=py
        endif

	 if t gt 1 and x eq 1 then begin
          print,'only Y pixel data'
          V=reform(V,y,t)
          ;V=transpose(V)
          px=total(V,2) & if d.logx eq 1 then px=alog10(px>.1)
	  DialTag,'showx',tag='VALUE'    ,set=px
          py=total(V,1) & if d.logy eq 1 then py=alog10(py>.1)
	  DialTag,'showy',tag='VALUE'    ,set=py
        endif



	;dialWset
	;contour,alog(V>.01),/fill
	if D.logv eq 1 then V=alog10(V>.1)
    DialModValue,V
end



;********************
FUNCTION dial_d17data
;********************
;**
;** The dial initialisation

   ;Dial Variables (optional)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='data'      ;when DialNewValue() is used, get the monitor
    ONOFF=0          ;state of the Dial 1=running
    FREQUENCY=6.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE=0          ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=0           ;-2=no plot 1=surface 2=contour n>2 means show vector of last n values of the scalar
    UPPERLIM=0.      ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file .his
    DURATION=0       ;if >0 then Dial is stopped after running duration seconds

   ;Own Variables (optional)
   ;-------------
    DIMS=lonarr(3)
    

return, {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,plot:PLOT,DIMS:dims,LOGX:0,LOGY:0,LOGV:1}
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_d17data_macro :
;** *************
;** V=DialNewValue([/SETVALUE, COMMENT=txt])   ;Get a new value from DIAL_'generic'_READ
;**                                            (a request is made to the instrument)
;**                                            (/SETVALUE means D.value is set to V)
;** C=DialControl ('command syntax',[CHECK=.5]);Send a command to the instrument control
;**                                            (CHECK means check every .5 sec till the
;**                                             command  is complete)
;** DialTag   ,    'temp2',TAG='VALUE',GET=V   ;Return V,the value for the tag  'VALUE'
;**                                                                of  the dial 'temp2'
;** DialTag   ,    'temp3',TAG='ONOFF',SET=1   ;Set to 1 the value of  the tag  'ONOFF'
;** DialStart ,    'temp3'                     ;A short  for previous call
;** DialStop  ,    'temp3'                     ;A short  too
;**
;** DialModValue,   V                          ;Set the new value for current dial or
;** D.value   =     V                          ;modify yourself the tag Value if type &
;**                                            ;dimensions don't change.(same for Error)
;** D.upperlim=   150.                         ;Set upper limit for plotting.
;**
;** R=DialOn ()                                ;Return 0 if Dial has been interrupted
;**                                            (To use inside loops)
;** DialInit,      'template4',[NEW='tmp4']    ;Initiate dial  'template4' from file:
;**                                                               dial_template4.pro
;**                                            (You may change its name to 'tmp4' and
;**                                            (use DialStart,'tmp4' to activate it)
;** DialMacro,     'template4'                 ;Force execution of DIAL_TEMPLATE4_MACRO
;**                                            ('template4'  is keept inactive, ONOFF=0)
;** DialClear,     'template4'                 ;Suppress dial  'template4' from memory
;** DialWSet                                   ;Reserve central draw window for next plot
;**
;** DialsFrequency,[GET=freq],[SET=.5],[/STOP] ;Set  or Get the general frequency value
;**                [DURATION=90.] ,   [/START] ;              (time is in seconds)
;**                                            ;Stop or Start the general process
;**                                            ;Set  Time  limit for the active process
