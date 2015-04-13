;*********************
PRO dial_temp_ramp_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_furnace_ramp

    IF(D.init NE 1) THEN BEGIN
	D.current=1.4
	D.init=1
    ENDIF
    V=DialNewValue(/setvalue)
    D.current=D.current+0.3
    command='pte '+STRTRIM(STRING(D.current),2)
    C=DialControl(command)

end



;*********************
FUNCTION dial_temp_ramp
;*********************
;**
;** The dial initialisation

   ;Dial Variables (optional)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='temp'   ;when DialNewValue() is used, get the monitor
    ONOFF=0          ;state of the Dial 1=running
    FREQUENCY=60.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE=1	 ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=0           ;-2=no plot 1=surface 2=contour n>2 means show vector of last n values of the scalar
    UPPERLIM=100.    ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file .his
    DURATION=0       ;if >0 then Dial is stopped after running duration seconds
    CURRENT=2.12
    INIT=0
   ;Own Variables (optional)
   ;-------------

return, {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,current:CURRENT,$
         init:INIT}
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_furnace_ramp_macro :
;** *************
;** V=DialNewValue([/SETVALUE],[COMMENT=txt]   ;Get a new value from DIAL_'generic'_READ
;**                [TYPE='monitor'])           (a request is made to the instrument)
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
;**                           ,[PATH=path ]                dial_template4.pro
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
