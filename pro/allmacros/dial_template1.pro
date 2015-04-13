;***********************
PRO dial_template1_macro, D
;***********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_template1
;** This macro procedure is called by George every D.frequency seconds

V= DialNewValue()
V= round(sin(V)*100)

if D.init  eq 0 then $
if V gt 50 then begin  D.init=1
                           Cd=DialControl("Start brothers")
                           DialsFrequency, SET=2.
                              DialInit ,  "template2", path=D.PATH
                              DialStart,  "template2"
                              DialTag  ,  "template2", TAG="PLOT", SET=1
                            	 DialInit ,  "template3", path=D.PATH
                            	 DialStart,  "template3"
                            	 DialTag  ,  "template3", TAG="PLOT", SET=0
                            	    DialInit, "template4",path=D.PATH
endif

if D.init then if V lt 50 then DialMacro, "template4"
D.VALUE = V
D.ERROR = sqrt(V)
end

;**********************
FUNCTION dial_template1
;**********************
;**
;** The dial initialisation

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
    GENERIC='template' ;Reading control procedure is:"dial_template_read.pro"
    TYPE='temperature' ;then V=DialNewValue() stands for V=DialNewValue(TYPE='temperature')
    ONOFF=0            ;state of the Dial 1=running
    FREQUENCY=1.       ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE=0            ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    ERROR=0.
    PLOT=50            ;-2=none 0=plot 1=surface 2=contour n>2 means show vector of last n scalar values
    UPPERLIM=150.      ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0          ;=1 to record values in file template1.his
    DURATION=0         ;if >0 then Dial is stopped after running duration seconds
    WUPDATE=0          ;=1 to automaticaly update corresponding workspace

   ;User Variables (Must be present in return statement to be available)
   ;-------------
    INIT=0           ;may be used in template1_macro when started or on reset
    XVALUE=fltarr(64);Abscissa of VALUE  (ordinates go in YVALUE)
    X_TIT='I am X'   ;X axis title       (Y axis title go in Y_TIT)

D={GENERIC:generic, VALUE:value, ONOFF:onoff, FREQUENCY:frequency,  $
              DURATION:duration , HISTORY:history, UPPERLIM:upperlim,$
              PLOT:plot, TYPE:type , INIT:0}

return,D
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_template1_macro :
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
;** DialModValue,   V ,[tag='VALUE']           ;Set the new value for current dial or
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
