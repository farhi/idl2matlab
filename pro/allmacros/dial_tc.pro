;    *********************************************************
;    ************************          ***********************
;    ************************  A DIAL  ***********************
;    ************************          ***********************
;    *********************************************************
;    dial TC
;    **************

;This dial monitors instrument time 

pro  DIAL_TC_MACRO, D
;**  ********************

DialWSet
xyouts, 10,10, STRING(dialNewValue(/SETVALUE)), /DEVICE
PRINT, 'Time- Counts: ',d.V


end





;******* ********************************
;******* ********************************
function DIAL_TC
;******* ********************************
;******* ********************************
;**
;** Input  <none>
;** Output D is the dial structure returned for DialInit()
;********* *********************************************

;inherits="template0"                   ;May inherits tags of Dial "template0"
 inherits=""                            ;    (so, its macro is compiled).

;** Usefull tags:                       ;Only one tag is necessary to validate a Dial
;** ************                        (George creates usefull tags if they are not)
name     ="tc"                   ;Consistent with "dial_template1.pro"
generic  ="mad"                    ;Reading instrument control procedure is:
                                        ;                "dial_template_read.pro"
type     ="time-counts"                 ;Requested value will be the temperature
plot     = -2                          ;Mode for plotting (ex: 50 last values,
                                        ;  -2 for no plot, -1 read into workspace) 
                                        ;  for 2D dials: 1= surface, 2= levels)
value    = 0L                           ;The dial value    set to a correct type &
                                        ;                              dimension
error    = 0.                           ;Idem for error -> value +- error
onoff    = 0                            ;The dial activity set to 0 initially [0,1]
number   = 0                            ;The dial number   set by george (d1...d20)
frequency= 2                         ;The dial specific frequency (in seconds)
duration = 0.0                          ;The dial specific duration  (in seconds)
history  = 0                            ;If 1, values are saved into history file:
                                        ;                     "dial_template1.his"

                ;** Other tags used in your procedure DIAL_TEMPLATE1_MACRO:
                ;** ******************************************************
                unit      =""           ;The dial string unit
                lowerlim  =0.           ;The dial lower limit value
                upperlim  =150.         ;The dial upper limit value
                                        ;   (used also for plotting)   etc......

D={NAME:name, GENERIC:generic, VALUE:value, ONOFF:onoff, FREQUENCY:frequency,  $
              DURATION:duration,UNIT:unit , HISTORY:history, UPPERLIM:upperlim,$
              LOWERLIM:lowerlim, PLOT:plot, TYPE:type, INHERITS:inherits}

return,D
end
