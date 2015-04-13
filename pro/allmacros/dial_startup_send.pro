FUNCTION dial_startup_send, dummy1,dummy2, text, button
;*******
;**
;** Called from the Pad, generaly to start and stop Dials

Dials=strlowcase(strtrim(str_sep(text(0),'~'),2))

FOR i=0,n_elements(Dials)-1 DO BEGIN
    DialInit ,Dials(i)
    DialTag  ,Dials(i),tag='FREQUENCY',set=1.0
    DialStart,Dials(i)
ENDFOR

return,0
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_startup_send :
;** *************
;** DialTag   ,    'temp2',TAG='VALUE',GET=V   ;Return V,the value for the tag  'VALUE'
;**                                                                of  the dial 'temp2'
;** DialTag   ,    'temp3',TAG='ONOFF',SET=1   ;Set to 1 the value of  the tag  'ONOFF'
;** DialStart ,    'temp3'                     ;A short  for previous call
;** DialStop  ,    'temp3'                     ;A short  too
;**
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
