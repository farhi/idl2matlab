PRO scheme
;** ******
FORWARD_FUNCTION DialControl, DialNewValue, DialOn
if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

dial_bygeorge,'scheme'   ;<-------- Name of your Dial !!!
END                      ;          ~~~~~~~~~~~~~~~~~ !!!

;-------------------------------------------------
;--------------------Here is your Dial----------
;---------------------------------------------

PRO dial_scheme_macro, D
;** *****************

V=DialNewValue('status',/setvalue)
IF V eq 'Idle' then BEGIN

   CASE D.init of
   0:BEGIN D.init = 1         & c=DialControl(' ')
           D.value='1'                        & END
   1:BEGIN D.init = 2         & c=DialControl(' ')
           D.frequency=2.1    & D.value='2'   & END	   
   2:BEGIN D.init = 3         & V=DialNewValue('t_para')
           D.value='3'                        & END
   3:BEGIN D.init = 4         & c=DialControl(' ')
           D.frequency=1.2    & D.value='4'   & END	   
   4:BEGIN D.value='off'      & c=DialControl(' ')
           DialStop                           & END
   ELSE:
   ENDCASE

ENDIF else D.value=V
END

;******* ***********
FUNCTION dial_scheme & return,{init:0} & END
;******* ***********
