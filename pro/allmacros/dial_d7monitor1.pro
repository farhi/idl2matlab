;-------------------------------------------------------------------------------
;*******************************************************************************

	pro dial_d7monitor1_macro, Dial

;	Prints the monitor 1 value from the previous measurement
;	updates every 10 mins
;						JRS 22/3/01
;
;-------------------------------------------------------------------------------
;*******************************************************************************

    b=DialNewValue()
    Dial.type='t_res' & a=DialNewValue()
    phn=a.phase_nb
    Dial.value=b(0:phn-1)
    end

function dial_d7monitor1
;******* *************
;**

    return, {NAME:'d7temp',FREQUENCY:600,GENERIC:'mad',TYPE:'monitor1'}
    end
