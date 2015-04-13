pro dial_getwave_macro, Dial
;** ****************
;**

    V=DialNewValue()
    Dial.value=string(V)
    end

function dial_getwave
;******* **********
;**

    return, {NAME:'getwave',TYPE:'wave',GENERIC:'mad',VALUE:''}
    end

;Replace the 4 occurences of 'model' by your 'dial name'
;File dial_template1.pro contains a list of usual Tags ans Calls
