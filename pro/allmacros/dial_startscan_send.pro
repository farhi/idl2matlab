FUNCTION DIAL_STARTSCAN_SEND, DialType, Check, Commd, DialName
;******* *******************
;**

print,"I'm StartScan and got cmd:"+Commd

DialInit ,"template1"
DialTag  ,"template1",tag="FREQUENCY",set=1
DialTag  ,"template1",tag="HISTORY"  ,set=1
DialStart,"template1"

R=dial_mad_send( DialType, Check, Commd, DialName)



return, R
end
