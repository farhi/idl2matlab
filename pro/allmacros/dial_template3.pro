;***********************
PRO dial_template3_macro, D
;***********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_template3
;** This macro procedure is called by George every D.frequency seconds

V= DialNewValue()
V= round(sin(V)*cos(V)*2*100)

if V lt 25  then DialMacro, "template4"
D.VALUE='S='+string(V)

end

;**********************
FUNCTION dial_template3
;**********************
;**
;** The dial initialisation

return, {GENERIC:'template',TYPE:'bidon',VALUE:''}
end
