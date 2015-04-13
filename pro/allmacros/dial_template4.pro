;***********************
PRO dial_template4_macro, D
;***********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_template4
;** This macro procedure is called by George every D.frequency seconds

V= DialNewValue()
V= round(sin(V)*100)

DialModValue, sin( dist(25)/((V/10.)>1) )

end

;**********************
FUNCTION dial_template4
;**********************
;**
;** The dial initialisation

return, {GENERIC:'template',TYPE:'bidon',VALUE:0}
end
