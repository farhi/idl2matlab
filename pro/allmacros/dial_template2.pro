;***********************
PRO dial_template2_macro, D
;***********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_template2
;** This macro procedure is called by George every D.frequency seconds

V= DialNewValue()
V= round(cos(V)*75)

if V lt 25  then DialMacro, "template4"
D.VALUE=V

end

;**********************
FUNCTION dial_template2
;**********************
;**
;** The dial initialisation

generic  ="template"
upperlim =150.
lowerlim =0.
;inherits="template1"   ;May inherits tags of Dial "template1"
 inherits=""

return, {GENERIC:generic,TYPE:'bidon',UPPERLIM:upperlim,LOWERLIM:lowerlim,INHERITS:inherits,VALUE:0}
end
