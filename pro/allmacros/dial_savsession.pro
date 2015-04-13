;*********************
PRO dial_savsession_macro, D
;*********************
;**
;** The dial method

SaveSession
DialModValue,systime()
end

;*********************
FUNCTION dial_savsession
;*********************
;**
;** The dial constructor

return, {FREQUENCY:600}  ;run every 10 minutes
end
