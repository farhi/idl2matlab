;*********************
PRO dial_prox_macro, D
;*********************
;**
;** Input D is the dial structure as defined by the function dial_prox
;** This macro procedure is called by George every D.frequency seconds

DialInit,'xbu',d=D.number ,NEW='prox'
DialTag ,d=D.number ,tag='GENERIC'   ,set='lamp'
DialTag ,d=D.number ,tag='FREQUENCY' ,set=.5
DialTag ,d=D.number ,tag='ONOFF'     ,set=1

end



;*********************
FUNCTION dial_prox
;*********************
;**
;** The dial constructor

    return,{ONOFF:1}
end
