FUNCTION rddef, INST , PATH , FILENAME , STATUS , DATP ; , p1 , p2 ,p3 ...
;********
;**
;** The call is w6=mymacro(...)

OPENR,unit,PATH+FILENAME,/GET_LUN
elements=0
READF,unit,elements
Wout=FLTARR(elements)
x=Wout
READF,unit,x,Wout
FREE_LUN,unit
DATP={X:                  x, $	;Pass those variables which were 
      Y_TIT:	   'counts', $ 	;read-in into the DATP structure
      X_TIT:	    '2theta', $ 					
      W_TIT:     INST+' : '+PATH+FILENAME,        $ 					
      OTHER_TIT: string(elements)+' values' }
;GIVE_DATP,datp
STATUS=0
return, Wout
end
