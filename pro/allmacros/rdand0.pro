FUNCTION rdand0 , name , first ,last, extension
;********
;**
;** The call is w6=rdand0('khp',1216,1363,'.prf')

Wout=0
d=1
wout=rdrun(name+STRCOMPRESS(first,/RE)+extension,datp=d)
e=d.e
n=d.n
pv=d.pv
FOR i=first+1,last DO BEGIN
  w=rdrun(name+STRCOMPRESS(i,/RE)+extension,datp=d)
  Wout=[[Wout],[w]]
  IF N_ELEMENTS(d.e ) EQ N_ELEMENTS(w) THEN e= [ [e ] , [d.e ]]
  IF N_ELEMENTS(d.n ) GT 0             THEN n= [[[n ]],[[d.n ]]]
  IF N_ELEMENTS(d.pv) GT 0             THEN pv=[ [pv] , [d.pv]]
ENDFOR
mod_datp,d,'e',e
mod_datp,d,'n',n
mod_datp,d,'pv',pv
mod_datp,d,'y',INDGEN(last-first+1)+first
give_datp,d
return, Wout
end
