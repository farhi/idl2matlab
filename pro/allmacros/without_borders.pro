FUNCTION without_borders,w,modulo,i
take_datp,datp
i=indgen(1600)
maxmod=max(i mod modulo)
wnew=w(where(((i mod modulo) ne 0) and ((i mod modulo) ne maxmod)))
x=datp.x(where(((i mod modulo) ne 0) and ((i mod modulo) ne maxmod)))
e=datp.e(where(((i mod modulo) ne 0) and ((i mod modulo) ne maxmod)))
mod_datp,datp,'x',x
mod_datp,datp,'e',e
give_datp,datp
RETURN,wnew
END
