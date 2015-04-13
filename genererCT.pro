; fichier utilise pour generer les tables de couleurs predefinies d'IDL

pro genererCT

OPENW, 1, '/home/cs/cs_guest/dessgi/DESS2002-2003/v18.2/defCT.m'

FOR i = 6,40 DO BEGIN

loadct,i
tvlct,r,g,b,/get

PRINTF, 1, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
PRINTF,1, 'loadct,',i,'= [ transpose( ...'
PRINTF,1, '[',r,'])/255.0, ...'
PRINTF,1, 'transpose( ...'
PRINTF,1, '[',g,'])/255.0, ...'
PRINTF,1, 'transpose( ...'
PRINTF,1, '[',b,'])/255.0 ];'

ENDFOR

CLOSE, 1

end