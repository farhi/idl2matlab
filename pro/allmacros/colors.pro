PRO colors,color,TRUECOLORs=truecolors,noprint=noprint,pseudo=pseudo

color=intarr(8)
mydevice=!D.NAME
IF mydevice EQ 'PS' OR mydevice EQ 'TEK' THEN DEVICE,/COLOR
IF mydevice EQ 'TEK' THEN DEVICE,/TEK4100
IF mydevice EQ 'PS' OR mydevice EQ 'X' OR mydevice EQ 'TEK' THEN BEGIN
  TVLCT,red,green,blue,/get
  IF NOT KEYWORD_SET(noprint) THEN PRINT,'Color bits : ',N_ELEMENTS(red)
  TVLCT,[0,255,0,0,255,255,0],[0,0,255,0,255,0,255],[0,0,0,255,0,255,255]
  white=255
  color=[indgen(4),255,indgen(3)+4]
ENDIF ELSE BEGIN
  IF mydevice EQ 'MAC' THEN BEGIN
    TVLCT,red,green,blue,/get
    bit=LONG(N_ELEMENTS(red))
    IF NOT KEYWORD_SET(noprint) THEN PRINT,'Color bits : ',N_ELEMENTS(red)
    white=bit^3-1
    IF NOT KEYWORD_SET(noprint) THEN print,white
    color=[0,bit-1,bit^2-bit,bit^3-bit^2,white,bit^2-1,white-(bit^2-bit),white-bit+1]
    IF NOT KEYWORD_SET(noprint) THEN print,color
  ENDIF ELSE PRINT,mydevice, ' not yet supported!'
ENDELSE
IF KEYWORD_SET(TRUECOLORS) THEN BEGIN
    TVLCT,red,green,blue,/get
    bit=LONG(N_ELEMENTS(red))
    IF NOT KEYWORD_SET(noprint) THEN PRINT,'Color bits : ',N_ELEMENTS(red)
    white=bit^3-1
    IF NOT KEYWORD_SET(noprint) THEN print,white
    color=[0,bit-1,bit^2-bit,bit^3-bit^2,white,bit^2-1,white-(bit^2-bit),white-bit+1]
    IF NOT KEYWORD_SET(noprint) THEN print,color
ENDIF
IF KEYWORD_SET(pseudo) THEN BEGIN
  TVLCT,red,green,blue,/get
  IF NOT KEYWORD_SET(noprint) THEN PRINT,'Color bits : ',N_ELEMENTS(red)
  TVLCT,[0,255,0,0,255,255,0,255],[0,0,255,0,255,0,255,255],[0,0,0,255,0,255,255,255]
  white=7
  color=[indgen(4),7,indgen(3)+4]
ENDIF
IF NOT KEYWORD_SET(noprint) THEN PRINT,'black-red-green-blue-white-yellow-magenta-cyan',STRCOMPRESS(color)

END
