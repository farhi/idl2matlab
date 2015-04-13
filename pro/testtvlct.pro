

pro testtvlct


R = BYTSCL(SIN(FINDGEN(256)))
G = BYTSCL(COS(FINDGEN(256)))
B = BINDGEN(256)


print,R
TVLCT, R, G, B,/HSV

;TVSCL, DIST(400)

;tvlct,R,V,B,/GET

;tvlct, R,V,B,15

;tvlct,R,V,B,/GET


;t = [[findgen(256)],[findgen(256)],[findgen(256)]]+1
;tvlct,t


end
