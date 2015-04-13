; Program/procedure for D20 PSD threshold calculation
; Programm "regseu3", 24.3.93 by Anton OED and Pierre CONVERT (BASIC)
; Modified 6.5.96 by Anton OED
; Procedure re-written for IDL/LAMP in February 1997 by Thomas HANSEN

PRO thresh, a,	thresh, boucle,$ ; a = counting rates, thresh = threshold values
              	fit=fit,tmin=tmin,tmax=tmax,marg=marg,start=start,sensibility=sens,$
              	x1=x1,w1=w1,x2=x2,w2=w2,flag=flag,printer=printer,$
              	noprint=noprint,$
              	var1=var1,var2=var2,damp=damp,prev=prev,p2=p2,p3=p3,$
		boxmean=boxmean, cellmean=cellmean,firstbox=firstbox,lastbox=lastbox
IF NOT KEYWORD_SET(marg)  THEN marg=10.
IF KEYWORD_SET(boxmean) AND KEYWORD_SET(marg) THEN BEGIN
	boxmarg=marg
	marg=0
ENDIF
IF NOT KEYWORD_SET(damp) THEN damp=1
a      = a(*,0)
olda=a
oldt=thresh
IF NOT KEYWORD_SET(prev) THEN prev=oldt 
IF NOT KEYWORD_SET(p2) THEN p2=prev 
IF NOT KEYWORD_SET(p3) THEN p3=p2 
ncan   = n_elements(a)                                ; number of detector cells
IF NOT KEYWORD_SET(start) THEN start=0
IF NOT KEYWORD_SET(sens)  THEN sens=fltarr(ncan+2)+1. ; sensibility, not yet implemented
IF NOT KEYWORD_SET(tmin)  THEN tmin=300.
IF NOT KEYWORD_SET(tmax)  THEN tmax=900.
IF N_ELEMENTS(boucle) eq 0  THEN boucle = 10          ; number of iterations0
IF N_ELEMENTS(flag) NE ncan THEN flag=intarr(ncan)
;------ corrections (detector borders etc.) -----------------------------------
a(0)            = 0
a(ncan-1)       = 0
sum1            = total(a(where(a,counts)))
moyen           = sum1 / counts
IF (N_ELEMENTS(fit) NE ncan) THEN a(where(a eq 0)) = moyen
sum1   = total(a)
abw    = SQRT (sum1 / ncan)   ; or square ?
mplus  = moyen + abw
mminus = moyen - abw

;-------------- max and min ---------------------------
min = min(a)
max = max(a)

;-------------- calculation ---------------------------
IF NOT KEYWORD_SET(var1) THEN var1=.0009                         ; as explained below  (changed 29. 8.96)
IF NOT KEYWORD_SET(var2) THEN var2=var1*0.32 ELSE var2=var1*var2 ; empirical values!!
a     = [moyen, a, moyen]     ; counting rates plus border dummy countings
thresh= [0.,thresh,0.]
IF (N_ELEMENTS(fit) EQ ncan) THEN fit = [0.,fit,0.]
mV    = a*0.0                 ; threshold variation in each loop
B     = a*0.0                 ; new neutron counting rate
ngain     = a*0.0             ; gain of neutron counting rate 
redis     = a*0.0             ; gain of neutron counting rate 
MV2   = a*0.0                 ; total threshold variation
FOR k = 1, boucle DO BEGIN
   FOR i = 2, ncan, 2 DO IF flag(i-1) NE -1 THEN BEGIN                          ;caneaux impair
        IF (N_ELEMENTS(fit) EQ ncan+2) THEN moyen = fit(i) 
        mV(i) = -(moyen - a(i)) / (var1 * a(i))
        IF KEYWORD_SET(x1) AND KEYWORD_SET(w1) THEN BEGIN
          mV(i)=(thresh(i)+MV2(i))/interpol(x1,w1,a(i)/moyen)-(thresh(i)+MV2(i))
          ideal=mV(i)
          var1=-(interpol(w1,x1,1)-interpol(w1,x1,(thresh(i)+MV2(i))/(thresh(i)+MV2(i)+ideal)))/(interpol(x1,w1,moyen)-interpol(x1,w1,a(i)/moyen))
        ENDIF
        IF ((thresh(i)+MV2(i)+mV(i)) GE tmax) THEN mV(i) =  tmax - (thresh(i)+MV2(i))
        IF ((thresh(i)+MV2(i)+mV(i)) LE tmin) THEN mV(i) = -(thresh(i)+MV2(i) - tmin)
        IF ((thresh(i)+MV2(i)+mV(i)) LE thresh(i)+marg) AND ((thresh(i)+MV2(i)+mV(i)) GE thresh(i)-marg) THEN mV(i) = -MV2(i)
        IF KEYWORD_SET(x1) AND KEYWORD_SET(w1) THEN BEGIN
          B(i)=moyen*interpol(w1,x1,(thresh(i)+MV2(i)+mV(i))/(thresh(i)+MV2(i)+ideal))
        ENDIF ELSE BEGIN
          B(i) = a(i) * (1- mV(i)*var1)                      ;stockage du can. i
        ENDELSE
        var2bak=var2
        IF KEYWORD_SET(x1) AND KEYWORD_SET(w1) AND KEYWORD_SET(x2) AND KEYWORD_SET(w2) THEN BEGIN
          var2=    -interpol(x1,w1,a(i)/moyen)
          var2=var2+interpol(x2,w2,a(i)/moyen)$
                   +interpol(x1,w1,B(i)/moyen)$
                   -interpol(x2,w2,B(i)/moyen)
          var2=var2/2.
          var2=var2(0)
          var2=var2*(B(i)-a(i))/(B(i)+a(i))*2
        ENDIF ELSE var2=mV(i)*var2 
        ;IF mV(i) NE 0 THEN print,start+i,mV(i),var2,var2/mV(i),mV(i),var2/mV(i)/var1
        B(i-1) = a(i-1)+var2 * a(i - 1)
        redis(i-1)=redis (i-1)+B(i-1) -a(i-1)
        redis(i+1)=redis (i+1)+a(i+1)
        a(i+1) = a(i+1)+var2 * a(i + 1)  
        redis(i+1)=redis (i+1)-a(i+1)
        var2=var2bak
   ENDIF
   a(1:i-2) = B(1:i-2)                                  ;nouvelles val.
   FOR i = 1, ncan, 2 DO  IF flag(i-1) NE -1 THEN BEGIN                          ;caneaux pair
        IF (N_ELEMENTS(fit) EQ ncan+2) THEN moyen = fit(i) 
        mV(i) = -(moyen - a(i)) / (var1 * a(i))
        IF KEYWORD_SET(x1) AND KEYWORD_SET(w1) THEN BEGIN
          mV(i)=(thresh(i)+MV2(i))/interpol(x1,w1,a(i)/moyen)-(thresh(i)+MV2(i))
          ideal=mV(i)
          var1=-(interpol(w1,x1,1)-interpol(w1,x1,(thresh(i)+MV2(i))/(thresh(i)+MV2(i)+ideal)))/(interpol(x1,w1,moyen)-interpol(x1,w1,a(i)/moyen))
        ENDIF
        IF ((thresh(i)+MV2(i)+mV(i)) GE tmax) THEN mV(i) =  tmax - (thresh(i)+MV2(i))
        IF ((thresh(i)+MV2(i)+mV(i)) LE tmin) THEN mV(i) = -(thresh(i)+MV2(i) - tmin)
        IF ((thresh(i)+MV2(i)+mV(i)) LE thresh(i)+marg) AND ((thresh(i)+MV2(i)+mV(i)) GE thresh(i)-marg) THEN mV(i) = -MV2(i)
        IF KEYWORD_SET(x1) AND KEYWORD_SET(w1) THEN BEGIN
          B(i)=moyen*interpol(w1,x1,(thresh(i)+MV2(i)+mV(i))/(thresh(i)+MV2(i)+ideal))
        ENDIF ELSE BEGIN
          B(i) = a(i) * (1- mV(i)*var1)                      ;stockage du can. i
        ENDELSE
        var2bak=var2
        IF KEYWORD_SET(x1) AND KEYWORD_SET(w1) AND KEYWORD_SET(x2) AND KEYWORD_SET(w2) THEN BEGIN
          var2=    -interpol(x1,w1,a(i)/moyen)
          var2=var2+interpol(x2,w2,a(i)/moyen)$
                   +interpol(x1,w1,B(i)/moyen)$
                   -interpol(x2,w2,B(i)/moyen)
          var2=var2/2.
          var2=var2(0)
          var2=var2*(B(i)-a(i))/(B(i)+a(i))*2
        ENDIF ELSE var2=mV(i)*var2 
        ;IF mV(i) NE 0 THEN print,start+i,mV(i),var2,var2/mV(i),mV(i),var2/mV(i)/var1
        B(i-1) = a(i-1)+var2 * a(i - 1)
        redis(i-1)=redis (i-1)+B(i-1) -a(i-1)
        redis(i+1)=redis (i+1)+a(i+1)
        a(i+1) = a(i+1)+var2 * a(i + 1)  
        redis(i+1)=redis (i+1)-a(i+1)
        var2=var2bak
   ENDIF
   a(0:i-2) = B(0:i-2)                                  ;nouvelles val.
   MV2 = MV2 + mV                                       ;sum des variations
   ;plot,MV2(1:ncan)
ENDFOR
sum2 = total(a(1:ncan))
mmax = max (ABS(MV2(1:ncan)))
PRINT,"New total counting and maximal threshold shift: ", sum2,mmax

;---- presentation du spectre effectuer par la changement des seuils
a      = a(1:ncan)            ; new counting rates minus dummy cells
ngain  = ngain(1:ncan)
tgain  =a-olda
ngain  =tgain-redis
thresh = thresh(1:ncan)
IF (N_ELEMENTS(fit) EQ ncan+2) THEN fit=fit(1:ncan)

;------- presentation de la variation des seuils ------------------
MV2 = MV2(1:ncan)         ; total threshold variation in mV minus dummy values
;plot,MV2
thresh = thresh + MV2/damp     ; new threshold values (optional output)

;------- output for printer ------------------
OPENW,out,'threshold.out',/get_lun
IF NOT KEYWORD_SET(firstbox) THEN firstbox=1
IF NOT KEYWORD_SET(lastbox)  THEN lastbox =1
IF KEYWORD_SET(boxmean) THEN BEGIN
	marg=boxmarg
	FOR i=0,ncan/32-1 DO BEGIN
		thresh(i*32:i*32+31)=thresh(i*32:i*32+31)/mean(thresh(i*32:i*32+31))*boxmean
	ENDFOR
ENDIF
IF KEYWORD_SET(cellmean) THEN BEGIN
	marg=boxmarg
	meancell=fltarr(32)
	FOR i=firstbox*32,lastbox*32+31 DO BEGIN
		meancell(i mod 32)=meancell(i mod 32)+thresh(i)
	ENDFOR
	FOR i=0,15 DO BEGIN
		meancell(i)=meancell(i)+meancell(31-i)
	ENDFOR
	FOR i=0,15 DO BEGIN
		meancell(31-i)=meancell(i)
	ENDFOR
	meancell=meancell/(lastbox-firstbox+1)/2
	FOR i=0,ncan-1 DO BEGIN
		thresh(i)=thresh(i)/meancell(i mod 32)*cellmean
	ENDFOR
ENDIF
IF KEYWORD_SET(cellmean) OR KEYWORD_SET(boxmean) THEN BEGIN
	FOR i=0,ncan-1 DO BEGIN
		IF ABS(thresh(i)-oldt(i)) LE marg THEN BEGIN
			thresh(i)=oldt(i)
		ENDIF
	ENDFOR
	FOR i=0,ncan-1 DO MV2(i)=thresh(i)-oldt(i)
ENDIF
cellstotouch=0
FOR j=0,ncan-1 DO BEGIN
   IF round(thresh(j)) NE round(oldt(j)) THEN cellstotouch=cellstotouch+1
   i=start+j
   format='("??",I4,I3,I3,I5," (",I4,",",I4,",",I4,",",I4,") Err",F8.3," (",F8.3,")")'
   IF MV2(j) GT 0 THEN format='("**",I4,I3,I3,I5," (",I4,",",I4,",",I4,",",I4,") +++",F8.3," (",F8.3,")")'
   IF MV2(j) LT 0 THEN format='("**",I4,I3,I3,I5," (",I4,",",I4,",",I4,",",I4,") ---",F8.3," (",F8.3,")")'
   IF round(thresh(j)) GE ROUND(tmax)    THEN format='("++",I4,I3,I3,I5," (",I4,",",I4,",",I4,",",I4,") max",F8.3," (",F8.3,")")'
   IF round(thresh(j)) LE ROUND(tmin)    THEN format='("--",I4,I3,I3,I5," (",I4,",",I4,",",I4,",",I4,") min",F8.3," (",F8.3,")")'
   IF round(thresh(j)) EQ ROUND(oldt(j)) THEN format='("  ",I4,I3,I3,I5," (",I4,",",I4,",",I4,",",I4,")    ",F8.3," (",F8.3,")")'
   IF (i MOD 32) EQ 0 THEN BEGIN
	printf,out
	printf,out
	print
	print,'Box:',j/32,': ','average:',mean(thresh(j:j+31)),' mV'
	printf,out,'Box',j/32,': average ',mean(thresh(j:j+31)),' mV'
   ENDIF
   IF (i MOD 64) EQ 0 THEN BEGIN
	;printf,out
   ENDIF
   IF (i MOD 32) EQ 0 THEN BEGIN
	printf,out,"  cell bx no  new   old prev   p1  p2     new count  old count"
   ENDIF
   IF ((i/32) MOD 2) EQ 0 THEN BEGIN
    printf,out,FORMAT=format,i,i/32,31-(i MOD 32),round(thresh(j)),round(oldt(j)),round(prev(j)),round(p2(j)),round(p3(j)),a(j),olda(j) 
   ENDIF ELSE BEGIN
    printf,out,FORMAT=format,i,i/32,(i MOD 32),round(thresh(j)),round(oldt(j)),round(prev(j)),round(p2(j)),round(p3(j)),a(j),olda(j) 
   ENDELSE
   IF ((i/32) MOD 2) EQ 0 THEN BEGIN
    print,     FORMAT=format,i,i/32,31-(i MOD 32),round(thresh(j)),round(oldt(j)),round(prev(j)),round(p2(j)),round(p3(j)),a(j),olda(j) 
   ENDIF ELSE BEGIN
    print,     FORMAT=format,i,i/32,(i MOD 32),round(thresh(j)),round(oldt(j)),round(prev(j)),round(p2(j)),round(p3(j)),a(j),olda(j) 
   ENDELSE
ENDFOR
CLOSE,out
FREE_LUN,out

thresh=round(thresh)

IF NOT KEYWORD_SET(printer) THEN line='$lp threshold.out' ELSE line='$lp -d'+printer+' threshold.out'
if NOT keyword_set(noprint) THEN  XICUTE,line 

IF KEYWORD_SET(cellmean) THEN BEGIN
	;FOR i=0,31 DO BEGIN
	;	PRINT,i,meancell(i)
	;ENDFOR
	;FOR i=firstbox*32,lastbox*32+31 DO BEGIN
	;	PRINT,i, thresh(i)
	;ENDFOR
ENDIF
print,cellstotouch,' cells to be adjusted'
END
 ;w8=w6&w9=w7&thresh,w8,w9,/noprint,var1=0.0009,var2=0.05,start=400
;Lamp> w12=w1
;w12: Float   dim = 1600 min=0.00000 max=305575.
;Lamp> w13=w2
;w13: Float   dim = 1600 min=300.000 max=900.000
;Lamp> w3=w1&w4=w2&thresh,w3,w4,50,fit=w5,flag=w14,marg=25,tmin=300,tmax=1000,/noprint
