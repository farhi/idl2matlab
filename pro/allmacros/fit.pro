FUNCTION freefit,xx,ww,aa,astep,amin,amax
;
success=0
a = aa(*,0,0)
x = xx(*,0,0)
w = ww(*,0,0)
inistep=astep
ncycles = 4
IF n_elements(x) gt 6 AND n_elements(w) eq n_elements(x) THEN BEGIN
  success = 1
  WHILE n_elements(a) lt 6 DO a = [a(0:n_elements(a)-1),0]
  FUNCT,x,a,f
  FOR cycle= 0,ncycles DO IF success EQ 1 THEN BEGIN
    FOR i=0,5 DO IF astep(i) GT 0 AND success EQ 1 THEN BEGIN
      step=inistep(i)
      Rnew=TOTAL(ABS(w-f))
	     Rold=Rnew
	     print,cycle,i,a(i),Rnew
      R1=0
						R2=0
						R3=0
      REPEAT BEGIN
						  R = Rnew
        IF R3 NE 0 THEN BEGIN
								  R2=R3
										a2=a3
								ENDIF
        IF R1 NE 0 THEN BEGIN
								  R3=R1
										a3=a1
								ENDIF
        a(i)=a(i)+step
	       FUNCT,x,a,f
        Rnew=TOTAL(ABS(w-f))
								step=step*2
	      print,cycle,i,a(i),Rnew,R
						  a1=a(i)
						  R1=Rnew
      ENDREP UNTIL Rnew GT R OR a(i) GE amax(i)+step; result becomes worse
						IF a(i) GE amax(i)+step THEN success = 0
      step=inistep(i)
      IF success EQ 1 THEN IF R2 EQ 0 THEN REPEAT BEGIN
						  R = Rnew
        a(i)=a(i)-step
	       FUNCT,x,a,f
        Rnew=TOTAL(ABS(w-f))
								step=step*2
	      print,cycle,i,a(i),Rnew,R
						  a2=a(i)
						  R2=Rnew
      ENDREP UNTIL Rnew GT R OR a(i) LE amin(i)-step; result becomes worse
      IF a(i) LE amin(i)-step THEN success = 0
			  	IF success EQ 1 THEN BEGIN
				  		a(i)=a2+R2/(R2+R1)*(a1-a2)
	       FUNCT,x,a,f
        R3=TOTAL(ABS(w-f))
				  		a3=a(i)
        print,a2,a3,a1,R3
        REPEAT BEGIN
						    a(i)=a3+R3/(R3+R1)*(a1-a3)
	         FUNCT,x,a,f
          R13=TOTAL(ABS(w-f))
						  	 a13=a(i)
  				  		a(i)=a2+R2/(R2+R3)*(a3-a2)
	         FUNCT,x,a,f
          R23=TOTAL(ABS(w-f))
						  		a23=a(i)
						  	 IF R13 LE R23 THEN BEGIN
								    R2=R3
								  		R3=R13
								  		a2=a3
								  		a3=a13
							  	ENDIF ELSE BEGIN
								    R1=R3
									  	R3=R23
								  		a1=a3
									  	a3=a23
						  		ENDELSE
          print,a2,a3,a1,R3
        ENDREP UNTIL (a1-a2) LE inistep(i)/10. ; OR  (a1-a2) LE ABS(a3)/1000000.; AND Rold GE R3; stepwidth
 					  a(i)=a3
						  IF a(i) GT amax(i) OR a(i) LT amin(i) THEN success = 0
      ENDIF
    ENDIF
  ENDIF
		aa=a
  return,success
 ; print,'freefallfit success: ',aa
ENDIF ELSE BEGIN
;  print,'freefallfit without success'
  return,success
ENDELSE
END

FUNCTION fit,w,a,PRINT=printing,freeprint=freeprint,PLOT=plot,$
                 freefall=free,xx=xx,datp=datp,$
                 xmin=xmin,xmax=xmax,$
                 nterms=nterms,$
                 pikmin=pikmin,pikmax=pikmax,$
                 sigmaa=sigmaa,$
                 voigt=voigt,asym=asym,peaks=NbOfPeaks,around=around
;
; Started: 04-Nov-96 by Th.Hansen, ILL-Grenoble
; Gaussian fit for sequential diagrams and multiple peaks 
;
; w is a LAMP workspace, it might be three-dimensional 
; - in that case sequential fits will be performed
; a is a vector with max. six elements or an array (<6 x number_of_peaks)
; each peak is fitted seperately sequentially after their intensities
; they are NOT simultaneously fitted and that may cause problems, even crashes of LAMP
;
; Modification 22-May-97 by Th. HANSEN:; 
; Modification 08-Sep-97 by Th. HANSEN:; several bugs fixed (array-indexing for a and x, etc.)
; Modification 04-Nov-97 by Th. HANSEN:; Voigt function and asymmetry correction ... ?

IF NOT KEYWORD_SET(datp) THEN take_datp,datp
f = 0.0*w
IF NOT KEYWORD_SET(xx) THEN x=datp.x ELSE x=xx
IF n_params() le 1 OR TOTAL(a) EQ 0 THEN BEGIN
  IF NOT KEYWORD_SET(xmin) THEN BEGIN
    a=[max(w(*,0),i),xx(i,0),xx(1)-xx(0),min(w(*,0))];,0.0,0.0] 
  ENDIF ELSE BEGIN
    a=FLTARR(4,N_ELEMENTS(xmin))
    FOR j=0,N_ELEMENTS(xmin)-1 DO BEGIN
      index=WHERE(xx(*,0) GE xmin AND xx(*,0) LE xmax)
      a(*,j)=[max(w(index,0),i),xx(i,0),xx(1)-xx(0),min(w(index,0))];,0.0,0.0] 
    ENDFOR  
  ENDELSE
ENDIF
IF NOT KEYWORD_SET(nterms) THEN nterms=(n_elements(a(*,0,0))>4)<6
IF n_elements(a(*,0,0)) lt 4 THEN BEGIN
  a=[a(0:n_elements(a(*,0,0))-1,*,*),fltarr(4-n_elements(a(*,0,0)),n_elements(a(0,*,0)),n_elements(a(0,0,*)))]
ENDIF
WHILE n_elements(a(0,0,*)) lt n_elements(w(0,*)) DO $
  a=[[[a(*,*,*)]],[[a(*,*,n_elements(a(0,0,*))-1)]]]
  sigmaa=a
WHILE n_elements(x(0,*)) lt n_elements(w(0,*)) DO $
  x=[[x(*,*)],[x(*,n_elements(x(0,*))-1)]]
IF keyword_set(printing) THEN print,'Step No Peak-Heigth    Position 2.355*Sigma Residu.' 
IF N_ELEMENTS(x(0,*)) LE 1 THEN BEGIN
  range=lindgen(n_elements(w(*,0)))
  points=n_elements(w(*,0))
  IF KEYWORD_SET(xmin)                     THEN range=min(range)+WHERE(x(range) GE xmin,points)
  IF points GT 6 THEN IF KEYWORD_SET(xmax) THEN range=min(range)+WHERE(x(range) LE xmax,points)
  IF points GT 6 THEN IF KEYWORD_SET(pikmin) THEN IF MAX(x(range)) LT pikmin THEN points=0
  IF points GT 6 THEN IF KEYWORD_SET(pikmax) THEN IF MIN(x(range)) GT pikmax THEN points=0
ENDIF
FOR i=0L,LONG(n_elements(w(0,*))-1) DO BEGIN
  IF NOT KEYWORD_SET(NbOfPeaks) THEN NbOfPeaks=n_elements(a(0,*))
  IF N_ELEMENTS(x(0,*)) GT 1 THEN BEGIN
    range=lindgen(n_elements(w(*,i)))
    points=n_elements(w(*,i))
    IF KEYWORD_SET(xmin)                     THEN range=min(range)+WHERE(x(range,i) GE xmin,points)
    IF points GT 6 THEN IF KEYWORD_SET(xmax) THEN range=min(range)+WHERE(x(range,i) LE xmax,points)
    IF points GT 6 THEN IF KEYWORD_SET(pikmin) THEN IF MAX(x(range,i)) LT pikmin THEN points=0
    IF points GT 6 THEN IF KEYWORD_SET(pikmax) THEN IF MIN(x(range,i)) GT pikmax THEN points=0
  ENDIF 
  outer_range=LONG(range)
  IF points GT 6 THEN BEGIN
   ws=smooth(w(range,i),3+N_ELEMENTS(w(range,i))/100)
   FOR j=0l,long(NbOfPeaks-1) DO BEGIN
    range=LONG(outer_range)
   ; print,points,min(range),max(range),x(min(range),i<(N_ELEMENTS(x(0,*))-1)),x(max(range),i<(N_ELEMENTS(x(0,*))-1))
	aa=fltarr(6)
	aa(3)=MIN(ws)>0
	aa(0)=MAX(ws,ind)-aa(3)
	aa(1)=x(LONG(min(range))+LONG(ind),i)
	tmp= (MAX(WHERE(ws(0:ind) LE aa(0)/2.+aa(3)))>0)-min(range)
	tmp2=(MIN(WHERE(ws(ind:N_ELEMENTS(ws)-1) LE aa(0)/2.+aa(3)))+ind)-min(range)
	IF tmp2 LT 0 THEN tmp2=N_ELEMENTS(ws)-1
	IF KEYWORD_SET(around) THEN range=WHERE((x(*,i) LE aa(1)+around) AND (x(*,i) GE aa(1)-around))
	aa(2)=(x(tmp2,i)-x(tmp,i))/2.5
    IF 9.*aa(0) LE ((2.*a(3,0,i)) > aa(3)) THEN BEGIN
      IF keyword_set(printing) THEN BEGIN
         print,FORMAT='(I4,I3,F12.2,F12.4,F12.4,F8.2," no peak")',i,j,aa(0),aa(1),aa(2),aa(3)
      ENDIF
	  aa=0
      j=long(NbOfPeaks-1)
	ENDIF ELSE BEGIN
      IF keyword_set(printing) AND KEYWORD_SET(freeprint) THEN BEGIN
         print,FORMAT='(I4,I3,F12.2,F12.4,F12.4,F8.2," okay")',i,j,aa(0),aa(1),aa(2),aa(3)
      ENDIF
	ENDELSE
	IF KEYWORD_SET(free) THEN BEGIN
      IF TOTAL(aa) NE 0 THEN BEGIN 
	    tmp=freefit(x(range,i), w(range,i)-f(range,i), aa,[1.,0.1,0.1,1.,0.,0.],[MIN(ws)>0,MIN(x(range,i)),0.05,MIN(ws),0,0],[MAX(ws)-(MIN(ws)<0),MAX(x(range,i)),10.,MAX(ws),0,0])
        IF keyword_set(printing) THEN BEGIN
          print,FORMAT='(I4,I3,F12.2,F12.4,F12.4,F8.2," freefit")',i,j,aa(0),aa(1),aa(2),aa(3)
        ENDIF
  	    IF tmp NE 1 OR aa(0) LE ((2.*a(3,0,i) > 3) > aa(3)) OR aa(1) LT MIN(x(range,i)) OR aa(1) GT MAX(x(range,i)) THEN BEGIN
          aa=0
          j=long(NbOfPeaks-1)
	    ENDIF ; freefallfit
	  ENDIF ; freefallfit
	ENDIF
    IF TOTAL(aa) NE 0 THEN BEGIN 
	  aa=aa(0:nterms-1)
	  sigmaaa=aa(0:nterms-1)
	  IF NOT KEYWORD_SET(voigt) THEN BEGIN     
        f(range,i) = f(range,i) + gaussfit (x(range,i), w(range,i)-f(range,i), aa,NTERMS=nterms,w=w(range,i)/datp.e(range,i),sigmaa=sigmaaa) 
      ENDIF ELSE BEGIN
        ;f(range,i) = f(range,i) + voigtfit (x(range,i), w(range,i)-f(range,i), aa,NTERMS=nterms,w=w(range,i)/datp.e(range,i),sigmaa=sigmaaa) 
      ENDELSE
      IF nterms LT 6 THEN aa=[aa,fltarr(6-nterms)]
      IF KEYWORD_SET(plot) THEN BEGIN
        PLOT,x(range,i),w(range,i),TITLE=STRING(i),YRANGE=[0,MAX(w(range,i))]
        PLOT,x(range,i),f(range,i),/NOERASE,TITLE=STRING(i),YRANGE=[0,MAX(w(range,i))]
      ENDIF
      siga=100.0*TOTAL(ABS(w(range,i)-f(range,i)))/TOTAL(w(range,i))
      a(0:nterms-1,j,i) = aa(0:nterms-1)
      sigmaa(0:nterms-1,j,i) = sigmaaa(0:nterms-1)
      IF keyword_set(printing) THEN print,FORMAT='(I4,I3,F12.2,F12.4,F12.4,F12.4,F12.4,F12.4)',i,j,a(0,j,i),a(1,j,i),a(2,j,i),a(3,j,i),siga,sigmaaa(0)
    ENDIF
    ws=smooth(w(range,i)-f(range,i),3+N_ELEMENTS(w(range,i))/100)
  ENDFOR
  ENDIF
ENDFOR
give_datp,datp
return,f
END

