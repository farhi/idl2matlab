PRO	GAUSS_POLY,X,A,F,PDER
;+
; NAME:
;	GAUSS_FUNCT
;
; PURPOSE:
;	EVALUATE THE SUM OF A GAUSSIAN AND A 2ND ORDER POLYNOMIAL
;	AND OPTIONALLY RETURN THE VALUE OF IT`S PARTIAL DERIVATIVES.
;	NORMALLY, THIS FUNCTION IS USED BY CURVEFIT TO FIT THE
;	SUM OF A LINE AND A VARYING BACKGROUND TO ACTUAL DATA.
;
; CATEGORY:
;	E2 - CURVE AND SURFACE FITTING.
; CALLING SEQUENCE:
;	FUNCT,X,A,F,PDER
; INPUTS:
;	X = VALUES OF INDEPENDENT VARIABLE.
;	A = PARAMETERS OF EQUATION DESCRIBED BELOW.
; OUTPUTS:
;	F = VALUE OF FUNCTION AT EACH X(I).
;
; OPTIONAL OUTPUT PARAMETERS:
;	PDER = (N_ELEMENTS(X),6) ARRAY CONTAINING THE
;		PARTIAL DERIVATIVES.  P(I,J) = DERIVATIVE
;		AT ITH POINT W/RESPECT TO JTH PARAMETER.
; COMMON BLOCKS:
;	NONE.
; SIDE EFFECTS:
;	NONE.
; RESTRICTIONS:
;	NONE.
; PROCEDURE:
;	F = A(0)*EXP(-Z^2/2) + A(3) + A(4)*X + A(5)*X^2
;	Z = (X-A(1))/A(2)
; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(2) is 0.
;	Added to Gauss_fit, when the variable function name to
;		Curve_fit was implemented.  DMS, Nov, 1990.
;   Modified, TCH, Nov 2000. Multipeakfit
;-
  COMMON fit,voigt,nterms,fitflag,key,undo,npeaks,rectangle,bragg,peakpars
  maxindex=N_ELEMENTS(A)-1
  IF voigt EQ 0 THEN peakpars=3 ELSE peakpars=4
  IF rectangle EQ 1 THEN peakpars=peakpars+1
  
; Bragg-Gauss
;  	variable FWHM    =w[2]/tan(w[1]/360*pi)/360*pi 
;  	return (w[0]*2*Sqrt(ln(2)/pi)/w[2] * exp(-((sin(x/360*pi)/sin(w[1]/360*pi)-1)/FWHM)^2*4*ln(2)))+w[4]

; Gauss
;    	return (w[0]*2*Sqrt(ln(2)/pi)/w[2] * exp(-(x-w[1])^2/w[2]^2*4*ln(2)))+w[4]

; Bragg-pseudo-Voigt
;	variable eta=sin(pi/2*w[13])
;	eta*=eta
;	variable FWHM    =w[2]/tan(w[1]/360*pi)/360*pi  
;	variable dx      =sin(x/360*pi)/sin(w[1]/360*pi)-1
; 	variable G       =w[0]*2*Sqrt(ln(2)/pi)/w[2] * exp(-(dx/FWHM)^2*4*ln(2))
;	variable aL=2/pi/FWHM
;	variable bL=4/FWHM^2
;	variable L=aL/(1+bL*dx)
;   	return (1-eta)*G+eta*L+w[4]

; Bragg-Rectangle-Gauss
;	  variable FWHM    =w[2]/tan(w[1]/360*pi)/360*pi 
;	  variable b = 2*Sqrt(ln(2))/FWHM
;	  variable a1= (sin(x/360*pi)/sin((w[1]-w[3]/2)/360*pi)-1)
;	  variable a2= (sin(x/360*pi)/sin((w[1]+w[3]/2)/360*pi)-1)
;	  return w[0]/2/w[3]*(erf((a1)*b)-erf((a2)*b)) +w[4]

; Rectangle-Gauss
;	 variable b = 2*Sqrt(ln(2))/w[2]
;	 variable a = x-w[1]
;	 variable c = w[3]/2
;	 return w[0]/2/w[3]*(erf((a+c)*b)-erf((a-c)*b))  +w[4]

; Bragg-Rectangle-pseudo-Voigt
;	variable eta=sin(pi/2*w[13])
;	eta*=eta
;	variable FWHM    = w[2]/tan(w[1]/360*pi)/360*pi  
;	variable sinx    = sin(x/360*pi)
;	variable dx      = sinx/sin(w[1]/360*pi)-1
;	variable bG      = 2*Sqrt(ln(2))/FWHM
;	variable aG1     = (sinx/sin((w[1]-w[3]/2)/360*pi)-1)
;	variable aG2     = (sinx/sin((w[1]+w[3]/2)/360*pi)-1)
;	variable G       = w[0]               /2/w[3] *(erf((aG1)*bG)-erf((aG2)*bG)) 
;	variable aL=2/pi/FWHM
;	variable bL=4/FWHM^2
;	variable L=2*aL/w[3]*(ln(1+bL*x)-ln(1+bL*(x-w[3]/2)))/bL
;   	return (1-eta)*G+eta*L+w[4]

  PDER = FLTARR(N_ELEMENTS(X),nterms+npeaks*peakpars) 
  j=0
  F=FLTARR(N_ELEMENTS(X))
  FOR i=0,npeaks-1 DO BEGIN
    IF bragg THEN BEGIN
      tana=1./TAN(A(j+1)/360.*!PI)/360.*!PI 
      sigma=A(j+2)*tana
      sinx=SIN(x/360.*!PI)
      sina=SIN(A(j+1)/360*!PI)
      dx   =ABS(SINx/SINA-1)
    ENDIF ELSE BEGIN
      sigma=A(j+2)
      dx   =x-A(j+1)
    ENDELSE
    IF rectangle THEN BEGIN
      bG=1/SQRT(2)/sigma
      IF bragg THEN BEGIN
        aG1=(sinx/SIN((A(j+1)-A(j+3+voigt)/2)/360*!PI)-1)
        aG2=(sinx/SIN((A(j+1)+A(j+3+voigt)/2)/360*!PI)-1)
      ENDIF ELSE BEGIN
        aG1=x-A(j+1)+A(j+3+voigt)/2
        aG2=x-A(j+1)-A(j+3+voigt)/2
      ENDELSE
      G=A(j)*SQRT(2*!PI)*sigma/2./A(j+peakpars-1)*(ERRORF(aG1*bG)-ERRORF(aG2*bG))
      IF voigt THEN BEGIN
        sin_eta=SIN(!PI/2*A(j+3))
        eta=sin_eta^2
        G=G*(1.-eta)
        aL= A(j)/SQRT(!PI*ALOG(2.))
        bL=1./2./SQRT(    ALOG(2.))/sigma^2
        sqrtbl=SQRT(bL)
        L=2.*aL/A(j+4)/sqrtbL*(ATAN(sqrtbL*(A(j+4)-2.*dx)/2.)-ATAN(-bL*(dx)/sqrtbL))*180./!PI
        ;PRINT,al,sigma,bl,l
        F=F+L*eta
      ENDIF
    ENDIF ELSE BEGIN
      IF sigma ne 0.0 THEN Z= dx/sigma ELSE Z = 10.
      EZ = EXP(-Z^2/2.)*(ABS(Z) LE 7.) 
      G=A(j)*EZ
      PDER(*,j) = EZ
      IF sigma ne 0. THEN PDER(*,j+1) = A(j) * EZ * Z/sigma
      PDER(*,j+2) = PDER(*,j+1) * Z
      IF voigt EQ 1 THEN BEGIN
        sin_eta=SIN(!PI/2*A(j+3))
        eta=sin_eta^2
        G=G*(1.-eta)
        PDER(*,j+[0,1,2])=PDER(*,j+[0,1,2])*(1.-eta)
        nenner=(1.0+Z*Z/2.0)
        L=A(j)/nenner
        F=F+L*eta
        PDER(*,j) = PDER(*,j) + 1.0/nenner*eta
        nenner=A(j)*Z/sigma/nenner*eta
        PDER(*,j+1) = PDER(*,j+1) + nenner
        PDER(*,j+2) = PDER(*,j+2) + nenner * Z
        PDER(*,j+3) = (L - G)*!PI*sin_eta
      ENDIF
    ENDELSE
    IF bragg THEN BEGIN
      PDER(j+1)=PDER(j+1)*SINx/SINA^2*!PI/360
      PDER(j+2)=PDER(j+2)*tana
    ENDIF
    F=F+G
    j=j+peakpars
  ENDFOR
  FOR i=0,nterms-1 DO BEGIN
    F=F+A(j+i)*X^i
    PDER(*,j+i)=X^i
  ENDFOR
  RETURN
END
