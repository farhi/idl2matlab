;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION phi2q, w_in

; Transforms scattering angle on x-axis to Q, assuming elastic scattering. 
;
;DIMENSIONS:
; w_in=w_out(nspectra)
;
;COMMAND SYNTAX:
; w10=phi2q(w9)
;								KHA 10/2/99
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp_access, inst

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start phi2q:'

	take_datp, datp

;-------------------------------------------------------------------------------
;Set constants

	IF (inst EQ 'D7') THEN lambda=datp.p(4) $
			ELSE lambda=datp.p(21)
	k=2.*!pi/lambda

	IF (iprint GT 0) THEN PRINT,'End of "set constants" section'

;-------------------------------------------------------------------------------
;transform phi to Q

	phi=ABS(datp.x)*!pi/180.

	x_out=k*SQRT(2.*(1.-COS(phi)))

	IF (iprint GT 0) THEN PRINT,'End of "transform phi to Q" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	w_out=w_in

	datp.x=x_out
	datp.x_tit='Q (A^-1)'
	s=STRTRIM(STRING(lambda),2)	& n=STRPOS(s,'.')+4	& s=STRMID(s,0,n)
	datp.other_tit=datp.other_tit+' -pq('+s+')'

finished:

	IF (iprint GT 0) THEN PRINT,'End phi2q:'

	give_datp, datp

	RETURN, w_out
	END
