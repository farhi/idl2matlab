;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION phi2qtof, w_in
;
; input: 1-D data, phi as x-axis, energy transfer as y-axis, e.g. output
; from elastic.pro
; transforms x-axis to Q
;							KHA 10/2/98
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	COMMON c_lamp_access, inst

	take_datp, datp

;-------------------------------------------------------------------------------
;calculate Q axis

	phi=datp.x*!pi/180.
	eps=w_in
	a=2.0719

	IF (inst EQ 'D7') THEN lambda=datp.p(4) $
			ELSE lambda=datp.p(21)
	Ei=81.8066/lambda^2
	Q=SQRT((2.*Ei-eps-2.*SQRT(Ei*(Ei-eps))*COS(phi))/a)
	datp.x=Q

	datp.x_tit='Q  (A^-1)'

	give_datp, datp

	RETURN, w_in
	END


