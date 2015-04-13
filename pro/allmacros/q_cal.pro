	FUNCTION Q_cal,w_in

; Function to calibrate axes in terms of Q of 1-D or 2-D input data

; 						JRS 	3/2/00

	COMMON c_lamp_access, inst

	iprint=1	;GT 0 turns on debugging

	IF (iprint GT 0) THEN PRINT,'Starting q_cal:'

	take_datp,datp

;------------------------------------------------------------------------------
;Check dimensions section

        s=SIZE(w_in)
	IF (s(0) EQ 3) THEN nruns=s(3) ELSE nruns=1

        IF (s(0) GT 1) THEN BEGIN
		X0=datp.p(13)
		Y0=datp.p(14)
	ENDIF ELSE BEGIN
		X0=0.0
		Y0=0.0
	ENDELSE

	IF (inst EQ 'D22') THEN BEGIN
		pix=0.75
	ENDIF ELSE BEGIN
		pix=1.0
	ENDELSE

	lambda=datp.p(15)
        detd=datp.p(9)
	IF (iprint GT 0) THEN BEGIN
		PRINT,'            nruns: ',nruns
		PRINT,'       wavelength: ',lambda
		PRINT,'Detector Distance: ',detd
		PRINT,'       Pixel size: ',pix
	ENDIF
	IF (iprint GT 0) THEN PRINT,'q_cal: End of "check dimensions" section'

;-------------------------------------------------------------------------------
; Transform to Q

	thetax=0.5*(ATAN(((datp.x-X0)*pix)/(detd*100.)))
	x=FLOAT(4*!pi*SIN(thetax)/lambda)
        IF (s(0) GE 2 AND s(1) EQ s(2)) THEN BEGIN
		thetay=0.5*(ATAN(((datp.y-Y0)*pix)/(detd*100.)))
		y=FLOAT(4*!pi*SIN(thetay)/lambda)
		datp.y_tit='Qy (Angtroms^-1)'
		mod_datp, datp, "y", y
	ENDIF

        IF (s(0) GE 2 AND s(1) EQ s(2)) THEN BEGIN
		datp.x_tit='Qx (Angtroms^-1)'
        ENDIF ELSE BEGIN
		datp.x_tit='Q (Angtroms^-1)'
	ENDELSE
	IF (iprint GT 0) THEN PRINT,'q_cal: End of "transform to Q" section'

;-------------------------------------------------------------------------------
; Return data

	swav=STRTRIM(STRING(lambda),2)
	sdet=STRTRIM(STRING(detd),2)
	datp.other_tit=datp.other_tit+' -qc('+inst+','+swav+','+sdet+')'
	mod_datp, datp, "x", x 
	give_datp, datp
	
	RETURN, w_in
	END
