	FUNCTION prime_cell, w_in1, w_in2

; calculates prime_cell from transmission runs given in w_in2 and applies
; to runs contained in w_in1
;						JRS 2/3/00

	iprint=0

	take_datp, dat1
	take_datp, dat2, /third
	
	beamcentre, w_in2, px, py

	IF(iprint GE 0) THEN BEGIN
		PRINT,'prime_cell: Beam Centre found at:'
		PRINT,'		X0 =',px
		PRINT,'		Y0 =',py
	ENDIF

	sw=SIZE(w_in1)
	IF (iprint GT 0) THEN PRINT,'SIZE(w_in1)=',sw

	IF (sw(0) EQ 3) THEN nruns=sw(3) ELSE nruns=1
	IF (nruns EQ 1) THEN BEGIN
		dat1.p(13)=px
		dat1.p(14)=py
	ENDIF ELSE BEGIN
		dat1.p(13)=px
		dat1.p(14)=py
		dat1.pv(13,*)=px
		dat1.pv(14,*)=py
	ENDELSE

;---------------------------------------------------------------------------

	prix=STRTRIM(STRING(px),2)
	priy=STRTRIM(STRING(py),2)
	dat1.other_tit=dat1.other_tit+' -pc('+prix+','+priy+')'
	GIVE_DATP, dat1

	RETURN, w_in1
	END
