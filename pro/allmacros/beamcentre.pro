;*******************************************************************************

	PRO beamcentre, w_in0, px, py

;	Finds the beam centre position from a transmission run. Called from
;	prime_cell.pro

;						JRS 2/3/00

;-------------------------------------------------------------------------------

	iprint=0        ; if iprint>0, show debugging messages

        IF (iprint GT 0) THEN PRINT,'Start beamcentre:'

        TAKE_DATP, datp

;-------------------------------------------------------------------------------
;define run parameters
;
	m_in0=datp.n	; monitor counts
        x_in0=datp.x    ; x-pixels
        y_in0=datp.y    ; y-pixels
        z_in0=datp.z    ; arbitrary number
        e_in0=datp.e	; errors
        par=datp.p
	
	se=SIZE(e_in0)
	sw=SIZE(w_in0)

	FOR i=0,se(0) DO IF (se(i) NE sw(i)) THEN GOTO, seterr
	GOTO, noseterr
seterr:
	IF (iprint GT 0) THEN PRINT,'bemacentre: No error bars defined for w_in. Use sqrt'
	e_in0=SQRT(w_in0)
noseterr:
	IF (sw(0) EQ 0) THEN BEGIN
		PRINT,'beamcentre: Error - workspace is empty'
		GOTO, finished
	ENDIF

	IF (sw(0) EQ 3) THEN BEGIN
		PRINT,'beamcentre: Error - don`t use concatenated runs'
		GOTO, finished
	ENDIF

	nspectra=sw(1)
	IF (iprint GT 0) THEN PRINT,'nspectra=',nspectra

;-------------------------------------------------------------------------------
; find beam centre

	w_bufx=FLTARR(nspectra)
	w_bufy=FLTARR(nspectra)
	e_bufx=FLTARR(nspectra)
	e_bufy=FLTARR(nspectra)
	w_bufx=total(w_in0,2)
	w_bufy=total(w_in0,1)
	e_bufx=sqrt(total(e_in0^2,2))
	e_bufy=sqrt(total(e_in0^2,1))

	fitgauss, x_in0, w_bufx, e_bufx, MIN(x_in0), MAX(x_in0), A, dA
	fitgauss, y_in0, w_bufy, e_bufy, MIN(y_in0), MAX(y_in0), B, dB

;-------------------------------------------------------------------------------
finished:
	IF (iprint GT 0) THEN PRINT,'Beamcentre finished'

	datp.p(13)=A(2)
	datp.p(14)=B(2)
	px=A(2)
	py=B(2)

	GIVE_DATP, datp

	END

	



