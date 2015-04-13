;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO output_allangles, w_in, file=out_file, ofi

;takes a 2-D workspace from t2e and writes it to a series of files, 
;one for each detector. x-axis must be energy transfer and y-axis 
;detector number.
;
;output files have names <file>.det2   detector #2
;			 <file>.det4   detector #4
;			 etc...
;
;ARGUMENTS
; file	: name of output file stem
; (ofi is obsolete, kept for backwards compatability)
;
;DIMSENSIONS
; w_in(nspectra,dE) output from t2e.pro [and reb.pro]
;
;COMMAND SYNTAX
; output_allangles, w17, file='<outfile>'
;							JRS, 8/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0

	take_datp, datp

	IF(N_ELEMENTS(ofi) GT 0) THEN out_file=ofi 

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	Earr=datp.x
	sE=size(Earr)
	nEs=sE(1)
	Aarr=datp.y	& sA=size(Aarr)	& nAs=sA(1)
	Sarr=w_in
	dSarr=datp.e
	Narr=INDGEN(nAs)+1


	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open and write to output files

	ON_IOERROR, giveup

	FOR iA=0,nAs-1 DO BEGIN
		ang=Aarr(iA)
                det=Narr(iA)
                Astring=STRTRIM(STRING(FIX(det)),2)
		Astring='det'+Astring
		PRINT,'Saving to ',STRTRIM(out_file,2),'.',Astring
		S=FLTARR(nEs)	& S(*)=Sarr(*,iA)
		dS=FLTARR(nEs)	& dS(*)=dSarr(*,iA)
		i=WHERE(dS GT -0.9, n)
		IF (n GT 0) THEN BEGIN
			IF (iprint GT 0) THEN PRINT,'n=',n
			E=Earr(i)	& S=S(i)	& dS=dS(i)
			OPENW, 1, out_file+'.'+Astring
			FOR i=0,n-1 DO PRINTF, 1, E(i), S(i), dS(i)
			PRINTF, 1, datp.x_tit
			PRINTF, 1, datp.z_tit
			PRINTF, 1, datp.w_tit+' | angle='+STRTRIM(ang,2)+' degrees'
			PRINTF, 1, datp.other_tit
			CLOSE, 1
		ENDIF
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of "Write to output files" section'

;-------------------------------------------------------------------------------
;Return and exit

	GOTO, finished
giveup:
	CLOSE, 1
finished:
	give_datp, datp

	RETURN
	END
