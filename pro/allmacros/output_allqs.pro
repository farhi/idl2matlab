;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO output_allqs, w_in, file=out_file, ofi

;takes a 2-D workspace from sqw_rebin.pro and writes it to a series of files, 
;one for each Q. x-axis must be Q and y-axis energy transfer.
;
;output files have names <file>.q005   Q=0.05 A-1
;			 <file>.q010   Q=0.10 A-1
;			 etc...
;
;ARGUMENTS
; file	: name of output file stem
; (ofi is obsolete, kept for backwards compatability)
;
;DIMSENSIONS
; w_in(dQ,dE) output from sqw_rebin.pro
;
;COMMAND SYNTAX
; output_allqs, w17, file='<outfile>'
;							KHA,JRS 8/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint=0

	take_datp, datp

	IF(N_ELEMENTS(ofi) GT 0) THEN out_file=ofi 

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	Qarr=datp.x
	sQ=size(Qarr)
	nQs=sQ(1)
	Earr=datp.y	& sE=size(Earr)	& nEs=sE(1)
	Sarr=w_in
	dSarr=datp.e

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open and write to output files

	ON_IOERROR, giveup

	FOR iQ=0,nQs-1 DO BEGIN
		Q=Qarr(iQ)	& Qstring=STRTRIM(STRING(FIX(100.*Q)),2)
		IF (Q LT 0.1) THEN Qstring='q00'+Qstring $
		ELSE IF (Q LT 1.0) THEN Qstring='q0'+Qstring $
		ELSE Qstring='q'+Qstring
		PRINT,'Saving to ',STRTRIM(out_file,2),'.',Qstring
		S=FLTARR(nEs)	& S(*)=Sarr(iQ,*)
		dS=FLTARR(nEs)	& dS(*)=dSarr(iQ,*)
		i=WHERE(dS GT -0.9, n)
		IF (n GT 0) THEN BEGIN
			IF (iprint GT 0) THEN PRINT,'n=',n
			E=Earr(i)	& S=S(i)	& dS=dS(i)
			OPENW, 1, out_file+'.'+Qstring
			ch=STRTRIM(STRING(Q),2)  & i=STRPOS(ch,'.') & Q=STRMID(ch,0,i(0)+4)
			FOR i=0,n-1 DO PRINTF, 1, E(i), S(i), dS(i)
			PRINTF, 1, datp.y_tit
			PRINTF, 1, datp.z_tit
			PRINTF, 1, datp.w_tit+' | Q='+Q+'A-1'
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
