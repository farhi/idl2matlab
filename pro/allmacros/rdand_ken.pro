FUNCTION rdand_isis, run1, run2, flip=step
; called from rdand for ISIS data. Concatenate data into w21. Read data into w'wi'
@lamp.cbk

	w21=0 & x21=0 & n21=0 & e21=0 & stati=0 & xcat=0
	IF (NOT KEYWORD_SET(step)) THEN step=1
	wi=one	& ws=STRTRIM(STRING(wi),2)

	p_did_getrun, run1, 21, status
	IF (status NE 0) THEN GOTO, finished
	sz=(SIZE(w21))(0)	& nruns=0
	FOR run=LONG(run1)+step,run2,step DO BEGIN
		p_did_getrun, run, wi, status
		IF (status NE 0) THEN GOTO, readerror
		IF (sz EQ 1) THEN BEGIN			; 1-dimensional data
			iii=EXECUTE('w21=[[w21],[w'+ws+']]')
			iii=EXECUTE('e21=[[e21],[e'+ws+']]')
			iii=EXECUTE('n21=[[n21],[n'+ws+']]')
		ENDIF ELSE IF (sz EQ 2) THEN BEGIN	; 2-dimensional data
			nx0=(SIZE(x21))(1) & iii=EXECUTE('nx1=(SIZE(x'+ws+'))(1)')
			IF (nx1 GT nx0) THEN BEGIN
				PRINT,'rdand_isis: error - first run must be the biggest'
				RETURN, 0
			ENDIF ELSE IF (nx1 LT nx0) THEN BEGIN
				xcat=1
				iii=EXECUTE('x'+ws+'=[x'+ws+',FLTARR(nx0-nx1)]')
				ny=N_ELEMENTS(y21)
				iii=EXECUTE('w'+ws+'=TEMPORARY([w'+ws+',FLTARR(nx0-nx1,ny)])')
				iii=EXECUTE('e'+ws+'=TEMPORARY([e'+ws+',FLTARR(nx0-nx1,ny)-1.])')
				ny=(SIZE(n21))(2)
				iii=EXECUTE('n'+ws+'=[n'+ws+',FLTARR(nx0-nx1,ny)]')
				iii=EXECUTE('x21=[ [x21] , [x'+ws+'] ]')
			ENDIF
			iii=EXECUTE('w21=TEMPORARY([[[w21]],[[w'+ws+']]])')
			iii=EXECUTE('e21=TEMPORARY([[[e21]],[[e'+ws+']]])')
			iii=EXECUTE('n21=[[[n21]],[[n'+ws+']]]')
		ENDIF
		iii=EXECUTE('pv21=[[pv21], [p'+ws+'] ]')
		nruns=nruns+1
		IF (rdstop(run1+step,run2,(run))) THEN run=run2+1
	ENDFOR
readerror:
	stati=run
	iii=EXECUTE('e'+ws+'= e21')
	iii=EXECUTE('n'+ws+'= n21')
	iii=EXECUTE('pv'+ws+'=pv21')
	iii=EXECUTE('p'+ws+'(4)='+STRTRIM(STRING(nruns),2))
	IF (sz EQ 1) THEN BEGIN
		iii=EXECUTE('y'+ws+'=REFORM(pv21(0,*))')
		y_tit(wi)='Run number'
	ENDIF ELSE IF (sz EQ 2 AND xcat EQ 1) THEN $
		iii=EXECUTE('x'+ws+'=x21')
finished:
	RETURN, w21
	END

