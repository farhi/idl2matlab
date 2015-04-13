;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO monitor, numor1, numor2

;					KHA, 19/5/98
;-------------------------------------------------------------------------------
;*******************************************************************************

@read_d7.cbk

	infile=''
	path='/usr/illdata/data-1/d7/'
	outfile='monitor.001'
	olddate=-10.

	IF (N_ELEMENTS(numor2) EQ 0) THEN numor2=numor1

	OPENW, 2, outfile
	FOR numor=numor1,numor2 DO BEGIN
		IF (numor GE 10000) THEN infile=path+'0'+STRTRIM(STRING(numor),2) $
				ELSE infile=path+'00'+STRTRIM(STRING(numor),2) 
		PRINT,' Reading run',numor
		read_d7sum, infile
		IF (numor EQ numor1) THEN PRINTF, 2, date, 0., 0.
		IF (date-olddate GT 1./24. OR numor EQ numor2) THEN $
			PRINTF, 2, date, mon1rate, dmon1rate
		IF (numor EQ numor2) THEN PRINTF, 2, date, 0., 0.
		olddate=date
	ENDFOR
	PRINTF, 2, 'Date (May 1998)'
	PRINTF, 2, 'Counts/second'
	PRINTF, 2, 'Monitor 1 rate'
	CLOSE, 2

	RETURN
	END
