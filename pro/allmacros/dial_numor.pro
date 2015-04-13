	PRO dial_numor_macro, D
;** ****************
;**

	saverun=D.par(0)
	lastnosaverun=D.par(1)

	D.type='log'	& V=DialNewValue()

	D.type='flagus'	& status=DialNewValue()
	IF (status NE 1 AND status NE 8) THEN RETURN	; not counting or updating

	numor=0
	saverun=-1
	found0=STRPOS(V,'Start run    NUMOR=')
	IF (found0 NE -1) THEN saverun=1
	found1=STRPOS(V,'Start run    (no save)')
	IF (found1 NE -1) THEN saverun=0
	IF (saverun EQ 1) THEN numor=LONG(STRMID(V,found0+19,7)) $
	ELSE IF (saverun EQ 0) THEN numor=lastnosaverun+1

	found2=STRPOS(V,'Temporary NUMOR')
	IF (found2 NE -1) THEN lastnosaverun=LONG(STRMID(V,found2+15,7))

	D.par(0)=saverun
	D.par(1)=lastnosaverun

	IF (numor NE 0) THEN D.value=numor

	PRINT,'numor=',D.value,' saverun=',D.par(0),' lastnosaverun=',D.par(1)

	RETURN
	END

	FUNCTION dial_numor
;******* ***********
;**
	RETURN, {NAME:'numor', GENERIC:'mad', TYPE:'log', VALUE:0L, $
		FREQUENCY:1.0, PLOT:-2, PAR:LONARR(10)}
	END
