	PRO d7_runsummary

COMMON mon,	date, mon1rate, dmon1rate, time1, time2

COMMON mut,	user, local_contact, main_title, sub_title, $
		start_time, run_time, pol_mode, Time_mode, scan_type, $
		nkmes, npmes

	inpath='/usr/illdata/data/d7/'
	outpath=''

	OPENR, 1, outpath+'ls.out'
	numor0=1L	& numor=0L
	ON_IOERROR, endoffile
	WHILE (numor0 GT 0) DO BEGIN
		READF, 1, FORMAT='(I6)', numor
		IF (numor GT 100 AND numor0 LT 100) THEN numor1=numor
		numor0=numor
	ENDWHILE
endoffile:
	numor2=numor0
	CLOSE, 1
		
	numor1=numor2-((numor2-numor1)<500)

	scan=0	& samerun=0
	olduser=''	& old_title=''	& oldtime1=-10.	& old_run_time=0
	old_pol_mode=''	& old_time_mode=''	& old_sub_title=''
	force=0

	OPENW, 7, outpath+'d7_runsummary.htm'
	PRINTF, 7, '<HTML>'
	PRINTF, 7, '<HEAD>'
	PRINTF, 7, '<TITLE>D7 Run Summary</TITLE>'
	PRINTF, 7, '</HEAD>'
	PRINTF, 7, '<PRE>'

	FOR numor=numor1, numor2 DO BEGIN
		IF (numor GE 10000) THEN file='0'+STRTRIM(STRING(numor),2) $
			ELSE file='00'+STRTRIM(STRING(numor),2)
		file=inpath+file
		read_d7sum, file
		run_time=LONG(run_time/60.)	; get it in minutes
		IF (scan EQ 1 AND ((scan_type NE '         ' AND npmes EQ 1) OR $
			(scan_type EQ '         '))) THEN BEGIN
			IF (numor-oldnumor GE 2) THEN PRINTF, 7, $
				FORMAT='("     ... ",A," scan up to #",I5)', STRTRIM(oldscan_type,2), numor-1
			scan=0
			samerun=0
		ENDIF ELSE IF (scan_type EQ '         ') THEN BEGIN
			IF (run_time EQ old_run_time AND pol_mode EQ old_pol_mode AND $
				time_mode EQ old_time_mode AND sub_title EQ old_sub_title AND $
				user EQ olduser AND main_title EQ old_title) THEN BEGIN
				samerun=1
			ENDIF ELSE IF (samerun EQ 1) THEN BEGIN
				IF (numor-oldnumor GE 2) THEN PRINTF, 7, $
					FORMAT='("     ... identical runs up to #",I5)', numor-1
				samerun=0
			ENDIF
		ENDIF
		IF (numor EQ numor1) THEN BEGIN
			PRINTF, 7
			PRINTF, 7,'****************************************************************'
			PRINTF, 7, FORMAT='("Numors",I6," to",I6)', numor1, numor2
			PRINTF, 7, 'Path: ',inpath
			PRINTF, 7,'----------------------------------------------------------------'
			PRINTF, 7
			PRINTF, 7, FORMAT='("User: ",A8,"   Local Contact: ",A8)', user, local_contact
			PRINTF, 7, FORMAT='("Experiment Title: ",A)', main_title
			PRINTF, 7
			PRINTF, 7, 'Numor  Start Time      Minutes Pol TOF   Title'
		ENDIF ELSE IF (user NE olduser) OR (main_title NE old_title) THEN BEGIN
			PRINTF, 7,'----------------------------------------------------------------'
			PRINTF, 7
			PRINTF, 7, FORMAT='("User: ",A8,"   Local Contact: ",A8)', user, local_contact
			PRINTF, 7, FORMAT='("Experiment Title: ",A)', main_title
			PRINTF, 7
			PRINTF, 7, 'Numor  Start Time      Minutes Pol TOF   Title'
		ENDIF
		IF (scan EQ 0 AND samerun EQ 0) THEN BEGIN
			PRINTF, 7, FORMAT='(I5,1X,A18,I5,A5,A6,1X,A)', $
				numor, start_time, run_time, pol_mode, Time_mode, sub_title
			oldnumor=numor
			oldscan_type=scan_type
		ENDIF ELSE IF (numor EQ numor2) THEN BEGIN
			IF (scan_type NE '         ' AND npmes NE 1) THEN $
				PRINTF, 7, FORMAT='("     ... ",A," scan up to #",I5)', $
					STRTRIM(scan_type,2), numor
			IF (scan_type EQ '         ' AND $
				run_time EQ old_run_time AND pol_mode EQ old_pol_mode AND $
				time_mode EQ old_time_mode AND sub_title EQ old_sub_title AND $
				user EQ olduser AND main_title EQ old_title) THEN PRINTF, 7, $
				FORMAT='("     ... identical runs up to #",I5)', numor
		ENDIF
		IF (scan_type NE '         ') THEN scan=1
		olduser=user
		old_title=main_title
		old_run_time=run_time
		old_pol_mode=pol_mode
		old_time_mode=time_mode
		old_sub_title=sub_title
	ENDFOR

	PRINTF, 7, '</PRE>'
	PRINTF, 7, '</BODY>'
	PRINTF, 7, '</HTML>'
	CLOSE, 7

	RETURN
	END
