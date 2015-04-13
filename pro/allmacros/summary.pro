;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO summary, numor1, numor2

; Assume that data format is new (i.e. same as September 1996)

;							KHA,JRS 25/8/00
;-------------------------------------------------------------------------------
;*******************************************************************************

@read_d7.cbk

	compressed=0
;	inpath='/usr/illdata/973/d7/'
	inpath='/hosts/d7/users/data/'
;	inpath='/usr/illdata/data-1/d7/'


	outpath='~lambda/SUMMARY/'

	IF (N_ELEMENTS(numor2) EQ 0) THEN numor2=numor1

	scan=0	& samerun=0
	olduser=''	& old_title=''	& oldtime1=-10.	& old_run_time=0
	old_pol_mode=''	& old_time_mode=''	& old_sub_title=''
	force=0

	num1=LONG(numor1)
        num2=LONG(numor2)

	FOR numor=num1, num2 DO BEGIN
		IF (numor GE 10000) THEN file='0'+STRTRIM(STRING(numor),2) $
			ELSE file='00'+STRTRIM(STRING(numor),2)
		IF (compressed EQ 1) THEN BEGIN
			command='cp '+inpath+file+'.Z '+outpath
			SPAWN, command
			command='uncompress '+outpath+file+'.Z'
			SPAWN, command
			file=outpath+file
		ENDIF ELSE file=inpath+file
		read_d7sum, file
;		print, run_time
		IF (compressed EQ 1) THEN BEGIN
			command='rm -f '+file
			SPAWN, command
		ENDIF
		run_time=LONG(run_time/60.)	; get it in minutes
		IF (scan EQ 1 AND ((scan_type NE '         ' AND npmes EQ 1) OR $
			(scan_type EQ '         '))) THEN BEGIN
			IF (numor-oldnumor GE 2) THEN PRINT, $
				FORMAT='("     ... ",A," scan up to #",I5)', STRTRIM(oldscan_type,2), numor-1
			scan=0
			samerun=0
		ENDIF ELSE IF (scan_type EQ '         ') THEN BEGIN
			IF (run_time EQ old_run_time AND pol_mode EQ old_pol_mode AND $
				time_mode EQ old_time_mode AND sub_title EQ old_sub_title AND $
				user EQ olduser AND main_title EQ old_title) THEN BEGIN
				samerun=1
			ENDIF ELSE IF (samerun EQ 1) THEN BEGIN
				IF (numor-oldnumor GE 2) THEN PRINT, $
					FORMAT='("     ... identical runs up to #",I5)', numor-1
				samerun=0
			ENDIF
		ENDIF
		IF (numor EQ numor1) THEN BEGIN
			PRINT
			PRINT,'****************************************************************'
			PRINT, FORMAT='("Numors",I6," to",I6)', numor1, numor2
			PRINT, 'Path: ',inpath
			PRINT,'----------------------------------------------------------------'
			PRINT
			PRINT, FORMAT='("User: ",A8,"   Local Contact: ",A8)', user, local_contact
			PRINT, FORMAT='("Experiment Title: ",A)', main_title
			PRINT
			PRINT, 'Numor  Start Time      Minutes Pol TOF   Title'
		ENDIF ELSE IF (user NE olduser) OR (main_title NE old_title) THEN BEGIN
			PRINT,'----------------------------------------------------------------'
			PRINT
			PRINT, FORMAT='("User: ",A8,"   Local Contact: ",A8)', user, local_contact
			PRINT, FORMAT='("Experiment Title: ",A)', main_title
			PRINT
			PRINT, 'Numor  Start Time      Minutes Pol TOF   Title'
		ENDIF
		IF (scan EQ 0 AND samerun EQ 0) THEN BEGIN
			PRINT, FORMAT='(I5,1X,A18,I5,A5,A6,1X,A)', $
				numor, start_time, run_time, pol_mode, Time_mode, sub_title
			oldnumor=numor
			oldscan_type=scan_type
		ENDIF ELSE IF (numor EQ numor2) THEN BEGIN
			IF (scan_type NE '         ' AND npmes NE 1) THEN $
				PRINT, FORMAT='("     ... ",A," scan up to #",I5)', $
					STRTRIM(scan_type,2), numor
			IF (scan_type EQ '         ' AND $
				run_time EQ old_run_time AND pol_mode EQ old_pol_mode AND $
				time_mode EQ old_time_mode AND sub_title EQ old_sub_title AND $
				user EQ olduser AND main_title EQ old_title) THEN PRINT, $
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

	RETURN
	END
