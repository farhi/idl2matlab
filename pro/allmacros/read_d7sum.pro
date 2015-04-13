;-------------------------------------------------------------------------------
;*******************************************************************************

	PRO read_d7sum, file

; Reads D7 data. Called from monitor.pro, summary.pro and newsummary.pro
; Assumes that data format is new (i.e. same as September 1996) or format 
; of Mompean experiment (November 1995)
;							KHA, 1/5/98
;-------------------------------------------------------------------------------
;*******************************************************************************


COMMON mon,	date, mon1rate, dmon1rate, time1, time2

COMMON mut,	user, local_contact, main_title, sub_title, $
		start_time, run_time, pol_mode, Time_mode, scan_type, $
		nkmes, npmes

	iprint=0

	IF (iprint GT 0) THEN PRINT,'read_d7: starting'
	
	ON_IOERROR, error_message

	ilun=1
	IF (iprint GT 0) THEN PRINT,'Opening ',file
	OPENR, ilun, file

	line=''	& num=LONG(0)	& num1=0	& num2=0

	READF, ilun, line
	READF, ilun, num	& numor=num

;	PRINT,'Reading run',numor

	IF (numor LE 5000) THEN BEGIN

	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	user=''	& local_contact=''	& main_title=''
	stop_time=''	& scan_type='         '
	nkmes=0	& npmes=0	& run_time=0.
	inst=STRMID(line,0,4)
	sub_title=STRMID(line,4,6)
	start_time=STRMID(line,14,18)
	WHILE (STRMID(line,0,3) NE 'SSS') DO BEGIN
		READF, ilun, line
;		PRINT, line
	ENDWHILE
	READF, ilun, i1, i2, i3, i4
	IF (i2 EQ 0) THEN BEGIN
		Time_mode='NOTOF'
		READF, ilun, line
		READF, ilun, n
		counts=LONARR(n-1)
		READF, ilun, counts
		i=WHERE(counts NE 0,n)
		imax=i(n-1)
		IF (imax LT 80) THEN pol_mode='NOP' $
		ELSE IF (imax LT 160) THEN pol_mode='ZP' $
		ELSE pol_mode='XYZP'
	ENDIF ELSE BEGIN
		Time_mode='TOF'
		IF (i3 EQ 64) THEN pol_mode='NOP' $
		ELSE IF (i3 EQ 128) THEN pol_mode='ZP' $
		ELSE pol_mode='XYZP'
	ENDELSE

	GOTO, end_read_d7

	ENDIF ELSE BEGIN	; Numor > 5000

	READF, ilun, line
	READF, ilun, line
	line=line+' -1'		& READS, line, num1, num2

	IF (num2 LT 0) THEN BEGIN
		PRINT,'Error: Old data format'
		GOTO, end_read_d7
	ENDIF


;     -------------------------------
;     READ HEADER & PARAMETERS BLOCKS
;     -------------------------------
	num3=LONG(0)	& num4=LONG(0)
	IF (numor GE 100 AND numor LE 7152) THEN BEGIN	; data from cycle 962 (Acet)
		IF (iprint GT 0) THEN PRINT,'Data is from cycle 962'
		FOR i=1,num2 DO READF, ilun, line
		READF, ilun, line
		inst       = STRMID(line,0,4)
		READF, ilun, line
		main_title = STRMID(line,0,71)
		READF, ilun, line
		sub_title  = STRMID(line,0,71)
		READF, ilun, line
		user       = STRMID(line,0,8)
		local_contact=STRMID(line,8,8)
		start_time = STRMID(line,16,18)
		stop_time  = STRMID(line,34,18)
		scan_type  = STRMID(line,52,8)
		pol_mode   = STRMID(line,60,5)
		Time_mode  = STRMID(line,65,6)
	ENDIF ELSE IF (numor GE 100 AND numor LE 9365) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'Data is from cycles 963-964'
; control characters written into character block: some text irretrievable
		READF, ilun, line
		inst       = STRMID(line,0,4)
		user       = STRMID(line,4,8)
;		start_time = STRMID(line,14,18)
;		stop_time  = STRMID(line,32,18)
;		scan_type  = STRMID(line,50,8)
;		pol_mode   = STRMID(line,50,6)
;		Time_mode  = STRMID(line,56,5)
		start_time = ''
		stop_time  = ''
		scan_type  = ''
		pol_mode   = ''
		Time_mode  = ''
		FOR i=2,num2 DO READF, ilun, line
		READF, ilun, line
		main_title = STRMID(line,0,71)
		READF, ilun, line
		sub_title  = STRMID(line,0,71)
		READF, ilun, line
;		local_contact=STRMID(line,8,8)
		local_contact=''
		IF (iprint GT 0) THEN BEGIN
			PRINT,'inst=',inst
			PRINT,'user=',user
			PRINT,'start_time=',start_time
			PRINT,'stop_time=',stop_time
			PRINT,'scan_type=',scan_type
			PRINT,'pol_mode=',pol_mode
			PRINT,'Time_mode=',Time_mode
			PRINT,'main_title=',main_title
			PRINT,'sub_title=',sub_title
			PRINT,'local_contact=',local_contact
		ENDIF
	ENDIF ELSE BEGIN
		IF (iprint GT 0) THEN PRINT,'Data is from cycle 965-'
		READF, ilun, line
		inst       = STRMID(line,0,4)
		user       = STRMID(line,4,8)
		save_time  = STRMID(line,14,18)
		READF, ilun, line
		READF, ilun, line
		READS, line, num1, num2
		FOR i=1,num2 DO READF, ilun, line
		READF, ilun, line
		main_title=line
		READF, ilun, line
		sub_title=line
		IF (numor GE 9609 AND numor LE 10318) OR $
		   (numor GE 10327 AND numor LE 11062) THEN $
			READF, ilun, line	; error in data format
		READF, ilun, line
		user       = STRMID(line,0,8)
		local_contact=STRMID(line,8,8)
		READF, ilun, line
		start_time = STRMID(line,0,18)
		stop_time  = STRMID(line,20,18)
		scan_type  = STRMID(line,38,9)
		pol_mode   = STRMID(line,47,5)
		Time_mode  = STRMID(line,52,6)
		IF (iprint GT 0) THEN BEGIN
			PRINT,'start_time=',start_time
			PRINT,'stop_time=',stop_time
			PRINT,'scan_type=',scan_type
			PRINT,'pol_mode=',pol_mode
			PRINT,'Time_mode=',Time_mode
		ENDIF
	ENDELSE

	main_title=STRTRIM(main_title,2)
	sub_title=STRTRIM(sub_title,2)

;------------------------------------
	READF, ilun, line
	READF, ilun, num1, num2
	FOR i=1,num2 DO READF, ilun, line
	block0=INTARR(num1)
	READF, ilun, block0
	IF (iprint GT 0) THEN PRINT,'Block0 read OK'
;------------------------------------
	READF, ilun, line
	READF, ilun, num1, num2
	FOR i=1,num2 DO READF, ilun, line
	block1=FLTARR(num1)
	READF, ilun, block1
	IF (iprint GT 0) THEN PRINT,'Block1 read OK'
;------------------------------------	
	READF, ilun, line
	READF, ilun, num1, num2
	FOR i=1,num2 DO READF, ilun, line
	block2=FLTARR(num1)
	READF, ilun, block2
	IF (iprint GT 0) THEN PRINT,'Block2 read OK'
;------------------------------------

	nkmes=block0(0)
	npmes=block0(1)

	n_buf=FLTARR(4,7)	; first line is zeros except for n(0,0)
	IF (numor LE 7043 AND numor GE 100) THEN BEGIN
		n_buf(0,1)=block2(8)		; Time      Z up-up    
		n_buf(0,2)=block2(12)	 	; Time      Z up-down
		n_buf(0,3)=block2(16)   	; Time      X up-up    
		n_buf(0,4)=block2(20)     	; Time      X up-down
		n_buf(0,5)=block2(24)		; Time      Y up-up 
		n_buf(0,6)=block2(28)		; Time      Y up-down
		n_buf(1,1)=block2(10)    	; Monitor 1 Z up-up
		n_buf(1,2)=block2(14) 		; Monitor 1 Z up-down 
		n_buf(1,3)=block2(18) 		; Monitor 1 X up-up
		n_buf(1,4)=block2(22)		; Monitor 1 X up-down
		n_buf(1,5)=block2(26)		; Monitor 1 Y up-up 
		n_buf(1,6)=block2(30)		; Monitor 1 Y up-down
		n_buf(2,1)=block2(11)    	; Monitor 2 Z up-up
		n_buf(2,2)=block2(15)	 	; Monitor 2 Z up-down 
		n_buf(2,3)=block2(19) 		; Monitor 2 X up-up
		n_buf(2,4)=block2(23)		; Monitor 2 X up-down
		n_buf(2,5)=block2(27)		; Monitor 2 Y up-up 
		n_buf(2,6)=block2(31)		; Monitor 2 Y up-down
		n_buf(3,1)=block2(9)		; Handshake Z up-up
		n_buf(3,2)=block2(13)		; Handshake Z up-down
		n_buf(3,3)=block2(17)		; Handshake X up-up
		n_buf(3,4)=block2(21)		; Handshake X up-down
		n_buf(3,5)=block2(25)		; Handshake Y up-up
		n_buf(3,6)=block2(29)		; Handshake Y up-down
	ENDIF ELSE BEGIN
		n_buf(0,1)=block2(13)		; Time      Z up-up    
		n_buf(0,2)=block2(22)	 	; Time      Z up-down
		n_buf(0,3)=block2(31)   	; Time      X up-up    
		n_buf(0,4)=block2(40)     	; Time      X up-down
		n_buf(0,5)=block2(49)		; Time      Y up-up 
		n_buf(0,6)=block2(58)		; Time      Y up-down
		n_buf(1,1)=block2(15)    	; Monitor 1 Z up-up
		n_buf(1,2)=block2(24) 		; Monitor 1 Z up-down 
		n_buf(1,3)=block2(33) 		; Monitor 1 X up-up
		n_buf(1,4)=block2(42)		; Monitor 1 X up-down
		n_buf(1,5)=block2(51)		; Monitor 1 Y up-up 
		n_buf(1,6)=block2(60)		; Monitor 1 Y up-down
		n_buf(2,1)=block2(16)    	; Monitor 2 Z up-up
		n_buf(2,2)=block2(25)	 	; Monitor 2 Z up-down 
		n_buf(2,3)=block2(34) 		; Monitor 2 X up-up
		n_buf(2,4)=block2(43)		; Monitor 2 X up-down
		n_buf(2,5)=block2(52)		; Monitor 2 Y up-up 
		n_buf(2,6)=block2(61)		; Monitor 2 Y up-down
		n_buf(3,1)=block2(14)		; Handshake Z up-up
		n_buf(3,2)=block2(23)		; Handshake Z up-down
		n_buf(3,3)=block2(32)		; Handshake X up-up
		n_buf(3,4)=block2(41)		; Handshake X up-down
		n_buf(3,5)=block2(50)		; Handshake Y up-up
		n_buf(3,6)=block2(59)		; Handshake Y up-down
	ENDELSE
	time=n_buf(0,1)	; time Z up-up
	mon1=n_buf(1,1)	; mon1 Z up-up
	mon1rate=mon1*100./time
	dmon1rate=SQRT(mon1)*100./time

	h1=LONG(0)	& m1=LONG(0)	& s1=LONG(0)
	h2=LONG(0)	& m2=LONG(0)	& s2=LONG(0)
	IF (start_time NE '' AND stop_time NE '') THEN BEGIN
		READS, start_time, FORMAT='(I2,8X,I2,1X,I2,1X,I2)', d1, h1, m1, s1
		READS, stop_time, FORMAT='(I2,8X,I2,1X,I2,1X,I2)', d2, h2, m2, s2
		time1=FLOAT(3600*h1+60*m1+s1)/3600.+24.*d1
		time2=FLOAT(3600*h2+60*m2+s2)/3600.+24.*d2
		IF (h2 GE h1) THEN run_time=FLOAT(3600*(h2-h1)+60*(m2-m1)+(s2-s1)) $
			ELSE run_time=FLOAT(3600*(24+h2-h1)+60*(m2-m1)+(s2-s1))
	ENDIF ELSE BEGIN
		time1=0.
		time2=0.
		run_time=0.
	ENDELSE
	date=(time1+time2)/48.

	IF (iprint GT 0) THEN PRINT,'run time OK'

	GOTO, end_read_d7

	ENDELSE

error_message:
	PRINT,'Input/Output error encountered in read_d7 while reading from ',file

end_read_d7:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'read_d7: finished'

	return
	end
