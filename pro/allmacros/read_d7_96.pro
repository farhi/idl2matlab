;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO read_d7_96, cyc, numor, w_out1

; Reads D7 data written using incorrect formatting during cycles
; 962-695, and less than numor #9583
; user must input cycle, numor and workspace (eg. read_d7_96, 965, 9502, w1) 
;
;							JRS 4/5/00
;-------------------------------------------------------------------------------
;*******************************************************************************

@lamp.cbk

	iprint=0

	TAKE_W, w_out1, W=inw

	datasource=0	; 0 => take data from appropriate counters
			; 1 => take data from hytec counters always
			; 2 => take data from tof counters always

	IF (iprint GT 0) THEN PRINT,'read_d7_96: starting'
	
	ON_IOERROR, error_message
	GET_LUN, ilun

	IF (cyc LT 962 OR cyc GT 965) THEN GOTO, error2
	IF (cyc EQ 965 AND numor GT 9583) THEN GOTO, error2	

	pthv='/usr/illdata/'+STRTRIM(STRING(cyc),2)+'/d7/'
	filename='00'+STRTRIM(STRING(numor),2)
	form=findfile(pthv+FILENAME+'.Z',count=cprs)
	IF cprs GT 0  THEN BEGIN
		IF pthv NE '' THEN bid=sys_dep('COPY',filename+'.Z',pthv) ELSE cprs=0
		bid=sys_dep('UN_Z',filename+'.Z')       &   pthv=''
	ENDIF
	file_found=pthv+filename

	OPENR, ilun, file_found

	line=''	& num=LONG(0)	& num1=0	& num2=0

	READF, ilun, line
	READF, ilun, num	& numor=num

	PRINT,'Reading run '+STRTRIM(STRING(numor),2)+' into w'+STRTRIM(STRING(inw),2)

	READF, ilun, line
	READF, ilun, line
	line=line+' -1'		& READS, line, num1, num2

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
	READF, ilun, line
	READF, ilun, num1, num2
	FOR i=1,num2 DO READF, ilun, line
	nsp_hytec=num1
	nph_hytec=num1/64
	c_hytec=FLTARR(nsp_hytec)
	READF, ilun, c_hytec
	IF (iprint GT 0) THEN BEGIN
		PRINT,'nsp_hytec=',nsp_hytec
		PRINT,'nph_hytec=',nph_hytec
		PRINT,'c_hytec read OK'
	ENDIF
;------------------------------------
	READF, ilun, line
	READF, ilun, num1, num2, num3, num4

	nspectra=64
	IF (num3 EQ 1) THEN TOFformat=0 ELSE TOFformat=1

	IF (NOT TOFformat AND datasource EQ 0) THEN BEGIN	; no TOF
		IF (iprint GT 0) THEN PRINT,'non-TOF data'
		TOF=0
		READF, ilun, line
		READF, ilun, num1
		nsp_tof=num1
		nph_tof=num1/nspectra
		nchannels=1
		c_tof=LONARR(nsp_tof)
		READF, ilun, c_tof
	ENDIF ELSE IF (datasource EQ 1) THEN TOF=0 $
	ELSE BEGIN	; with TOF
		IF (iprint GT 0) THEN PRINT,'TOF data'

		nspectra=64

		TOF=1
		nsp_tof=num3
		nph_tof=num3/nspectra
		nchannels=FIX(block1(2))
		c_tof=LONARR(nchannels,nsp_tof)
		temp=LONARR(nchannels)
		IF (iprint GT 0) THEN PRINT,'nsp=',nsp_tof,' nph=',nph_tof,' nchannels=',nchannels
		FOR ispec=0,nsp_tof-1 DO BEGIN
			IF (iprint GT 0) THEN PRINT,'ispec=',ispec
			READF, ilun, line
			READF, ilun, num1
			IF (num1 NE nchannels) THEN PRINT,'Error 1: ispec=',ispec,' num1=',num1 $
			ELSE BEGIN
			  READF, ilun, temp
			  c_tof(*,ispec)=temp
			  IF (ispec NE nsp_tof-1) THEN BEGIN
			    READF, ilun, line
			    READF, ilun, line
			  ENDIF
			ENDELSE
		ENDFOR
	ENDELSE
	IF (iprint GT 0) THEN PRINT,'Data read OK from ', nspectra, 'detectors'

;------------------------------------

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
	n_buf(*,0)=0.	; & n_buf(0,0)=n_buf(0,1)/100.	; Z up-up time in secs
	IF (iprint GT 0) THEN PRINT,'monitor assigned OK'
	IF (n_buf(0,2) EQ 0.) THEN BEGIN
		ratioZ=0.	& dratioZ=0.
		ratioX=0.	& dratioX=0.
		ratioY=0.	& dratioY=0.
	ENDIF ELSE IF (n_buf(0,3) EQ 0.) THEN BEGIN
		IF (n_buf(2,2) EQ 0.) THEN BEGIN
			ratioZ=999. & dratioZ=0.
		ENDIF ELSE BEGIN
			m1_0=n_buf(1,1)	& m1_1=n_buf(1,2)
			m2_0=n_buf(2,1)	& m2_1=n_buf(2,2)
			nonflip=m2_0/m1_0	& flip=m2_1/m1_1
			ratioZ=nonflip/flip
			dnonflip=SQRT(m2_0/m1_0^2+m2_0^2/m1_0^3)
			dflip=SQRT(m2_1/m1_1^2+m2_1^2/m1_1^3)
			dratioZ=SQRT((dnonflip/flip)^2+(nonflip*dflip/flip^2)^2)
		ENDELSE
		ratioX=0. & dratioX=0.
		ratioY=0. & dratioY=0.
	ENDIF ELSE BEGIN
		IF (n_buf(2,2) EQ 0.) THEN BEGIN
			ratioZ=999.	& dratioZ=0.
		ENDIF ELSE BEGIN
			m1_0=n_buf(1,1)	& m1_1=n_buf(1,2)
			m2_0=n_buf(2,1)	& m2_1=n_buf(2,2)
			nonflip=m2_0/m1_0	& flip=m2_1/m1_1
			ratioZ=nonflip/flip
			dnonflip=SQRT(m2_0/m1_0^2+m2_0^2/m1_0^3)
			dflip=SQRT(m2_1/m1_1^2+m2_1^2/m1_1^3)
			dratioZ=SQRT((dnonflip/flip)^2+(nonflip*dflip/flip^2)^2)
		ENDELSE
		IF (n_buf(2,4) EQ 0.) THEN BEGIN
			ratioX=999.	& dratioX=0.
		ENDIF ELSE BEGIN
			m1_0=n_buf(1,3)	& m1_1=n_buf(1,4)
			m2_0=n_buf(2,3)	& m2_1=n_buf(2,4)
			nonflip=m2_0/m1_0	& flip=m2_1/m1_1
			ratioX=nonflip/flip
			dnonflip=SQRT(m2_0/m1_0^2+m2_0^2/m1_0^3)
			dflip=SQRT(m2_1/m1_1^2+m2_1^2/m1_1^3)
			dratioX=SQRT((dnonflip/flip)^2+(nonflip*dflip/flip^2)^2)
		ENDELSE
		IF (n_buf(2,6) EQ 0.) THEN BEGIN
			ratioY=999.	& dratioY=0.
		ENDIF ELSE BEGIN
			m1_0=n_buf(1,5)	& m1_1=n_buf(1,6)
			m2_0=n_buf(2,5)	& m2_1=n_buf(2,6)
			nonflip=m2_0/m1_0	& flip=m2_1/m1_1
			ratioY=nonflip/flip
			dnonflip=SQRT(m2_0/m1_0^2+m2_0^2/m1_0^3)
			dflip=SQRT(m2_1/m1_1^2+m2_1^2/m1_1^3)
			dratioY=SQRT((dnonflip/flip)^2+(nonflip*dflip/flip^2)^2)
		ENDELSE
	ENDELSE
	IF (iprint GT 0) THEN PRINT,'Flipping Ratios OK'

	h1=LONG(0)	& m1=LONG(0)	& s1=LONG(0)
	h2=LONG(0)	& m2=LONG(0)	& s2=LONG(0)
	IF (start_time NE '' AND stop_time NE '') THEN BEGIN
		READS, start_time, FORMAT='(I2,8X,I2,1X,I2,1X,I2)', da1, h1, m1, s1
		READS, stop_time, FORMAT='(I2,8X,I2,1X,I2,1X,I2)', da2, h2, m2, s2
		time1=FLOAT(3600*h1+60*m1+s1)/3600.+24.*da1
		time2=FLOAT(3600*h2+60*m2+s2)/3600.+24.*da2
		IF (h2 GE h1) THEN run_time=FLOAT(3600*(h2-h1)+60*(m2-m1)+(s2-s1)) $
			ELSE run_time=FLOAT(3600*(24+h2-h1)+60*(m2-m1)+(s2-s1))
	ENDIF ELSE BEGIN
		time1=0.
		time2=0.
		run_time=0.
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'run time OK'

	nphases=nph_hytec
	TOFfactor=FLTARR(6)	& TOFfactor(*)=1.
	IF (TOF EQ 0) THEN BEGIN
		IF (iprint GT 0) THEN PRINT,'TOF=0'
		IF (datasource EQ 2) THEN w_buf=reform(c_tof,nspectra,nphases) $
			ELSE w_buf=reform(c_hytec,nspectra,nphases)
		x_buf=block1(6:69)
		IF (nphases EQ 1) THEN y_buf=LONG(numor) $
				    ELSE y_buf=INDGEN(nphases)
		z_buf=x_buf
		chel=0.
		norm=1.
	ENDIF ELSE BEGIN
		IF (iprint GT 0) THEN PRINT,'TOF=1'
		w_buf=c_tof
		x_buf=INDGEN(nchannels)+1
		y_buf=FLTARR(nsp_tof)
		FOR iph=0,nph_tof-1 DO y_buf(iph*nspectra:iph*nspectra+63)=block1(6:69)
		z_buf=y_buf
		w_tmp=LONARR(nchannels)	& w_tmp(*)=0
		FOR i=1,nsp_tof,2 DO w_tmp(*)=w_tmp(*)+w_buf(*,i)
		Imax=MAX(w_tmp,ichel)
		chel=FLOAT(ichel)
		FOR iphase=0,nphases-1 DO BEGIN
			sumh=c_hytec(64*iphase)
			sumt=TOTAL(FLOAT(c_tof(*,nspectra*iphase)))
			IF (sumh EQ 0) THEN sumh=sumt
			TOFfactor(iphase)=sumt/sumh
		ENDFOR
		IF (iprint GT 0) THEN PRINT,'sumh=',sumh,' sumt=',sumt
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'scattering angles and chel OK'

	lambda=block1(0)
	IF (lambda GT 3.5) AND (lambda LE 5.0) THEN lambda=4.838 $
	ELSE IF (lambda LT 3.5) THEN lambda=3.02
	IF (lambda GT 5.0) THEN lambda=5.98
	IF (numor GE 8434 AND numor LE 8573) THEN lambda=3.02
	IF (iprint GT 0) THEN PRINT,'lambda=',lambda
	IF (iprint GT 0) THEN PRINT,'npars=',49

	p_buf=FLTARR(49)
	p_buf(0)=numor		& par_txt(inw,0) =' 0 Numor                   ='
	p_buf(1)=nspectra	& par_txt(inw,1) =' 1 Number of spectra       ='
	p_buf(2)=nph_hytec	& par_txt(inw,2) =' 2 Number of phases        ='
	p_buf(3)=1.		& par_txt(inw,3) =' 3 Number of runs          ='
	p_buf(4)=lambda		& par_txt(inw,4) =' 4 Wavelength (A)          ='
	p_buf(5)=block1(1)	& par_txt(inw,5) =' 5 Chopper Speed (rpm)     ='
	p_buf(6)=block1(2)	& par_txt(inw,6) =' 6 Number of Time Channels ='
	p_buf(7)=block1(3)	& par_txt(inw,7) =' 7 Channel Width (mcs)     ='
	p_buf(8)=TOF		& par_txt(inw,8) =' 8 TOF mode (0/1)          ='
	p_buf(9)=chel		& par_txt(inw,9) =' 9 Elastic Peak Channel    ='
	p_buf(10)=block2(0)	& par_txt(inw,10)='10 Set-pt Temp Start       ='
	p_buf(11)=block2(3)	& par_txt(inw,11)='11 Set-pt Temp End         ='
	p_buf(12)=block2(1)	& par_txt(inw,12)='12 Regul Temp Start        ='
	p_buf(13)=block2(4)	& par_txt(inw,13)='13 Regul Temp End          ='
	p_buf(14)=block2(2)	& par_txt(inw,14)='14 Sample Temp Start       ='
	p_buf(15)=block2(5)	& par_txt(inw,15)='15 Sample Temp End         ='
	p_buf(16)=block1(76)	& par_txt(inw,16)='16 Bank1 Position          ='
	p_buf(17)=block1(77)	& par_txt(inw,17)='17 Bank2 Position          ='
	p_buf(18)=block1(78)	& par_txt(inw,18)='18 Bank3 Position          ='
	p_buf(19)=block1(79)	& par_txt(inw,19)='19 Bank4 Position          ='
	p_buf(20)=block1(80)	& par_txt(inw,20)='20 Lower Sample Rotation   ='
	p_buf(21)=block1(82)	& par_txt(inw,21)='21 Upper Sample Rotation   ='
	p_buf(22)=block2(17)	& par_txt(inw,22)='22 Z Flipper Current       ='
	p_buf(23)=block2(18)	& par_txt(inw,23)='23 Z Correction Current    ='
	p_buf(24)=block2(35)	& par_txt(inw,24)='24 X Flipper Current       ='
	p_buf(25)=block2(36)	& par_txt(inw,25)='25 X Correction Current    ='
	p_buf(26)=block2(53)	& par_txt(inw,26)='26 Y Flipper Current       ='
	p_buf(27)=block2(54)	& par_txt(inw,27)='27 Y Correction Current    ='
	p_buf(28)=ratioZ	& par_txt(inw,28)='28 Z Flipping ratio        ='
	p_buf(29)=ratioX	& par_txt(inw,29)='29 X Flipping ratio        ='
	p_buf(30)=ratioY	& par_txt(inw,30)='30 Y Flipping ratio        ='
	p_buf(31)=run_time	& par_txt(inw,31)='31 Run Time (seconds)      ='
	p_buf(32)=time1		& par_txt(inw,32)='32 Start time (hours)      ='
	p_buf(33)=time2		& par_txt(inw,33)='33 End Time (hours)        ='
	p_buf(34)=block1(70)	& par_txt(inw,34)='34 Mono1 (psiA)            ='
	p_buf(35)=block1(71)	& par_txt(inw,35)='35 Mono2 (psiB)            ='
	p_buf(36)=block1(72)	& par_txt(inw,36)='36 Mono3 (psiC)            ='
	p_buf(37)=block1(73)	& par_txt(inw,37)='37 Mono4 (thetaA)          ='
	p_buf(38)=block1(74)	& par_txt(inw,38)='38 Mono5 (thetaB)          ='
	p_buf(39)=block1(75)	& par_txt(inw,39)='39 Mono6 (thetaC)          ='
	p_buf(40)=TOFfactor(0)	& par_txt(inw,40)='40 Z  TOFfactor            ='
	p_buf(41)=TOFfactor(1)	& par_txt(inw,41)='41 Z1 TOFfactor            ='
	p_buf(42)=TOFfactor(2)	& par_txt(inw,42)='42 X  TOFfactor            ='
	p_buf(43)=TOFfactor(3)	& par_txt(inw,43)='43 X1 TOFfactor            ='
	p_buf(44)=TOFfactor(4)	& par_txt(inw,44)='44 Y  TOFfactor            ='
	p_buf(45)=TOFfactor(5)	& par_txt(inw,45)='45 Y1 TOFfactor            ='
	p_buf(46)=dratioZ	& par_txt(inw,46)='46 Z dFlipping ratio       ='
	p_buf(47)=dratioX	& par_txt(inw,47)='47 X dFlipping ratio       ='
	p_buf(48)=dratioY	& par_txt(inw,48)='48 Y dFlipping ratio       ='
	
	IF (iprint GT 0) THEN PRINT,'parameters assigned OK'
	
	sti=STRTRIM(STRING(inw),2)	

	junk= EXECUTE('x'+sti+'=x_buf')
	junk= EXECUTE('y'+sti+'=y_buf')
	junk= EXECUTE('w'+sti+'=w_buf')
	junk= EXECUTE('n'+sti+'=n_buf')
	junk= EXECUTE('p'+sti+'=p_buf')
       	junk= EXECUTE('z'+sti+'=z_buf')


	w_tit    (inw) = strtrim(sub_title,2)
	other_tit(inw) = strtrim(main_title,2)
 
	IF (TOF EQ 0) THEN BEGIN
		x_tit(inw) = 'Scattering Angle'
		IF (nphases EQ 1) THEN y_tit(inw)='Counts' $
		ELSE BEGIN
			y_tit(inw)='Phase'
			z_tit(inw)='Counts'
		ENDELSE
	ENDIF ELSE BEGIN
		x_tit(inw) = 'Time Channels'
		y_tit(inw) = 'Scattering Angle'
		z_tit(inw) = 'Counts'
	ENDELSE

	head_tit(inw,0) = sub_title
	head_tit(inw,1) = main_title
	head_tit(inw,2) = inst_value
	head_tit(inw,3) = STRING(numor)
	head_tit(inw,4) = start_time
	head_tit(inw,5) = ''
	head_tit(inw,6) = x_tit(inw)
	head_tit(inw,7) = y_tit(inw)
	head_tit(inw,8) = z_tit(inw)
	head_tit(inw,9) = ''

	GOTO, end_read_d7_96

error_message:
	PRINT,'Input/Output error encountered in read_d7'
	GOTO, end_read_d7_96

error2:
	PRINT,'You should read this data normally'


end_read_d7_96:
	FREE_LUN, ilun
	bid=sys_dep('DELET',filename)

	IF (iprint GT 0) THEN PRINT,'read_d7_96: finished'

	return
	end
