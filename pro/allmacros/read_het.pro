
;-----------------------------------------------------------------------------
;*****************************************************************************

	FUNCTION read_het, inst, path, filename, status, datp

; Reads in HET data.  Requires LAMP running on Windows, and ISIS_GI.DLL and
; ISIS_GI.DLM in the '..rsi\idl\bin\bin.x86' directory.

;						JRS  27/3/02
;
;	w1(nchan,nspec) is counts, normalised to channel bin width
;	n1(nchan,0) = M1 counts/mus
;	n1(nchan,1) = M1 errors
;	n1(nchan,2) = M2 counts/mus
;	n1(nchan,3) = M2 errors
;	n1(nchan,4) = M3 counts/mus
;	n1(nchan,5) = M3 errors
;	n1(nchan,6) = M4 counts/mus
;	n1(nchan,7) = M4 errors
;	n1(nchan,8) = Lineup Monitor counts/mus
;	n1(nchan,9) = Lineup Monitor errors
;	n1(nchan,10) = time channel widths
;	x1(nchan) = time of flight
;	y1(nchan) = 2 theta
;	z1(nspec) = L2 for each detector
;
;-----------------------------------------------------------------------------
;*****************************************************************************

	iprint = 0		;turn on debugging messages

; Make OpenGenie friendly path variable
	a = STR_SEP(path,'\')
	b = SIZE(a)
	cpath = a(0)
	FOR j = 1, b(1)-1 DO cpath = cpath+'\\'+a(j)
	cpath = STRTRIM(cpath,2)

; Check data file exists

	in_file = 'HET'+STRMID(filename,1)+'.RAW'
	j = FINDFILE(path+in_file,COUNT=found)
	IF (found EQ 0) THEN BEGIN
		PRINT, 'File: '+path+in_file+' not found'
		GOTO, finished
	ENDIF ELSE BEGIN
		PRINT, 'Reading run ',LONG(filename)
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'Read_het: finished checking data'
;-----------------------------------------------------------------------------
;*****************************************************************************
; OpenGenie read in routines using ISIS_GI.DLM

	i = GI_activate_session('GENIE', iprint, iprint)
	i = GI_select_source(cpath+in_file)
	i = GI_get('h_HDR',  'HDR',  0)
	i = GI_get('h_NSP1', 'NSP1', 0)
	i = GI_get('h_NTC1', 'NTC1', 0)
	i = GI_get('h_RUN',  'RUN',  0)
	i = GI_get('h_TITL', 'TITL', 0)
	i = GI_get('h_SPEC', 'SPEC', 0)
	i = GI_get('h_TTHE', 'TTHE', 0)
	i = GI_get('h_CNT1', 'CNT1', 0)
	i = GI_get('h_LEN2', 'LEN2', 0)
	i = GI_get('h_NMON', 'NMON', 0)
	i = GI_get('h_MDET', 'MDET', 0)
	i = GI_get('h_NDET', 'NDET', 0)
	i = GI_get('h_IVPB', 'IVPB', 0)
	i = GI_get('h_RPB',  'RPB',  0)
	i = GI_get('h_DATA', '',0)
	i = GI_transfer('h_HDR',  '-->', 'STRING',   infob,  1, [80L])
	i = GI_transfer('h_NSP1', '-->', 'INT32',    nspec, 0, 0)
	i = GI_transfer('h_NTC1', '-->', 'INT32',    nchan, 0, 0)
	i = GI_transfer('h_RUN',  '-->', 'INT32',    numor, 0, 0)
	i = GI_transfer('h_TITL', '-->', 'STRING',   title, 1, [80L])
	i = GI_transfer('h_NDET', '-->', 'INT32',    ndet, 0, 0)
	i = GI_transfer('h_SPEC', '-->', 'INT32[]',  specarr, 1, [ndet])
	i = GI_transfer('h_TTHE', '-->', 'FLOAT32[]',thetarr, 1, [ndet])
	i = GI_transfer('h_CNT1', '-->', 'INT32[]',  cnt, 1, [(nspec+1)*(nchan+1)])
	i = GI_transfer('h_LEN2', '-->', 'FLOAT32[]',l2arr, 1, [ndet])
	i = GI_transfer('h_NMON', '-->', 'INT32',    nmon, 0, 0)
	i = GI_transfer('h_MDET', '-->', 'INT32[]',  mdet, 1, [nmon])
	i = GI_transfer('h_IVPB', '-->', 'INT32[]',  ivpb, 1, [64])
	i = GI_transfer('h_RPB',  '-->', 'INT32[]',  rpb, 1, [32])
	i = GI_transfer('h_DATA.X','-->','FLOAT64[]',xvals,1, [nchan+1])

	IF (iprint GT 0) THEN PRINT, 'Read_HET: finished reading data'
;------------------------------------------------------------------------------
;******************************************************************************
; Data Manipulations

	IF (iprint GT 0) THEN PRINT,'Read_HET: Arranging workspace...'
	w_out = REFORM(cnt,nchan+1,nspec+1)
	w_out = FLOAT(w_out(1:*,1:*)) ;remove first time channel and spectrum 0 (unassigned data)
	e_out = SQRT(w_out)

	IF (iprint GT 0) THEN BEGIN
		PRINT, 'Nspec:  ',nspec
		PRINT, 'Ndet:   ',ndet
		PRINT, 'Nchan:  ',nchan
	ENDIF

; assign spectra indexes

	IF (iprint GT 0) THEN PRINT,'Read_HET: Assigning 2theta and L2...'
	y_out = FLTARR(nspec)
	z_out = FLTARR(nspec)
	y_out(specarr-1) = thetarr		;2 theta indexes
	z_out(specarr-1) = l2arr		;L2 indexes

; normalise data to channel widths

	IF (iprint GT 0) THEN PRINT,'Read_HET: Normalising to channel widths..'
	i = INDGEN(nchan) + 1
	tbin = xvals(i) - xvals(i-1)		;time channel width array
	FOR i=0,nspec-1 DO BEGIN
		w_out(*,i) = w_out(*,i)/tbin
		e_out(*,i) = e_out(*,i)/tbin
	ENDFOR

; prepare monitor array

	IF (iprint GT 0) THEN PRINT,'Read_HET: Assigning monitors...'
	m_ind = specarr(mdet-1)			;monitor indexes
	sm = SIZE(m_ind)
	lineupm_ind = m_ind(sm(1)-1) + 1
	m_ind = [m_ind,lineupm_ind]		;monitor & lineup indexes
	m = w_out[*,m_ind-1]			;monitors 1 to 3
	em = e_out[*,m_ind-1]			;errors
	narr = [[m(*,0)],[em(*,0)]]
	sm = SIZE(m)
	FOR i = 1,sm(2)-1 DO narr = [[narr],[m(*,i)],[em(*,i)]]
	narr = [[narr],[tbin]]

; exclude monitors in data array

	IF (iprint GT 0) THEN PRINT,'Read_HET: Assigning data...'
	sw = SIZE(w_out)
	w_ind = REPLICATE(1L, sw(2))
	w_ind(m_ind-1) = 0L
	i = WHERE(w_ind EQ 1)
	tmp = w_out[*,i] & w_out = tmp
	tmp = e_out[*,i] & e_out = tmp
	z_out = z_out[i]
	y_out = y_out[i]

	IF (iprint GT 0) THEN PRINT, 'Read_HET: finished data manipulation'

;-----------------------------------------------------------------------------
;*****************************************************************************
;parameters

	p = FLTARR(31)
	p_txt = STRARR(31)

	numaps = FLOAT(STRMID(infob,72,8))

	p(0) = rpb(0)		& p_txt(0) =  '0)  Run duration (s)           ='
	p(1) = rpb(0)*10.0	& p_txt(1) =  '1)  Run duration (0.1s)        ='
	p(2) = nchan		& p_txt(2) =  '2)  Number of time channels    ='
	p(3) = rpb(13)		& p_txt(3) =  '3)  Counts in M1               ='
	p(4) = rpb(14)		& p_txt(4) =  '4)  Counts in M2               ='
	p(5) = rpb(15)		& p_txt(5) =  '5)  Counts in M3               ='
	p(6) = numaps		& p_txt(6) =  '6)  muAmp Hours                ='
	p(7) = ivpb(21)		& p_txt(7) =  '7)  Detector vaccumm (0/1)     ='
	p(8) = 5146.83		& p_txt(8) =  '8)  Elastic t-o-f   (4m bank)  ='
	p(9) = 4647.61		& p_txt(9) =  '9)  Elastic t-o-f (2.5m bank)  ='
	p(10) = numor		& p_txt(10) = '10) Numor                      ='
	p(11) = 0.0		& p_txt(11) = '11) Sample Temperature (K)     ='
	p(12) = ivpb(23)	& p_txt(12) = '12) Chopper frequency (×50)    ='
	p(13) = xvals(nchan-1)	& p_txt(13) = '13) Repetition period (musecs) ='
	p(14) = ivpb(26)	& p_txt(14) = '14) Slit package               ='
	p(15) = ivpb(15)	& p_txt(15) = '15) Main shutter (0/1)         ='
	p(16) = ivpb(16)	& p_txt(16) = '16) Thermal shutter (0/1)      ='
	p(17) = ivpb(20)	& p_txt(17) = '17) Moderator type             ='
	p(18) = 0.0		& p_txt(18) = '18) Channel width is variable, contained in n1(*,10)         '
	p(19) = nchan		& p_txt(19) = '19) No. of channels used       ='
	p(20) = 0.0		& p_txt(20) = '20) TOF delay variable                                       '
	p(21) = 0.0		& p_txt(21) = '21) Incident Energy            ='
	p(22) = 0.0		& p_txt(22) = '22) Not used                   ='
	p(23) = 0.0		& p_txt(23) = '23) Not used                   ='
	p(24) = 0.0		& p_txt(24) = '24) Not used                   ='
	p(25) = 0.0		& p_txt(25) = '25) Not used                   ='
	p(26) = 0.0		& p_txt(26) = '26) Not used                   ='
	p(27) = 0.0		& p_txt(27) = '27) L2 is variable, contained in z1                          '
	p(28) = 0.0		& p_txt(28) = '28) Not used                   ='
	p(29) = 0.0		& p_txt(29) = '29) Not used                   ='
	p(30) = ndet		& p_txt(30) = '30) Number of detectors        ='
;-----------------------------------------------------------------------------
;*****************************************************************************
;Return data and parameters

	STATUS = 0
	DATA = w_out
	other_tit='HET: #'+STRTRIM(STRING(numor),2)+STRMID(infob,52,12)
	datp = {X:xvals(1:*), $
		Y:y_out, $
		Z:z_out, $
		E:e_out, $
		P:p, $
		PAR_TXT:p_txt, $
		W_TIT:title,N:narr, $
		X_TIT:'Time of flight (!4m!3s)', $
		Y_TIT:'Detector Angle (degrees)',$
		OTHER_TIT:other_tit}
	IF (iprint GT 0) THEN PRINT,'Read_HET: Returning data...'
	RETURN, DATA

finished:
	IF(iprint GT 0) THEN PRINT, 'Read_HET: finished'

	END
