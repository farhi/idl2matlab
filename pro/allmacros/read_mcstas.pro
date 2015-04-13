;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION read_mcstas, in_file

;Reads McStas 1d and 2d monitor files and scan files into LAMP
;
;						JRS 22/2/02
;
;-------------------------------------------------------------------------------
;*******************************************************************************
	
	iprint=0	; if iprint>0, show debugging messages
	IF (iprint GT 0) THEN PRINT,'Start input:'
	take_datp, dat

	OPENR, 1, in_file, ERROR=err
	IF (err NE 0) THEN BEGIN
		PRINT, !ERR_STRING
		GOTO, finished
	ENDIF

	PRINT,'Opening McStas file: '+STRTRIM(in_file,2)

	nparams = 0
	param = STRARR(100)
	line=' '

;------------------------------------------------------------------------------
; Read Header block	

	FOR i = 1,100 DO BEGIN
		READF, 1, line
		IF(STRPOS(line,'#') EQ 0) THEN BEGIN
			a = STRPOS(line,':')
			head = STRMID(line,2,a-2)
			linepos = STRMID(line,a+2)
			IF (head EQ 'Instrument-source') THEN inst = linepos
			IF (head EQ 'Date') THEN date = linepos
			IF (head EQ 'Ncount') THEN READS, linepos, N
			IF (head EQ 'Param') THEN BEGIN
				param(nparams) = linepos
				nparams = nparams + 1
			ENDIF
			IF (head EQ 'type') THEN BEGIN
				b = STRPOS(line,'(') & c = STRPOS(line,')')
				IF(STRMID(line, b-2, 2) EQ '1d') THEN BEGIN
					READS, STRMID(line,b+1,c-b+1), xdim
					ydim = 1
				ENDIF ELSE BEGIN
					READS, STRMID(line,b+1,c-b+1), xdim, ydim
				ENDELSE
			ENDIF
			IF (head EQ 'component') THEN comp = linepos
			IF (head EQ 'title') THEN title = linepos
			IF (head EQ 'xlabel') THEN BEGIN
				b = STRPOS(line,"'") & c = RSTRPOS(line,"'")
				dat.x_tit = STRMID(line,b+1,c-b-1)
			ENDIF
			IF (head EQ 'ylabel') THEN BEGIN
				b = STRPOS(line,"'") & c = RSTRPOS(line,"'")
				dat.y_tit = STRMID(line,b+1,c-b-1)
			ENDIF	
			IF (head EQ 'xylimits') THEN READS, STRMID(line,a+2),xmin,xmax,ymin,ymax
			IF (head EQ 'xlimits') THEN READS, STRMID(line,a+2),xmin,xmax
			IF (head EQ 'variables') THEN BEGIN
				parts = STR_SEP(STRMID(line,a+2),' ')
				sp = SIZE(parts)
				keep = WHERE(STRLEN(parts) NE 0)
				parts = parts(keep)
				sp = SIZE(parts)
				IF(N_ELEMENTS(comp) EQ 0) THEN comp = ' '
			ENDIF

;-------------------------------------------------------------------------------
; Read data

		ENDIF ELSE BEGIN
			POINT_LUN, 1, 0
			heads = STRARR(i-1)
			READF, 1, heads
			xdim = FIX(xdim) & ydim = FIX(ydim)
			IF(ydim NE 1) THEN BEGIN
			w_out = DBLARR(xdim,ydim) & e_out = DBLARR(xdim,ydim)
				READF, 1, w_out
				READF, 1, line
				READF, 1, line
				READF, 1, e_out
			ENDIF ELSE BEGIN
				in = DBLARR(sp(1),xdim)
				nscans = (sp(1) - 1)/2
				w_out = DBLARR(xdim,nscans)
				e_out = DBLARR(xdim,nscans)
				w_index = INDGEN(nscans)*2 + 1
				e_index = w_index + 1
				READF, 1, in
				x_out = in(0,*)
				FOR i = 0,nscans-1 DO BEGIN
					w_out(*,i) = in(w_index(i),*)
					e_out(*,i) = in(e_index(i),*)
				ENDFOR
			ENDELSE
			i = WHERE(w_out LT 1e-20) & w_out(i) = 0.0
			i = WHERE(e_out LT 1e-20) & e_out(i) = 0.0
			CLOSE, 1
			GOTO, nomoreheads
		ENDELSE
	ENDFOR

nomoreheads:

	IF(ydim NE 1) THEN x_out = FINDGEN(xdim)*(xmax - xmin)/(xdim - 1) + xmin
	IF(ydim NE 1) THEN y_out = FINDGEN(ydim)*(ymax - ymin)/(ydim - 1) + ymin
	IF(comp NE ' ') THEN dat.w_tit = title + ': ' + comp ELSE dat.w_tit = title
	n_out = N
	par = FLTARR(nparams + 1)
	partxt = STRARR(nparams + 1)
	ot = ' '
	par(0) = N
	partxt(0) = 'Counts='
	FOR i = 0,nparams-1 DO BEGIN
		a = STR_SEP(param(i),'=')
		par(i + 1) = FLOAT(a(1))
		partxt(i + 1) = a(0)
		ot = ot + param(i) + ' '
	ENDFOR
	dat.other_tit = date + ot + ' Inst = ' + inst
	mod_datp, dat, "x", x_out
	mod_datp, dat, "y", y_out
	mod_datp, dat, "e", e_out
	mod_datp, dat, "n", n_out
	mod_datp, dat, "p", par
	mod_datp, dat, "par_txt", partxt
	give_datp, dat

finished:
	RETURN, w_out
	END
	
	
		
						
