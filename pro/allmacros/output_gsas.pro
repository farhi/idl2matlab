;-------------------------------------------------------------------------------
;*******************************************************************************

	PRO output_gsas, w_in, file = out_file
	
;takes a 1-d workspace and writes it to a gsas format data file. 
;Since gsas requires constant angle steps, binq.pro should be run on the data
;first.  
;
;COMMAND SYNTAX
; output_gsas, w10, file=<filename> 
;							JRS 24/11/01
;-------------------------------------------------------------------------------
;*******************************************************************************
	
	iprint=1	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start output_gsas:'

	take_datp, datp	

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	x_in=datp.x
	y_in=datp.y
	z_in=datp.z
	e_in=datp.e

	sw=SIZE(w_in)
		
	IF (N_ELEMENTS(out_file) EQ 0) THEN BEGIN
		PRINT,'output: Error - output file name must be specified'
		GOTO, finished
	ENDIF

	se=SIZE(e_in)
	FOR i=0,se(0) DO IF (se(i) NE sw(i)) THEN e_in=w_in*0.

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open output file and arrange data and headers

	IF (iprint GT 0) THEN PRINT,'Opening output file'
	ON_IOERROR, close_file

	OPENW, 1, out_file+'.gss', ERROR=err	& IF (err NE 0) THEN PRINT, !ERR_STRING
	title = datp.w_tit

	IF (N_ELEMENTS(datp.p) GT 1) THEN BEGIN
		IF (datp.p(4) EQ 3.02) THEN ibank = 1 ELSE ibank = 2
	ENDIF ELSE BEGIN
		ibank = 2
	ENDELSE

	nchan = sw(1)
	bintyp = ' CONST      '
	tmin = x_in(0)
	step = x_in(1) - x_in(0)
	FOR i=0,nchan-2 DO BEGIN
		IF(ABS(x_in(i+1) - x_in(i) - step) GT 0.01) THEN BEGIN
			gap = x_in(i+1) - x_in(i)
			PRINT, 'Gap found between, ',x_in(i),' and ',x_in(i+1)
			npsg = ROUND((gap/step) - 1)
			gappar = (INDGEN(npsg)+1)*step + x_in(i)
			warr = FLTARR(npsg)
			IF(iprint GT 0) THEN BEGIN
				PRINT,'gap =',gap
				PRINT,'npsg =',npsg
				PRINT,'gappar =',gappar
				PRINT,'warr =',warr
			ENDIF
			x_buf = [x_in(0:i),gappar,x_in((i+1):nchan-1)]
			w_buf = [w_in(0:i),warr,w_in((i+1):nchan-1)]
			e_buf = [e_in(0:i),warr,e_in((i+1):nchan-1)]
			nchan = nchan + npsg
			x_in = x_buf
			w_in = w_buf
			e_in = e_buf
		ENDIF
	ENDFOR

	nrec = CEIL(2.*FLOAT(nchan)/10.)

	IF (iprint GT 0) THEN BEGIN
		PRINT, 'nchan = ',nchan
		PRINT, 'nrec  = ',nrec
		PRINT, 'tmin  = ',tmin
		PRINT, 'step  = ',step
	ENDIF

	a = [[w_in],[e_in]]
	b = ROTATE(a,4)
	w_buf = REFORM(b,2*nchan)
	rem = 10*nrec - 2*nchan

	IF (rem NE 0) THEN BEGIN
		addon = FLTARR(rem)
		w_buf2 = [w_buf,addon]
		w_buf1 = REFORM(w_buf2, 10, nrec)
	ENDIF ELSE BEGIN
		w_buf1 = REFORM(w_buf,10,nrec) 
	ENDELSE

;------------------------------------------------------------------------------
;Write gsas file
	
	PRINTF, 1, title
	PRINTF, 1, FORMAT = '("BANK", I2, I6, I5, A, 4F7.1, A)', $
	ibank, nchan, nrec, bintyp, tmin*100., step*100., 0.0, 0.0, ' ESD'
	PRINTF, 1, FORMAT = '(10F8.4)', w_buf1
	PRINT,'output_gsas: data written to - '+out_file+'.gss'

;------------------------------------------------------------------------------
;Return and exit

close_file:
	CLOSE, 1

finished:
	RETURN
	END
	
