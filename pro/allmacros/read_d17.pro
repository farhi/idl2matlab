	PRO read_d17
; Reads D17 data. Called from MICS.PRO
;
;						JRS,RC 26/04/00
@lamp.cbk
@mics.cbk

	iprint=0

	IF (iprint GT 0) THEN PRINT,'read_d17: starting'
	
	ON_IOERROR, error_message
	GET_LUN, ilun

	OPENR, ilun, file_found(0)

	line=''	& num=LONG(0)	& num1=0	& num2=0

	READF, ilun, line
	READF, ilun, num	& numor=num

	PRINT,'Reading run',numor

	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	inst       = STRMID(line,0,4)
	user       = STRMID(line,4,12)
	READF, ilun, line
	IF(iprint GT 0) THEN BEGIN
		PRINT, 'inst =',inst
		PRINT, 'user =',user
	ENDIF

;------------------------------------
	READF, ilun, num1
	block0=INTARR(num1)
	READF, ilun, block0
	IF (iprint GT 0) THEN PRINT,'Block0 read OK'

;------------------------------------
	READF, ilun, line
	READF, ilun, num1
	READF, ilun, line
	user=STRMID(line,0,10)
	title=STRMID(line,10,39)
	prop=STRMID(line,49,11)
	subt=STRMID(line,60,20)
	IF (iprint GT 0) THEN BEGIN
		PRINT,'user =',user
		PRINT,'title =',title
		PRINT,'prop =',prop
		PRINT,'subt =',subt
	ENDIF
	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	IF (iprint GT 0) THEN PRINT,'Block1 read OK'
;------------------------------------
	READF, ilun, line
	READF, ilun, num1
	block2=FLTARR(num1)
	READF, ilun, block2
	IF (iprint GT 0) THEN PRINT,'Block2 read OK'

;------------------------------------
	READF, ilun, line
	READF, ilun, num1
	block3=FLTARR(num1)
	READF, ilun, block3
	IF (iprint GT 0) THEN PRINT,'Block3 read OK'

;------------------------------------

	p_buf=FLTARR(npars)

	p_buf(0)=numor 		& par_txt(nwk_select,0) =' 0 Numor ='
	p_buf(1)=block2(94)	& par_txt(nwk_select,1) =' 1 Time Channels ='
	p_buf(2)=block2(97)	& par_txt(nwk_select,2) =' 2 X1 ='
	p_buf(3)=block2(98)	& par_txt(nwk_select,3) =' 3 X2 ='
	p_buf(4)=block2(99)	& par_txt(nwk_select,4) =' 4 Y1 ='
	p_buf(5)=block2(100)	& par_txt(nwk_select,5) =' 5 Y2 ='
	p_buf(6)=block2(95)	& par_txt(nwk_select,6) =' 6 Channel width ='
	p_buf(7)=block2(96)	& par_txt(nwk_select,7) =' 7 Electronic TOF delay ='
	p_buf(8)=block2(101)	& par_txt(nwk_select,8) =' 8 NX ='
	p_buf(9)=block2(102)	& par_txt(nwk_select,9) =' 9 NY ='
	p_buf(10)=block3(40)	& par_txt(nwk_select,10) =' 10 chop 1 speed req ='
	p_buf(11)=block3(41)	& par_txt(nwk_select,11) =' 11 chop 1 phase req ='
	p_buf(12)=block3(42)	& par_txt(nwk_select,12) =' 12 chop 2 speed req ='
	p_buf(13)=block3(43)	& par_txt(nwk_select,13) =' 13 chop 2 phase req ='
	p_buf(14)=block3(44)	& par_txt(nwk_select,14) =' 14 chop 1 speed act ='
	p_buf(15)=block3(45)	& par_txt(nwk_select,15) =' 15 chop 1 phase act ='
	p_buf(16)=block3(46)	& par_txt(nwk_select,16) =' 16 chop 2 speed act ='
	p_buf(17)=block3(47)	& par_txt(nwk_select,17) =' 17 chop 2 phase act ='

	p_buf(18)=45.-(block3(43)-block3(41))	& par_txt(nwk_select,18) =' 18 chop opening req ='
	p_buf(19)=45.-(block3(47)-block3(45))	& par_txt(nwk_select,19) =' 19 chop opening act ='
	p_buf(20)=60./block3(44)		& par_txt(nwk_select,20) =' 20 chop period ='
	p_buf(21)=(285.-p_buf(19))/2.		& par_txt(nwk_select,21) =' 21 chop delay angle ='
	p_buf(22)=(p_buf(21)/360.)*p_buf(20)	& par_txt(nwk_select,22) =' 22 chop delay time ='

	p_buf(23)=block3(15)	& par_txt(nwk_select,23) =' 23 sample-detector distance ='
	p_buf(24)=block3(2)	& par_txt(nwk_select,24) =' 24 sample angle ='
	p_buf(25)=block3(16)	& par_txt(nwk_select,25) =' 25 detector angle ='
	p_buf(26)=block2(2)/10.	& par_txt(nwk_select,26) =' 26 run time ='
	
	IF (iprint GT 0) THEN PRINT,'parameters assigned OK'

;------------------------------------

	READF, ilun, line
	READF, ilun, line
	READF, ilun, line
	READF, ilun, tot

	tsize=long(block2(94))
	xsize=long(block2(98)-block2(97)+1)
	ysize=long(block2(100)-block2(99)+1)

	IF (tot ne ((xsize*ysize*tsize)+tsize)) THEN PRINT,'read_d17: Error in data array dimensions'
	IF (iprint GT 0) THEN BEGIN	
		print,'tsize= ',tsize,' detector size= ',xsize*ysize
		print,'xsize= ',xsize,' ysize= ',ysize,' tot1= ',xsize*ysize*tsize,' tot2= ',tot
	ENDIF

	det=lonarr(ysize,xsize,tsize)
	mon=lonarr(tsize)

	READF, ilun,det,mon

	w_buf=FLTARR(xsize,ysize,tsize)
	FOR i=0,tsize-1 DO w_buf(*,*,i)=ROTATE(det(*,*,i),3) 
	x_buf=INDGEN(xsize)+block2(97)
	y_buf=INDGEN(ysize)+block2(99)
	z_buf=INDGEN(tsize)
	n_buf=mon

	junk= EXECUTE('x'+swk_select+'=x_buf')
	junk= EXECUTE('y'+swk_select+'=y_buf')
	junk= EXECUTE('w'+swk_select+'=w_buf')
	junk= EXECUTE('n'+swk_select+'=n_buf')
	junk= EXECUTE('p'+swk_select+'=p_buf')
       	junk= EXECUTE('z'+swk_select+'=z_buf')


	w_tit    (nwk_select) = strtrim(subt,2)
	other_tit(nwk_select) = 'D17B '+strtrim(user,2)+' '+strtrim(title,2)
 	x_tit    (nwk_select) = 'X pixels'
	y_tit    (nwk_select) = 'Y pixels'
	z_tit    (nwk_select) = 'Time channels'
	
	head_tit(nwk_select,0) = subt
	head_tit(nwk_select,1) = title
	head_tit(nwk_select,2) = 'D17B'
	head_tit(nwk_select,3) = STRING(numor)
	head_tit(nwk_select,4) = ''
	head_tit(nwk_select,5) = ''
	head_tit(nwk_select,6) = x_tit(nwk_select)
	head_tit(nwk_select,7) = y_tit(nwk_select)
	head_tit(nwk_select,8) = z_tit(nwk_select)
	head_tit(nwk_select,9) = ''

	GOTO, end_read_d17

error_message:
	PRINT,'Input/Output error encountered in read_d17'

end_read_d17:
	FREE_LUN, ilun

	IF (iprint GT 0) THEN PRINT,'read_d17: finished'

	return
	end
