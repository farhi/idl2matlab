	PRO d7_tlog, numor1, numor2
;
; interrogates MAD.LOG and MAD.LOG-1 for temperatures.   Plots all temperatures
; for files MAD.LOG and MAD.LOG-1, (default) or plots temperature of desired
; numor range.
;					
;							JRS   20/3/00
;
;------------------------------------------------------------------------------


	IF(N_ELEMENTS(numor2) GT 0) THEN numors=[numor1,numor2] ELSE $
	IF(N_ELEMENTS(numor1) GT 0) THEN numors=[numor1,numor1]
	
	inpath='/usr/illdata/data/d7/'

	outpath=''

	mp=10000
	line=''	& time=FLTARR(mp)	& Treg=time	& Tsam=time
	numor=LONARR(mp/10)	& start=FLTARR(mp/10)	& stop=start
	np=INTARR(mp/10) 	& startr=FLTARR(mp/10)	& numorr=numor
	numor(*)=0.	& start(*)=-24.	& stop(*)=24.
	startr(*)=-24. 	& numorr(*)=0.

	PRINT,'Starting d7_tlog'
	nt=-1
	nr=0
	nrr=0
	fn=0
	inp=0
	today=0
	numorflg=0
	npflag=0
	infile='MAD.LOG-1'
	line0=line
	firstnumor=0 & lastnumor=0
;
;-------------------------------------------------------------------------------
;
readdata:
	OPENR, 1, inpath+infile
	WHILE (NOT EOF(1)) DO BEGIN
		READF, 1, line
		col1=STRMID(line,3,1)	& col2=STRMID(line,6,1)	& pol=STRMID(line,9,4)
		sp=STRMID(line,13,1)
		IF (col1 EQ ':' AND col2 EQ ':' AND sp NE ' ') THEN BEGIN
;			PRINT,'colons found. line=',line
			nt=nt+1
			h=FLOAT(STRMID(line,1,2))	& m=FLOAT(STRMID(line,4,2))	& s=FLOAT(STRMID(line,7,2))
			IF (today EQ 0) THEN BEGIN
				time(nt)=h+m/60.+s/60^2-24.
			ENDIF ELSE BEGIN
				time(nt)=h+m/60.+s/60^2
			ENDELSE
			Treg(nt)=FLOAT(STRMID(line,64,7))	& Tsam(nt)=FLOAT(STRMID(line,73,6))
		ENDIF ELSE IF (STRMID(line,1,6) EQ 'Ntimes') THEN BEGIN
			np(inp)=FIX(STRMID(line,8,3))
			print, inp,np(inp)
			npflag=1
		ENDIF ELSE IF (pol EQ 'POL.' AND npflag EQ 1) THEN BEGIN
;			PRINT,'POL. found. line=',line
			h=FLOAT(STRMID(line0,0,2))	& m=FLOAT(STRMID(line0,3,2))
			IF (today EQ 0) THEN BEGIN
				start(nr)=h+m/60.-24.
				startr(nrr)=h+m/60.-24.
			ENDIF ELSE BEGIN
				start(nr)=h+m/60.
				startr(nrr)=h+m/60.
			ENDELSE
			numorflg=1
		ENDIF ELSE IF(numorflg EQ 1 AND STRMID(line,7,5) EQ 'NUMOR') THEN BEGIN
;			PRINT,'NUMOR found. line=',line
			numor(nr)=LONG(STRMID(line,12,7))
			numorr(nrr)=numor(nr)
			nrr=nrr+1
			IF (fn EQ 0) THEN firstnumor=numor(nr)
			fn=1
			numorflg=0
			npflag=0
		ENDIF ELSE IF (STRMID(line,1,5) EQ 'duree' AND nt NE -1 AND np(inp) EQ 1) THEN BEGIN
;			PRINT,'duree found. line=',line
			inp=inp+1
			nr=nr+1
			stop(nr)=time(nt)
		ENDIF
		line0=line
	ENDWHILE
	lastnumor=numor(nr)

	CLOSE, 1
		PRINT, 'After reading ',infile,': nr=',nr,' nt=',nt
	IF (infile EQ 'MAD.LOG-1') THEN BEGIN
		print, 'MAD.LOG'
		infile='MAD.LOG'
		today=1
		GOTO, readdata
	ENDIF
;
;-------------------------------------------------------------------------------
;
	IF (nr EQ -1 OR nt EQ -1) THEN BEGIN
		OPENW, 1, outpath+'d7_tlog.txt'
		PRINTF, 1, 'No temperature data in logfiles'
		CLOSE, 1
		GOTO, finished
	ENDIF

	stop(nr)=time(nt)
	
	start=start(0:nr)	& stop=stop(0:nr)	& numor=numor(0:nr)
	time=time(0:nt)		& Treg=Treg(0:nt)	& Tsam=Tsam(0:nt)
	startr=startr(0:nrr)

	xmax=time(nt-1)+1.	& xmin=-24.
	ymax=MAX([Treg,Tsam])+10.
	ymin=MIN([Treg,Tsam])-10.
	IF (N_ELEMENTS(numors) GT 0) THEN BEGIN
		IF(N_ELEMENTS(numors) EQ 1) THEN numors=[numors,numors]
		i=WHERE(numorr EQ numors(0))
		j=WHERE(numorr EQ numors(1))
		xmin=startr(i(0))
		xmax=startr(j(0)+1)
		numor=numorr
		start=startr
		stop=start
		firstnumor=numors(0)
		lastnumor=numors(1)
		FOR i=0,nrr-1 DO stop(i)=start(i+1)
		nr=nrr
	ENDIF


;	DEVICE,PSEUDO=8
;	WINDOW,1,XSIZE=1024,YSIZE=512,/PIXMAP,RETAIN=2 ; obsolate (barns)
;	WINDOW,1,XSIZE=1024,YSIZE=512,RETAIN=2
;	WSET,1
	SET_PLOT,'Z'  &  DEVICE,set_resolution=[1024,512]
	LOADCT, 5
	PLOT, [0,0], [0,0], XRANGE=[xmin,xmax], XTITLE='Time (hours) -0 hours is midnight', YTITLE='Sample Temperature (K)', $
		XSTYLE=1, YRANGE=[ymin,ymax], YSTYLE=1, COLOR=0,BACKGROUND=255,TITLE='Temperature Log for '+STRTRIM(STRING(firstnumor),2)+':'+STRTRIM(STRING(lastnumor),2)
	FOR i=0,nr-1 DO BEGIN
		s=FLTARR(2,4)
		s0=start(i)	& s1=stop(i)
;		IF (s1 GT xmin(0)) THEN BEGIN
			s0=MAX([xmin(0),s0])  
			s(0,0)=s0 & s(0,1)=s1 & s(0,2)=s1   & s(0,3)=s0
			s(1,0)=0. & s(1,1)=0. & s(1,2)=ymax & s(1,3)=ymax
;			POLYFILL, s, COLOR=50
			IF (s0 GT xmin AND s0 LT xmax) THEN OPLOT, [s0,s0], [0,ymax], COLOR=40
;		ENDIF
	ENDFOR
	nmax=25
	dy=(ymax-ymin-5.)/FLOAT(nmax)
	FOR i=0,nr-1 DO BEGIN
		yval=dy*FLOAT(i+1-nmax*(i/nmax))+ymin
		IF(firstnumor NE numor(0) AND i EQ 0 AND N_ELEMENTS(numors) LT 1) THEN XYOUTS, xmin, yval, ' #'+STRTRIM(STRING(firstnumor),2),COLOR=50
		IF (start(i) GE xmin AND start(i) LT xmax ) THEN XYOUTS, start(i), yval, ' #'+STRTRIM(STRING(numor(i)),2),COLOR=50
	ENDFOR
	OPLOT, time, Tsam, PSYM=5, SYMSIZE=0.5,COLOR=20
	AXIS, 0, 0, XSTYLE=1, YSTYLE=1

	WRITE_GIF, outpath+'d7_tlog.gif', TVRD()

finished:
	RETURN
	END
