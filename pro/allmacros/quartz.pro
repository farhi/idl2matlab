;------------------------------------------------------------------------------
;******************************************************************************
;
	FUNCTION quartz, w_in, nosave=nosave, FR_depol=FRR, $
		 temp=temp, sk, fr1, tmp

;For D7 data only
;
;takes 2-D or 3-D workspace containing a normalised non-tof z-PA 
;or xyz-PA background-subtracted quartz run (or runs). Puts the flipping ratios 
;into w_out and also into the file 'quartz_"numor".dat'.  Will also correct
;for depolarisation due to the sample (assuming cylindrical geometry) if desired
;
;
;ARGUMENTS:
; FR_depol	:if depolarisation is present in the sample - 
;		 FR_depol is flipping ratio (IN M2)in the presence of 
;		 depolarisation
; temp		:if depolarisation is present in the sample - 
;		 temp is the temperature (used to name the output file
;		 "quartz_<numor>_<temp>.dat") to identify the depolarisation
;		 correction at that temperature
; (sk, fr1 and tmp are obsolete, kept for backwards compatability)
;
;KEYWORDS:
; /nosave	:don't save flipping ratios to file
;
;DIMENSIONS:
; w_in(nspectra, nphases) -> w_out(nspectra)
;
;COMMAND SYNTAX:
; w5=quartz(w4[,/nosave][,FR_depol=<FR_depol>,temp=<temp>]
;
; (optional keywords/arguments shown in square brackets)
;
;							KHA/JRS, 6/10/00
;-------------------------------------------------------------------------------
;*******************************************************************************
	COMMON c_lamp

	iprint=0	; if iprint>0, show debugging messages

	IF (iprint GT 0) THEN PRINT,'Start quartz:'

	take_datp, datp

	skip=0

	IF(N_ELEMENTS(sk) GT 0) THEN skip=sk
	IF(N_ELEMENTS(fr1) GT 0) THEN FRR=fr1
	IF(N_ELEMENTS(tmp) GT 0) THEN temp=tmp

	IF KEYWORD_SET(nosave) THEN skip=1
;-------------------------------------------------------------------------------
;Check dimensions of input workspaces


	mon=datp.n
	e_in=datp.e

	sw=SIZE(w_in)

	se=SIZE(e_in)
	FOR i=0,se(0) DO IF (se(i) NE sw(i)) THEN GOTO, seterr
	GOTO, noseterr
seterr:
	PRINT,'Normalise: No error bars defined for w_in. Use sqrt'
	e_in=SQRT(w_in)
noseterr:

	nspectra=sw(1)
	nphases=sw(2)/2		; i.e. 1 for z-PA and 3 for xyz-PA
	IF(sw(0) EQ 2) THEN nruns=1 ELSE nruns=sw(3)
	IF(nruns EQ 1) THEN x_in=datp.x ELSE x_in=datp.z

	IF (N_ELEMENTS(skip) EQ 0) THEN skip=0

	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Calculate flipping ratios

	fr=FLTARR(nspectra)		& dfr=fr
	x_out=x_in
	IF(nruns GT 1) THEN z_out=INDGEN(nruns)
	y_out=INDGEN(nphases)
	w_out=FLTARR(nspectra,nphases,nruns)
	e_out=w_out
	
	FOR irun=0,nruns-1 DO BEGIN
		FOR iphase=0,nphases-1 DO BEGIN
			ip0=2*iphase	& ip1=2*iphase+1
			IF (iprint GT 0) THEN PRINT,'iphase=',iphase,' ip0=',ip0,' ip1=',ip1
			fr=(w_in(*,ip0,irun)/w_in(*,ip1,irun))
			dfr=SQRT((e_in(*,ip0,irun)/w_in(*,ip1,irun))^2 $
				+(e_in(*,ip1,irun)*w_in(*,ip0,irun)/w_in(*,ip1,irun)^2)^2)
			w_out(*,iphase,irun)=fr
			e_out(*,iphase,irun)=dfr
			IF (iprint GT 0) THEN PRINT,'fr=',fr
			IF (iprint GT 0) THEN PRINT,'dfr=',dfr
		ENDFOR
	ENDFOR

	IF (iprint GT 0) THEN PRINT,'End of "calculate flip. etc." section'
;-------------------------------------------------------------------------------
;Blech-Averbach correction for angular dependence (drops out if FRR=1)

	IF (N_ELEMENTS(FRR) EQ 0) THEN FRR=0
	IF (FRR EQ 0) THEN GOTO, write

	IF(nphases EQ 3) THEN BEGIN
		x=[datp.p(28),datp.p(29),datp.p(30)]
		FRRT1=MOMENT(x)
		FRRT=FRRT1(0)
	ENDIF ELSE BEGIN
		FRRT=datp.p(28)
	ENDELSE

	print, FRRT
	p1=(FRR-1.)/(FRR+1.)	& p0=(FRRT-1.)/(FRRT+1.)

	a1=1.7133
	a2=-0.0927
	b1=-0.0368
	b2=-0.3750

	muR=(-a1+SQRT(a1^2-4.*a2*ALOG(p1/p0)))/(2*a2)
	muR=ABS(muR)

	IF (IPRINT GT 0) THEN PRINT, 'p1=',p1,'p0=',p0,'muR=',muR

	ptheta=x_in
	x_buf=(ABS(x_in)*!pi/180.)/2.	;theta in radians

	ptheta=EXP(-(a1+b1*SIN(x_buf)^2)*muR-(a2+b2*SIN(x_buf)^2)*muR^2)

	w_buf=(w_out-1.)/(w_out+1.)
	e_buf=e_out*(2./((w_out+1.)^2))

	FOR iphase=0,nphases-1 DO BEGIN
		w_buf(*,iphase,*)=w_buf(*,iphase,*)*ptheta
		e_buf(*,iphase,*)=e_buf(*,iphase,*)*ptheta
	ENDFOR

	w1=1.+w_buf	& e1=e_buf
	w2=1.-w_buf	& e2=e_buf
	w_out=w1/w2
	e_out=e_buf*(2./((1.-w_buf)^2))

	IF (IPRINT GT 0) THEN PRINT,'Flipping ratios, and errors are:',w_out, e_out
	IF (IPRINT GT 0) THEN PRINT,'End of Blech-Averbach correction section.'

;-------------------------------------------------------------------------------
;Write flipping ratios to quartz file

write:	help, w_out
	numor=LONARR(nruns)
	FOR irun=0,nruns-1 DO BEGIN
		IF(iprint GT 0) THEN PRINT,'irun=',irun
		IF(nruns EQ 1) THEN numor(irun)=LONG(datp.p(0)) ELSE $
			numor(irun)=LONG(datp.pv(0,irun))
		mon=LONG(mon)

		title='Flipping Ratios from '+STRTRIM(datp.w_tit,2)

		IF (skip NE 0) THEN GOTO, dontsave
	
		IF(FRR EQ 0) THEN BEGIN
			out_file='quartz_'+STRTRIM(STRING(numor(irun)),2)+'.dat'
		ENDIF ELSE BEGIN
			out_file='quartz_'+STRTRIM(STRING(numor(irun)),2)+'_'+STRTRIM(STRING(temp),2)+'.dat'
		ENDELSE

		OPENW, 1, out_file
		PRINTF, 1, STRTRIM(title,2)
		IF (nphases EQ 1) THEN BEGIN
			PRINTF, 1, 'Spectrum    Angle      Flip     dFlip'
			FOR i=0,nspectra-1 DO PRINTF, 1, FORMAT='(I7,2F11.3,F7.3)', $
						2*(i+1), x_out(i,irun), w_out(i,0,irun), e_out(i,0,irun)
			PRINTF, 1, FORMAT='("   Time 0  ",I18)', mon(0,1,irun)
			PRINTF, 1, FORMAT='("   Time 1  ",I18)', mon(0,2,irun)
			PRINTF, 1, FORMAT='("Monitor 0  ",I18)', mon(1,1,irun)
			PRINTF, 1, FORMAT='("Monitor 1  ",I18)', mon(1,2,irun)
		ENDIF ELSE BEGIN
			PRINTF, 1, 'Spectrum    Angle      FlipZ  dFlipZ     FlipX  dFlipX     FlipY  dFlipY'
			FOR i=0,nspectra-1 DO PRINTF, 1, FORMAT='(I7,2F11.3,F7.3,F11.3,F7.3,F11.3,F7.3)', $
						2*(i+1), x_out(i,irun), w_out(i,0,irun), e_out(i,0,irun), $
						w_out(i,1,irun), e_out(i,1,irun), w_out(i,2,irun), e_out(i,2,irun)
			PRINTF, 1, FORMAT='("   Time 0  ",3I18)', mon(0,1,irun), mon(0,3,irun), mon(0,5,irun)
			PRINTF, 1, FORMAT='("   Time 1  ",3I18)', mon(0,2,irun), mon(0,4,irun), mon(0,6,irun)
			PRINTF, 1, FORMAT='("Monitor 0  ",3I18)', mon(1,1,irun), mon(1,3,irun), mon(1,5,irun)
			PRINTF, 1, FORMAT='("Monitor 1  ",3I18)', mon(1,2,irun), mon(1,4,irun), mon(1,6,irun)
		ENDELSE
		CLOSE, 1	
		IF(lamp_host EQ 'd7' OR lamp_host EQ 'd7sgi' OR lamp_host EQ 'd7lin') THEN $
	   		spawn, 'cp '+out_file+' /home/vis/d7/lambda/QUARTZFILES/'
	ENDFOR
dontsave:

	IF (iprint GT 0) THEN PRINT,'End of "Write to quartz_file" section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	mod_datp, datp, "y", y_out
	mod_datp, datp, "e", e_out

	datp.w_tit=title
	datp.other_tit=datp.other_tit+' -qu'

	IF (nphases EQ 1) THEN datp.y_tit='Flipping Ratio' ELSE BEGIN
		datp.y_tit='Phase'
		datp.z_tit='Flipping Ratio'
	ENDELSE

finished:

	IF (iprint GT 0) THEN PRINT,'End quartz:'

	give_datp, datp

	RETURN, w_out
	END
