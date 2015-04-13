;-------------------------------------------------------------------------------
;*******************************************************************************
;
;     FUNCTION normalise, w_in, raw=raw, monitor=monitor, time=time,  $
;			  detector=detector, alldetectors=alldetectors, ei=ei$
;			  madangles=madangles, zeroshift=zeroshift, inorm, ikeep
;
;For IN4, IN5, IN6, HET and D7 data.
;
;For IN4, IN5 and IN6:
;---------------------
; Normalises raw data to monitor or counting time, depending on value of inorm.
; Finds the position of the elastic peak.
;
;KEYWORDS
; /raw	        : no normlisation (error bars calculated)
; /monitor      : normalise data to 1000 monitor1 counts (DEFAULT)
; /time         : normalise data to counting time (will not work for summed data)
;
; (inorm and ikeep are obsolete, kept for backwards compatability)
;
;For HET:
;--------
; Normalises raw data to monitor 1 (must be monitor 1 for ISIS data).  Removes
; empty spectra.  User must input incident energy.
;
;ARGUMENTS
; ei	: incident energy of measurement
;
;For D7:
;--------
; Normalises raw data to monitor (default) or counting time or individual
; detector.  Extracts every other spectrum if desired (=> nspectra=32).
; Recalculates detector angles based on a YIG calibration by default.
;
;ARGUMENTS
; detector      : detector number to normalise to
; zeroshift     : angle in degrees of 2theta = 0
;
; (inorm and ikeep are obsolete, kept for backwards compatability)
;
;KEYWORDS
; /raw	        : no normlisation (error bars calculated)
; /monitor      : normalise data to 1000 monitor1 counts (DEFAULT)
; /time	        : normalise data to counting time
; /alldetectors : supresses the removal of the odd numbered detectors
; /madangles    : supresses the recalculation of the detector angles using the
;		  current YIG calibration
;
;DIMENSIONS
;   non-TOF data: w_out(nspectra,nphases,nruns)           -unless nphases is 1
;	TOF data: w_out(nchannels,nspectra*nphases,nruns)
;
;COMMAND SYNTAX
;  w2=normalise(w1[,/raw][,/monitor][,/time][,detector=#][,/alldetectors][,/rawangles][,ei=#])
;
;  (optional keywords/arguments shown in square brackets)
;
;						     KHA,JRS 16/7/02
;
;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION normalise_tof, w_in, inorm

	COMMON c_lamp_access, inst
	COMMON printing, iprint, outstring

	IF iprint THEN PRINT,'Start normalise_tof:'
	take_datp, datp

;-------------------------------------------------------------------------------
;Set up start parameters

	mon  = datp.n
	par  = datp.p
	x_in = datp.x
	y_in = datp.y

	sw = SIZE(w_in)
	IF iprint THEN PRINT,'SIZE(w_in) = ',sw
	nchannels = sw(1)
	IF sw(0) EQ 1 THEN nspectra = 1     ELSE $
	IF sw(0) EQ 2 THEN nspectra = sw(2) ELSE BEGIN $
		i = WIDGET_MESSAGE('Normalise: unknown data format',/ERROR)
		RETURN, w_in
	ENDELSE
	norm = 1
	IF N_ELEMENTS(inorm) GE 1 THEN norm = inorm
	IF iprint THEN PRINT, 'norm = ',norm
	gauss  = FLTARR(4)
	dgauss = FLTARR(4)

	IF iprint THEN PRINT,'End of setup data section'
;-------------------------------------------------------------------------------
;Find elastic peak

	IF inst EQ 'IN5' THEN i=WHERE(y_in GT 10.) ELSE i=INDGEN(nspectra)
	xtot = x_in
	ytot = TOTAL(w_in(*,i),2)
	etot = SQRT(ytot)
	y0   = MAX(ytot,i0)
	xmin = xtot((i0-20)>0)
	xmax = xtot((i0+20)<(nchannels-1))

	FITGAUSS, xtot, ytot, etot, xmin, xmax, gauss, dgauss

	chel   = gauss(2)
	IF (inst NE 'MiBeMol') AND (inst NE 'DCSasc') THEN par(9)=chel

        IF (iprint GT 0) THEN PRINT,'End of elastic peak section'

; ** Normalization for Mibemol data and DCS
; ** added by S. Rols 09/01 srols@anl.gov
; ** *************************************

	CASE inst OF
	'MiBeMol':BEGIN
		normf = TOTAL(mon)/N_ELEMENTS(mon)
		normf = normf/1000.
		chel = gauss(2) & par(9) = chel
		GOTO, mibdcs
		END
	'DCSasc':BEGIN
		normf = TOTAL(mon)
		normf = normf/1000.
		chel  = gauss(2) & par(10) = chel
		GOTO, mibdcs
		END
	ELSE:
	ENDCASE
;------------------------------------------------------------------------------
;Find monitor peak, integrate and normalise

	IF norm EQ 0 THEN $
		normf=1. $
	ELSE IF (norm EQ 1) THEN BEGIN
		xmon = x_in
		ymon = mon(*,0)
		emon = SQRT(ymon)
		y0=MAX(ymon,i0)
		xmin = xtot((i0-20)>0)
		xmax = xtot((i0+20)<(nchannels-1))

		FITGAUSS, xmon, ymon, emon, xmin, xmax, gauss, dgauss

		bkgd   = gauss(0)
		height = gauss(1)
		centre = gauss(2)
		sigma  = gauss(3)
		IF  iprint THEN BEGIN
			PRINT, 'bkgd =',bkgd
			PRINT, 'height =',height
			PRINT, 'centre =',centre
			PRINT, 'sigma =',sigma
		ENDIF

		irange  = WHERE(ABS(xmon - centre) LT 8.*sigma, nrange)
		peaksum = TOTAL(ymon(irange),1)
		bkgdsum = TOTAL(ymon,1)-peaksum
		bkgd    = bkgdsum/FLOAT(nchannels-nrange)
		normf   = (peaksum - bkgd*FLOAT(nrange))/1000.
	ENDIF ELSE IF norm EQ 2 THEN BEGIN
		IF inst EQ 'IN5' THEN normf=par(0) ELSE normf=par(0)/60.
	ENDIF

mibdcs:
	w_out = w_in/normf
	e_out = SQRT(w_in)/normf

        IF iprint THEN PRINT,'End of monitor peak section'
;-------------------------------------------------------------------------------
;Return parameters and exit

	s = STRTRIM(STRING(chel),2)  & i = STRPOS(s,'.') & chel  = STRMID(s,0,i(0)+3)
	s = STRTRIM(STRING(normf),2) & i = STRPOS(s,'.') & normf = STRMID(s,0,i(0)+3)

	CASE norm OF
		0: BEGIN
		   nors = '/raw'
		   nstring = 'not normalised. '
		   END
		1: BEGIN
		   nors = '/m'
		   nstring = 'normalised to '+normf+'/1000 M1 counts. '
		   END
		2: BEGIN
		   nors = '/t'
		   nstring='normalised to '+normf+' minutes. '
		   END
	ENDCASE
	normalisation = ' no('+nors+',ce='+chel+')'
	cstring='Elastic channel='+chel
	outstring=nstring+cstring

	datp.p=par

	s = datp.other_tit & i=RSTRPOS(s,' ') & n=STRLEN(s) & numor=STRMID(s,i+1,n-1)
	IF (STRPOS(numor,'>') EQ -1) THEN numor=STRTRIM(STRING(FIX(par(10))),2)
	datp.other_tit=inst+' #'+numor+normalisation

	datp.x_tit='Channel number'
	datp.y_tit='Scattering angle'
	datp.z_tit='Counts / 1000 M1'

	mod_datp, datp, "e", e_out

	give_datp, datp

finished:
	RETURN, w_out
	END


;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION normalise_d7, w_in0, inorm, ikeep, zeroshift

	COMMON c_lamp_access, inst
	COMMON printing, iprint, outstring

	IF iprint THEN PRINT,'Start normalise_d7:'
	take_datp, datp

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	m_in0     = datp.n
	x_in0     = datp.x
	y_in0     = datp.y
	z_in0     = datp.z
	e_in0     = datp.e
	par       = datp.p
	parv      = datp.pv
	e_in0     = SQRT(w_in0)
	rearrange = 0
	TOF       = FIX(par(8))
	nchannels = FIX(par(6))
	nspectra  = FIX(par(1))
	nphases   = FIX(par(2))
	se        = SIZE(e_in0)
	sw        = SIZE(w_in0)

	IF NOT TOF AND (nphases EQ 1) THEN BEGIN
		IF N_ELEMENTS(sw) EQ 4 THEN nruns = 1 ELSE nruns = sw(sw(0))
		PRINT,'Normalise_d7: Assuming workspace contains a series of no-PA runs'
		rearrange = 1
		w_in = FLTARR(64,nphases,nruns) & w_in(*,0,*)=w_in0(*,*)
		e_in = FLTARR(64,nphases,nruns) & e_in(*,0,*)=e_in0(*,*)
		y_in = y_in0
		sw   = size(w_in)
	ENDIF ELSE IF sw(0) EQ 2 THEN nruns = 1 ELSE nruns = sw(3)

	IF nruns EQ 1 THEN parv = par

	norm = 1
	keep = 0

	IF N_ELEMENTS(inorm) NE 0 THEN BEGIN
		norm = inorm
		IF N_ELEMENTS(ikeep) NE 0 THEN keep=ikeep
	ENDIF

	IF TOF AND (norm EQ 1) AND (nspectra EQ 66) THEN norm = -65
	IF keep LE 0 THEN nspectra = 32 ELSE nspectra = 64

	par(1) = nspectra
	par(3) = nruns

	IF iprint THEN BEGIN
		PRINT,'size(w_in) = ',sw
		PRINT,'TOF        = ',TOF
		PRINT,'nchannels  = ',nchannels
		PRINT,'nspectra   = ',nspectra
		PRINT,'nphases    = ',nphases
		PRINT,'nruns      = ',nruns
		PRINT,'norm       = ',norm
		PRINT,'keep       = ',keep
	ENDIF

	m_in = m_in0
	x_in = x_in0
	sz   = SIZE(z_in0)
	IF sz(0) EQ 3 THEN BEGIN
		ns   = sz(1)
		nr   = sz(3)
		z_in = REFORM(z_in0,ns,nr)
	ENDIF ELSE $
		z_in = z_in0
	IF rearrange EQ 0 THEN BEGIN
		w_in = w_in0
		e_in = e_in0
		y_in = y_in0
	ENDIF

	IF iprint THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;For TOF data, find elastic peak

	IF TOF THEN BEGIN
		nspecs = nspectra
		IF nphases GT 1 THEN adder = 1 ELSE adder = 0
		specs  = INDGEN(nspectra)*nphases + adder
		ytot = TOTAL(w_in(*,specs,*),2)
		IF nruns GT 1 THEN ytot = TOTAL(ytot,2)
		etot = SQRT(ytot)
		xtot = x_in
		y0 = MAX(ytot,i0)
		xmin = xtot((i0-20)>0)
		xmax = xtot((i0+20)<(nchannels-1))
		gauss  = FLTARR(4)
		dgauss = FLTARR(4)

		FITGAUSS, xtot, ytot, etot, xmin, xmax, gauss, dgauss

		chel   = gauss(2)
		par(9) = chel
	ENDIF

;-------------------------------------------------------------------------------
;Perform normalisation

	IF NOT TOF THEN BEGIN
		z_out = FLTARR(nspectra,nruns)
		x_out = FLTARR(nspectra)

		IF keep LE 0 THEN BEGIN
			x_out(INDGEN(32))   = x_in(2*INDGEN(32)+1)
			IF nruns GT 1  THEN $
			z_out(INDGEN(32),*) = z_in(2*INDGEN(32)+1,*) ELSE z_out = x_out
		ENDIF ELSE BEGIN
			x_out = x_in
			z_out = z_in
		ENDELSE

		specs  = INDGEN(nspectra)*2+1
		iphase = INDGEN(nphases)
		irun   = INDGEN(nruns)

		IF (keep LE 0) THEN BEGIN
			w_in = w_in(specs,*,*)
			e_in = e_in(specs,*,*)
		ENDIF

		IF norm EQ 0 THEN normf = 1.
		IF norm EQ 1 THEN normf = m_in(1,iphase+1,irun)/1000.
		IF norm EQ 2 THEN normf = m_in(0,iphase+1,irun)/100.
		IF norm LT 0 THEN normf = w_in(-norm-1,iphase,irun)/1000.

		nspecs = (SIZE(w_in))(1)
		normf = normf(INTARR(nspecs),*,*)
		IF iprint THEN HELP, normf

		w_out = w_in/normf
		e_out = e_in/normf

		IF (nruns EQ 1) OR (nphases NE 1) THEN y_out = y_in $
		ELSE BEGIN
			y_out=INTARR(nruns)
			y_out(*)=FIX(parv(0,*))
		ENDELSE
	ENDIF ELSE BEGIN
		x_out  = x_in
		y_out  = FLTARR(nspectra*nphases)
		iphase = INDGEN(nphases)
		irun = INDGEN(nruns)

		IF keep LE 0 THEN $
			y_out(iphase*32) = y_in(iphase*64 + 1) $
		ELSE IF (keep GT 0) AND (norm EQ -65) THEN $
			y_out = y_in((nspectra+2)*iphase(0):nspectra*(iphase(0) + 1)) $
		ELSE $
			y_out = y_in

		IF keep LE 0 THEN a = 2*INDGEN(32) + 1 ELSE a = INDGEN(64)
		z_out = (y_in(a))(*,INTARR(nruns))

		IF norm EQ -65 THEN nspecs = 66 ELSE nspecs = 64

		IF iprint THEN PRINT,'TOF factor for run #'+ $
			STRTRIM(STRING(LONG(parv(0, irun))),2)+' = '+ $
			STRTRIM(STRING(parv(40,irun)),2)

; Build normalisation array

		CASE 1 OF
		norm EQ 0: normf = 1.
		norm EQ 1: normf = m_in(1,iphase + 1,irun)*parv(40 + iphase,irun)/1000.
		norm EQ 2: normf = m_in(0,iphase + 1,irun)/100.  ; time
		norm LT 0: BEGIN     ; detector
			   ispec = -norm - 1 + iphase*nspecs
			   normf = TOTAL(w_in(*,ispec,irun),1)/1000.
			   w_in(*,ispec,irun) = 0.
			   e_in(*,ispec,irun) = -1.
			   END
		ENDCASE

		IF norm NE 0 THEN BEGIN
			a = (FLTARR(nspecs) + 1.) # TRANSPOSE(iphase)
			b = REFORM(a,nspecs*nphases)
			c = REFORM(normf(b,*),1,nspecs*nphases,nruns)
			normf = c(INTARR(nchannels),*,*)
		ENDIF

		w_out = w_in/normf
		e_out = e_in/normf

; Remove unwanted spectra

		IF norm EQ -65 THEN BEGIN
			zspecs = [(iphase+1)*66 - 2,(iphase+1)*66 - 1]
			e_out(*,zspecs,*) = -1.
		ENDIF
		IF keep LE 0 THEN BEGIN
		        zspecs = INTARR((nspecs/2),nphases)
			FOR i = 0, nphases - 1 DO zspecs(*,i) = INDGEN(nspecs/2)*2 + i*nspecs
			zspecs = REFORM(zspecs,(nspecs/2)*nphases)
			e_out(*,zspecs,*) = -1.
		ENDIF
		inz = WHERE((TOTAL(e_out,1))(*,0) GE 0.0, nnz)
		IF nnz GT 0 THEN BEGIN
			w_out = w_out(*,inz,*)
			e_out = e_out(*,inz,*)
		ENDIF
		par(6) = nchannels
	ENDELSE

	IF iprint THEN BEGIN
		PRINT,'size(w_out)=',size(w_out)
		PRINT,'size(e_out)=',size(e_out)
		PRINT,'size(x_out)=',size(x_out)
		PRINT,'size(y_out)=',size(y_out)
		PRINT,'size(z_out)=',size(z_out)
	ENDIF

	IF iprint THEN PRINT,'End of "Perform normalisation" section'

;-------------------------------------------------------------------------------
;Recalculate detector angles from YIG calibration
;MOST RECENT CORRECTION; YIG scan December 2001

	IF (keep EQ 0) OR (keep EQ 2) THEN BEGIN
		IF iprint THEN PRINT,'Recalculating detector angles'
		bank = FLTARR(4,nruns)
		IF nruns EQ 1  THEN bank =    par(16:19) $
			  ELSE bank =   parv(16:19,*)
		IF bank(0,0) EQ 0. THEN BEGIN
			i = WIDGET_MESSAGE('Normalise: Cannot recalibrate detector angles',/ERROR)
			GOTO, endangles
		ENDIF

		angle = FLTARR(16,4) - zeroshift
		angle(*,0) = angle(*,0) + [205.82,202.89,200.18,197.20,194.37,191.54,188.75,185.85, $
				           183.00,180.17,177.40,174.50,171.67,168.80,166.12,163.09]
		angle(*,1) = angle(*,1) + [197.35,194.45,191.73,188.77,185.91,183.09,180.30,177.42, $
				           174.57,171.73,168.91,166.03,163.15,160.33,157.52,154.61]
		angle(*,2) = angle(*,2) + [204.18,201.31,198.55,195.64,192.77,189.90,187.11,184.18, $
				           181.35,178.53,175.77,172.82,169.98,167.15,164.39,161.41]
		angle(*,3) = angle(*,3) + [200.13,197.29,194.58,191.63,188.78,185.94,183.16,180.26, $
				           177.39,174.54,171.80,168.85,166.02,163.14,160.33,157.42]
		IF keep EQ 0 THEN BEGIN
			i = INDGEN(8)*2 + 1
			angle2 = angle(i,*)
			phi=FLTARR(32)
			FOR ibank=0,3 DO $
				phi(ibank*8:ibank*8+7)=bank(ibank)-angle2(*,ibank)
		ENDIF ELSE BEGIN
			phi=FLTARR(64)
			FOR ibank=0,3 DO $
				phi(ibank*16:ibank*16+15)=bank(ibank)-angle(*,ibank)
		ENDELSE
		IF NOT TOF THEN BEGIN
			x_out=phi
		ENDIF ELSE BEGIN
			IF nphases EQ 1 THEN y_out=phi $
			ELSE IF nphases EQ 2 THEN y_out=[phi,phi] $
			ELSE y_out=[phi,phi,phi,phi,phi,phi]
			IF iprint THEN PRINT,'y_out=',y_out
		ENDELSE
		IF nruns GT 1 THEN FOR irun=0,nruns-1 DO BEGIN
			IF keep EQ 0 THEN FOR ibank=0,3 DO $
				phi(ibank*8:ibank*8+7)=bank(ibank,irun)-angle2(*,ibank) $
			ELSE FOR ibank=0,3 DO $
				phi(ibank*16:ibank*16+15)=bank(ibank,irun)-angle(*,ibank)
			z_out(*,irun)=phi
		ENDFOR
		IF nruns GT 1 THEN IF iprint THEN PRINT,'z_out=',z_out
	ENDIF
endangles:

	IF (iprint GT 0) THEN PRINT,'End of "Calibrate angles" section'


;-------------------------------------------------------------------------------
;Return parameters and exit

	datp.p=par
	mod_datp, datp, "x", x_out
	mod_datp, datp, "z", z_out

	IF NOT TOF AND nphases EQ 1 THEN BEGIN
		w_out0=FLTARR(nspectra,nruns) & e_out0=w_out0
		w_out0(*,*)=w_out(*,0,*)
		e_out0(*,*)=e_out(*,0,*)
		IF nruns GT 1 THEN mod_datp, datp, "y", y_out
	ENDIF ELSE BEGIN
		w_out0=w_out
		e_out0=e_out
		mod_datp, datp, "y", y_out
	ENDELSE

	mod_datp, datp, "e", e_out0

	label=' '

	CASE norm OF
		0: BEGIN
			nors = '/r'
			nstring = 'no normalisation'
		   END
		1: BEGIN
			nors = '/m'
			nstring = 'normalised to M1'
		   END
		2: BEGIN
			nors = '/t'
			nstring = 'normalised to counting time'
		   END
	      -65: BEGIN
		   	nors = '/m'
			nstring = 'normalised to TOF-M1'
		   END
 	     ELSE: BEGIN
			nors = 'det='+STRTRIM(STRING(-norm),2)
			nstring='normalised to s'+STRTRIM(STRING(-norm),2)
		   END
	ENDCASE

	CASE keep OF
	        1: BEGIN
			kees = ',/all,/mad'
			kstring = 'all detectors kept, raw det angles used'
		   END
	       -1: BEGIN
			kees = ',/mad'
			kstring = 'odd detectors discarded, raw det angles used'
		   END
	        2: BEGIN
			kees = ',/all'
			kstring = 'all detectors kept, det angles recalculated'
		   END
	     ELSE: BEGIN
			kees = ''
			kstring = 'odd detectors discarded, det angles recalculated'
		   END
	ENDCASE

	normalisation = ' no('+nors+kees
	IF zeroshift GT 0.0 THEN BEGIN
		s=STRTRIM(STRING(zeroshift),2) & i=STRPOS(s,'.') & zers=STRMID(s,0,i+3)
		normalisation = normalisation+',zs='+zers
	ENDIF
	IF NOT TOF THEN normalisation = normalisation+')' ELSE BEGIN
		s=STRTRIM(STRING(chel),2) & i=STRPOS(s,'.') & chel=STRMID(s,0,i+3)
		normalisation = normalisation+',ce='+chel+')'
	cstring = '.  Elastic Channel = '+chel
	ENDELSE

	IF NOT TOF THEN BEGIN
		cstring = ''
		IF (nruns EQ 1) THEN BEGIN
			IF (nphases EQ 1) THEN $
				datp.y_tit = STRTRIM(label,2) $
			ELSE BEGIN
				datp.y_tit = 'Phase'
				datp.z_tit = STRTRIM(label,2)
			ENDELSE
		ENDIF ELSE BEGIN
			IF (nphases EQ 1) THEN BEGIN
				datp.y_tit = 'Run Number'
				datp.z_tit = STRTRIM(label,2)
			ENDIF ELSE BEGIN
				datp.y_tit = 'Phase'
				datp.z_tit = 'Run Number'
			ENDELSE
		ENDELSE
	ENDIF ELSE BEGIN
		datp.y_tit = 'Scattering Angle'
		IF (nruns EQ 1) THEN datp.z_tit = STRTRIM(label,2) $
				ELSE datp.z_tit = 'Numor'
	ENDELSE

	s=datp.other_tit & i=RSTRPOS(s,' ') & n=STRLEN(s) & numor=STRMID(s,i+1,n-1)
	IF (nruns EQ 1) AND (STRPOS(numor,'>') EQ -1) THEN numor = STRTRIM(STRING(LONG(par(0))),2)
	IF (nruns EQ 1) AND (STRPOS(numor,'>') NE -1) THEN numor = numor
	IF nruns NE 1 THEN $
	 numor=STRTRIM(STRING(LONG(parv(0,0))),2)+':'+STRTRIM(STRING(LONG(parv(0,nruns-1))),2)

	IF (STRPOS(s,' -ax') EQ -1 AND STRPOS(s,' -cc') EQ -1) THEN $  ; data not previously concatenated, or add_xyz'd
		datp.other_tit='D7 #'+numor+normalisation $
	ELSE datp.other_tit=datp.other_tit+normalisation

	outstring=nstring+', '+kstring+cstring

finished:

	IF (iprint GT 0) THEN PRINT,'End normalise:'

	give_datp, datp

	RETURN, w_out0
	END
;-----------------------------------------------------------------------------
;*****************************************************************************

	FUNCTION normalise_het, w_in, ei = ei

;-----------------------------------------------------------------------------
;*****************************************************************************

	COMMON printing, iprint, outstring
	take_datp, datp

	n = datp.n
	e_in = datp.e
	y_in = datp.y
	z_in = datp.z
	x_in = datp.x
	s = SIZE(w_in)
	nchannels = s(1)
	nspectra = s(2)
	IF iprint THEN BEGIN
		PRINT, 'Nspectra  = ', nspectra
		PRINT, 'Nchannels = ', nchannels
	ENDIF

	IF iprint THEN PRINT,'Normalise_HET: End of check dimensions, etc. section'
;-------------------------------------------------------------------------------------
; Find elastic lines

	offset = 100			; offset required to ignore prompt peak

;2.5m bank
	i = WHERE(z_in LT 2.6 AND z_in GT 2.4)
	xtot = x_in(offset : nchannels - 1)
	ytot = TOTAL(w_in(offset:nchannels-1,i),2)
	etot = SQRT(TOTAL(e_in(offset:nchannels-1,i)^2,2))
	y0   = MAX(ytot,i0)
	xmin = xtot((i0 - 20) > 0)
	xmax = xtot((i0 + 20) < (nchannels - 1))
	FITGAUSS, xtot, ytot, etot, xmin, xmax, gauss, dgauss
	chel1   = gauss(2)
	datp.p(9) = chel1

;4m bank
	i = WHERE(z_in LT 4.1 AND z_in GT 3.9)
	xtot = x_in(offset:nchannels - 1)
	ytot = TOTAL(w_in(offset:nchannels-1,i),2)
	etot = SQRT(TOTAL(e_in(100:nchannels-1,i)^2,2))
	y0   = MAX(ytot,i0)
	xmin = xtot((i0 - 20) > 0)
	xmax = xtot((i0 + 20) < (nchannels - 1))
	FITGAUSS, xtot, ytot, etot, xmin, xmax, gauss, dgauss
	chel2   = gauss(2)
	datp.p(8) = chel2

	IF (iprint GT 0) THEN PRINT,'End of elastic peak section'

;----------------------------------------------------------------------------
; Put Incident energy into parameter block

	IF N_ELEMENTS(ei) EQ 0 THEN BEGIN
		 s = STRARR(3)
		 s(0) = 'You need to include the incident energy'
		 s(1) = 'for HET data'
		 s(2) = 'e.g.   w2 = normalise(w1, Ei = 100)'
		 i = WIDGET_MESSAGE(s, /ERROR)
		 GOTO, finished
	ENDIF ELSE BEGIN
		ei = FLOAT(ei)
		lambda = SQRT(81.8066/ei)
		datp.p(21) = lambda
	ENDELSE
;----------------------------------------------------------------------------
; Sort data in ascending 2-theta order

	sorty = SORT(y_in)
	w_buf = w_in(*,sorty)
	e_buf = e_in(*,sorty)
	z_buf = z_in(sorty)
	y_buf = y_in(sorty)

; Perform normalisation to M1 (must use M1 for HET data)

	narr = INTARR(nspectra)
	w_out = w_buf/n(*,narr)
	e_out = SQRT( (e_buf/n(*,narr))^2 + (w_buf*n(*,narr+1)/(n(*,narr)^2))^2 )

; Remove zero spectra
	i = WHERE(y_buf NE 0, nnz)
	w_out = w_out(*,i)
	e_out = e_out(*,i)
	y_out = y_buf(i)
	z_out = z_buf(i)
	s = SIZE(w_out)
	nspectra = s(2)

	IF iprint THEN PRINT, 'Normalise_HET: End of normalisation section'

;---------------------------------------------------------------------------
; Return data and paramaters

	s=STRTRIM(STRING(ei),2)     & i=STRPOS(s,'.') & ei    =STRMID(s,0,i+3)
	s=STRTRIM(STRING(lambda),2) & i=STRPOS(s,'.') & lambda=STRMID(s,0,i+3)
	s=STRTRIM(STRING(chel1),2)  & i=STRPOS(s,'.') & chel1 =STRMID(s,0,i+3)
	s=STRTRIM(STRING(chel2),2)  & i=STRPOS(s,'.') & chel2 =STRMID(s,0,i+3)
	datp.other_tit = datp.other_tit + ' no(Ei='+ei+') '
	mod_datp, datp, "e", e_out
	mod_datp, datp, "y", y_out
	mod_datp, datp, "z", z_out
	give_datp, datp
	outstring = ' Normalised to HET monitor 1, Lambda = '+ lambda
        cstring = ', Elastic times: 2.5m bank - ' + chel1 + ', 4m bank - ' + chel2
	outstring = outstring + cstring
	RETURN, w_out

finished:
	END
;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION normalise, w_in, raw=raw, monitor=monitor, time=time, $
			    detector=detector, alldetectors=alldetectors, ei=ei, $
			    madangles=madangles, zeroshift=zeroshift, inorm, ikeep

	COMMON c_lamp_access, inst
	COMMON printing, iprint, outstring

	iprint = 0   ; if iprint>0, show debugging messages

	IF iprint THEN PRINT,'Start normalise:'

	IF(N_ELEMENTS(inorm) EQ 0) THEN inorm=1								     ;default
	IF KEYWORD_SET(raw) THEN inorm=0
	IF KEYWORD_SET(monitor) THEN inorm=1
	IF KEYWORD_SET(time) THEN inorm=2
	IF KEYWORD_SET(detector) THEN BEGIN
		IF(detector NE 0) THEN inorm=-detector
	ENDIF

	IF(N_ELEMENTS(ikeep) EQ 0) THEN ikeep=0								     ;default
	IF KEYWORD_SET(madangles) THEN BEGIN
		IF KEYWORD_SET(alldetectors) THEN ikeep=1 ELSE ikeep=-1
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(alldetectors) THEN ikeep=2
	ENDELSE
	IF(N_ELEMENTS(zeroshift) LE 0) THEN zeroshift=0.0

;-------------------------------------------------------------------------------
;Check instrument name and call appropriate function

	IF (inst EQ 'IN4' OR inst EQ 'IN5' OR inst EQ 'IN6' OR inst EQ 'DCSasc' OR inst EQ 'MiBeMol') THEN $
		w_out=normalise_tof(w_in,inorm) $
	ELSE IF (inst EQ 'D7') THEN $
		w_out=normalise_d7(w_in,inorm,ikeep,zeroshift) $
	ELSE IF (inst EQ 'HET') THEN $
		w_out=normalise_HET(w_in, ei = ei) $
	ELSE BEGIN
		s = STRARR(9)
		s(0) = 'Normalise:  Instrument must be specfied as one of the following
		s(1) = ''
		s(2) = 'IN4'
		s(3) = 'IN5'
		s(4) = 'IN6'
		s(5) = 'D7'
		s(6) = 'HET'
		s(7) = 'MiBeMol'
		s(8) = 'DCSasc'
		i = WIDGET_MESSAGE(s,/ERROR)
		return,w_in
	ENDELSE

;-------------------------------------------------------------------------------
;Return parameters and exit

	PRINT,'normalise: '+outstring

finished:
	RETURN, w_out
	END
