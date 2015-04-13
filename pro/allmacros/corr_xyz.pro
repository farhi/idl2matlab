;-------------------------------------------------------------------------------
;*******************************************************************************

	FUNCTION corr_xyz, w_in0, filenum=iFR, fudge = fudge, frn

;For D7 polarisation analysis data only
;
;Corrects for finite polarization using a previously measured quartz run
;
;ARGUMENTS:
; filenum	: numor(s) of flipping ratio file(s) to use:
;		  e.g. filenum=27134 to use the file "quartz_27134.dat'
;		       filenum=[27134,27135,27136] to use multiple files
; fudge		: Specifies an array of fudgefactors (for depolarization 
;		  corrections) for each spectrum
;
;DIMENSIONS:
; TOF   -> w_in=w_out(nchannels,nspectra*nphases)
; NOTOF -> w_in=w_out(nspectra,nphases,nruns)
;
;COMMAND SYNTAX 
;	w2=corr_xyz(w1,filenum=<filenum>[,temp=<temp>])
;
;							KHA,JRS 27/6/02
;-------------------------------------------------------------------------------
;*******************************************************************************

	iprint = 0	; if iprint>0, show debugging messages

	IF iprint GT 0 THEN PRINT,'Start corr_xyz:'
	ON_IOERROR, finished

	take_datp, datp

	IF N_ELEMENTS(frn) GT 0 THEN iFR  = frn
	IF N_ELEMENTS(tmp) GT 0 THEN temp = tmp

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces

	sw = SIZE(w_in0)
	IF iprint GT 0 THEN PRINT, 'SIZE(w_in0) = ', sw

	par       = datp.p
	nspectra  = FIX(par[1])
	nphases   = FIX(par[2])
	nruns     = FIX(par[3])
	nchannels = FIX(par[6])
	TOF       = FIX(par[8])

	IF iprint GT 0 THEN BEGIN
		PRINT,'TOF=',TOF
		PRINT,'nspectra=',nspectra
		PRINT,'nphases=',nphases
		PRINT,'nruns=',nruns
	ENDIF

	IF nruns EQ 1 THEN x_in = datp.x ELSE x_in = datp.z
	y_in  = datp.y
	e_in0 = datp.e

	se = SIZE(e_in0)
	IF (se[0] NE sw[0]) OR (se[1] NE sw[1]) THEN BEGIN
		PRINT,'Error - SIZE(w_in0) = ',sw
		PRINT,'        SIZE(e_in0) = ',se
		PRINT,'      - must be the same size'
		GOTO, finished
	ENDIF

	IF (TOF EQ 0) AND (nphases EQ 1) THEN BEGIN
		w_in = FLTARR(nspectra,nphases,nruns)
		e_in = FLTARR(nspectra,nphases,nruns)
		w_in[*,0,*] = w_in0[*,*]
		e_in[*,0,*] = e_in0[*,*]
	ENDIF ELSE BEGIN
		w_in = w_in0
		e_in = e_in0
	ENDELSE

	nFR = N_ELEMENTS(iFR)
	FR_file = STRARR(nFR)
	corr_FR = 1

	CASE nFR OF
	0: BEGIN
		PRINT, 'Corr_xyz: no flipping ratio correction'
		iFR = 0
		corr_FR = 0
	   END
	1: BEGIN
		PRINT,'Corr_xyz: performing FR correction with quartz file:'
		FR_file[0] = 'quartz_'+STRTRIM(STRING(iFR),2)+'.dat'
		junk = FINDFILE(FR_file(0), COUNT = co)
		IF co EQ 0 THEN BEGIN
			FR_file[0] = '/home/vis/d7/lambda/QUARTZFILES/'+FR_file[0]
			junk = FINDFILE(FR_file(0), COUNT = co)
			IF co EQ 0 THEN PRINT, !ERR_STRING
		ENDIF
		PRINT,'          ',FR_file[0]
	   END
	ELSE: BEGIN
		PRINT,'Corr_xyz: performing FR correction with quartz files:'
		FOR i = 0, nFR - 1 DO BEGIN
			FR_file[i] = 'quartz_' + STRTRIM(STRING(iFR(i)),2) + '.dat'
			junk = FINDFILE(FR_file(i),COUNT = co)
			IF co EQ 0 THEN BEGIN
				FR_file[i] = '/home/vis/d7/lambda/QUARTZFILES/' + FR_file[i]
				junk=FINDFILE(FR_file[i],COUNT = co)
				IF co EQ 0 THEN PRINT, !ERR_STRING
			ENDIF
			PRINT,'          ',FR_file[i]
		ENDFOR
	   END
	ENDCASE

	IF iprint GT 0 THEN BEGIN
		PRINT,'nruns   = ',nruns
		PRINT,'corr_FR = ', corr_FR, ' nFR = ', nFR
	ENDIF

	IF (nruns EQ 1) AND (nFR GT 1) THEN BEGIN
		PRINT,'Corr_xyz: Error - w_in contains data from only one run,'
		PRINT,'          but more than one FRfile is given'
		GOTO, finished
	ENDIF

	IF iprint GT 0 THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
;Open and read from input files

	line=''

	IF corr_FR EQ 1 THEN BEGIN
		num = FLTARR(5)
		Flip  = FLTARR(nspectra,nphases/2,nruns)
		dFlip = FLTARR(nspectra,nphases/2,nruns)

		FOR irun =0, nFR - 1 DO BEGIN
			IF iprint GT 0 THEN PRINT,'Opening FR file: ',FR_file[irun]
			OPENR, lun, FR_file[irun], ERROR=err, /GET_LUN
			IF err NE 0 THEN BEGIN
				PRINT, !ERR_STRING
				GOTO, finished
			ENDIF
			FOR i = 0,2 DO READF, lun, line
			line = line + ' -1'
			READS, line, num
			IF (num[4] GT -1.1) AND (num[4] LT -0.9) THEN BEGIN
				F = FLTARR(4,32)
				IF nphases EQ 6 THEN BEGIN
					PRINT,'Corr_xyz: Error - only 2 quartz phases in file'
					GOTO, finished
				ENDIF
			ENDIF ELSE BEGIN
				F = FLTARR(8,32)
				IF (nphases EQ 2) AND (iprint GT 0) THEN $
					PRINT,'Corr_xyz: Taking Z quartz phase for FR correction'
			ENDELSE
			POINT_LUN, lun, 0
			FOR i = 0, 1 DO READF, lun, line
			READF, lun, F
			CLOSE, lun
			iphase = INDGEN(nphases/2)*2
			 Flip[*,iphase/2,irun] = TRANSPOSE(F[2+iphase,*])
			dFlip[*,iphase/2,irun] = TRANSPOSE(F[3+iphase,*])
		ENDFOR

		IF nruns GT 1 THEN BEGIN
			inrun = INTARR(nruns)
			IF nFR EQ 1 THEN BEGIN
				 Flip[*,*,0:nruns-1] =  Flip[*,*,inrun]
				dFlip[*,*,0:nruns-1] = dFlip[*,*,inrun]
			ENDIF ELSE IF (nruns GT 2) AND (nFR EQ 2) THEN BEGIN
				FOR irun=nruns-1,1,-1 DO BEGIN
					Flip[*,*,irun] =  Flip[*,*,0] +  (Flip[*,*,1] - $
					Flip[*,*,0])*FLOAT(irun)/FLOAT(nruns-1)
				       dFlip[*,*,irun] = dFlip[*,*,0] + (dFlip[*,*,1] - $
				       dFlip[*,*,0])*FLOAT(irun)/FLOAT(nruns-1)
				ENDFOR
			ENDIF
		ENDIF
	ENDIF

	IF (iprint GT 0) THEN PRINT,'End of "read from input files" section'

;-------------------------------------------------------------------------------
;Perform correction

	IF (N_ELEMENTS(fudge) GT 0) AND (corr_FR EQ 1) THEN BEGIN
		i = INDGEN(32)
		 Flip[i,*,*] = fudge[i]*Flip[i,*,*]
		dFlip[i,*,*] = fudge[i]*dFlip[i,*,*]
	ENDIF

	w_out = w_in
	e_out = e_in

	IF (TOF EQ 0) THEN BEGIN
		w_out = FLTARR(nspectra,nphases,nruns)
		e_out = FLTARR(nspectra,nphases,nruns)
		iphase = INDGEN(nphases/2)*2
		R0  = w_in[*,iphase,*]
		R1  = w_in[*,iphase + 1,*]
		F   = Flip[*,iphase/2,*]
		dR0 = e_in[*,iphase,*]
		dR1 = e_in[*,iphase + 1,*]
		dF  = dFlip[*,iphase/2,*]	
		w_out[*,iphase,*]     = R0 + (R0 - R1)/(F - 1.)
		e_out[*,iphase,*]     = SQRT((dR0*F/(F - 1))^2 + $
		     		        (dF*(R0 - R1)/(F - 1.)^2)^2)
		w_out[*,iphase + 1,*] = R1 - (R0 - R1)/(F - 1.)
		e_out[*,iphase + 1,*] = SQRT((dR1*F/(F-1))^2 + $
		     		        (dF*(R0 - R1)/(F - 1.)^2)^2)
		ispec = INDGEN(nspectra)
		i = WHERE(w_in[ispec,0,0] EQ 0. AND e_in[ispec,0,0] LT 0., nz)
		IF nz NE 0 THEN w_out[i,*,*] = 0.
		IF nz NE 0 THEN e_out[i,*,*] = -1.
	ENDIF ELSE BEGIN
		w_out  = FLTARR(nchannels,nphases*nspectra)
		e_out  = FLTARR(nchannels,nphases*nspectra)
		iphase = INDGEN(nphases/2)*2
		ispec  = INDGEN(nspectra,nphases)
		R0  = w_in[*,REFORM(ispec[*,iphase])]
		R1  = w_in[*,REFORM(ispec[*,iphase + 1])]
		dR0 = e_in[*,REFORM(ispec[*,iphase])]
		dR1 = e_in[*,REFORM(ispec[*,iphase + 1])]
		ones = FLTARR(nchannels) + 1.0
		F   = ones # Flip[*,iphase/2]
		dF  = ones # dFlip[*,iphase/2]	
		
		w_out[*,REFORM(ispec[*,iphase])]   = R0 + (R0 - R1)/(F - 1.)
		e_out[*,REFORM(ispec[*,iphase])]   = SQRT((dR0*F/(F - 1.))^2 + $
		     		    		     (dF*(R0 - R1)/(F - 1.)^2)^2)
		w_out[*,REFORM(ispec[*,iphase+1])] = R1 - (R0 - R1)/(F - 1.)
		e_out[*,REFORM(ispec[*,iphase+1])] = SQRT((dR1*F/(F - 1))^2 + $
		     		        	     (dF*(R0 - R1)/(F - 1.)^2)^2)
		ispec = INDGEN(nspectra)
		i = WHERE(TOTAL(w_in(*,ispec),1) EQ 0.)
		is = [iphase*nspectra + i, (iphase + 1)*nspectra + i]
		w_out[*,i] = 0.
		e_out[*,i] = -1.
	ENDELSE

	IF (iprint GT 0) THEN PRINT,'End of Correction section'

;-------------------------------------------------------------------------------
;Return parameters and exit

	IF (TOF EQ 0 AND nphases EQ 1) THEN BEGIN
		w_out0=FLTARR(nspectra,nruns)	& e_out0=w_out0
		w_out0(*,*)=w_out(*,0,*)
		e_out0(*,*)=e_out(*,0,*)
	ENDIF ELSE BEGIN
		w_out0=w_out
		e_out0=e_out
	ENDELSE
	datp.e=e_out0
	datp.z_tit='Intensity (arb. units)'

	IF (corr_FR EQ 0) THEN FRs='0' ELSE FRs=STRTRIM(STRING(iFR(0)),2)
	IF (nFR GT 1) THEN FOR i=1,nFR-1 DO FRs=FRs+'+'+STRTRIM(STRING(iFR(i)),2)

	datp.other_tit=datp.other_tit+' -cx('+FRs+')'

finished:
	CLOSE, 1

	IF (iprint GT 0) THEN PRINT,'End corr_xyz:'

	give_datp, datp

	RETURN, w_out0
	END
