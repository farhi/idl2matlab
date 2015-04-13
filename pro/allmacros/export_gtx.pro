pro export_gtx, FileNameInit , Data , XC=x, YC=y , ZC=z , E=e  , N=n   $
                                , PR=p, PV=pv, PAR_TXT=p_txt       $
                                , W_tit=wt  , X_tit=xt , Y_TIT=yt  $
                                , Z_tit=zt  , OTHER_TIT=ot
;** *********
;** 
;**	Standard call for a data-write procedure called by LAMP.
;**
;
;	This macro export_gtx exports data files readable for GSAS
;	Filename.gsa and Filename.gda (no line-feeds)
;   Depending on angular stepwidth and available ESDs one of three file formats will be written
;   so, no interpolation is necessary anymore!
;   At this stage, only the first diagram of a series is written out
;   Count rates and ESDs are written as integers
;   zero counts and negative angles are left out
;		  
;  *** modified by Thomas Hansen and Andrew Wills 31 August 2001		  
;  *** cleaned up by Thomas Hansen 19 September 2001
;  *** modern filenames established (Thomas Hansen, as done for Hermann Pauly)		  
;		  
;**	Keywords:		  
;**		  XC       =  vector of x coordinates.
;**		  YC       = [vector of y coordinates.]
;**		  ZC       = [vector of z coordinates.]
;**		  W_TIT    =   main title
;**		  X_TIT    = x axis title
;**		  Y_TIT    = y axis title
;**		  Z_TIT    = z axis title
;**		  OTHER_TIT=    sub title
;**		  N        = monitors
;**		  PR       = vector of parameter values
;**		  PAR_TXT  = string array of text associated to PR (same size)
;**		  PV       = an array of any dimensions containing other parameter values
;**		  E        = the errors associated to DATA (same size) or 0

;CATCH,stat & IF stat ne 0 then begin print,!err_string & return & endif
;ON_IOERROR,mis

Filename=FileNameInit
IF KEYWORD_SET(x) AND NOT KEYWORD_SET(e) THEN e=SQRT(data)
IF NOT (KEYWORD_SET(x) AND KEYWORD_SET(e) AND KEYWORD_SET(pv)) THEN BEGIN
  IF NOT KEYWORD_SET(datp) THEN BEGIN
    IF N_ELEMENTS(FilenameInit) GT 1 THEN BEGIN
      IF N_PARAMS() EQ 1 THEN data='data'
      tmp=FileNameInit
      Filename=data
      data=tmp
      flag=1
    ENDIF 
    TAKE_DATP,datpp
    datp=datpp
  ENDIF ELSE IF N_ELEMENTS(data) EQ 1 THEN BEGIN
    tmp=FileNameInit
    Filename=data
    data=tmp
  ENDIF 
  x=datp.x
  n=datp.n
  y=datp.y
  z=datp.z
  e=datp.e
  p=datp.p
  par_txt=datp.par_txt
  pv=datp.pv
  p_txt=datp.par_txt
  wt=datp.w_tit
  xt=datp.x_tit
  yt=datp.y_tit
  zt=datp.z_tit
  ot=datp.other_tit
ENDIF 
subs=N_ELEMENTS(data(0,*))
IF subs GT 1 THEN BEGIN
  FILENAME=FILENAME
   PRINT,subs,' subsets'
  IF (pv(29,subs-1)-pv(29,0)) EQ (subs-1) THEN numflag=1 ELSE numflag=0
ENDIF

OPENW ,unit, FileName+'.gsa', /get_lun 
OPENW ,script, FileName+'.cif', /get_lun 
PRINT ,FileName+'.gsa'
;*********************************************************************************************
;The input powder data file for GSAS is a direct access, fixed record length file. It contains
;a few header records followed by blocks of data each corresponding to an individual	
;powder pattern. Each block is preceded by its own header. All records must be 80
;characters in length. The main header has one or two records:
;(80A)TITLE
;A title record is first; the information is used by GSAS for titles only. GSAS can only use
;the first 66 characters of this record but it must be filled out to 80 characters.
;('Instrument parameter',60A)FNAME
;An optional record that must start with the 20 characters shown followed by the instrument
;parameter file name. If this record is omitted, GSAS requests this file name from the user
;in EXPEDT.
;*********************************************************************************************
PRINTF,script,'loop_'
PRINTF,script,'_riet_meas_datafile_name'
PRINTF,script,'_pd_meas_orientation_omega'
PRINTF,script,'_pd_meas_orientation_chi'
PRINTF,script,'_pd_meas_orientation_phi'
PRINTF,unit,strmid(wt+ot+'                                                                                        ',0,80)
;gout=strmid(wt+ot+'                                                                                        ',0,80)
;gout=gout+strmid('Instrument parameter'+' inst_d20.prm'+'                                                                                        ',0,80)
PRINTF,unit,strmid('Instrument parameter'+' inst_d20.prm'+'                                                                                        ',0,80)
step=0.0
PRINT,strmid(wt+ot+'                                                                                        ',0,80)
XX=x(*,0)
steps=x(1:N_ELEMENTS(XX)-1,0)-x(0:N_ELEMENTS(XX)-2,0)
step=Moment(steps,sdev=stepdev)
step=step(0)
PRINT,"Average angular steps: ", step,"+/-",stepdev, " degrees"
;** Begin of implementation for GSAS (containing ESDs)
FOR k=0,N_ELEMENTS(Data(0,*))-1 DO BEGIN
    WW=Data(*,k)
    EE=0
    IF KEYWORD_SET(E) THEN IF N_ELEMENTS(E) EQ N_ELEMENTS(Data) THEN EE=E(*,k)
    XX=X(*,k<(N_ELEMENTS(x(0,*))-1))
	index=WHERE(XX GT 0)
	IF KEYWORD_SET(E) THEN EE=EE(index)
	XX=X(index)
	WW=WW(index)
	index=WHERE(WW GT 0)
	IF KEYWORD_SET(E) THEN EE=EE(index)
 	XX=XX(index)
	WW=WW(index)
	IF KEYWORD_SET(pv) THEN BEGIN
	  PRINT,filename+'.gsa('+STRCOMPRESS(k,/RE)+')'+strcompress(pv(14,k))+strcompress(pv(15,k))+strcompress(pv(16,k))
      PRINTF,script,filename+'.gsa('+STRCOMPRESS(k,/RE)+')'+strcompress(pv(14,k))+strcompress(pv(15,k))+strcompress(pv(16,k))
    ENDIF
	;*********************************************************************************************
	;Then follows a number of data blocks each beginning with a header. The header record is
	;in free format and has different forms depending on the type of data that follows it. For
	;standard type data the header is:
	;('BANK',3I,A,4F,' STD')IBANK,NCHAN,NREC,BINTYP,(BCOEF(I),I=1,4)
	;The fields are separated by spaces and 'BANK' must be upper case. IBANK is the bank
	;number; if it is zero the data block is from the incident beam monitor. The value of IBANK
	;must be unique within a data file. For TOF data the value of IBANK must corrrespond to
	;the bank number found in the instrument parameter file for the detectors which produced
	;the data. This correspondence is not needed for CW data. NCHAN is the number of data
	;points in the block of data and NREC is the number of records. There are 10 data points
	;per record so NREC is NCHAN/10 rounded up. The value of BINTYP depends on the
	;way the stepwidths are determined. This format is suitable only when the esd associated
	;with each profile point can be calculated directly from the given intensity (i.e. sI = Ö ` I ). If
	;the esd can not be obtained in this way, then one of the other data formats which explicitly
	;input the esd should be used (see below).
	;If BINTYP is 'CONST' then the data has a constant stepwidth; BCOEF(1) is the offset for
	;the first step and BCOEF(2) is the stepsize, both in microseconds or centidegrees. If
	;BINTYP is 'LOG6' then the data has a specific logarithmic scaling for the stepwidth;
	;BCOEF(1) is the offset in microseconds and BCOEF(2) is either 102.4 or 409.6 which is
	;the allowed stepsize increment for the Los Alamos Model 6 TOF clock. If BINTYP equals
	;'TIME_MAP' then a complete time map which defines the step sizes and positions is read
	;from another section of this file (see below) and BCOEF(1) is the number of the time map
	;to be read. If BINTYP is 'LPSD' then BCOEF(1) is the nominal 2Q value in centidegrees
	;for the linear position sensitive detector (LPSD), BCOEFF(2) is the channel number for
	;this 2Q angle, and BCOEF(3) is the width of one channel in centidegrees at this position.
	;Usually CW data has BINTYP equal to 'CONST' or 'LPSD' and TOF data can be any one
	;of the BINTYP`s mentioned above except 'LPSD'.
	;This header is followed by 'NREC' records containing the data in the following format.
	;(10(I2,F6.0))(NCTR(I),YO(I),I=1,NCHAN)
	;where NCTR is the number of counters used to collect the data for each step and YO is the
	;number of counts per counter observed. If NCTR is zero or missing then it is assumed to
	;be one.
	;*********************************************************************************************
	outstr=$
		'BANK'+strcompress(k+1)+strcompress(string(N_ELEMENTS(WW)))+strcompress(string((N_ELEMENTS(WW)-1)/10+1))+$
		' CONST'+strcompress(string(round(100.*min(XX))))+strcompress(string(round(step*100.)))+' 0 0 STD     '
	;*********************************************************************************************
	;The first alternate form for the header is designed to handle data which has been corrected
	;for some instrumental effects (incident intensity, absorption, etc.) so that the weights can
	;no longer be derived directly from the intensities. The header is:
	;('BANK',3I,A,4F,' ESD')IBANK,NCHAN,NREC,BINTYP,(BCOEF(I),I=1,4)
	;As above the value of IBANK must be unique within a data file. Each data point consists
	;of the intensity and an esd for that intensity. Thus there are 5 data points per record and
	;NREC is NCHAN/5 rounded up. The values of BINTYP and BCOEF are the same as
	;described for the standard data header.
	;The data records are in the following form:
	;(10F8)(YOT(I),YE(I),I=1,NCHAN)
	;where YOT is the intensity and YE is the esd for YOT. The decimal point must be given to
	;force its proper placement in these values.
	;*********************************************************************************************
	IF N_ELEMENTS(EE) EQ N_ELEMENTS(WW) THEN outstr=$	
		'BANK'+strcompress(k+1)+strcompress(string(N_ELEMENTS(WW)))+strcompress(string((N_ELEMENTS(WW)-1)/5+1))+$
		' CONST'+strcompress(string(round(100.*min(XX))))+strcompress(string(round(step*100.)))+' 0 0 ESD     '
	;*********************************************************************************************
	;The second alternate form for the header is designed to handle data which has been
	;collected on a diffractometer where the steps between values is somewhat uneven (e.g.
	;TOF data collected on the diffractometers at the ISIS Facility, Rutherford-Appleton
	;Laboratory, UK) The header is:
	;('BANK',3I,A,4F,' ALT')IBANK,NCHAN,NREC,BINTYP,(BCOEF(I),I=1,4)
	;As above the value of IBANK must be unique within a data file. For TOF data the value of
	;IBANK must corrrespond to the bank number found in the instrument parameter file for the
	;detectors which produced the data. This correspondence is not needed for CW data. Each
	;data point consists of the TOF, intensity and an esd for that intensity. Thus there are 4 data
	;points per record and NREC is NCHAN/4 rounded up. The values of BINTYP and
	;BCOEF are the same as described for the standard data header with the addition of
	;BINTYP = 'RALF' which forces the use of the TOFT values below for the positions. In
	;this case BCOEF(1) is the starting TOF in msec*32, BCOEF(2) is the width of the first step
	;in msec*32, BCOEF(3) is the start of the log scaled step portion of the data in msec*32 and
	;BCOEF(4) is the resolution to be used in approximating the size of each step beyond
	;BCOEF(3).
	;The data records are in the following form:
	;(4(F8.0,F7.4,F5.4))((TOFT(I),YOT(I),YE(I),I=1,NCHAN)
	;where TOFT is in microsteps (TOF in pulses of width CLCKWDT times 32 or
	;centidegrees times 32), YOT is the normalized count and YE is the esd for YOT. Normally
	;the decimal point is left off the values and the format statement properly scales the values.
	;*********************************************************************************************
	IF ABS(stepdev) GT 1E-5 THEN outstr=$
		'BANK'+strcompress(k+1)+strcompress(string(N_ELEMENTS(WW)))+strcompress(string((N_ELEMENTS(WW)-1)/4+1))+$
		' RALF'+strcompress(string(round(32.*100.*XX(0))))+strcompress(string(round((XX(1)-XX(0))*100.*32.)))+strcompress(string(round(max(XX)*100.*32.)))+strcompress(string(round(step*100.*32.)))+' ALT     '
	outstr=outstr+'                                                                                              '
	PRINTF,unit,strmid(outstr,0,80)
;	gout=gout+strmid(outstr,0,80)
	outstr=''
	IF ABS(stepdev) LE 1E-5 THEN BEGIN
		IF N_ELEMENTS(EE) EQ N_ELEMENTS(WW) THEN BEGIN
			IF k EQ N_ELEMENTS(Data(0,*))-1 THEN PRINT,'GSAS ESD format'
			FOR i=0,N_ELEMENTS(WW)-1,5 DO BEGIN
				FOR j=0,5 DO BEGIN
					IF (i+j) LT N_ELEMENTS(WW) THEN BEGIN
						strng='        '+STRING(ROUND(WW(i+j)))
						outstr=outstr+STRMID(strng,STRLEN(strng)-8,STRLEN(strng))
						strng='        '+STRING(ROUND(EE(i+j)))
						outstr=outstr+STRMID(strng,STRLEN(strng)-8,STRLEN(strng))
					ENDIF
				ENDFOR
				PRINTF,unit,strmid(outstr,0,80)
;				gout=gout+strmid(outstr,0,80)
				outstr=''
			ENDFOR
		ENDIF ELSE BEGIN
			IF k EQ N_ELEMENTS(Data(0,*))-1 THEN PRINT,'GSAS STD (CONST) format'
			FOR i=0,N_ELEMENTS(WW)-1,10 DO BEGIN
				FOR j=0,10 DO BEGIN
					IF (i+j) LT N_ELEMENTS(WW) THEN BEGIN
						strng='  1'
						outstr=outstr+STRMID(strng,STRLEN(strng)-2,STRLEN(strng))
						strng='        '+STRING(ROUND(WW(i+j)))
						outstr=outstr+STRMID(strng,STRLEN(strng)-6,STRLEN(strng))
					ENDIF
				ENDFOR
				PRINTF,unit,strmid(outstr,0,80)
				PRINT,strmid(outstr,0,80)
;				gout=gout+strmid(outstr,0,80)
				outstr=''
			ENDFOR
		ENDELSE
	ENDIF ELSE BEGIN
		IF N_ELEMENTS(EE) NE N_ELEMENTS(WW) THEN EE=SQRT(WW)
		IF k EQ N_ELEMENTS(Data(0,*))-1 THEN PRINT,'GSAS ALT (RALF) format'
		FOR i=0,N_ELEMENTS(WW)-1,4 DO BEGIN
			FOR j=0,4 DO BEGIN
				IF (i+j) LT N_ELEMENTS(WW) THEN BEGIN
					strng='        '+STRING(ROUND(100.*32.*XX(i+j)))
					outstr=outstr+STRMID(strng,STRLEN(strng)-8,STRLEN(strng))
					strng=STRCOMPRESS((WW(i+j)),/RE)
					IF strlen(strng) LT 7 THEN strng=strmid('       '+strng,strlen(strng),strlen(strng)+7)
					outstr=outstr+STRMID(strng,0,7)
					strng=STRCOMPRESS((EE(i+j)),/RE)
					IF strlen(strng) LT 5 THEN strng=strmid('     '+strng,strlen(strng),strlen(strng)+5)
					outstr=outstr+STRMID(strng,0,5)
				ENDIF
			ENDFOR
			PRINTF,unit,strmid(outstr,0,80)
;			gout=gout+strmid(outstr,0,80)
			outstr=''
		ENDFOR
	ENDELSE
ENDFOR
FREE_LUN,script
FREE_LUN,unit
;OPENW ,unit, FileName+'.gda', /get_lun 
;PRINTF,unit,gout
;FREE_LUN,unit

return

mis:print,!err_string
end
