function read_mibemol, INST , PATH , FILENAME , STATUS , DATP
;*****************************************************************************
;** Reading Data files from raw MiBeMol data files
;** input file #1 : ????.asc file = contains raw DATA (S(n_channel,2theta))
;** input file #2 : ????.ust files = contains PARAMETERS of the run # ????
;**
;** Stephane Rols 11/2001 srols@anl.gov
;** modified by S. Rols (put free_lun ...) on 03/18/02
;*****************************************************************************

; ** Initializing of some parameters and arrays
; ** *******************************************
print,'Begin reading ...'
iprint=0
DATA  =0
PAR_TXT=STRARR(31) & P=fltarr(31)
n_channels=512 & str_n_channels=STRCOMPRESS(STRING(n_channels-4),/REMOVE_ALL)
xv=FINDGEN(n_channels-4)+1.							; The channel array
STATUS=7
;CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif

; ** First Step : Read the parameters of the run # FILENAME contained
; ** in the FILENAME.ust file
; ** ****************************************************************

ON_IOERROR, no_file
FILENAME=STRMID(FILENAME,2,4)
print, PATH+'d'+FILENAME+'.ust'
OPENR,unit, PATH+'d'+FILENAME+'.ust',/get_lun                   ;Open the ????.ust file
print, PATH+'d'+FILENAME+'.ust'

; ** File is open ... now read it:

ON_IOERROR, read_err
STATUS=13
str='stringv'
READF,unit,str										; Read the first blank line
READF,unit,str										; Read the second line ... but don't record it ... we already have the run # in FILENAME
READF,unit,str									; Read start date and time line
str_date=STRMID(str,0,10) & str_time=STRMID(str,11,8)
READF,unit,str										;Read the second blank line
READF,unit,str										; Read the '# of subrun asked' line ... don't care
READF,unit,str										; Read the 'current # of subrun ' line ... don't care
READF,unit,str									; Read the Ratemeter control line
str_ratemT=STRMID(str,0,17) & str_act=STRMID(str,18,6)
READF,unit,str										; Read the Temperature control param line ... don't care
READF,unit,str										; Read the temperature asked ... don't care
READF,unit,str									; Read the initial measured temperature of the sample
str=STRCOMPRESS(str,/REMOVE_ALL) & str_temp_i=STRMID(str,29,7)
READF,unit,str									; Read the final measured temperature of the sample
str=STRCOMPRESS(str,/REMOVE_ALL) & str_temp_f=STRMID(str,27,7)
READF,unit,str										; Read the final temp regulation line ... don't care
READF,unit,str									; Read the TOF delay
str=STRCOMPRESS(str,/REMOVE_ALL) & str_delay=STRMID(str,9,STRLEN(str)-9)
READF,unit,str									; Read the channel width value
str=STRCOMPRESS(str,/REMOVE_ALL) & str=STRMID(str,16,STRLEN(str)-16) & str_ch_width=strcompress(string(float(str)/10.),/REMOVE_ALL)
READF,unit,str										; Read the time preset line ... don't care
READF,unit,str										; Read the Exp. param line ... don't care
READF,unit,str										; Read dashed line
READF,unit,str									; Read the run title
str_rtitle=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read the user names
str_uname=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read the incident wavelength
str_lambda=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read the sample holder description
str_s_holder=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read the sample angle
str_sangle=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read the frequency of chopper 4
str_fre_ch4=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read value of ratio
str_ratio=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read value of ratio2 (chopper #)
str_ratio2=STRMID(str,23,STRLEN(str)-23)
READF,unit,str									; Read the number of detector used
str_n_spectra=STRMID(str,30,STRLEN(str)-30)
READF,unit,str										; Read "position of the detectors:" line

; ** Now read the value of the 2theta angle in degree for each detector
n_spectra=FIX(str_n_spectra) & n_spectra=n_spectra(0)
yv=FLTARR(n_spectra) & str_yv=STRARR(n_spectra)
nlines=1 & nlines=n_spectra/10
;FOR i=0,7 DO READF,unit,str,FORMAT='(A6)'
FOR i=1,nlines DO BEGIN
READF,unit,str,FORMAT='(A60)'
FOR j=0,9 DO BEGIN
yv((i-1)*10+j)=FLOAT(STRCOMPRESS(STRMID(str,j*6,6),/REMOVE_ALL))
ENDFOR
ENDFOR
IF nlines NE n_spectra*10 THEN BEGIN
imax=n_spectra-nlines*10
str_format="'(A"+STRCOMPRESS(STRING(6*imax),/REMOVE_ALL)+")'" & str_format=str_format(0)
READF,unit,str,FORMAT='(A60)'
FOR i=0,imax-1 DO yv(10*nlines+i)=FLOAT(STRCOMPRESS(STRMID(str,i*6,6),/REMOVE_ALL))
ENDIF
free_lun,unit

IF iprint THEN PRINT,yv

; ** Second Step : Read the DATA of the run # FILENAME contained
; ** in the FILENAME.asc file
; ** ****************************************************************

ON_IOERROR, no_file
print, PATH+'d'+FILENAME+'.asc'
OPENR,unit, PATH+'d'+FILENAME+'.asc',/get_lun                   ;Open the ????.asc file
print, PATH+'d'+FILENAME+'.asc'

; ** File is open ... now read it:

DATA_LECT=FLTARR(n_channels,n_spectra)                    ;=S(t,2theta)
ON_IOERROR, read_err
STATUS=13
READF,unit,DATA_LECT
free_lun,unit
;CLOSE,unit

; ** Data are read ... Now extract the monitor and the error arrays
; ** and pass into the DATP structure
; ** ***************************************************************

DATA=DATA_LECT(0:n_channels-5,1:n_spectra-1)
DATPN=DATA_LECT(n_channels-4,*)
DATAE=SQRT(DATA)
DATPPV=DATA_LECT(n_channels-3,1:n_spectra-1)
DATPX=xv & DATPY=yv(1:n_spectra-1)
DATPW_TIT=str_rtitle
DATPX_TIT='Channel #'
DATPY_TIT='2 theta (degree)'
DATPOTHER_TIT=str_uname
DATPTIME=str_date

; ** Also find the elastic channel and pass it into the DATP str
; ** ****************************************************************

x_in=xv & w_in=DATA &
i=INDGEN(n_spectra)
xtot=x_in & ytot=TOTAL(w_in(*,i),2) & etot=SQRT(ytot)
y0=MAX(ytot,i0)   & xmin=xtot((i0-20)>0) & xmax=xtot((i0+20)<(n_channels-1))
fitgauss, xtot, ytot, etot, xmin, xmax, gauss, dgauss
chel=gauss(2) & str_chel=STRCOMPRESS(STRING(chel),/REMOVE_ALL)   ; elastic channel

; ** Pass into the DATP structure
; ** *****************************

; ** Now write the read parameters in the DATP.P corresponding array
; ** ***************************************************************

P(0)=0.             &         PAR_TXT(0)='Not used                           ='
;P(0)=str_rtitle       &         PAR_TXT(0)='Experimental Title                 ='
P(1)=0.             &         PAR_TXT(1)='Not used                           ='
P(2)=float(str_n_channels)   &         PAR_TXT(2)='Number of reserved channels        =' 
P(3)=0.             &         PAR_TXT(3)='Not used                           ='
;P(3)=str_date         &         PAR_TXT(3)='Start Date                         ='
P(4)=0.             &         PAR_TXT(4)='Not used                           ='
;P(4)=str_time         &         PAR_TXT(4)='Start Time                         ='
P(5)=0.             &         PAR_TXT(5)='Not used                           ='
;P(5)=str_act          &         PAR_TXT(5)='Ratemeter Control                  ='
P(6)=float(str_temp_i)	  	  &		      PAR_TXT(6)='Initial Temperature                ='
P(7)=float(str_temp_f)	   	  &		      PAR_TXT(7)='Final Temperature                  ='
P(8)=float(str_sangle)	      &		      PAR_TXT(8)='Sample angle (deg.)                ='
P(9)=float(str_chel)         &         PAR_TXT(9)='Elastic peak position (channel)    ='
P(10)=float(FILENAME)        &         PAR_TXT(10)='Numor                              ='
P(11)=float(str_temp_f)      &         PAR_TXT(11)='Sample temperature (K)             ='
P(12)=0.             &         PAR_TXT(12)='Not used                           ='
;P(12)=str_uname		    &		      PAR_TXT(12)='User Name                          ='
P(13)=float(str_fre_ch4)	    &		      PAR_TXT(13)='Frequency Chopper 4                ='
P(14)=float(str_ratio)	  	  &		      PAR_TXT(14)='Ratio                              ='
P(15)=float(str_ratio2)	    &		      PAR_TXT(15)='Ratio 2                            ='
P(16)=0.            &         PAR_TXT(16)='Not used                           ='
P(17)=0.             &         PAR_TXT(17)='Not used                           ='
;P(17)=str_s_holder	  &		      PAR_TXT(17)='Sample Holder type                 ='
P(18)=float(str_ch_width)	  &		      PAR_TXT(18)='Channel width (microsec.)          ='
P(19)=510.           &         PAR_TXT(19)='Number of channels used            ='
P(20)=float(str_delay)		    &		      PAR_TXT(20)='TOF delay (microsec.)              ='
P(21)=float(str_lambda)      &		      PAR_TXT(21)='Wavelength (angstroms)             ='
P(22)='0.'            &         PAR_TXT(22)='Not used                           ='
P(23)='0.'            &         PAR_TXT(23)='Not used                           ='
P(24)='0.'            &         PAR_TXT(24)='Not used                           ='
P(25)='0.'            &         PAR_TXT(25)='Not used                           ='
P(26)='0.'            &         PAR_TXT(26)='Not used                           ='
P(27)=3.58		      &		      PAR_TXT(27)='Distance Det - Sample (meter)      ='
P(28)='0.'            &         PAR_TXT(28)='Not used                           ='
P(29)='0.'            &         PAR_TXT(29)='Not used                           ='
P(30)=float(str_n_spectra)   &		      PAR_TXT(30)='Number of angles                   ='


DATP={X:			DATPX,			$
	  Y:			DATPY,			$
	  W_TIT:		DATPW_TIT,		$
	  X_TIT:		DATPX_TIT,		$
	  Y_TIT:		DATPY_TIT,		$
	  N:			DATPN,			$
	  E:			DATAE,			$
	  OTHER_TIT:	DATPOTHER_TIT,	$
	  TIME:			DATPTIME,		$
	  PV:			DATPPV,			$
	  P:			P,				$
	  PAR_TXT:		PAR_TXT }
STATUS=0
print,'Reading successful !'
read_err:  FREE_LUN,unit                             ;Free the unit number
no_file:

RETURN, DATA                                         ;Return the data values
;************
END
