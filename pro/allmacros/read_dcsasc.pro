; *********************************************************************************
; S. Rols August 2001
; for question : srols@anl.gov
; This program reads ascii dcs file derived
; from the dcs_binary2ascii routines written by J. Copley
; N.B: all the experimental parameter were saved ... will read only a few of them
; *********************************************************************************
function read_dcsasc, INST , PATH , FILENAME , STATUS , DATP

 DATA  =0
 STATUS=11	;Handle non-IO errors
 CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif

;** ***********************************
;** newpath and newfile construction **
;** ***********************************
    NEWPATH=PATH
    NEWFILE=FILENAME(0)
    if strlen(NEWFILE) eq 6 then begin
;	Input PATH     is of the form "/........../2001/"
;	Input FILENAME is of the form "MMDDnb"
;	--------------------------------------
	ii=strlen(NEWPATH)
	yy=strmid(NEWPATH,ii-5,4)
	mm=strmid(NEWFILE,0,2)
	dd=strmid(NEWFILE,2,2)
	nb=strmid(NEWFILE,4,2)
	if (mm gt 12) or (dd gt 31) then return,DATA
	NEWPATH=strmid(NEWPATH,0,ii-1)+mm+strmid(NEWPATH,ii-1,1)
	NEWFILE=yy+mm+dd+'_'+nb+'.dcs'
    strrunn=yy+mm+dd+nb
	run_number=float(strrunn)
	rr=findfile(NEWPATH+NEWFILE+'.gz.',count=n)
	if n gt 0 then NEWFILE=NEWFILE+'.gz.'
    endif else begin
    ii=strlen(NEWFILE)
    yy=strmid(NEWFILE,0,4)
	mm=strmid(NEWFILE,4,2)
	dd=strmid(NEWFILE,6,2)
	nb=strmid(NEWFILE,9,2)
	strrunn=yy+mm+dd+nb
	run_number=float(strrunn)
	endelse

;   Open  the data file, use EXECUTE for compatibility with Idl5.2
;   --------------------------------------------------------------
;   ON_IOERROR, no_file
    if strpos(NEWFILE,'.gz') gt 0 then cmps=',/compress' else cmps=''
    unit=-1
    rr=EXECUTE('OPENR,unit, NEWPATH+NEWFILE,/get_lun'+cmps)
    if unit le 0 then return,DATA

;** **********************
;** End of construction ******************************************
;** **********************


      STATUS=13
      ON_IOERROR, read_err
      iunit=unit

;** **************************************
;** Reading in the .asc files and dealing
;** with data and parameter
;** **************************************
twolines=strarr(2)
while (not eof(iunit)) do begin
readf, iunit, twolines
;print, twolines
strname=strcompress(twolines(0),/remove_all) & len0=strlen(strname)-6
strtype=strcompress(twolines(1),/remove_all) & len1=strlen(strtype)-6
;print, strname
;print, strtype
strname=strmid(strname,6,len0)
strtype=strmid(strtype,6,len1)
;print, strname
;print, strtype
; On a lu le nom de la variable dans strname
; On a lu le type de cette variable et on va creer une variable tampon du meme type: vartamp
case strtype of
"scalar": begin
vartamp=0.
end
"matrix": begin
readf,iunit, twolines
twolines=strcompress(twolines,/remove_all)
strrow=twolines(0) & len0=strlen(strrow)-6
strcolumn=twolines(1) & len1=strlen(strcolumn)-9
strrow=strmid(strrow,6,len0)
strcolumn=strmid(strcolumn,9,len1)
nrow=float(strrow) & ncolumn=float(strcolumn)
vartamp=fltarr(nrow,ncolumn)
end
"stringarray": begin
strnumel=strarr(1)
readf, iunit, strnumel
strnumel=strcompress(strnumel,/remove_all) & len0=strlen(strnumel(0))-10
strnumel=strmid(strnumel,10,len0)
nelement=float(strnumel(0))
;print, strnumel
;print, nelement
if (nelement gt 1.) then begin
vartamp=strarr(2*nelement)
endif else begin
strlength=strarr(1)
readf,iunit, strlength
vartamp=strarr(1)
endelse
end
endcase
case strname of
"CH_INPUT": begin ; des frequences de rotation des choppers
readf, iunit,vartamp
w_chop=vartamp(1) & ratio1=vartamp(2)
ratio2=vartamp(3) & ratio3=vartamp(4)
w_aoverlap=vartamp(5)
end
"COMMENTS": begin ; lecture du titre du run
title=''
readf, iunit, vartamp
title=vartamp(0)
end
"DET_DIS" : begin ; lecture de la distance Sample-detector
readf, iunit, vartamp
dist_sd=vartamp
end
"DURATION": begin ; lecture de la duree du run
readf, iunit, vartamp
duration=vartamp
end
"HISTOHIGH": begin ; lecture du moniteur
readf, iunit, vartamp
spec_mon=vartamp(*,3)
end
"HISTODATA": begin ;lecture des donnees
readf, iunit, vartamp
DATA=vartamp
end
"START_DATE": begin ; lecture de la date START
startd=''
readf, iunit, vartamp
startd=vartamp
end
"STOP_DATE": begin ; lecture de la date STOP
stopd=''
readf, iunit, vartamp
stopd=vartamp
end
"CH_WL": begin ; lecture de la long. d'onde incidente
readf, iunit, vartamp
lambda=vartamp
end
"TSDMIN": begin ; lecture du TOF-delay
readf, iunit, vartamp
tof_delay=vartamp
print, 'tof_delay',vartamp,tof_delay
end
"DETSUM": begin ; lecture des moniteurs
readf, iunit, vartamp
mon1=vartamp(915,0)
mon2=vartamp(916,0)
end
else: readf,iunit, vartamp ; on lit pour rien ATTENTION AUX STRING ARRAY !!!
endcase
endwhile
Result=dialog_message('Lecture successful !!',/information)
close, iunit

; ** Correspondance between detector # and position (in 2 theta)
; ** Reading from Jeremy Cook's file det_pos_jcc2.dat (col1=det#;col5=2Theta)
; ** This array doesn't appear in the raw data file ... have to enter it "by hand"
; ********************************************************************************

    vartamp=fltarr(10,913)
    get_lun,unit_angle
    ;angle_file=dialog_pickfile(title='Open the angle file',filter='*.dat')
    angle_file='angles.dat'
    openr,unit_angle,angle_file
    readf,unit_angle,vartamp
    close,unit_angle
    free_lun,unit_angle
    Y_wk=vartamp(9,*)

    dist_sd=4010.0
	X_wk=findgen(1024)+1.
;	S   =size(DATA)
;	NN= indgen(20)
	ch_width=12.                                       ;!!!!!!! ch_width used 07/2001 to be modified !!!!!!!
	nrji=81.799/(lambda^2)
	tel=sqrt(5.2267*(1.e-6*dist_sd)^2*lambda^2/81.799) ;temps de vol S-D pour un neutron elastique en s
	nch_el=(1.e6*tel-tof_delay)/ch_width
	inch_el=fix(nch_el)


; ** Dealing with parameters
; ** ************************
	PP=fltarr(15)
	PT=strarr(15)

    PT(0)='Run number'                         &    PP(0)=run_number
	PT(1)='Chopper speed (rpm)'                &    PP(1)=w_chop
	PT(2)='Ratio 1'                            &    PP(2)=ratio1
	PT(3)='Ratio 2'                            &    PP(3)=ratio2
	PT(4)='Ratio 3'                            &    PP(4)=ratio3
	PT(5)='Anti-overlap speed (rpm)'           &    PP(5)=w_aoverlap
	PT(6)='Sample to Detector distance (mm)'   &    PP(6)=dist_sd
	PT(7)='Duration of the run (s)'            &    PP(7)=duration
	PT(8)='Incident wavelength (A)'            &    PP(8)=lambda
	PT(9)='Incident energy (meV)'              &    PP(9)=nrji
	PT(10)='Calculated elastic channel #'      &    PP(10)=inch_el
	PT(11)='TOF delay (us)'                    &    PP(11)=tof_delay
	PT(12)='Sum Monitor 0 (counts)'            &    PP(12)=mon1
	PT(13)='Sum Monitor 1 (counts)'            &    PP(13)=mon2
	PT(14)='Channel Width (us)'				   &	PP(14)=ch_width




      STATUS=0							;Status is ok
;     ********

  DATP={X:      X_wk,                $
        Y:      Y_wk,                $
        W_TIT:  title,               $
        X_TIT:  'Channel #',        $
        Y_TIT:  'Detector angle',         $
        P:      PP,                  $
        PAR_TXT:PT,                  $
        N:      spec_mon,            $
        TIME:   startd+' to '+stopd}

;     **********************

 read_err:  FREE_LUN,unit					;Free the unit number
 no_file:   FREE_LUN,unit
 RETURN, DATA							;Return the data values
;************

 END
;******* ***********
;**
;**	Standard call for a data-read function interfacing LAMP.

;**	Return of the function
;**	 DATA     is an array of any dimensions and type containing the data values (spectra).

;**	Input  parameters:
;**	 INST(0)  is the file_type  (or instrument_name ) (string defined in customize tables).
;**	 INST(1)  is the file_group (or instrument_group) (string defined in customize tables).
;**	 INST(2)  is "1" if raw  button is set.
;**	 PATH     is the full path where to find the data (string defined in customize tables).
;**	 FILENAME is the name of the data file.
;**		  if FILENAME(1) exists,  this is the requested image number in the file.

;**	Output parameters:
;**	 STATUS   is the returned error code you can choose from the following list:
;**		  0 =' Successfull read'
;**		  1 =' Client/server on local node not established'
;**		  2 =' Client/server on router node not established'
;**		  3 =' The local  node cannot access the server node'
;**		  4 =' The router node cannot access the server node'
;**		  5 =' VME memory read error'
;**		  7 =' Sequence error in data transfer'
;**		  9 =' Parameter error'
;**		  10=' Router is busy with other transfer'
;**		  11=' Cant open the file or file not found'
;**		  13=' Data file incomplete'
;**		  14=' Bad instrument data definition'
;**		  24=' Cant read the file'.
;**
;**	 DATP     is a structure defined as follow: (all tags are OPTIONAL)
;**		  DATP.X        = vector of x coordinates.
;**		  DATP.Y        = vector of y coordinates.
;**		  DATP.Z        = vector of z coordinates.
;**		  DATP.W_TIT    =   main title
;**		  DATP.X_TIT    = x axis title
;**		  DATP.Y_TIT    = y axis title
;**		  DATP.Z_TIT    = z axis title
;**		  DATP.OTHER_TIT=    sub title
;**		  DATP.N        = monitors
;**		  DATP.P        = vector of float parameter values up to 41
;**		  DATP.PAR_TXT  = string array of text associated to DATP.P (same size)
;**		  DATP.PV       = an array of any dimensions containing other parameter values
;**		  DATP.E        = the errors associated to DATA (same size)
;**		  DATP.TIME     = string date of the experiment.
