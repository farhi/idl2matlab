function rdspe, INST , PATH , FILENAME , STATUS , DATP
;******* *****
;**
;**	Standard call for a data-read function interfacing LAMP.
		  
;**	Return of the function
;**	 DATA     is an array of any dimensions and type containing the data values (spectra).

;**	Input  parameters:
;**	 INST(0)  is the file_type  (or instrument_name ) (string defined in customize tables).
;**	 INST(1)  is the file_group (or instrument_group) (string defined in customize tables).
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
;**		  DATP.P        = vector of parameter values up to 31
;**		  DATP.PAR_TXT  = string array of text associated to DATP.P (same size)
;**		  DATP.PV       = an array of any dimensions containing other parameter values
;**		  DATP.E        = the errors associated to DATA (same size)
;**		  DATP.TIME     = string date of the experiment.

 common c_rdid , dzap, pzap, pzip ,pzup

 DATA  =0
 UNIT  =-1
 STATUS=11
 CATCH,stat &	IF stat ne 0 then begin print,!err_string
 		IF unit gt 0 THEN FREE_LUN,unit & RETURN, DATA & endif
 IF n_elements(pzup) eq 0 THEN pzup=""				;Check sub-directory & filename
 MYFILE=FILENAME
 MYPATH=PATH
 abc   =0
 dvd   =sys_dep("DIVIDER")
 IF strpos(strlowcase(MYFILE), '.spe')  ge 0 THEN BEGIN
 			  IF  strpos(MYFILE,'-') eq 2 then $
			  IF rstrpos(MYFILE,'-') eq 5 then $
					     MYPATH=MYPATH+strmid(MYFILE,0,8)+dvd
 ENDIF ELSE		  IF pzup ne "" THEN BEGIN  ON_IOERROR,misfix
					     MYPATH=MYPATH+strmid(pzup,0,8)  +dvd
					     abc=fix(MYFILE)  & tmp=strtrim(string(abc),2)
					     if abc lt 100 then tmp= '0'+tmp
					     if abc lt  10 then tmp= '0'+tmp
					     MYFILE=pzup+tmp+'.spe' &  misfix: & ENDIF
 ON_IOERROR, no_file
 OPENR,unit, MYPATH+MYFILE,/get_lun				;Open the data file

      STATUS=13
      ON_IOERROR, read_err
      IF !version.os_family eq 'unix' then swap=1 else swap=0
      HEADER =BYTARR(4100)
      READU,unit,HEADER						;Read the header

	nx = fix (header, 42)	& if swap then BYTEORDER,nx,/sswap
	ny = fix (header, 656)	& if swap then BYTEORDER,ny,/sswap
	nf = long(header, 1446) & if swap then BYTEORDER,nf,/lswap & nf = nf>1
	ty = fix (header, 108)	& if swap then BYTEORDER,ty,/sswap
	case ty of
        0: DATA = fltarr(nx, ny, nf)
        1: DATA = lonarr(nx, ny, nf)
        2: DATA = lonarr(nx, ny, nf)
        3: DATA = intarr(nx, ny, nf)
        else:
	endcase
	READU,unit,DATA & DATA=reform(DATA)			;Read the data

      IF swap then begin
	    if (ty eq 1) or (ty eq 2) then BYTEORDER,DATA,/lswap
	    if (ty eq 3)              then BYTEORDER,DATA,/sswap ;Swap the bytes
      ENDIF
      ;if (ty eq 3)  then DATA = long(DATA) and 'ffff'x  ;does not have unsigned int

      index=where ( DATA lt 0 ) & DATA =long  ( DATA )
      IF index(0) ge 0 then DATA(index)=65536+DATA(index)

      STATUS=0							;Status is ok
;     ********

;offset = 3000
;xcal = { $
;        offset:         double(header, offset), $
;        factor:         double(header, offset+8), $
;        current_unit:   byte  (header, offset+16), $
;        reserved1:      byte  (header, offset+17), $
;        string1:        byte  (header, offset+18, 40), $
;        reserved2:      byte  (header, offset+58, 40), $
;        calib_valid:    byte  (header, offset+98), $
;        input_unit:     byte  (header, offset+99), $
;        polynom_unit:   byte  (header, offset+100), $
;        polynom_order:  byte  (header, offset+101), $
;        calib_count:    byte  (header, offset+102), $
;        pixel_pos:      double(header, offset+103, 10), $
;        calib_value:    double(header, offset+183, 10), $
;        polynom_coeff:  double(header, offset+263, 6), $
;        laser_position: double(header, offset+311), $
;        reserved3:      byte  (header, offset+319), $
;        new_calib_flag: byte  (header, offset+320), $
;        calib_label:    byte  (header, offset+321, 81), $
;        expansion:      byte  (header, offset+402, 87) $
;}

;offset = 3489
;ycal = { $
;    offset:         double(header, offset), $
;    factor:         double(header, offset+8), $
;    current_unit:   byte  (header, offset+16), $
;    reserved1:      byte  (header, offset+17), $
;    string1:        byte  (header, offset+18, 40), $
;    reserved2:      byte  (header, offset+58, 40), $
;    calib_valid:    byte  (header, offset+98), $
;    input_unit:     byte  (header, offset+99), $
;    polynom_unit:   byte  (header, offset+100), $
;    polynom_order:  byte  (header, offset+101), $
;    calib_count:    byte  (header, offset+102), $
;    pixel_pos:      double(header, offset+103, 10), $
;    calib_value:    double(header, offset+183, 10), $
;    polynom_coeff:  double(header, offset+263, 6), $
;    laser_position: double(header, offset+311), $
;    reserved3:      byte  (header, offset+319), $
;    new_calib_flag: byte  (header, offset+320), $
;    calib_label:    byte  (header, offset+321, 81), $
;    expansion:      byte  (header, offset+402, 87) $
;}
;x_calibration = poly(findgen(nx), xcal.polynom_coeff(0:xcal.polynom_order))
;y_calibration = poly(findgen(ny), ycal.polynom_coeff(0:ycal.polynom_order))
 
;comments= byte  (header, 200, 80, 5) & comments=strtrim(string (comments),2)+' '
;date    = byte  (header, 20 , 10)    & date    = string(date)
;hour    = fix   (header, 30) & if swap then BYTEORDER,hour  ,/sswap
;minute  = fix   (header, 32) & if swap then BYTEORDER,minute,/sswap
;second  = fix   (header, 38) & if swap then BYTEORDER,second,/sswap
;date    = date + ":" + string(hour,   format='(i2.2)') $
;               + ":" + string(minute, format='(i2.2)') $
;               + ":" + string(second, format='(i2.2)')

x_calibration =       indgen(nx)+1
y_calibration =       indgen(ny)+1

offset  = 3566

user    = byte  (header, offset+0  ,10)  & user   =strtrim(string (user),2)
date    = byte  (header, offset+50 ,10)  & date   =strtrim(string (date),2)
time    = byte  (header, offset+60 ,10)  & time   =strtrim(string (time),2)
sample  = byte  (header, offset+70 ,10)  & sample =strtrim(string (sample) ,2)
comment = byte  (header, offset+80 ,50)  & comment=strtrim(string (comment),2)
Diffrac = byte  (header, offset+130)

Collima = byte  (header, offset+131,4 )  & if swap then Collima=reverse(Collima)
Collima = float (Collima,0,1)

Axname  = byte  (header, offset+169,100) & Axname=reform(Axname,10,10)
Axname  = strtrim(string (Axname),2)+' '


Omeg=fltarr(10)
for i=0,9 do begin
Omega   = byte  (header, offset+279+i*4,4 )  & if swap then Omega  =reverse(Omega)
Omega   = float (Omega  ,0,1) & Omeg(i)=Omega
endfor
if omeg(0) ne 0 then ZOB=omeg(0) else ZOB=0.0001

if comment eq "" then comment=MYFILE+' ('+sample+')'

      DATP={X:     x_calibration, X_TIT:'X detector',$
            Y:     y_calibration, Y_TIT:'Y detector',$
	    Z:     ZOB ,$
	    PAR_TXT: ['file number ',Axname  ,'Diffractometer ','Collimator (mm) '] ,$
	    P:       [ abc          , Omeg   , Diffrac         , Collima] ,$
            TIME:  date,$
            W_TIT: comment,$
            other_TIT:  MYFILE+' User:'+user+' Sample:'+sample+' Time:'+date+' '+time}
;     **********************  					

 read_err:  FREE_LUN,unit					;Free the unit number
 no_file:   IF unit lt 0 THEN print,!err_string, string(7b)
 
 RETURN, reverse(DATA,2)					;Return the data values
;************

 END
