function rd_d2b, INST , PATH , FILENAME , STATUS , DATP
;******* *************  
;**
;**	Standard call for a data-read function interfacing LAMP.
		  
;**	Return of the function
;**	 DATA     is an array of any dimensions and type containing the data values (spectra).

;**	Input  parameters:
;**	 INST(0)  is the file_type  (or instrument_name ) (string defined in customize tables).
;**	 INST(1)  is the file_group (or instrument_group) (string defined in customize tables).
;**	 PATH     is the full path where to find the data (string defined in customize tables).
;**	 FILENAME is the name of the data file.

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
 DATA  =0
 STATUS=11
 CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif

 ON_IOERROR, no_file
 OPENR,unit, PATH+FILENAME,/get_lun				;Open the data file

     STATUS=13
     ON_IOERROR, read_err
     wt=' '
     line=' '
     readf, unit , wt
     readf, unit , line

     IF strpos(line,'BANK') lt 0 THEN BEGIN  ; CASE .DAT FORMAT
;                                             *******************
        bidon=0 & zero= -220. & d2th= double(10.)
        reads, line, bidon, bidon, d2th
	readf, unit, zero
	readf, unit , line

	npoints=5000                ;make big array (2,5000) for data
	datall=intarr(2,npoints)
	ON_IOERROR, read_err2
	readf, unit, datall , format="(10(I2,I6))"

	print, "Table of data too small:",npoints

	read_err2:FREE_LUN,unit

	data=reform(datall(1,*))*1.
	err =reform(datall(0,*))*1.

	i=npoints-1
	WHILE data(i) le 0 DO  i=i-1
	data=data(0:i)
	err =err (0:i)
	npoints=i+1

	err=sqrt(data)/sqrt((err >.25))

     ENDIF ELSE BEGIN                        ; CASE .GSAS FORMAT
;                                             ********************
	npoints=0 & nlines=0 & nn=1 & zero= -220. & d2th= double(10.)
	vb1=0 & vb2=0
	line1=strmid(line, 4,80)
	line2=strmid(line,47,80)
	reads,line1, nn, npoints, nlines
	reads,line2+' -77 -77', zero, d2th, vb1, vb2
	d2th=d2th/100.
	zero=zero/100.

	if vb1 eq -77 then readf, unit , line ;one line to be read or not!


	datall=intarr(2,npoints)
	readf, unit, datall, format="(10(I2,I6))"

	data=reform(datall(1,*))*1.
	err=sqrt(data)/sqrt((datall(0,*)>.25))
     ENDELSE

	    xv       =  FINDGEN(npoints)*d2th+zero	;Normaly in data file
	    xt       =' 2 theta (degrees) '			;         .
	    par      = [        3.0       ,        5.5        ]	;	  .
	    ptxt     = ['First  parameter','Second parameter' ]	;	  .
	    pall     =  INDGEN(20,8)

      STATUS=0							;Status is ok
;     ********
      DATP={X:      xv,    $					;Pass those variables which were 
            W_TIT:  wt,    $ 				;read-in into the DATP structure
	           X_TIT:  xt,    $                   
	           P:      par,   $
	           PAR_TXT:ptxt,  $
            E:      err,        $
            N:      fltarr(npoints)+100000. , $
	           PV:     pall   }
;     **********************  					
	    

 RETURN, DATA
 read_err:  FREE_LUN,unit
 no_file: print,!err_string
 RETURN, DATA

 END
