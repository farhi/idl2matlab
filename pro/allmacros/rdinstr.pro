function rdinstr, INST , PATH , FILENAME , STATUS , DATP
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
 if !version.release ge '5.0' then ii=execute('FORWARD_FUNCTION RDRUN')
 DATA  =0
 STATUS=11
 CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif

 ON_IOERROR, no_file
 OPENR,unit, PATH+FILENAME,/get_lun				;Open the data file

     STATUS=13
     ON_IOERROR, read_err
  line='text'
  READF,unit,line
 IF INST(0) EQ 'd1b' OR INST(0) EQ 'fil' THEN BEGIN
    p=FLTARR(40)
    par_txt=STRARR(40)
    READF,unit,line
    tit=line
    READF,unit,line
    bid=1
    numor=1000000
    READS,line,bid,numor
    print,' *** Fullprof INSTR format 3 (D1B "new" format created by fild1b/d20) *** '
    sub=PATH+FILENAME
    READF,unit,line
    bid=100
    step=0.1
    start=-2.2
    stop=157.7
    mon=100000.
    time=0.
    om=0.
    chi=0.
    phi=0.
    float_bid=0.
    wav=0.
    sp=0.
    tr=0.
    ts=0.
    nd=1600
    READS,line,mon,time,start,om,chi,phi,float_bid,wav,step,sp,tr,ts
    READF,unit,line
    READS,line,nd
    p(10)=sp
    p(11)=tr
    p(12)=ts
    p(13)=start
    p(14)=om
    p(15)=chi
    p(16)=phi
    par_txt(10)='Set Point/K             '
    par_txt(11)='Regulation Temperature/K'
    par_txt(12)='Sample Temperature      '
    par_txt(13)='2*Theta/deg.            '
    par_txt(13)='Omega/deg.              '
    par_txt(13)='Chi/deg.                '
    par_txt(13)='Phi/deg.                '
    READF,unit,line
    bid=0.
    SP=0.
    Treg=0.
    Tsamp=0.
    READS,line,mon,bid,SP,Treg,Tsamp
    p(10)=SP
    p(11)=Treg
    p(12)=Tsamp
    par_txt(10)='Set Point/K             '
    par_txt(11)='Regulation Temperature/K'
    par_txt(12)='Sample Temperature/K    '
    READF,unit,line
    datall=long(intarr(2,10))
    WHILE STRPOS(line,'   -1000') NE 0 DO BEGIN
      READS,line,datall, format = '(10(I2, I6))'
      IF N_ELEMENTS(data) LE 1 THEN BEGIN
        data=REFORM(datall(1,*),10)
        err= REFORM(sqrt(datall(1,*))/sqrt((datall(0,*)>.25)),10)
      ENDIF ELSE BEGIN
        data=[data,REFORM(datall(1,*),10)]
        err= [err, REFORM(sqrt(datall(1,*))/sqrt((datall(0,*)>.25)),10)]
      ENDELSE
      READF,unit,line     
    ENDWHILE     
    x=findgen(N_ELEMENTS(data))*step+start
 ENDIF ELSE IF strmid(line,0,2) EQ '# ' OR strmid(line,0,4) EQ '#XY ' OR strmid(line,0,6) EQ 'XYDATA' OR INST(0) EQ 'xy' THEN BEGIN ; *** Fullprof INSTR format 10 (X,Y,Sigma format with Header lines starting with ! or #) ***
   IF strmid(line,0,6) EQ 'XYDATA' THEN BEGIN
     READF,unit,line
     READF,unit,line
     READF,unit,line
   ENDIF   
   tit=strmid(line,2,78)
   print,' *** Fullprof INSTR format 10 (X,Y,Sigma format with Header lines starting with ! or #) *** '
   sub=PATH+FILENAME
   REPEAT BEGIN
     READF,unit,line
     IF strmid(line,0,1) NE '#' AND strmid(line,0,1) NE '!' THEN BEGIN
       IF N_ELEMENTS(x) LT 1 THEN BEGIN
         data=0.
         x=0.
         err=0.
         xx=x
         y=data
         dy=err
         READS,line,xx,y,dy
       ENDIF ELSE BEGIN
         READS,line,xx,y,dy
         x   = [x,xx]
         data= [data,y]
         err = [err,dy]
       ENDELSE
     ENDIF ELSE PRINT,STRMID(line,2,78)
   ENDREP UNTIL EOF(unit)
   p=FLTARR(40)
   par_txt=STRARR(40)
   mon=0.
 ENDIF ELSE IF strmid(line,0,5) EQ 'D1A5 ' THEN BEGIN ; *** Fullprof INSTR format 5 (general data format) ***
    IF strmid(line,0,9) NE 'D1A5 D1A6' THEN ff=1 ELSE ff=0
    print,' *** Fullprof INSTR format 5 (general data format) *** '
    READF,unit,line
    tit=STRCOMPRESS(line)+' '+PATH+FILENAME
    tit=tit+STRCOMPRESS(line)
    READF,unit,line
    sub=STRCOMPRESS(line)
    READF,unit,line
    line=line+' 0 0 0 0 0 0'
    nd =1600
    tem=300.
    wav=1.3
    typ=1 
    mon=100000.
    raw=100000.
    READS,line,nd,tem,wav,typ,mon,raw
    IF ff eq 1 THEN nd=(nd/10)*10 
    READF,unit,line
    step=0.1
    start=-2.2
    stop=157.7
    READS,line,start,step,stop
    x=findgen(nd)*step+start
    data=fltarr(nd)
    READF,unit,data
    err=fltarr(nd)
    READF,unit,err
    p=FLTARR(40)
    p(12)=tem
    p(13)=start
    p(30)=raw
    p(33)=wav
    par_txt=STRARR(40)
    par_txt(12)='Sample Temperature      '
    par_txt(13)='2*Theta/deg.            '
    par_txt(30)='Original Monitor Counts '
    par_txt(33)='Wavelength / Angstroem  '
  ENDIF ELSE  BEGIN
   ; The line should contain only numbers, points, hyphens and blanks!
   test=STRCOMPRESS(line,/REMOVE_ALL)
   test=STR_SEP(test,'.')
   new=''
   FOR i=0,N_ELEMENTS(test)-1 DO new=new+test(i)
   test=new
   test=STR_SEP(test,'-')
   new=''
   FOR i=0,N_ELEMENTS(test)-1 DO new=new+test(i)
   test=new
   FOR j=0,9 DO BEGIN
      test=STR_SEP(test,STRCOMPRESS(j,/REMOVE_ALL))
      new=''
      FOR i=0,N_ELEMENTS(test)-1 DO new=new+test(i)
      test=new
   ENDFOR
  IF strlen(test) EQ 0 THEN BEGIN ; *** Fullprof INSTR format ... (*.dat) ***
    tit=line
    print,' *** Fullprof INSTR format 1 (D1A/D2B - original Rietveld-Hewat format) *** '
    sub=PATH+FILENAME
    line=line+' 0 0 0 0 0 0 0 '
    bid=100
    step=0.1
    start=-2.2
    stop=157.7
    mon=100000.
    READS,line,start,step,stop
    nd=ROUND((stop-start)/step)
    x=findgen(nd)*step+start
    datall=long(intarr(2,nd))
    readf, unit, datall, format = '(10(I2, I6))'      
    data=fltarr(nd)+1.
    data=data*datall(1,*)
    err=fltarr(nd)+1.
    err=err*sqrt(data)/sqrt((datall(0,*)>.25))
    p=FLTARR(40)
    par_txt=STRARR(40)
    p(13)=start
    par_txt=STRARR(40)
    par_txt(13)='2*Theta/deg.            '
   ENDIF ELSE BEGIN  ; *** Fullprof INSTR format ... (*.ill) ***
    p=FLTARR(40)
    par_txt=STRARR(40)
    tit=line
    print,' *** Fullprof INSTR format 6 (D1A/D2B/ILL standard format created by sumd2b/d1a) *** '
    sub=PATH+FILENAME
    READF,unit,line
    bid=100
    step=0.1
    start=-2.2
    stop=157.7
    mon=100000.
    READS,line,bid,bid,step
    READF,unit,line
    READS,line,start
    p(13)=start
    par_txt(13)='2*Theta/deg.            '
    READF,unit,line
    bid=0.
    SP=0.
    Treg=0.
    Tsamp=0.
    READS,line,mon,bid,SP,Treg,Tsamp
    p(10)=SP
    p(11)=Treg
    p(12)=Tsamp
    par_txt(10)='Set Point/K             '
    par_txt(11)='Regulation Temperature/K'
    par_txt(12)='Sample Temperature/K    '
    READF,unit,line
    datall=long(intarr(2,10))
    dummy=' 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'
    WHILE STRPOS(line,'   -1000') NE 0 DO BEGIN
      READS,line+dummy,datall, format = '(10(I2, I6))'
      IF N_ELEMENTS(data) LE 1 THEN BEGIN
        data=REFORM(datall(1,*),10)
        err= REFORM(sqrt(datall(1,*))/sqrt((datall(0,*)>.25)),10)
      ENDIF ELSE BEGIN
        data=[data,REFORM(datall(1,*),10)]
        err= [err, REFORM(sqrt(datall(1,*))/sqrt((datall(0,*)>.25)),10)]
      ENDELSE
      READF,unit,line     
    ENDWHILE   
    x=findgen(N_ELEMENTS(data))*step+start
   ENDELSE
  ENDELSE
      STATUS=0							;Status is ok
;     ********
      DATP={X:      x,    $					;Pass those variables which were 
            W_TIT:  tit,    $ 				;read-in into the DATP structure
            OTHER_TIT:  sub,    $ 				;read-in into the DATP structure
	           X_TIT:  '2theta',    $                   
	           Y_TIT:  'counts',    $                   
            E:      err,        $
            P:      p,        $
            PAR_TXT: PAR_TXT,        $
            N:      mon   }
;     **********************  					
	    
 read_err:  FREE_LUN,unit					;Free the unit number
 no_file:
 
 RETURN, DATA							;Return the data values
;************

 END
