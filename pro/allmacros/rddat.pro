function rddat, INST , PATH , FILENAME , STATUS , DATP
  ;print,'rddat'
;******* *****
;**
;**	Standard call for a data-read function interfacing LAMP.
		  
;**	Return of the function
;**	 DATA     is an array of any dimensions and type containing the data values (spectra).
PRINT, 'Filetype/Instrumentname:    ',inst(0)
IF N_ELEMENTS(inst) GT 1 THEN PRINT, 'Filegroup/Instrumentgroup: ',inst(1)
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
 STATUS=11
 CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif
 IF n_elements(pzup) eq 0 THEN pzup=""				;Check sub-directory & filename
 MYFILE=FILENAME
 MYPATH=PATH
 IF strpos(strlowcase(MYFILE), 'cyc.res')  ge 0 THEN BEGIN
			  IF pzup ne "" THEN MYPATH=MYPATH+pzup+'/' $
					;ELSE MYPATH=MYPATH+strmid(MYFILE,0,5)+'-'+strmid(!stime,9,2)+'/'
					ELSE MYPATH=MYPATH;+'/'
 ENDIF ELSE		  IF pzup ne "" THEN BEGIN  ON_IOERROR,misfix
					     MYPATH=MYPATH+pzup+'/'
					     tmp=fix(MYFILE)  & if tmp ge 10 then tmp=string(tmp,format='(i2)') $
					     			else	     tmp= '0'+string(tmp,format='(i1)')
					     MYFILE=strmid(pzup,0,5)+'n'+tmp+'.spe' &  misfix: & ENDIF
 ON_IOERROR, no_file & unit=-1
 OPENR,unit, MYPATH+MYFILE,/get_lun				;Open the data file

      STATUS=13
      ON_IOERROR, read_err

;PRINT, strpos(strlowcase(MYFILE), '.xyz')

IF strpos(strlowcase(INST(0)), '1600')  ge 0 THEN BEGIN
  a=1600
  DATA=FLTARR(a)
  READF,unit,DATA
      STATUS=0							;Status is ok
      DATP={X:         INDGEN(a),     $		;Pass those variables which were 
            Y_TIT:	    'value',       $ 	;read-in into the DATP structure
            X_TIT:	    'index', $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE,        $ 					
            OTHER_TIT: string(a)+' values' }

ENDIF ELSE IF strpos(strlowcase(MYFILE), '.xyz')  ge 0 THEN BEGIN
  print,'try to read xyz format (Turrillas/Hansen, July 1998)'
  row=FLTARR(4)
  READF,unit,row
  data=[[row]]
  y=row(1)
  PRINT,'y=',y
  WHILE NOT EOF(unit) DO BEGIN
    READF,unit,row
    IF y NE row(1) THEN BEGIN
      y=row(1)
      PRINT,'y=',y
    ENDIF
    ;PRINT,N_ELEMENTS(data),row
    data=[[data],[row]]
  ENDWHILE  
  ny=N_ELEMENTS(UNIQ(data(1,*)))
  nx=N_ELEMENTS(data(1,*))/ny
      STATUS=0							;Status is ok
      DATP={X:         REFORM(data(0,*),nx,ny),   $ ;Pass those variables which were 
            Y:         REFORM(data(1,*),nx,ny),   $ ;Pass those variables which were 
            E:         REFORM(data(3,*),nx,ny),   $ ;Pass those variables which were 
            Y_TIT:	    'value',       $ 	;read-in into the DATP structure
            X_TIT:	    'index', $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE,        $ 					
            OTHER_TIT: strcompress(N_ELEMENTS(data(0,*)))+' values' }
data=REFORM(data(2,*),nx,ny)

ENDIF ELSE BEGIN
IF strpos(strlowcase(MYFILE), 'cyc.res')  ge 0 THEN BEGIN
  print,'try to read result file from "cyclic" fullprof run'
  line='text'
  READF,unit,line
  tit=strmid(line,5,STRLEN(line)-5)
  print,tit
  cyc=0
  par=0
  x_tit='Step'
  WHILE NOT EOF(unit) DO BEGIN
    WHILE strmid(line,0,4) NE 'PHAS' DO BEGIN ; Header
      READF,unit,line
      IF strmid(line,0,4) EQ 'DATT' THEN BEGIN
        print,cyc,float(strmid(line,17,12))
        IF par GT 0 THEN PP=[PP,float(strmid(line,17,12))] ELSE  PP=[float(strmid(line,17,12))]
        par=par+1
        IF cyc EQ 0 THEN BEGIN
            IF N_ELEMENTS(txt) GT 0 THEN BEGIN
              txt=[txt,'Temperature ']
            ENDIF ELSE txt='Temperature '
        ENDIF
        IF cyc GT 0 THEN x=[x,float(strmid(line,17,12))] ELSE BEGIN
          x=float(strmid(line,17,12))
          x_tit='Temperature'
        ENDELSE
      ENDIF
    ENDWHILE
    IF N_ELEMENTS(x) NE cyc+1 THEN IF cyc THEN x=[x,cyc] ELSE x=cyc
    WHILE strmid(line,0,3) NE '---' DO BEGIN ; Phases
      IF cyc EQ 0 THEN print,line
      phase=long(strmid(line,4,1))
      READF,unit,line
      WHILE strmid(line,0,4) NE 'PHAS' AND strmid(line,0,3) NE '---' DO BEGIN ; Phase
        READF,unit,line
        IF strmid(line,0,4) EQ 'CELL' THEN BEGIN
          cell=fltarr(6)
          READS,strmid(line,4,STRLEN(line)-4),cell
          IF par GT 0 THEN PP=[PP,float(cell)] ELSE  PP=[float(cell)]
          par=par+6
          IF cyc EQ 0 THEN BEGIN
            IF N_ELEMENTS(txt) GT 0 THEN BEGIN
              txt=[txt,'a('+strcompress(PHASE,/re)+')     ']
            ENDIF ELSE txt='a'+strcompress(PHASE,/re)+')     '
            txt=[txt,'b('+strcompress(PHASE,/re)+')     ']
            txt=[txt,'c('+strcompress(PHASE,/re)+')     ']
            txt=[txt,'alpha('+strcompress(PHASE,/re)+') ']
            txt=[txt,'beta ('+strcompress(PHASE,/re)+') ']
            txt=[txt,'gamma('+strcompress(PHASE,/re)+') ']
          ENDIF
          READF,unit,line
          sig_cell=fltarr(6)
          READS,strmid(line,4,STRLEN(line)-4),sig_cell
          IF par GT 0 THEN PP=[PP,float(sig_cell)] ELSE  PP=[float(sig_cell)]
          par=par+6
          IF cyc EQ 0 THEN BEGIN
            IF N_ELEMENTS(txt) GT 0 THEN BEGIN
              txt=[txt,'sigma (a('+strcompress(PHASE,/re)+'))     ']
            ENDIF ELSE txt='sigma (a'+strcompress(PHASE,/re)+'))     '
            txt=[txt,'sigma (b('+strcompress(PHASE,/re)+'))     ']
            txt=[txt,'sigma (c('+strcompress(PHASE,/re)+'))     ']
            txt=[txt,'sigma (alpha('+strcompress(PHASE,/re)+')) ']
            txt=[txt,'sigma (beta ('+strcompress(PHASE,/re)+')) ']
            txt=[txt,'sigma (gamma('+strcompress(PHASE,/re)+')) ']
          ENDIF
          IF phase EQ 1 THEN IF cyc GT 0 THEN BEGIN
            DATA=[[DATA],    [cell]] 
            EE=  [[EE],  [sig_cell]] 
          ENDIF ELSE BEGIN
            DATA=    cell
            EE  =sig_cell
          ENDELSE
        ENDIF ELSE IF strmid(line,0,4) EQ 'SCAL' THEN BEGIN
          bid=fltarr(2)
          READS,strmid(line,4,STRLEN(line)-4),bid
          IF par GT 0 THEN PP=[PP,float(bid)] ELSE  PP=[float(bid)]
          IF cyc EQ 0 THEN IF N_ELEMENTS(txt) GT 0 THEN txt=[txt,'scale('+strcompress(PHASE,/re)+')    ','sigma (scale('+strcompress(PHASE,/re)+')) '] ELSE txt=['scale('+strcompress(PHASE,/re)+')    ','sigma (scale('+strcompress(PHASE,/re)+')) ']
          par=par+2
        ENDIF
      ENDWHILE
    ENDWHILE
    IF cyc GT 0 THEN PV=[[PV],[PP]] ELSE PV=PP
    cyc=cyc+1
    par=0
  ENDWHILE
      STATUS=0				;Status is ok
      DATP={PV:        PV,$
            P:         PP,$
            PAR_TXT:   txt,$
            E:         EE,            $	 ; 
            X:         indgen(6), $	 ;
            Y:         X,                       $	 ;Pass those variables which were 
            Z_TIT:     'lattice',               $ 	;read-in into the DATP structure
            Y_TIT:     'abc/angles',            $ 	;
            X_TIT:     x_tit, $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE,        $ 					
            OTHER_TIT: tit }

ENDIF ELSE  IF strpos(strlowcase(MYFILE), '.prf')  ge 0 THEN BEGIN
  print,'try to read profile (prf) file'
  hkl=[[0,0,0]]
  bragg=0
  line='text'
  READF,unit,line
 IF strmid(line,0,4) EQ 'IGOR' THEN BEGIN ; *** Fullprof output format 2 (IGOR) ***
  PRINT,'*** Fullprof output format 2 (IGOR)'
  a=0.
  b=0.
  c=0.
  TwoTheta=0.0
  Iobs=0.0
  Icalc=0.0
  READF,unit,line
  READF,unit,line
  READF,unit,line
  WHILE strmid(line,0,3) NE 'END' DO BEGIN
    READS,line+' 0 0 0',a,b,c
    TwoTheta=[Twotheta,a]
    Iobs= [Iobs,b]
    Icalc=[Icalc,c]
    READF,unit,line
  ENDWHILE
  other_tit=line
  WHILE strmid(other_tit,0,3) NE 'X |' DO BEGIN
    READF,unit,other_tit
  ENDWHILE
  READF,unit,line
  other_tit=strmid(line,17,strlen(line)-17)+' '+strmid(other_tit,23,strlen(other_tit)-23)
  Twotheta=twotheta(1:n_elements(twotheta)-1)
  iobs=iobs(1:n_elements(iobs)-1)
  icalc=icalc(1:n_elements(icalc)-1)
  diff=iobs-icalc
 ENDIF ELSE BEGIN
  ttstart =0.
  ttstep=0.
  bid=0.
  phases=0
  lambda1=0.
  lambda2=0.
  ratio=0.
  steps=long(0)
  other_tit='Profile (fit) : '+STRCOMPRESS(line)
  READF,unit,line
  PRINT,line
  IF strmid(line,0,3) EQ '  1' OR strmid(line,0,3) EQ '  2'  OR strmid(line,0,3) EQ '  3'THEN BEGIN ; *** Fullprof output format -3 (if input INSTR=10) ***
   PRINT,'*** Fullprof output format -3 (if input INSTR=10) ***'
   READS,line+' 0 0 0 0 0 0 0',phases, steps, lambda1,lambda2,bid,bid,bid
    Icalc=FLTARR(steps)
    TwoTheta=FLTARR(steps)
    Iobs=FLTARR(steps)
    Idiff=FLTARR(steps)
    bg=FLTARR(steps)
    READF,unit,line
    phase=INTARR(phases)
    IF phases GT 1 THEN BEGIN
      READS,line+' 0 0 0',bid,reflections,phase,excludedregions
    ENDIF ELSE READS,line+' 0 0 0',reflections,phase,excludedregions
    IF excludedregions GT 0 THEN BEGIN
      excluded0=FLTARR(excludedregions)
      excluded1=FLTARR(excludedregions)
    ENDIF
    ;help,steps,bg,excludedregions,reflections
    hkl=INTARR(3,reflections)
    bragg=FLTARR(reflections)
    e0=0.
    e1=0.
    FOR i=0,excludedregions-1 DO BEGIN
      READF,unit,e0,e1
      excluded0(i)=e0
      excluded1(i)=e1
    ENDFOR
    READF,unit,line
    tt=0.
    braggpos=0.
    h=0
    k=0
    l=0
    FOR i=0,steps-1 DO BEGIN
      READF,unit,line
      IF i LT reflections THEN BEGIN
        ;PRINT,line
        READS,line,tt,bid,int,bid,bid,braggpos
        bragg(i)=braggpos
        ;print,braggpos,bid
        line=STRMID(line,STRPOS(line,'(')+1, STRLEN(line)-STRPOS(line,'(')-1)
        ;PRINT,line
        READS,line,h,k,l
        hkl(*,i)=[h,k,l]
        ;print,hkl(*,i)
      ENDIF ELSE BEGIN
        READS,line,tt,bid,int,bid
      ENDELSE
      TwoTheta(i)=tt
      Icalc(i)=int
    ENDFOR
  ENDIF ELSE BEGIN ; *** Fullprof output format 1 (standard : PLOTPOW etc.) ***
    PRINT,'*** Fullprof output format 1 (standard : PLOTPOW etc.) ***'
    READF,unit,line
    READS,line+' 0 0 0',bid,ttstart,ttstep
    READF,unit,line
    READS,line+' 0 0 0 0 0',bid,steps,lambda1,lambda2,ratio
    other_tit=other_tit+' lambda='+STRCOMPRESS(lambda1)
    READF,unit,line
    TwoTheta=ttstart+ttstep*findgen(steps) 
    Iobs    =fltarr(steps) 
    READF,unit,Iobs
    Icalc    =fltarr(steps) 
    READF,unit,Icalc
  ENDELSE
      STATUS=0				;Status is ok
      DATP={P:         [lambda1,lambda2],$
            PAR_TXT:   ['lambda1','lambda2'],$
            X:         twotheta, $	 ;Pass those variables which were 
            Y:         hkl,$
            Z:         bragg, $	 ;Pass those variables which were 
            Y_TIT:     'counts', $ 	;read-in into the DATP structure
            X_TIT:     '2Theta', $ 					
            Z_TIT:     'hkl pos.', $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE,        $ 					
            OTHER_TIT: other_tit }
 ENDELSE
 DATA=Icalc 

ENDIF ELSE IF strpos(strlowcase(MYFILE), 'd20_') ge 0 AND strpos(strlowcase(MYFILE), '.bad') lt 0  THEN BEGIN
  print,'read d20 efficiency correction file'
  line='text'
  READF,unit,line
  nd_cal=0 ;FIX(line)
  IF nd_cal EQ 0 THEN nd_cal=1600
  x=findgen(nd_cal)
  data=fltarr(nd_cal)
  READF,unit,x,DATA
      STATUS=0							;Status is ok
      DATP={X:          x,                 $					;Pass those variables which were 
            Y_TIT:	    'eff-cor',          $ 					;read-in into the DATP structure
            X_TIT:	    '2Theta+Offset',    $ 					
            OTHER_TIT:	line,               $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE }

ENDIF ELSE IF strpos(strlowcase(MYPATH), 'bad_cells') ge 0 OR strpos(strlowcase(MYFILE), '.bad') ge 0 THEN BEGIN
  ;print,'try to read bad cells'
  a=0
  READF,unit,a
  DATA=FLTARR(a)
  READF,unit,DATA
      STATUS=0							;Status is ok
      DATP={X:         INDGEN(a),     $		;Pass those variables which were 
            Y_TIT:	    'value',       $ 	;read-in into the DATP structure
            X_TIT:	    'index', $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE,        $ 					
            OTHER_TIT: string(a)+' values' }

ENDIF ELSE BEGIN
  print,'try to read simple array'
  a=0
  READF,unit,a
  DATA=FLTARR(a)
  READF,unit,DATA
      STATUS=0							;Status is ok
      DATP={X:         INDGEN(a),     $		;Pass those variables which were 
            Y_TIT:	    'value',       $ 	;read-in into the DATP structure
            X_TIT:	    'index', $ 					
            W_TIT:     INST+' : '+MYPATH+MYFILE,        $ 					
            OTHER_TIT: string(a)+' values' }

ENDELSE
ENDELSE
 read_err:  FREE_LUN,unit					;Free the unit number
 no_file:   IF unit lt 0 THEN print,!err_string, string(7b)

 RETURN, DATA							;Return the data values

 END
