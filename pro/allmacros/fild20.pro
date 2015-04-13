PRO fild20,w,filename,STEP=step,HELP=help,numors=numors,virtual=virtual,$
                      submin=submin,submax=submax,bg=bg,b=b,c=c,mag=mag,$
                      start=start,n0=n0,n1=n1,$
                      nof20=nof20,nodat=nodat,nod20=nod20,nonum=nonum,$
                      append=append,equidist=equidist

IF KEYWORD_SET(help) THEN BEGIN
  print,''
  print,'                         PRO FilD20.PRO'
  print,''
  print,'LAMP-IDL Macro (Procedure) started: 07-Jun-97 by Th.Hansen, ILL-Grenoble (D20)'
  print,'Output of data in "new" D1B - and D20 (of course!) -format for FullProf (cyclic!), ABfit, XRFit etc.'
  print,"It acts (or better: should act) in some way like a LAMP / D20 version of FILD1B"
  print,''
  print,'Attention : Set your flags (efficiency correction, bad cell interpolation, normalisation before!'
  print,''
  print,"Call:      fild20,W[,'filename'][,STEP=0.1][,/HELP]"
  print,"Variables: Workspace (necessary, won't be changed)"
  print,"           basic filename, 3 letters, different from 'fil' (optional, will be changed)"
  print,'Keywords:  STEP :    evt. different 2*Theta stepwidth'
  print,'           HELP :    this hopefully helping text'
  print,'           VIRTUAL : first numor of incremented numors for each ACQ'
  print,'           NUMORS :  Array of numor-numbers to be read instead of using the given workspace (which will be changed)'
  print,'                     if just the keyword is given (/numors), filename.run, '
  print,'                     containing the number of numors and the numors, will be read in'
  print,'           SUBMIN :  First sub-numor to be used (or list of sub-numors for each numor in the numors-array)'
  print,'           SUBMAX :  Last  sub-numor to be used (or list of sub-numors for each numor in the numors-array)'
  print,'           BG :      Workspace containing background to be substracted (pay attention to normalisation!)'
  print,'           B :       Workspace containing 2Theta-depending correction coefficients'
  print,'           C :       Workspace containing 2Theta depending attenuation coefficients for substracted background'
  print,''
  print,'     Modification: 21-Aug-97 by Th.Hansen:'
  print,'                   Normalisation and Calibration now controlled by common flag_d20 (calibration) and procedure flag'
  print,'                   The old version (used by Rennie, Brown, Clarke, Chatteigner and Lutterotti is backuped'
  print,'     Modification: 03-Sep-97 by Th.Hansen: Bug-fix '
  print,'                   Errors not correct for normalized data (variable count were always 1)'
  print,'     Modification: 05-Sep-97 by Th.Hansen: Bug-fix'
  print,'                   fil.f20 file for sequential simple ACQs did not contain counts'
  print,'     Modification: 30-Sep-97 by Th.Hansen: Bug-fix'
  print,'                   one-dimensional array datp.x for multiple diagrams was not correctly treated'
  print,'     Modification: 22-Oct-97 by Th.Hansen: '
  print,'                   Background substraction (Ferrand) and temperature in *.num file (Soubeyroux)'
  print,'     Modification: 03-Nov-97 by Th.Hansen: '
  print,'                   first and last numor, longer filenames, *.d20 files and options not to create files (Ouladdiaf)'
  print,'Last modification: 30-Aug-01 by Th.Hansen: '
  print,'                   equidist keyword, avoiding 0 counts due to rebinning'
  print,'                   '
  print,''
ENDIF

zerocountingcells=0
IF KEYWORD_SET(n0) THEN BEGIN
  IF NOT KEYWORD_SET(n1) THEN n1=n0
  IF n1 LE n0 THEN BEGIN
    n2=n1
    n1=n0
    n0=n1
  ENDIF
  numors=indgen(n1-n0+1)+n0
ENDIF
iii=0
IF NOT KEYWORD_SET(b)  THEN b =1.
IF NOT KEYWORD_SET(c)  THEN c =1.
IF KEYWORD_SET(bg) THEN BEGIN
  take_w, wbg, w=bg
  take_datp, datbg, w=bg
  IF N_ELEMENTS(c) EQ N_ELEMENTS(wbg) OR N_ELEMENTS(c) EQ 1 THEN BEGIN
    datbg.e=datbg.e*c
    wbg=wbg*c  
  ENDIF ELSE PRINT,'No multiplication of background by coeff. C(theta)'
ENDIF


IF n_elements(filename) EQ 0 THEN filename = 'fil'
IF KEYWORD_SET(numors) THEN IF N_ELEMENTS(w) EQ 1 THEN IF N_PARAMS() EQ 1 THEN filename=w
IF STRLEN(filename) EQ 0 THEN filename ='fil'
longname=string(filename)
filename=strmid(string(filename)+'000',0,3)
filename = STRCOMPRESS(filename,/REMOVE_ALL)

line=' '
IF KEYWORD_SET(append) THEN BEGIN
  IF NOT KEYWORD_SET(nof20) THEN BEGIN
    PRINT,    'Append full multiple DataSet - FileName: ',filename+'.f20'
    XICUTE,'$mv '+longname+'.f20 '+longname+'.old'
    OPENR, in,longname+'.old',/get_lun
    OPENW, f20,longname+'.f20',/get_lun
    READF,in,line
    WHILE STRMID(line,0,10) NE '    -10000' DO BEGIN
      PRINTF,f20,line
      READF,in,line
    ENDWHILE
    FREE_LUN,in
  ENDIF
  IF NOT KEYWORD_SET(nonum) THEN BEGIN
    XICUTE,'$mv '+longname+'.num '+longname+'.old'
    OPENR, in,longname+'.old',/get_lun
    OPENW, num,longname+'.num',/get_lun
    READF,in,line
    WHILE STRMID(line,0,10) NE '    -10000' DO BEGIN
      PRINTF,num,line
      READF,in,line
    ENDWHILE
    FREE_LUN,in
  ENDIF
ENDIF ELSE BEGIN
  IF NOT KEYWORD_SET(nof20) THEN PRINT,    'Full multiple DataSet - FileName: ',filename+'.f20'
  IF NOT KEYWORD_SET(nof20) THEN OPENW, f20,longname+'.f20',/get_lun
  IF NOT KEYWORD_SET(nof20) THEN IF KEYWORD_SET(numors) THEN PRINTF,f20,FORMAT='(I5)',N_ELEMENTS(numors) ELSE PRINTF,f20,FORMAT='(I5)',n_elements(w(0,*,*))
  IF NOT KEYWORD_SET(nonum) THEN OPENW, num,longname+'.num',/get_lun
ENDELSE

IF NOT KEYWORD_SET(start) THEN start=0
counter=-1

IF KEYWORD_SET(numors) THEN BEGIN 
 IF N_ELEMENTS(numors) EQ 1 THEN IF numors EQ 1 THEN BEGIN
   numors=rdarr(filename+'.run') 
   PRINT,'Reading Numors from ',filename+'.run'
 ENDIF ELSE BEGIN
   IF N_ELEMENTS(numors) EQ 1 AND string(numors) GE string(999999) THEN BEGIN
     numors=rdarr(string(numors)) 
	 PRINT,'Reading Numors from ',string(numors)
   ENDIF
 ENDELSE
 numors=ROUND(numors)
 PRINT,'Reading Numor Files     ',numors
 IF KEYWORD_SET(submin) THEN BEGIN
  IF submin EQ 1 THEN BEGIN
    submin=rdarr(filename+'.min') 
    PRINT,'Reading min. Sub-Numors from ',filename+'.min'
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(submin) EQ 1 AND string(submin) GE string(999999) THEN BEGIN
      submin=rdarr(string(submin)) 
 	  PRINT,'Reading min. Sub-Numors from ',string(submin)
    ENDIF
  ENDELSE
  submin=ROUND(submin)
  PRINT,'First Sub-Numors used   ',submin
 ENDIF
 IF KEYWORD_SET(submax) THEN BEGIN
  IF submax EQ 1 THEN BEGIN
    submax=rdarr(filename+'.max') 
    PRINT,'Reading max. Sub-Numors from ',filename+'.max'
  ENDIF ELSE BEGIN
    IF N_ELEMENTS(submax) EQ 1 AND string(submax) GE string(999999) THEN BEGIN
      submax=rdarr(string(submax)) 
	  PRINT,'Reading max. Sub-Numors from ',string(submax)
    ENDIF
  ENDELSE
  submax=ROUND(submax)
  PRINT,' Last Sub-Numors used   ',submax
 ENDIF ELSE submax=999999
 IF NOT KEYWORD_SET(mag) THEN mag=ceil(4)
 magnitude = ceil(alog10(start+n_elements(numors)))
 magnitude = ceil(alog10(long(10.0^(magnitude-1))+n_elements(numors)))
 magnitude = mag > magnitude
 virtual_numor=round(min(numors))
 factor=long(1)
  lastn=N_ELEMENTS(numors)-1 
  IF N_ELEMENTS(submin) EQ 1                  THEN submin=intarr(N_ELEMENTS(numors))+submin
  IF N_ELEMENTS(submin) NE N_ELEMENTS(numors) THEN submin=intarr(N_ELEMENTS(numors))
  IF N_ELEMENTS(submax) EQ 1                  THEN submax=intarr(N_ELEMENTS(numors))+submax
  IF N_ELEMENTS(submax) NE N_ELEMENTS(numors) THEN submax=intarr(N_ELEMENTS(numors))+9999
ENDIF ELSE BEGIN
  lastn=0
  IF NOT KEYWORD_SET(submin) THEN submin=0
  IF NOT KEYWORD_SET(submax) THEN submax=N_ELEMENTS(w(0,*,*))-1
  factor=long(1)
  WHILE long(max(w)/factor) ge 100000000 DO factor=factor+1
  PRINT,'Factor: ',factor
 IF NOT KEYWORD_SET(mag) THEN mag=ceil(0)
  magnitude=ceil(alog10(start+n_elements(w(0,*,*))))
  magnitude=ceil(alog10(long(10.0^(magnitude-1))+n_elements(w(0,*))))
  magnitude = mag > magnitude
  lastn=0
  IF NOT KEYWORD_SET(submin) THEN submin=0
  IF NOT KEYWORD_SET(submax) THEN submax=N_ELEMENTS(w(0,*,*))-1
ENDELSE
submin=submin>0
submax=submax>submin

FOR nnn=0,lastn DO BEGIN
  datp=1
  IF KEYWORD_SET(numors) THEN BEGIN
    w=rdrun(numors(nnn),datp=datp)
  ENDIF ELSE BEGIN
    take_datp,datp
    virtual_numor=LONG(datp.p(29)) MOD 100000
  ENDELSE
  first=submin(nnn)<(N_ELEMENTS(w(0,*))-1)
  last =submax(nnn)<(N_ELEMENTS(w(0,*))-1)
  IF KEYWORD_SET(numors) THEN BEGIN  
    IF N_ELEMENTS(w(0,*)) GT 1 THEN print,'Numor',LONG(datp.p(29)),' from subnumor',first,' to',last	ELSE print,'Numor',LONG(datp.p(29))
  ENDIF
  ; Modification TCH JLS 27/3/1
  IF N_ELEMENTS (datp.e) NE N_ELEMENTS(w) THEN mod_datp,datp,'e' ,SQRT(w)
  w      =w      (*,first:last)
  mod_datp,datp,'e' ,datp.e (*,first:last)
  IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN mod_datp,datp,'x',datp.x (*,first:last)
  mod_datp,datp,'y' ,datp.y (    first:last)
  mod_datp,datp,'n' ,datp.n (*,*,first<((N_ELEMENTS(datp.n(0,0,*))-1)>0):last<((N_ELEMENTS(datp.n(0,0,*))-1)>0))
  mod_datp,datp,'pv',datp.pv(*,  first:last)

pitch=TOTAL(datp.x(1:N_elements(datp.x(*,0,0))-1,0,0)-datp.x(0:N_elements(datp.x(*,0,0))-2,0,0))/(N_elements(datp.x(*,0,0))-1)
IF NOT KEYWORD_SET(step) THEN BEGIN
   step=pitch
   IF ROUND(datp.p(0)) EQ 3 THEN BEGIN
    step=datp.x(N_ELEMENTS(datp.x(*,0))/2,0)-datp.x(N_ELEMENTS(datp.x(*,0))/2-1,0) ; 2theta scan
  ENDIF
ENDIF ELSE PRINT,'The average angular pitch is',pitch,' degrees, but you want it to be',step
   dev=TOTAL(ABS((datp.x(1:N_elements(datp.x(*,0,0))-1,0,0)-datp.x(0:N_elements(datp.x(*,0,0))-2,0,0))-step))/(N_elements(datp.x(*,0,0))-1)
   IF dev GE step/10000. THEN BEGIN
     PRINT,'ATTENTION : You do not really have equidistant data: average deviation +/-', dev
     PRINT,'In such a case you should better choose another export format (d20reg), or interpolate your workspace before!'
     IF KEYWORD_SET(equidist) THEN PRINT,'The data will not be rebinned but supposed to be equidistant'
     IF NOT KEYWORD_SET(equidist) THEN PRINT,'The data will be rebinned to an equidistant grid'
   ENDIF
   print,'2Theta step : ',step
e=datp.e
cell     = INDGEN(n_elements(w(*,0)))
IF NOT KEYWORD_SET(equidist) THEN cell(*)  = ROUND((datp.x(*,0)-datp.x(0,0))/step)
counter=counter+start
FOR ii=0,n_elements(w(0,0,*))-1 DO FOR i=0,n_elements(w(0,*,0))-1 DO BEGIN
  IF datp.pv(30,i) NE datp.n(0,0,i<((N_ELEMENTS(datp.n(0,0,*))-1)>0)) THEN BEGIN
    PRINT,"Attention to non-integer normalisation factor (use exriet's GENeral data format or d20reg!) "+string(datp.pv(30,i) / datp.n(0,0,i<((N_ELEMENTS(datp.n(0,0,*))-1)>0)))
  ENDIF
  counter=counter+1
  filext = string(long(10.0^magnitude))+strcompress(string(counter+long(10.0^(magnitude-1))),/REMOVE_ALL)
  filext = strmid(filext,strlen(filext)-magnitude,magnitude)
  pp=datp.p 
  IF n_elements(datp.n(*,0)) GT 1 THEN BEGIN
    nn=datp.n(i,0:1) 
  ENDIF ELSE BEGIN
    nn=datp.n(0,0:1<((N_ELEMENTS(datp.n(0,*,0))-1)>0))
  ENDELSE
  IF N_ELEMENTS(w(*,i,ii)) EQ N_ELEMENTS(b ) OR N_ELEMENTS(b ) EQ 1 THEN BEGIN
    w(*,i,ii)=w(*,i,ii)*b  
    datp.e(*,i,ii)=datp.e(*,i,ii)*b  
  ENDIF ELSE PRINT,'No multiplication by coeff. B(theta)',nnn,i,ii
  IF KEYWORD_SET(bg) THEN BEGIN
    IF N_ELEMENTS(w(*,i,ii)) EQ N_ELEMENTS(wbg) OR N_ELEMENTS(wbg) EQ 1 THEN BEGIN
      w(*,i,ii)=w(*,i,ii)-wbg*nn/datbg.n 
      datp.e(*,i,ii)=datp.e(*,i,ii)+datbg.e*nn/datbg.n
    ENDIF ELSE PRINT,'No substraction by Background       ',nnn,i,ii
  ENDIF
  IF n_elements(w(0,*,*)) GT 1 THEN BEGIN
    pv=datp.pv(*,i,ii)
    xx=datp.x(*,i<(N_ELEMENTS(datp.x(0,*,0))-1),ii)
  ENDIF ELSE BEGIN
    pv=datp.pv
    xx=datp.x
  ENDELSE
  timemag=ceil(alog10(pv(5)))
  IF NOT KEYWORD_SET(nodat) THEN BEGIN
    IF (i EQ 0) or (i EQ n_elements(w(0,*))-1) THEN print,'FileName: ',filename+filext+'.dat'
  ENDIF ELSE IF NOT KEYWORD_SET(nod20) THEN BEGIN
    IF (i EQ 0) or (i EQ n_elements(w(0,*))-1) THEN print,'FileName: ',filename+filext+'.d20'
  ENDIF
  IF KEYWORD_SET(virtual) THEN  numor=virtual_numor+counter ELSE numor=round(pv(29)) mod 100000
  IF NOT KEYWORD_SET(nodat) THEN OPENW,out,filename+filext+'.dat',/get_lun
  IF NOT KEYWORD_SET(nod20) THEN OPENW,d20,filename+filext+'.d20',/get_lun
  IF NOT KEYWORD_SET(nodat) THEN PRINTF,out,FORMAT='(I5)',1
  IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20,FORMAT='(I5)',1

; Write .DAT files and .NUM file

    outstr=' '+strmid(datp.other_tit,3,strlen(datp.other_tit)-3)+datp.w_tit+'                                                                                              '
    IF NOT KEYWORD_SET(nodat) THEN PRINTF,out,strmid(outstr,0,132)
    IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20,strmid(outstr,0,132)
    IF NOT KEYWORD_SET(nonum) THEN PRINTF,num,strmid(outstr,0,132)
    outstr=''
    iii=iii+1
    IF NOT KEYWORD_SET(nodat) THEN PRINTF,out,FORMAT='(I4,I7,"                                                                                                                         ")',$
      iii,numor
    IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20,FORMAT='(I4,I7,"                                                                                                                         ")',$
      iii,numor
    IF NOT KEYWORD_SET(nonum) THEN PRINTF,num,FORMAT='(I4,I7,"                                                                                                                         ")',$
      iii,numor
    IF timemag LE 0 THEN BEGIN
      IF NOT KEYWORD_SET(nodat) THEN printf,out,FORMAT="(F13.0,F9.6,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
        nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
      IF NOT KEYWORD_SET(nod20) THEN printf,d20,FORMAT="(F13.0,F9.6,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
        nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
      IF NOT KEYWORD_SET(nonum) THEN printf,num,FORMAT="(F13.0,F9.6,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
        nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
    ENDIF ELSE BEGIN 
      IF timemag LE 3 THEN BEGIN
        IF NOT KEYWORD_SET(nodat) THEN printf,out,FORMAT="(F13.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
          nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
        IF NOT KEYWORD_SET(nod20) THEN printf,d20,FORMAT="(F13.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
          nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
        IF NOT KEYWORD_SET(nonum) THEN printf,num,FORMAT="(F13.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
          nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
      ENDIF ELSE  BEGIN
        IF NOT KEYWORD_SET(nodat) THEN printf,out,FORMAT="(F13.0,F9.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
          nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
        IF NOT KEYWORD_SET(nod20) THEN printf,d20,FORMAT="(F13.0,F9.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
          nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
        IF NOT KEYWORD_SET(nonum) THEN printf,num,FORMAT="(F13.0,F9.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
          nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
      ENDELSE 
    ENDELSE
    outstr=''

    IF NOT KEYWORD_SET(nodat) THEN PRINTf,out,FORMAT='(I4," ",F9.3," ",F9.3," ",F9.3," ",F9.3," ",F9.3,"  EffCor=1,AngCal=2,Intpol=4: ",I6,"   ",I6,"                                 ")',$
      MAX(cell)+1,pv(17:21),pp(38),0
    IF NOT KEYWORD_SET(nod20) THEN PRINTf,d20,FORMAT='(I4," ",F9.3," ",F9.3," ",F9.3," ",F9.3," ",F9.3,"  EffCor=1,AngCal=2,Intpol=4: ",I6,"   ",I6,"                                 ")',$
      MAX(cell)+1,pv(17:21),pp(38),0

; Write .F20 file

      outstr=' '+strmid(datp.other_tit,3,strlen(datp.other_tit)-3)+datp.w_tit+'                                                                                              '
      IF NOT KEYWORD_SET(nof20) THEN PRINTF,f20,strmid(outstr,0,132)
      outstr=''
      IF NOT KEYWORD_SET(nof20) THEN PRINTF,f20,FORMAT='(I4,I7,"                                                                                                                         ")',$
        iii,numor
  IF timemag LE 0 THEN BEGIN
       IF NOT KEYWORD_SET(nof20) THEN printf,f20,FORMAT="(F13.0,F9.6,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
         nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
  ENDIF ELSE BEGIN 
    IF timemag LE 3 THEN BEGIN
      IF NOT KEYWORD_SET(nof20) THEN printf,f20,FORMAT="(F13.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
        nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
    ENDIF ELSE  BEGIN
      IF NOT KEYWORD_SET(nof20) THEN printf,f20,FORMAT="(F13.0,F9.0,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,F9.3,'                    ')",$
        nn,xx(0),pv(14:16),0.,pp(33),step,pv(10:12)<9999
    ENDELSE 
  ENDELSE
    outstr=''
    IF NOT KEYWORD_SET(nof20) THEN PRINTf,f20,FORMAT='(I4," ",F9.3," ",F9.3," ",F9.3," ",F9.3," ",F9.3,"  EffCor=1,AngCal=2,Intpol=4: ",I6,"   ",I6,"                                 ")',$
      MAX(cell)+1,pv(17:21),pp(38),0
  outstr=''

  FOR j=0,max(cell) DO BEGIN
    IF j NE 0 THEN count_old=count
    index = where (cell eq j,count)
    IF count gt 0 THEN BEGIN
      IF e(index(0),i) GT 0 THEN count=long(round((SQRT(ABS(w(index(0),i,ii)))/e(index(0),i,ii))^2)) ELSE count=long(1)
      count = factor*count
      rate=long((w(index(0),i,ii)+0.49999999)/factor)
      IF rate EQ 0 THEN BEGIN
        count=long(1)
      ENDIF
    ENDIF ELSE BEGIN          ;  no cell for this angle in the workspace
      IF (i EQ 0) THEN zerocountingcells=zerocountingcells+1
      IF (i EQ 0) THEN PRINT,j,count,rate,count_old,zerocountingcells
      count = long(1) 
      rate=long(0)  
    ENDELSE
    outstr=outstr+strmid(string(count),10,2)+strmid(string(rate),4,8)
    IF strlen(outstr) gt 90 THEN BEGIN
      outstr=outstr+'                                                                                              '
      IF NOT KEYWORD_SET(nodat) THEN PRINTF,out,strmid(outstr,0,132)
      IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20,strmid(outstr,0,132)
      IF NOT KEYWORD_SET(nof20) THEN PRINTF,f20, strmid(outstr,0,132)
      outstr=''
    ENDIF
  ENDFOR
  IF strlen(outstr) GT 1 THEN BEGIN
    outstr=outstr+'                                                                                              '
    IF NOT KEYWORD_SET(nodat) THEN PRINTF,out, strmid(outstr,0,132)
    IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20, strmid(outstr,0,132)
    IF NOT KEYWORD_SET(nof20) THEN PRINTF,f20, strmid(outstr,0,132)
  ENDIF
  IF NOT KEYWORD_SET(nof20) THEN PRINTF,f20,'     -1000                                                                                                                          '
  IF NOT KEYWORD_SET(nodat) THEN PRINTF,out,'     -1000                                                                                                                          '
  IF NOT KEYWORD_SET(nodat) THEN PRINTF,out,'    -10000                                                                                                                          '
  IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20,'     -1000                                                                                                                          '
  IF NOT KEYWORD_SET(nod20) THEN PRINTF,d20,'    -10000                                                                                                                          '
  IF NOT KEYWORD_SET(nodat) THEN FREE_LUN,out
  IF NOT KEYWORD_SET(nod20) THEN FREE_LUN,d20
ENDFOR
ENDFOR

  IF NOT KEYWORD_SET(nof20) THEN PRINTF,f20,'    -10000                                                                                                                          '
  IF NOT KEYWORD_SET(nof20) THEN FREE_LUN,f20
  IF NOT KEYWORD_SET(nonum) THEN PRINTF,num,'    -10000                                                                                                                          '
  IF NOT KEYWORD_SET(nonum) THEN FREE_LUN,num
  IF zerocountingcells NE 0  THEN BEGIN
    PRINT,'You will find some zerocounting cells in your data set(s), which is most probably due to rebinning of non-equidistant scattering data to an equidistant grid.'
    PRINT,'You can use the keyword /equidist to force the scattering angles to be calculated as x(i)=x(0)+step. '
    PRINT,'If the keyword step is not given, it will be set to the average angular pitch, close to 0.1 degrees in the case of D20.'
  ENDIF
END

