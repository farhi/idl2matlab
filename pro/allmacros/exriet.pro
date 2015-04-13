PRO exriet ,w,filename,STEP=step,HELP=help,  general=general,illdat=illdat,numors=numors,n0=n0,n1=n1
IF KEYWORD_SET(help) THEN BEGIN
  print,''
  print,'                         PRO ExRiet.PRO'
  print,''
  print,'LAMP-IDL Macro (Procedure) started: 29-Oct-96 by Th.Hansen, ILL-Grenoble (D20)'
  print,'Output of single diagrams in D1B/D1A/D2B-format for Fullprof etc. (*.dat)
  print,'and in GSAS-Format (sequential *.gsas and direct access *.gdat format)'
  print,'for Rietveld refinement calculations, 2*Theta scans are normally not foreseen ...'
  print,'In case of a 2Theta-scan W_new=UNIC(W_old) has eventually to be executed before'
  print,'For multiple diagrams (scans, strob. acq.) use fild20.pro, creating D1B-format'
  print,"Finally it should act (I'm dreamin' ...) like a LAMP / D20 version of SUMD2B"
  ;print,'This macro should be replaced by SumD20.Pro ....'
  print,''
  print,"Call:      EXRIET,W[,'filename'][,STEP=0.1][,/HELP]"
  ;print,"OR:        SUMD20,W[,'filename'][,STEP=0.1][,/HELP]"
  print,"Variables: Workspace (necessary, won't be changed)"
  print,"           basic filename different from 'd20' (optional, will be changed)"
  print,'Keywords:  STEP:   different 2*Theta stepwidth'
  print,'           HELP:   this hopefully helping text'
  print,'           ILLDAT: *.dat and *.ill exchanged (good for plotd1a ...)'
  print,'           GENERAL: *.dat in General Format for Two Axis Instruments (Type 5 in FullProf)'
  print,''
  print,'     Modification: 21-Aug-97 by Th.Hansen: bug-fix'
  print,'     Modification: 28-Aug-97 by Th.Hansen: bug-fix for GSAS format, 2nd line'
  print,'     modification: 03-Sep-97 by Th.Hansen: bug-fix for GSAS format: errors (variable count)'
  print,'     Modification: 29-Sep-97 by Th.Hansen: Addition of standard ILL format (Cerius)'
  print,'     Modification: 13-Oct-97 by Th.Hansen: gout sometimes too long ..., very small steps'
  print,'     Modification: 17-Nov-97 by Th.Hansen: so-called ILL-format (D2B, PlotD1A) corrected (Prassides)'
  print,'     Modification: 18-Nov-97 by Th.Hansen: automatic reading of numors - and correct titles (Prassides)'
  print,'Last modification: 10-Feb-98 by Th.Hansen: General Format for Two Axis Instruments (FullProf 5) (Fischer/Fauth)'
  print,''
ENDIF
IF N_ELEMENTS(w(*,0,0)) NE 1600 THEN BEGIN
  PRINT,'ATTENTION : Do you really have D20 data? Be aware of bugs!'
ENDIF
IF KEYWORD_SET(n0) THEN BEGIN
  IF NOT KEYWORD_SET(n1) THEN n1=n0
  IF n1 LE n0 THEN BEGIN
    n2=n1
    n1=n0
    n0=n1
  ENDIF
  numors=indgen(n1-n0+1)+n0
ENDIF
IF n_elements(filename) EQ 0 THEN filename = 'sum'
IF KEYWORD_SET(numors) THEN IF N_ELEMENTS(w) EQ 1 THEN IF N_PARAMS() EQ 1 THEN filename=w
IF STRLEN(filename) EQ 0 THEN filename ='sum'
IF (KEYWORD_SET(numors) AND N_ELEMENTS(numors) GT 1) THEN BEGIN
  filename=strmid(string(filename)+'000',0,3)
ENDIF ELSE IF (N_ELEMENTS(w(0,*)) GT 1) THEN filename=strmid(string(filename)+'000',0,3)
filename=string(filename)
filename = strcompress(filename,/REMOVE_ALL)
IF NOT KEYWORD_SET(numors) THEN BEGIN
  magnitude=ceil(alog10(n_elements(w(0,*))))
  nnn=0 
ENDIF ELSE BEGIN
  nnn=N_ELEMENTS(numors)-1  
  magnitude=ceil(alog10(nnn))
ENDELSE
ii=0
FOR k=0,nnn DO BEGIN
 IF KEYWORD_SET(numors) THEN BEGIN
   datp=1   
   w=rdrun(numors(k),datp=datp)
 ENDIF ELSE take_datp,datp 
 IF NOT KEYWORD_SET(step) THEN BEGIN
   step=TOTAL(datp.x(1:N_elements(datp.x(*,0,0))-1,0,0)-datp.x(0:N_elements(datp.x(*,0,0))-2,0,0))/(N_elements(datp.x(*,0,0))-1)
   dev=TOTAL(ABS((datp.x(1:N_elements(datp.x(*,0,0))-1,0,0)-datp.x(0:N_elements(datp.x(*,0,0))-2,0,0))-step))/(N_elements(datp.x(*,0,0))-1)
   IF dev GE step/10000. THEN BEGIN
     PRINT,'ATTENTION : Do you really have equidistant data? Deviation/step =', dev
     PRINT,'Otherwise you should better choose another export format (d20reg),'
     PRINT,'or interpolate your workspace before!'
   ENDIF
   print,'2Theta step : ',step
   IF datp.p(10<(N_ELEMENTS(datp.p)-1)) EQ 3.0 THEN step=datp.p(25<(N_ELEMENTS(datp.p)-1))
 ENDIF
 ;print,'Step: ',step
 e=datp.e
 IF N_ELEMENTS(e) NE N_ELEMENTS(w) THEN BEGIN
   e= SQRT(w)
   PRINT,'ATTENTION : error created (square root of counting rate)!'
 ENDIF
 cell = indgen(n_elements(w(*,0)))
 cell(*) = ROUND((datp.x(*,0)-datp.x(0,0))/step)
 factor=long(1)
 WHILE long(max(w)/factor) ge 1000000 DO factor=factor+1
 IF factor GT 1 THEN PRINT,'Factor is ',factor
 FOR i=0,n_elements(w(0,*))-1 DO BEGIN
  filext = strcompress(string(long(10.0^magnitude)+ii),/REMOVE_ALL)
  filext = strmid(filext,strlen(filext)-magnitude,magnitude)
  IF KEYWORD_SET(numors) THEN BEGIN
    print,'Numor ',numors(k),' => files : ',filename+filext+'.dat/.ill/.gsas/.gdat'
  ENDIF ELSE print,'Workspace => files : ',filename+filext+'.dat/.ill/.gsas/.gdat'
  ii=ii+1
  IF KEYWORD_SET(illdat) THEN BEGIN
    OPENW,ill,filename+filext+'.dat',/get_lun
    OPENW,out,filename+filext+'.ill',/get_lun
    OPENW,gen,filename+filext+'.gen',/get_lun
  ENDIF ELSE BEGIN
    IF KEYWORD_SET(general) THEN BEGIN
      OPENW,gen,filename+filext+'.dat',/get_lun
      OPENW,out,filename+filext+'.gen',/get_lun
      OPENW,ill,filename+filext+'.ill',/get_lun
    ENDIF ELSE BEGIN
      OPENW,out,filename+filext+'.dat',/get_lun
      OPENW,gen,filename+filext+'.gen',/get_lun
      OPENW,ill,filename+filext+'.ill',/get_lun
    ENDELSE
  ENDELSE
  OPENW,gsas,filename+filext+'.gsas',/get_lun
  line=STRMID(datp.w_tit,0,STRLEN(datp.w_tit)-1)+' '+STRMID(datp.other_tit,0,STRLEN(datp.other_tit)-1)
  line=STRMID(line,0,80<STRLEN(line))
  PRINTF,gen,'D1A5 D1A6 '
  PRINTF,gen,format='(A80)',datp.w_tit
  PRINTF,gen,format='(A80)',datp.other_tit
  p1=(N_ELEMENTS(datp.pv(*,0))-1)
  p2=(N_ELEMENTS(datp.pv(*,0))-1)
  PRINTF,gen,format='(I6,1X,2F10.3,i5,2f10.1)',max(cell)+1,datp.pv(12<p1,i<p2)<9999,datp.pv(11<p1,i<p2)<9999,1,datp.pv(30<p1,i<p2)<9999999,datp.n(0,0,i<((N_ELEMENTS(datp.n(0,0,*))-1)>0))/factor
  IF datp.pv(30<p1,i<p2) NE datp.n(0,0,i<((N_ELEMENTS(datp.n(0,0,*))-1)>0)) THEN BEGIN
    PRINT,'ATTENTION to non-integer normalisation factor (use only GENeral data format!) ',datp.pv(30<p1,i<p2) / datp.n(0,0,i<((N_ELEMENTS(datp.n(0,0,*))-1)>0))
  ENDIF
  PRINTF,ill,format='(A80)',line
  PRINTF,ill,format='(2I8,F8.3,3I8)',100,0,step,1,0,0;(max(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0)))-min(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0))))/step+1
  PRINTF,ill,format='(F8.2)',min(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0)))
  PRINTF,ill,format='(2F8.0,3F8.2)',(datp.n(0,0,i<((N_ELEMENTS(datp.n(0,0,*))-1)>0))/factor)<9999999,0,datp.pv((indgen(3)+10)<p1,i<p2)
  PRINTF,out,min(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0))),step,max(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0)))
  PRINTF,gen,format='(F10.4,F10.4,F10.4)',min(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0))),step,max(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0)))
  PRINTF,gsas,strmid(datp.w_tit+datp.other_tit+'                                                                                        ',0,80)
  gout=strmid(datp.w_tit+datp.other_tit+'                                                                                        ',0,80)
  outstr='BANK 1'+strcompress(string(max(cell)+1))+strcompress(string(max(cell)/10+1))+' CONST'+strcompress(string(round(100.*min(datp.x(*,i<((N_ELEMENTS(datp.x(0,*))-1)>0))))))+strcompress(string(round(step*100.)))+' 0 0 STD     '
  outstr=outstr+'                                                                                              '
  PRINTF,gsas,strmid(outstr,0,80)
  gout=gout+strmid(outstr,0,80)
  outstr=''
  genstr=''
  FOR j=0,max(cell) DO BEGIN
    IF j NE 0 THEN count_old=count
    index = where (cell eq j,count)
    IF count gt 0 THEN BEGIN
      IF e(index(0),i) GT 0 THEN count=long(round((SQRT(ABS(w(index(0),i)))/e(index(0),i))^2)) ELSE count=long(1)
      count = factor*count
      rate=long((w(index(0),i)+0.49999999)/factor)
      IF rate EQ 0 THEN BEGIN
        count=long(1)
      ENDIF
    ENDIF ELSE BEGIN          ;  no cell for this angle in the workspace
      print,'ATTENTION : no cell for this angle in the workspace!'
      print,'exriet.pro problem',j,count,rate,count_old
      count = long(1) 
      rate=long(0)  
    ENDELSE
    outstr=outstr+strmid(string(count<99),10,2)+strmid(string(rate),6,6)
    IF count gt 0 THEN genstr=genstr+string(w(index(0),i)/factor,format='(F8.0)') ELSE genstr=genstr+string(0,format='(F8.0)') 
    IF strlen(outstr) gt 72 THEN BEGIN
      PRINTF,gen,genstr
      PRINTF,out,outstr
      PRINTF,ill,outstr
      PRINTF,gsas,outstr
      IF STRLEN (gout) LT 32000 THEN gout=gout+outstr
      outstr=''
      genstr=''
    ENDIF
  ENDFOR
  IF strlen(outstr) GT 1 THEN BEGIN
    outstr=outstr+'                                                                                              '
    genstr=genstr+'                                                                                              '
    PRINTF,gen, strmid(genstr,0,80)
    PRINTF,out, strmid(outstr,0,80)
    PRINTF,ill, strmid(outstr,0,80)
    PRINTF,gsas,strmid(outstr,0,80)
    IF STRLEN (gout) LT 32000 THEN gout=gout+strmid(outstr,0,80)
  ENDIF

  genstr=''
  FOR j=0,max(cell) DO BEGIN
    index = where (cell eq j,count)
    IF count gt 0 THEN genstr=genstr+string(e(index(0),i)/factor,format='(F8.1)') ELSE genstr=genstr+string(0,format='(F8.1)')
    IF strlen(genstr) gt 72 THEN BEGIN
      PRINTF,gen,genstr
      genstr=''
    ENDIF
  ENDFOR
  IF strlen(genstr) GT 1 THEN BEGIN
    genstr=genstr+'                                                                                              '
    PRINTF,gen, strmid(genstr,0,80)
  ENDIF


  PRINTF,out,'   -1000'
  PRINTF,out,'  -10000'
  PRINTF,ill,'   -1000'
  PRINTF,ill,'  -10000'
  FREE_LUN,ill,out,gsas,gen
  OPENW,gdat,filename+filext+'.gdat',/get_lun
  PRINTF,gdat,gout
  FREE_LUN,gdat
 ENDFOR
ENDFOR
END
