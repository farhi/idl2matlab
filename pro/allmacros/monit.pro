function monit,help=help,noprint=noprint,plot=plot,start=n1,cyc=cyc,wait=wait,stop=n2,dir=lambdm,vis=vis,hole=hole,loops=loops
;+
; Extracts the monitor counting rate from your raw data and updates catalogues
; in the directory d20sgi.ill.fr:~lambda'+divider+'MONITOR such as 981.cat, 981.132 and 
; 981.all for a cycle 981. These files can only be created 
; on d20sgi.ill.fr. The first time in a new cycle the file last.cyc in 
; ~lambda/CALIBRATION should be updated and the keyword start, giving the first
; numor of that cycle, be used. With the keyword vis you write the output
; not to the directory ~lambda/MONITOR but to your actual directory where 
; you work in - and have write access. With the keyword dir you can define
; another directory to read/write in. Especially on
; tektronix terminals you should use the keyword noplot.
; Modified    28 April 1998, Th. HANSEN
; Modified 27 November 2000, Th. HANSEN
; Modified    12 April 2001, Th. HANSEN
;-
if !version.release ge '5.0' then FORWARD_FUNCTION RDRUN
common calibration
common d20
divider = sys_dep('DIVIDER')
if keyword_set(help) then begin
  print,"Extracts the monitor counting rate from your raw data and updates catalogues"
  print,"in the directory d20sgi.ill.fr:~lambda'+divider+'MONITOR such as 981.cat, 981.132 and "
  print,"981.all for a cycle 981. These files can only be created "
  print,"on d20sgi.ill.fr. The first time in a new cycle the file last.cyc in "
  print,"~lambda/CALIBRATION should be updated and the keyword start, giving the first"
  print,"numor of that cycle, be used. With the keyword vis you write the output"
  print,"not to the directory ~lambda/MONITOR but to your actual directory where "
  print,"you work in - and have write access. With the keyword dir you can define"
  print,"another directory to read/write in. Especially on"
  print,"tektronix terminals you should use the keyword noplot."
  print,"Last Update 28 April 1998, Th. HANSEN"
endif
if NOT keyword_set(loops) then loops=999999 
loop=0
if keyword_set(plot) then noplot=0 ELSE noplot=1
submon=0
subtim=0
unit=1
unittxt='s '
NbOfZeroCounters=0
P_LAMBDA,lambda
Mhor=0
Mver=0
alpha1=0
wav=0
IF KEYWORD_SET(cyc) THEN cyc=ROUND(cyc)  ELSE BEGIN
  OPENR,in,LAMBDA+'CALIBRATION'+divider+'last.cyc',/get_lun
  READF,in,cyc
  CLOSE,in
  FREE_LUN,in
ENDELSE
xicutstr="RDSET  ,inst='D20'  ,base='C_Year "
IF cyc LT 700 THEN xicutstr=xicutstr+'20' ELSE xicutstr=xicutstr+'19'
xicutstr=xicutstr+STRMID(STRCOMPRESS(STRING(LONG(1000+cyc)),/remove_all),1,2)+" L',cycle='"
xicutstr=xicutstr+STRMID(STRCOMPRESS(STRING(LONG(1000+cyc)),/remove_all),1,3)
xicutstr=xicutstr+"'"
XICUTE,xicutstr
print,xicutstr
IF KEYWORD_SET(vis) THEN BEGIN
  IF NOT KEYWORD_SET(lambdm) THEN lambdm=''
  print,'Welcome, Visitor! Your catalogue/monitor files are written to ',lambdm
  lambdm=lambdm+divider+STRMID(STRCOMPRESS(STRING(LONG(1000+cyc)),/remove_all),1,3)
ENDIF ELSE BEGIN
  IF NOT KEYWORD_SET(lambdm) THEN lambdm=LAMBDA+'MONITOR'
  lambdm  =lambdm+divider+STRMID(STRCOMPRESS(STRING(LONG(1000+cyc)),/remove_all),1,3)
  print,'Welcome! Your catalogue/monitor files are written to ',lambdm
ENDELSE
filename=lambdm+'.mon'
lastnum=lambdm+'.'
print,"Files : ",filename," etc."
catname =lambdm+'.cat'
lctname =lambdm+'.132'
allname =lambdm+'.all'
corname =lambdm+'.cor'
attname =lambdm+'.att'
badname =lambdm+'.bad'
corrections=0
IF NOT KEYWORD_SET(n1) THEN BEGIN
  OPENR,cor,corname,/get_lun
  READF,cor,corrections
  IF corrections GT 0 THEN BEGIN
      cornum=fltarr(3,corrections)
        READF,cor,cornum
  ENDIF
  CLOSE,cor
  FREE_LUN,cor
  OPENW,cat,catname,/get_lun,/APPEND
  OPENW,lct,lctname,/get_lun,/APPEND
  OPENW,all,allname,/get_lun,/APPEND
  OPENW,att,attname,/get_lun,/APPEND
  OPENW,bad,badname,/get_lun,/APPEND
  OPENR,in,filename,/get_lun
  READF,in,n0,n1,refday,elements,unit
  n0=LONG(n0)
  n1=LONG(n1)
  refday=LONG(refday)
  elements=LONG(elements)
  unit=LONG(unit)
  IF unit EQ 7*86400 THEN unittxt='w '
  IF unit EQ   86400 THEN unittxt='d '
  IF unit EQ   3600  THEN unittxt='h '
  IF unit EQ 60      THEN      unittxt='mn'
  x=fltarr(elements)
  e=lonarr(elements)
  monitor=lonarr(elements)
  y=intarr(elements)
  n=lonarr(elements)
  READF,in,x
  READF,in,monitor
  READF,in,e
  READF,in,n
  READF,in,y
  CLOSE,in
  FREE_LUN,in
  CALDAT,refday,MON,DD,YY
  since=STRCOMPRESS(STRING(DD),/remove_all)+divider+STRCOMPRESS(STRING(MON),/remove_all)+divider+$
          STRCOMPRESS(STRING(YY),/remove_all)
  newfiles=0
ENDIF ELSE BEGIN
  OPENW,cor,corname,/get_lun
  PRINTF,cor,corrections
  CLOSE,cor
  FREE_LUN,cor
  OPENW,cat,catname,/get_lun
  OPENW,lct,lctname,/get_lun
  OPENW,all,allname,/get_lun
  OPENW,att,attname,/get_lun
  OPENW,bad,badname,/get_lun
  PRINT,'Numor Sub      Date     Time Usr Wav Scan Mod Time Monitor PSD-cnts Sample/Title'
  PRINTF,cat,'Numor Sub      Date     Time Usr Wav Scan Mod Time Monitor PSD-cnts Sample/Title'
  PRINTF,lct,'Numor Sub      Date     Time Usr Wav Scan Mod Time Monitor PSD-cnts 2Theta Omega   Chi   Phi   Tr1   Tr2 Sample/Title'
  PRINTF,all,'Numor Sub      Date     Time  User     Wav alp1 Scn Step Mod Cyc SliceTime total CntTim   Monit PSD-cnts 2Theta Omega   Chi   Phi Transl1 Transl2 Tset Treg Tsam Title                                    Sample'
  n0=n1
  refday=0
  newfiles=1
ENDELSE
IF NOT KEYWORD_SET(n2) THEN n2=999999
datp=1
counter=0
IF NOT KEYWORD_SET(hole) THEN hole=0
for numor = long(n1), LONG(n2) do begin
  flag,/soft,old=flag_restore,/noprint,/eff,/nobad,/noang,/noint,/nowav,/flp,/nonor,/nocor
  w=rdrun(numor,datp=datp)
  print,numor ; 20 September 2002
  flag,/soft,new=flag_restore,/noprint
  counter=counter + N_ELEMENTS(w(0,*))
    nodata=0
  WHILE (n_elements(w) le 1 or N_ELEMENTS(datp.p) LE 1) DO BEGIN 
     IF nodata EQ 0 THEN nomore=numor
     IF NOT KEYWORD_SET(wait) THEN BEGIN
        nodata=nodata+1
        print,'No data for ',numor
        numor=numor+1
     ENDIF ELSE BEGIN
       IF nodata EQ 0 THEN BEGIN
         print,'Waiting for ',numor,' since ',systime()
         printF,att,'Waiting for ',numor,' since ',systime()
         numor=nomore
        OPENW ,out,filename,/get_lun
        PRINTF,out,LONG(n0),LONG(numor),LONG(refday),LONG(N_elements(monitor)),LONG(unit)
        PRINTF,out,FLOAT(x),LONG(monitor),FLOAT(e),LONG(n),ROUND(y)
        CLOSE,out
        WAIT,wait*60
        FLUSH,cat
        FLUSH,lct
        FLUSH,all
        FLUSH,att
        FLUSH,mon ; 20 September 2002
        mod_datp,datp,'x',x
        mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
        mod_datp,datp,'p',[n1,numor-1]
        mod_datp,datp,'e',e
        mod_datp,datp,'n',n
        mod_datp,datp,'y',y
        mod_datp,datp,'w_tit','Monitor/time for Cycle'+STRING(cyc)
        mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
        mod_datp,datp,'y_tit','cnt/sec'
        mod_datp,datp,'x_tit','time/s'
        give_datp,datp
      ENDIF
      nodata=1
    ENDELSE
    flag,/soft,old=flag_restore,/noprint,/eff,/nobad,/noang,/noint,/nowav,/flp,/nonor,/nocor
    w=rdrun(numor,datp=datp)
    flag,/soft,new=flag_restore,/noprint
    counter=counter + N_ELEMENTS(w(0,*))
    IF ((n_elements(w) le 1 AND nodata GE hole) and not newfiles) THEN BEGIN
      numor=nomore
      print,'No more numors from ',numor
      OPENW ,out,filename,/get_lun
      PRINTF,out,LONG(n0),LONG(numor),LONG(refday),LONG(N_elements(monitor)),LONG(unit)
      PRINTF,out,FLOAT(x),LONG(monitor),FLOAT(e),LONG(n),ROUND(y)
      CLOSE,out
      CLOSE,cat
      CLOSE,lct
      CLOSE,all
      CLOSE,att
      CLOSE,bad
      FREE_LUN,out,cat,lct,all,att,bad
      PRINT,filename ,' and ', catname ,' written'
      flag,new=flag_restore,/noprint
      mod_datp,datp,'x',x
      mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
      mod_datp,datp,'p',[n1,numor-1]
      mod_datp,datp,'e',e
      mod_datp,datp,'n',n
      mod_datp,datp,'y',y
      mod_datp,datp,'w_tit','Monitor/time for Cycle'+STRING(cyc)
      mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
      mod_datp,datp,'y_tit','cnt/sec'
      mod_datp,datp,'x_tit','time/s'+unittxt+' since '+since
      give_datp,datp
      RETURN,monitor
    ENDIF
  ENDWHILE
  newfiles=0
  j=WHERE(cal_d20)
  FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN 
    w(j,i)=w(j,i)/cal_d20(j) 
  ENDFOR
    ;IF KEYWORD_SET(wait) AND nodata EQ 1 THEN BEGIN
    ;ENDIF
  index=WHERE(datp.n EQ 1.95646080e+07,counts)
  IF counts GE 1 THEN datp.n(0,0,index) =0
  corflag=0.
  IF datp.p(0) NE 0 OR N_ELEMENTS(w(0,*)) EQ 1 THEN BEGIN
    comtim=datp.p(25)*datp.p(35)*2./8000000.
    IF cyc LE 972 AND cyc GE 963 OR cyc EQ 973 AND numor LE 8918 THEN comtim=comtim/2.
        if ABS(comtim-datp.p(5)/N_ELEMENTS(w(0,*))) GE 0.01>(datp.p(5)/N_ELEMENTS(w(0,*)))/20000 THEN BEGIN
          corflag=1.
      PRINT,FORMAT="(I6,' PresetTime(/Step) NE CntTime because of reparable bug:',F7.2,'-',F7.2)",numor,comtim,datp.p(5)/N_ELEMENTS(w(0,*))
      PRINTF,att,FORMAT="(I6,' PresetTime(/Step) NE CntTime because of reparable bug:',F7.2,'-',F7.2)",numor,comtim,datp.p(5)/N_ELEMENTS(w(0,*))
      FLUSH,att
            IF corrections EQ 0 THEN BEGIN
              cornum=[numor,submon,subtim]
            ENDIF ELSE BEGIN
               cornum=[[cornum],[numor,submon,subtim]]
       ENDELSE
      OPENW,cor,corname,/get_lun
      PRINTF,cor,corrections
        PRINTF,cor,cornum
      CLOSE,cor
        FREE_LUN,cor
            corrections=corrections+1
            datp.p(5)=datp.p(5)-subtim*N_ELEMENTS(w(0,*))
            datp.pv(5,*)=datp.pv(5,*)-subtim
            datp.pv(30,*)=datp.pv(30,*)-submon
            datp.p(30)=datp.p(30)-submon
            datp.n(0,0,*)=datp.n(0,0,*)-submon
        ENDIF
      comtim=comtim*N_ELEMENTS(w(0,*))
  ENDIF ELSE BEGIN
      comtim=TOTAL(datp.pv(25,*)*datp.p(35)/8000000.)
    IF datp.p(0) EQ 0 THEN BEGIN
          submon=datp.n(0,0,1)
            subtim=datp.pv(5,1)
        ENDIF    
    ENDELSE
    IF ABS(comtim-datp.p(5)) GE 0.01>(datp.p(5))/500 THEN BEGIN
      PRINT,FORMAT="(I6,' (Total) PresetTime (still) NE CntTime:',F8.2,'-',F8.2)",numor,comtim,datp.p(5)
      PRINTF,att,FORMAT="(I6,' (Total) PresetTime (still) NE CntTime:',F8.2,'-',F8.2)",numor,comtim,datp.p(5)
      FLUSH,att
    ENDIF
    totmon=ROUND(TOTAL(datp.n/datp.p(5)))
    ;totPSD=TOTAL(FLOAT(w<100000*MAX(datp.pv(5,*)))/datp.p(5)/1600.)
    ;totmon=ROUND(TOTAL(datp.n(0,0,*))/datp.p(5))
    IF N_ELEMENTS(w(0,*)) GT 1 THEN BEGIN
      totPSD=TOTAL(w,2)/datp.p(5)/1600. 
      maxPSD=MAX(TOTAL(w,2))/datp.p(5)
    ENDIF ELSE BEGIN
      totPSD=TOTAL(w)/datp.p(5)/1600.
      maxPSD=MAX(TOTAL(w))/datp.p(5)
    ENDELSE
    RA=0
    IF STRLEN(datp.w_tit) GT 11 THEN RA=1
    print,'again: ',numor ; 20 September 2002
    PRINT,FORMAT="(I6,I4,A19,A5,F4.1,'A',I3,I2,I6,'s',I6,'/s',I5,'/c/s ',A11)",$
      numor,N_ELEMENTS(w(0,*)),strmid(datp.other_tit,3,19),strmid(datp.other_tit,28,4),$
        datp.p(33),ROUND(datp.p(0)),LONG(datp.p(34)-1),ROUND(datp.p(5)),totmon,$
        total(totPSD),$
        STRMID(STRCOMPRESS(datp.w_tit,REMOVE_ALL=RA),0,11)
		help,datp.w_tit,datp.other_tit,w(0,*),datp.p,totmon,totPSD,total(totpsd)
    print,'again and again: ',numor ; 20 September 2002
    IF numor gt 0 THEN PRINTF,cat,FORMAT="(I6,I4,A19,A5,F4.1,'A',I3,I2,I6,'s',I6,'/s',I5,'/c/s ',A11)",$
      numor,N_ELEMENTS(w(0,*)),strmid(datp.other_tit,3,19),strmid(datp.other_tit,28,4),$
        datp.p(33),ROUND(datp.p(0)),LONG(datp.p(34)-1),ROUND(datp.p(5)),totmon,$
        total(totPSD),$
        STRMID(STRCOMPRESS(datp.w_tit,REMOVE_ALL=RA),0,11)
    RA=0
    IF STRLEN(datp.w_tit) GT 27 THEN RA=1
    IF numor gt 0 THEN PRINTF,lct,FORMAT="(I6,I4,A19,A5,F4.1,'A',I3,I2,I6,'s',I6,'/s',I5,'/c/s ',6F6.1,A27)",$
      numor,N_ELEMENTS(w(0,*)),strmid(datp.other_tit,3,19),strmid(datp.other_tit,28,4),$
        datp.p(33),LONG(datp.p(0)),LONG(datp.p(34)),ROUND(datp.p(5)),totmon,$
        total(totPSD),datp.p(13:18)<999,$
        STRMID(STRCOMPRESS(datp.w_tit,REMOVE_ALL=RA),0,27)
    IF numor gt 0 THEN PRINTF,all,FORMAT="(I6,I4,A19,A9,F4.1,'A',I3,'`',I3,F6.2,I2,I6,F9.2,'ms',I6,'s',I5,'s',I6,'/s',I5,'/c/s ',4F6.1,F6.1,'mm',F6.1,'mm',I4,'K',I4,'K',I4,'K',A80,F5.2,I7,I8)",$
      numor,N_ELEMENTS(w(0,*)),strmid(datp.other_tit,3,19),strmid(datp.other_tit,28,8),$
        datp.p([33,31]),LONG(datp.p(0)),(datp.p(0) GT 1)*datp.p(28),LONG(datp.p(34:35)),datp.p(25)/8000.,ROUND(datp.p(5)),$
        ROUND(datp.pv(5,0)),totmon,$
        total(totPSD),datp.p(13:18)<999,$
        LONG(datp.p(10:12)<999),STRMID(datp.w_tit+'                                                                                                        ',0,80),[subtim,submon]*corflag,maxpsd

  zerocounters= WHERE(TOTAL(REFORM(w,N_ELEMENTS(w(*,0)),N_ELEMENTS(w(0,*))),2) LE 0)
  tmp=N_ELEMENTS(ZeroCounters)
    IF tmp GT NbOfZeroCounters THEN BEGIN
      PRINTF,att,FORMAT="(I6,' More zero-counting cells (',I4,') than before (',I4,')')",numor,tmp,NbOfZeroCounters
      IF tmp GT 3 AND tmp-1 GT NbOfZeroCounters THEN PRINT,FORMAT="(I6,' More zero-counting cells (',I4,') than before (',I4,')')",numor,tmp,NbOfZeroCounters
    ENDIF
    NbOfZeroCounters=tmp
  PRINTF,bad,' '
  IF NbOfZeroCounters GE 1 THEN PRINTF,bad,STRMID(numor,strlen(numor)-6,6),' Zero ', STRMID(ZeroCounters(0:13<N_ELEMENTS(ZeroCounters)-1),8,4)

  tmp=TOTAL(REFORM(w,N_ELEMENTS(w(*,0)),N_ELEMENTS(w(0,*))),2)*cal_d20

  ind=findgen(N_ELEMENTS(tmp)-4)+2

  smoothed=tmp
  smoothed(ind)=2./3.*(tmp(ind-1)+tmp(ind-1))-(tmp(ind-2)+tmp(ind-2))/6.>0

  MCounters=2+WHERE(tmp(ind) LT (tmp(ind-2)+tmp(ind+2))/2./1.06 AND tmp(ind-1)/1.02 GT (3.*tmp(ind-2)+tmp(ind+2))/4. AND tmp(ind+1)/1.02 GT (tmp(ind-2)+3.*tmp(ind+2))/4.,NbOfMCounters)
  IF NbOfMCounters GE 2 THEN MCounters=MCounters(SORT((tmp(MCounters+2)+tmp(MCounters+1)+tmp(MCounters)+tmp(MCounters-1)+tmp(MCounters-2))/((tmp(MCounters+1)+tmp(MCounters-1))/2.-tmp(MCounters))))
  IF NbOfMCounters GE 1 THEN PRINTF,bad,STRMID(numor,strlen(numor)-6,6),' "M"  ', STRMID(MCounters(0:13<N_ELEMENTS(MCounters)-1),8,4)

  WCounters=2+WHERE(tmp(ind)/1.06 GT (tmp(ind-2)+tmp(ind+2))/2. AND tmp(ind-1) LT (3.*tmp(ind-2)+tmp(ind+2))/4./1.02 AND tmp(ind+1) LT (tmp(ind-2)+3.*tmp(ind+2))/4./1.02,NbOfWCounters)
  IF NbOfWCounters GE 2 THEN WCounters=WCounters(SORT((tmp(WCounters+2)+tmp(WCounters+1)+tmp(WCounters)+tmp(WCounters-1)+tmp(WCounters-2))/(tmp(WCounters)-(tmp(WCounters+1)+tmp(WCounters-1))/2.)))
  IF NbOfWCounters GE 1 THEN PRINTF,bad,STRMID(numor,strlen(numor)-6,6),' "W"  ', STRMID(WCounters(0:13<N_ELEMENTS(WCounters)-1),8,4)

  ind=findgen(N_ELEMENTS(tmp)-4)+2

  LowCounters =2+WHERE(tmp(ind) LT (smoothed(ind)-4.*SQRT(smoothed(ind)>0))-ABS(2.*smoothed(ind)-tmp(ind-1)-tmp(ind+1))/1.-ABS(tmp(ind-1)-tmp(ind+1))/2. AND tmp(ind) GT 0,NbOfLowCounters)
  IF NbOfLowCounters GE 2 THEN LowCounters=LowCounters(SORT(smoothed(LowCounters)/(smoothed(LowCounters)-tmp(LowCounters))))
  IF NbOfLowCounters GE 1 THEN PRINTF,bad,STRMID(numor,strlen(numor)-6,6),' Low  ', STRMID(LowCounters(0:13<N_ELEMENTS(LowCounters)-1),8,4)

  HighCounters =2+WHERE(tmp(ind) GT (smoothed(ind)+4.*SQRT(smoothed(ind)>0))+ABS(2.*smoothed(ind)-tmp(ind-1)-tmp(ind+1))/1.+ABS(tmp(ind-1)-tmp(ind+1))/2.,NbOfHighCounters)
  IF NbOfHighCounters GE 2 THEN HighCounters=HighCounters(SORT(smoothed(HighCounters)/tmp((HighCounters)-smoothed(HighCounters))))
;  FOR hi=0,NbOfHighCounters-1 DO BEGIN
;    PRINTF,bad,HighCounters(hi)
;    PRINTF,bad,HighCounters(hi)-2+indgen(5)
;    PRINTF,bad,tmp(HighCounters(hi)-2+indgen(5))
;    PRINTF,bad,smoothed(HighCounters(hi)-2+indgen(5))
;  ENDFOR
  IF NbOfHighCounters GE 1 THEN PRINTF,bad,STRMID(numor,strlen(numor)-6,6),' High ', STRMID(HighCounters(0:13<N_ELEMENTS(HighCounters)-1),8,4)

  FLUSH,bad
  DD=0
  MON=0
  YY=0
  HH=0
  MM=0
  SS=0
  day      = strmid(datp.other_tit,4,2)
  READS,day,DD
  month    = strmid(datp.other_tit,7,3)
  year     = strmid(datp.other_tit,11,2)
  READS,year,YY
  IF YY LT 70 THEN YY=YY+2000 ELSE IF YY LE 99 THEN YY=YY+1900
  hour     = strmid(datp.other_tit,14,2)
  READS,hour,HH
  min      = strmid(datp.other_tit,17,2)
  READS,min,MM
  sec      = strmid(datp.other_tit,20,2)
  READS,sec,SS
  CASE month OF
     'Jan':  MON=1
     'Feb':  MON=2
     'Mar':  MON=3
     'Apr':  MON=4
     'May':  MON=5
     'Jun':  MON=6
     'Jul':  MON=7
     'Aug':  MON=8
     'Sep':  MON=9
     'Oct':  MON=10
     'Nov':  MON=11
     'Dec':  MON=12
  ENDCASE          
  IF refday EQ 0 THEN BEGIN
    refday=JULDAY(MON,DD,YY)
    day = 0
  since=STRCOMPRESS(STRING(DD),/remove_all)+divider+STRCOMPRESS(STRING(MON),/remove_all)+divider+$
          STRCOMPRESS(STRING(YY),/remove_all)
    PRINT,'First Day of Cycle',STRCOMPRESS(STRING(LONG(cyc))),': ',since
  ENDIF ELSE day=JULDAY(MON,DD,YY)-refday
  sec=(day*24.*3600.+HH*3600.+MM*60.+SS)
 lowpassed=0
  highpassed=0
  PRINT ,'Elements:',LONG(N_ELEMENTS(w(0,*)))
  FOR i=0L,LONG(N_ELEMENTS(w(0,*)))-1L DO BEGIN
  help,i
      if sec/unit GE 120. THEN BEGIN
        IF unit EQ 86400 THEN BEGIN
              unit=604800
              unittxt='w '
              IF N_ELEMENTS(x) GE 1 THEN x=x/7.
        ENDIF
        IF unit EQ 3600 THEN BEGIN
              unit=86400
              unittxt='d '
              IF N_ELEMENTS(x) GE 1 THEN x=x/24.
        ENDIF
        IF unit EQ 60 THEN BEGIN
              unit=3600
              unittxt='h '
              IF N_ELEMENTS(x) GE 1 THEN x=x/60.
        ENDIF
        IF unit EQ 1 THEN BEGIN
              unit=60
              unittxt='mn'
              IF sec/unit GE 120. THEN BEGIN
                  unit=3600
                  unittxt='h '
              ENDIF
              IF N_ELEMENTS(x) GE 1 THEN x=x/unit
        ENDIF
      ENDIF
      IF NOT KEYWORD_SET(noprint) THEN BEGIN
          PRINT,FORMAT="(I6,I5,A19,F7.2,A3,I7,'/s',A5,F4.1,'A',I6,'s',F6.2,I7)",$
               numor,i,strmid(datp.other_tit,4,18),$
                 sec/unit,unittxt, datp.n(0,0,i)/datp.pv(5,i),strmid(datp.other_tit,28,4),$
                    datp.p(33),$
                    ROUND(datp.pv(5,i)),[subtim,submon]*corflag
      ENDIF
      IF ROUND(10.*datp.p(33)) GE 23 THEN BEGIN     ; HOPG Monochromator
            lowmon=43000
            highmon=54000
        ENDIF ELSE BEGIN
          IF ROUND(10.*datp.p(33)) GE 12 THEN BEGIN   ; Copper Monochromator, high take-off
              lowmon=39000
             highmon=49000
          ENDIF ELSE BEGIN                             ; Copper Monochromator, low take-off
            IF ROUND(10.*datp.p(33)) GE 9 THEN BEGIN   ; Copper Monochromator, low take-off
                lowmon=24000
               highmon=28000
            ENDIF ELSE BEGIN                            ; Copper Monochromator, low take-off
                lowmon=10000
               highmon=13000
              ENDELSE
            ENDELSE
        ENDELSE 
    IF ROUND(datp.p(31)) LE 21 THEN BEGIN     ; alpha1 = 20 minutes
                lowmon=lowmon/2
                highmon=highmon/2
        ENDIF                    
    IF ROUND(datp.p(31)) LE 11 THEN BEGIN     ; alpha1 = 10 minutes
               lowmon=lowmon/5
                highmon=highmon/3
       ENDIF
    IF datp.p(20) LT 60 THEN BEGIN
      lowmon=round(lowmon/60.*datp.p(20))
      highmon=round(highmon/60.*datp.p(20))
    ENDIF
    IF datp.p(21) LT 280 THEN BEGIN
      lowmon=round(lowmon/280.*datp.p(21))
      highmon=round(highmon/280.*datp.p(21))
    ENDIF
	;print,'test'
    IF Mhor NE datp.p(20) OR Mver NE datp.p(21) OR alpha1 NE datp.p(31) OR wav NE datp.p(33) THEN BEGIN
            PRINT,FORMAT="('New Config. ',I5,'/s<Monitor<',I5,'/s for lam=',F3.1,'A, alph1=',I2,'`, M-apert=',I2,'/',I3,'mm')",lowmon,highmon,datp.p([33,31,20,21])
            ;help,lowmon,highmon,datp.p([33,31,20,21])
            PRINTF,att,FORMAT="('New Config. ',I5,'/s<Monitor<',I5,'/s for lam=',F3.1,'A, alph1=',I2,'`, M-apert=',I2,'/',I3,'mm')",lowmon,highmon,datp.p([33,31,20,21])
       IF KEYWORD_SET(noprint) THEN BEGIN
              PRINT,FORMAT="(I6,A19,I7,'/s',A5)",$
                  numor,strmid(datp.other_tit,4,18),$
                    datp.n(0,0,i)/datp.pv(5,i),strmid(datp.other_tit,28,4)
          ENDIF
    ENDIF
	print,'test2'
    Mhor=datp.p(20)
    Mver=datp.p(21)
    alpha1=datp.p(31)
    wav=datp.p(33)
    IF datp.pv(5,i) LE 0 THEN BEGIN
      datp.n(0,0,i) = 0
      datp.pv(5,i) = 1
      PRINTF,att,FORMAT="(I6,I5,' CountingTime is zero or negative')",numor,i
      PRINT,FORMAT="(I6,I4,' CountingTime is zero or negative')",numor,i
    ENDIF ELSE BEGIN
          IF datp.n(0,0,i)/datp.pv(5,i) GE highmon+SQRT((datp.n(0,0,i)))/datp.pv(5,i) AND NOT highpassed THEN BEGIN
            PRINTF,att,FORMAT="(I6,I5,' Monitor too HIGH for lambda/alpha1:',I7,'/s (',F3.1,'A,',I3,'`)')",numor,i,datp.n(0,0,i)/datp.pv(5,i),datp.p([33,31])
            ;help,numor,i,datp.n(0,0,i)/datp.pv(5,i),highmon,datp.p([33,31,20,21])
            PRINT,FORMAT="(I6,I5,' Monitor=',I7,'/s>',I5,'/s for lam=',F3.1,'A, alph1=',I2,'`, M-apert=',I2,'/',I3,'mm')",numor,i,datp.n(0,0,i)/datp.pv(5,i),highmon,datp.p([33,31,20,21])
;	print,'test3'
        IF KEYWORD_SET(noprint) THEN BEGIN
              PRINT,FORMAT="(I6,A19,I7,'/s',A5)",$
                  numor,strmid(datp.other_tit,4,18),$
                    datp.n(0,0,i)/datp.pv(5,i),strmid(datp.other_tit,28,4)
          ENDIF
        FLUSH,att
        FLUSH,mon ; 20 September 2002
        highpassed=1
; 	print,'test4'
         ENDIF ELSE BEGIN
            IF datp.n(0,0,i)/datp.pv(5,i) LE  lowmon-SQRT((datp.n(0,0,i)))/datp.pv(5,i) AND NOT lowpassed THEN BEGIN
          IF datp.n(0,0,i) LE 0 THEN BEGIN
            datp.n(0,0,i) = 0
            PRINTF,att,FORMAT="(I6,I5,' Monitor is zero or negative')",numor,i
            PRINT,FORMAT="(I6,I4,' Monitor is zero or negative')",numor,i
            IF KEYWORD_SET(noprint) THEN BEGIN
                  PRINT,FORMAT="(I6,A19,I7,'/s',A5)",$
                      numor,strmid(datp.other_tit,4,18),$
                        datp.n(0,0,i)/datp.pv(5,i),strmid(datp.other_tit,28,4)
             ENDIF
          ENDIF ELSE BEGIN
                PRINTF,att,FORMAT="(I6,I5,' Monitor too low for lambda/alpha1:',I7,'/s (',F3.1,'A,',I3,'`)')",numor,i,datp.n(0,0,i)/datp.pv(5,i),datp.p([33,31])
                ;PRINT,FORMAT="(I6,I5,' Monitor too low for lambda/alpha1:',I7,'/s (',F3.1,'A,',I3,'`)')",numor,i,datp.n(0,0,i)/datp.pv(5,i),datp.p([33,31])
                PRINT,FORMAT="(I6,I5,' Monitor=',I7,'/s<',I5,'/s for lam=',F3.1,'A, alph1=',I2,'`, M-apert=',I2,'/',I3,'mm')",numor,i,datp.n(0,0,i)/datp.pv(5,i),lowmon,datp.p([33,31,20,21])
            IF KEYWORD_SET(noprint) THEN BEGIN
                  PRINT,FORMAT="(I6,A19,I7,'/s',A5)",$
                      numor,strmid(datp.other_tit,4,18),$
                        datp.n(0,0,i)/datp.pv(5,i),strmid(datp.other_tit,28,4)
             ENDIF
          FLUSH,att
                  ENDELSE
                  lowpassed=1
        ENDIF
          ENDELSE
        ENDELSE
    datp.pv(5,i)=datp.pv(5,i)<10000.
    datp.pv(5,i)=datp.pv(5,i)>0.000000125
        datp.n(0,0,i)=datp.n(0,0,i)<(100000*datp.pv(5,i))
    datp.n(0,0,i)=datp.n(0,0,i)>0
    monerr=SQRT(datp.n(0,0,i)>0.)/datp.pv(5,i) > 0.
    monerr=monerr < 1000000.
 	;print,'test5'
    IF N_ELEMENTS(monitor) EQ 0 THEN BEGIN
 	;print,'test5aa'
      monitor=(datp.n(0,0,i)/datp.pv(5,i))
      e=monerr
      x=sec/unit
      n=numor
      y=i
    ENDIF ELSE BEGIN
 	;print,'test5ab'
      monitor=[monitor,(datp.n(0,0,i)/datp.pv(5,i))]
      e=[e,monerr]
      x=[x,sec/unit]
      n=[n,numor]
      y=[y,i]
    ENDELSE
 	;print,'test5b'
    sec=sec+datp.pv(5,i)
    monitor=[monitor,(datp.n(0,0,i)/datp.pv(5,i))]
    e=[e,monerr]
    x=[x,sec/unit]
    n=[n,numor]
    y=[y,i]
 	;print,'test5c'
    IF NOT KEYWORD_SET(noplot) THEN BEGIN
 	print,'test5d'
       PLOT,x,monitor,psym=3,$
         yrange=[min(monitor),max(monitor)],$
         ytitle='Monitor/Time [cnts/sec]',$
         xtitle='Time/'+unittxt+' since '+since ,xticks=4
 	;print,'test5e'
	;help,x,monitor,e
         if n_elements(x) lt 2000 then OPLOTERR,x,monitor,e,3
 	;print,'test5f'
    ENDIF
 	;print,'test6',i
  ENDFOR 
 	;print,'test7',i
  ;help,loop
  loop=loop+1
  IF loop GE loops  THEN BEGIN
      numor=numor+1
      print,loop, " numors read, as you've asked for - that's enough for the moment, see you later!"
      OPENW ,out,filename,/get_lun
      PRINTF,out,LONG(n0),LONG(numor),LONG(refday),LONG(N_elements(monitor)),LONG(unit)
      PRINTF,out,FLOAT(x),LONG(monitor),FLOAT(e),LONG(n),ROUND(y)
      CLOSE,out
      CLOSE,cat
      CLOSE,lct
      CLOSE,all
      CLOSE,att
      CLOSE,bad
      FREE_LUN,out,cat,lct,all,att,bad
      PRINT,filename ,' and ', catname ,' written'
      flag,new=flag_restore,/noprint
      mod_datp,datp,'x',x
      mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
      mod_datp,datp,'p',[n1,numor-1]
      mod_datp,datp,'e',e
      mod_datp,datp,'n',n
      mod_datp,datp,'y',y
      mod_datp,datp,'w_tit','Monitor/time for Cycle'+STRING(cyc)
      mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
      mod_datp,datp,'y_tit','cnt/sec'
      mod_datp,datp,'x_tit','time/s'+unittxt+' since '+since
      give_datp,datp
      RETURN,monitor
  ENDIF
ENDFOR
OPENW,out,filename,/get_lun
PRINTF,out,LONG(n0),LONG(numor),LONG(refday),LONG(N_elements(monitor)),LONG(unit)
PRINTF,out,FLOAT(x),LONG(monitor),FLOAT(e),LONG(n),ROUND(y)
CLOSE,out
CLOSE,cat
CLOSE,lct
CLOSE,all
CLOSE,att
CLOSE,bad
FREE_LUN,out,cat,lct,all,att,bad
PRINT,filename ,' and ', catname ,' written'
mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
mod_datp,datp,'p',[n1,numor-1]
mod_datp,datp,'e',e
mod_datp,datp,'n',n
mod_datp,datp,'y',y
mod_datp,datp,'w_tit','Monitor/time for Cycle'+STRING(cyc)
mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
mod_datp,datp,'y_tit','cnt/sec'
mod_datp,datp,'x_tit','time/'+unittxt+' since '+since
give_datp,datp

return,monitor
END
