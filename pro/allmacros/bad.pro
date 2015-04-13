function bad,n1,stop=n2,sensitivity=sens,zero=zerostat,low=lowstat,high=highstat,m=mstat,w=wstat

common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
, cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
, shf_in6 , cal_d20 , ang_d20 , inf_d20 , bad_d20 ,flag_d20, wav_d20, psd_d20  $
, inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5  , inf_in6
			   
zerostat=fltarr(1600)
lowstat=fltarr(1600)
highstat=fltarr(1600)
mstat=fltarr(1600)
wstat=fltarr(1600)
IF NOT KEYWORD_SET(sens) THEN sens=3
NbOfZeroCounters=0
lambda  =P_LAMBDA()
badname =strcompress(n1,/re)+'.bad'
print,"File containing bad cells : ",badname," "
OPENW,bad,badname,/get_lun
n0=n1
IF NOT KEYWORD_SET(n2) THEN n2=999999
datp=1
counter=0
hole=1
for numor = long(n1), LONG(n2) do begin
  flag,/soft,old=flag_restore,/noprint,/eff,/nobad,/noang,/noint,/nowav,/flp,/nonor,/nocor
  w=rdrun(numor,datp=datp)
  flag,/soft,new=flag_restore,/noprint
  counter=counter + N_ELEMENTS(w(0,*))
		nodata=0
  WHILE n_elements(w) le 1 DO BEGIN 
			IF nodata EQ 0 THEN nomore=numor
		 IF NOT KEYWORD_SET(wait) THEN BEGIN
			  nodata=nodata+1
			  print,'No data for ',numor
			  numor=numor+1
			ENDIF ELSE BEGIN
			 IF nodata EQ 0 THEN BEGIN
			   print,'Waiting for ',numor,' since ',systime()
			   numor=nomore
      WAIT,wait*60
      FLUSH,bad
      mod_datp,datp,'x',indgen(1600)
      mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
      mod_datp,datp,'p',[n1,numor-1]
      mod_datp,datp,'e',error
      mod_datp,datp,'n',0
      mod_datp,datp,'y',y
      mod_datp,datp,'w_tit','Deviation from expected counting rate'
      mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
      mod_datp,datp,'y_tit','factor'
      mod_datp,datp,'x_tit','cell'
      give_datp,datp
				ENDIF
				nodata=1
			ENDELSE
   flag,/soft,old=flag_restore,/noprint,/eff,/nobad,/noang,/noint,/nowav,/flp,/nonor,/nocor
   w=rdrun(numor,datp=datp)
   flag,/soft,new=flag_restore,/noprint
   counter=counter + N_ELEMENTS(w(0,*))
			IF n_elements(w) le 1 AND nodata GE hole THEN BEGIN
			 numor=nomore
    print,'No more numors from ',numor
    CLOSE,bad
				FREE_LUN,bad
				PRINT,badname ,' written
				flag,new=flag_restore,/noprint
    mod_datp,datp,'x',indgen(1600)
    mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
    mod_datp,datp,'p',[n1,numor-1]
    mod_datp,datp,'e',error
    mod_datp,datp,'n',0
    mod_datp,datp,'y',y
    mod_datp,datp,'w_tit','Deviation from expected counting rate'
    mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
    mod_datp,datp,'y_tit','factor'
    mod_datp,datp,'x_tit','cell'
    give_datp,datp
		  RETURN,deviation
   ENDIF
  ENDWHILE
  j=WHERE(cal_d20)
  FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN w(j,i)=w(j,i)/cal_d20(j) 
  ENDFOR
		IF KEYWORD_SET(wait) AND nodata EQ 1 THEN BEGIN
		ENDIF
		index=WHERE(datp.n EQ 1.95646080e+07,counts)
  IF counts GE 1 THEN datp.n(index) =0

  WOUT=w
  EE=datp.e
  
  zerocounters= WHERE(TOTAL(REFORM(wout,N_ELEMENTS(wout(*,0)),N_ELEMENTS(wout(0,*))),2) LE 0)
  tmp=N_ELEMENTS(ZeroCounters)
		NbOfZeroCounters=tmp
  print,''
  print,'Numor ',numor
  printf,bad,''
  printf,bad,'Numor ',numor
  IF NbOfZeroCounters GE 1 THEN PRINT,' Zero ', STRMID(ZeroCounters(0:13<N_ELEMENTS(ZeroCounters)-1),8,4)
  IF NbOfZeroCounters GE 1 THEN PRINTf,bad,' Zero ', STRMID(ZeroCounters(0:13<N_ELEMENTS(ZeroCounters)-1),8,4)
  tmp=0*ZeroCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfZeroCounters GE 1 THEN FOR i=0,NbOfZeroCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ ZeroCounters(i) THEN tmp(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfZeroCounters GE 1 THEN BEGIN
    ind=where(tmp,count)
    IF count GE 1 THEN PRINT,' excl.', STRMID(ZeroCounters(ind),8,4)
    IF count GE 1 THEN PRINTf,bad,' excl.', STRMID(ZeroCounters(ind),8,4)
  ENDIF
  IF NbOfZeroCounters GE 1 THEN zerostat(ZeroCounters)=zerostat(ZeroCounters)+1
  tmp=TOTAL(REFORM(w,N_ELEMENTS(w(*,0)),N_ELEMENTS(w(0,*))),2)
  ind=findgen(N_ELEMENTS(tmp)-4)+2

  smoothed=tmp
  smoothed(ind)=2./3.*(tmp(ind-1)+tmp(ind-1))-(tmp(ind-2)+tmp(ind-2))/6.>0

  sigm=TOTAL(REFORM(EE,N_ELEMENTS(EE(*,0)),N_ELEMENTS(EE(0,*))),2)/SQRT(N_ELEMENTS(EE(0,*)))
  tmp1=tmp-sens*sigm
  tmp2=tmp+sens*sigm

  MCounters=2+WHERE(tmp2(ind) LT (tmp1(ind-2)+tmp1(ind+2))/2./1.00 AND tmp1(ind-1)/1.00 GT (3.*tmp2(ind-2)+tmp2(ind+2))/4. AND tmp1(ind+1)/1.00 GT (tmp2(ind-2)+3.*tmp2(ind+2))/4.,NbOfMCounters)
  IF NbOfMCounters GE 2 THEN MCounters=MCounters(SORT((tmp(MCounters+2)+tmp(MCounters+1)+tmp(MCounters)+tmp(MCounters-1)+tmp(MCounters-2))/((tmp(MCounters+1)+tmp(MCounters-1))/2.-tmp(MCounters))))
  IF NbOfMCounters GE 1 THEN PRINT,' "M"  ', STRMID(MCounters(0:13<N_ELEMENTS(MCounters)-1),8,4)
  IF NbOfMCounters GE 1 THEN PRINTf,bad,' "M"  ', STRMID(MCounters(0:13<N_ELEMENTS(MCounters)-1),8,4)
  tmp0=0*MCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfMCounters GE 1 THEN FOR i=0,NbOfMCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ MCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfMCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
    IF count GE 1 THEN PRINT,' excl.', STRMID(MCounters(ind0),8,4)
    IF count GE 1 THEN PRINTf,bad,' excl.', STRMID(MCounters(ind0),8,4)
  ENDIF
  IF NbOfMCounters GE 1 THEN Mstat(MCounters)=Mstat(MCounters)+1

  WCounters=2+WHERE(tmp1(ind)/1.00 GT (tmp2(ind-2)+tmp2(ind+2))/2. AND tmp2(ind-1) LT (3.*tmp1(ind-2)+tmp1(ind+2))/4./1.00 AND tmp2(ind+1) LT (tmp1(ind-2)+3.*tmp1(ind+2))/4./1.00,NbOfWCounters)
  IF NbOfWCounters GE 2 THEN WCounters=WCounters(SORT((tmp(WCounters+2)+tmp(WCounters+1)+tmp(WCounters)+tmp(WCounters-1)+tmp(WCounters-2))/(tmp(WCounters)-(tmp(WCounters+1)+tmp(WCounters-1))/2.)))
  IF NbOfWCounters GE 1 THEN PRINT,' "W"  ', STRMID(WCounters(0:13<N_ELEMENTS(WCounters)-1),8,4)
  IF NbOfWCounters GE 1 THEN PRINTf,bad,' "W"  ', STRMID(WCounters(0:13<N_ELEMENTS(WCounters)-1),8,4)
  tmp0=0*WCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfWCounters GE 1 THEN FOR i=0,NbOfWCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ WCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfWCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
    IF count GE 1 THEN PRINT,' excl.', STRMID(WCounters(ind0),8,4)
    IF count GE 1 THEN PRINTf,bad,' excl.', STRMID(WCounters(ind0),8,4)
  ENDIF
  IF NbOfWCounters GE 1 THEN Wstat(WCounters)=Wstat(WCounters)+1

  ind=findgen(N_ELEMENTS(tmp)-4)+2

  LowCounters =2+WHERE(tmp(ind) LT (smoothed(ind)-4.*SQRT(smoothed(ind)>0))-ABS(2.*smoothed(ind)-tmp(ind-1)-tmp(ind+1))/1.-ABS(tmp(ind-1)-tmp(ind+1))/2. AND tmp(ind) GT 0,NbOfLowCounters)
  IF NbOfLowCounters GE 2 THEN LowCounters=LowCounters(SORT(smoothed(LowCounters)/(smoothed(LowCounters)-tmp(LowCounters))))
  IF NbOfLowCounters GE 1 THEN PRINT,' Low  ', STRMID(LowCounters(0:13<N_ELEMENTS(LowCounters)-1),8,4)
  IF NbOfLowCounters GE 1 THEN PRINTf,bad,' Low  ', STRMID(LowCounters(0:13<N_ELEMENTS(LowCounters)-1),8,4)
  tmp0=0*LoWCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfLoWCounters GE 1 THEN FOR i=0,NbOfLoWCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ LoWCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfLoWCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
    IF count GE 1 THEN PRINT,' excl.', STRMID(LoWCounters(ind0),8,4)
    IF count GE 1 THEN PRINTf,bad,' excl.', STRMID(LoWCounters(ind0),8,4)
  ENDIF
  IF NbOfLoWCounters GE 1 THEN LoWstat(LoWCounters)=LoWstat(LoWCounters)+1

  HighCounters =2+WHERE(tmp(ind) GT (smoothed(ind)+4.*SQRT(smoothed(ind)>0))+ABS(2.*smoothed(ind)-tmp(ind-1)-tmp(ind+1))/1.+ABS(tmp(ind-1)-tmp(ind+1))/2.,NbOfHighCounters)
  IF NbOfHighCounters GE 2 THEN HighCounters=HighCounters(SORT(smoothed(HighCounters)/tmp((HighCounters)-smoothed(HighCounters))))
  IF NbOfHighCounters GE 1 THEN PRINT,' High ', STRMID(HighCounters(0:13<N_ELEMENTS(HighCounters)-1),8,4)
  IF NbOfHighCounters GE 1 THEN PRINTf,bad,' High ', STRMID(HighCounters(0:13<N_ELEMENTS(HighCounters)-1),8,4)
  tmp0=0*HighCounters
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfHighCounters GE 1 THEN FOR i=0,NbOfHighCounters-1 DO BEGIN
    FOR j=0,N_ELEMENTS(bad_d20)-1 DO BEGIN
      IF bad_d20(j) EQ HighCounters(i) THEN tmp0(i)=1 
    ENDFOR
  ENDFOR
  IF N_ELEMENTS(bad_d20) GE 1 AND NbOfHighCounters GE 1 THEN BEGIN
    ind0=where(tmp0,count)
    IF count GE 1 THEN PRINT,' excl.', STRMID(HighCounters(ind0),8,4)
    IF count GE 1 THEN PRINTf,bad,' excl.', STRMID(HighCounters(ind0),8,4)
  ENDIF
  IF NbOfHighCounters GE 1 THEN Highstat(HighCounters)=Highstat(HighCounters)+1
  
  ind=indgen(N_ELEMENTS(tmp))
  IF N_ELEMENTS(deviation) EQ 0 THEN BEGIN
    deviation=7.*tmp(ind)/(tmp(ind)+tmp((ind-1)>0)+tmp((ind+1)<(N_ELEMENTS(tmp)-1))+tmp((ind-2)>0)+tmp((ind+2)<(N_ELEMENTS(tmp)-1))+tmp((ind-3)>0)+tmp((ind+3)<(N_ELEMENTS(tmp)-1)))
    error    =2.*sigm(ind)/(tmp((ind-1)>0)+tmp((ind+1)<(N_ELEMENTS(tmp)-1)))+2.*tmp(ind)/(tmp((ind-1)>0)+tmp((ind+1)<(N_ELEMENTS(tmp)-1)))^2*(sigm((ind-1)>0)+sigm((ind+1)<(N_ELEMENTS(tmp)-1)))
  ENDIF ELSE BEGIN
    deviation=[[deviation], [7.*tmp(ind)/(tmp(ind)+tmp((ind-1)>0)+tmp((ind+1)<(N_ELEMENTS(tmp)-1))+tmp((ind-2)>0)+tmp((ind+2)<(N_ELEMENTS(tmp)-1))+tmp((ind-3)>0)+tmp((ind+3)<(N_ELEMENTS(tmp)-1)))]]
    error    =[[error    ], [2.*sigm(ind)/(tmp((ind-1)>0)+tmp((ind+1)<(N_ELEMENTS(tmp)-1)))+2.*tmp(ind)/(tmp((ind-1)>0)+tmp((ind+1)<(N_ELEMENTS(tmp)-1)))^2*(sigm((ind-1)>0)+sigm((ind+1)<(N_ELEMENTS(tmp)-1)))]]
  ENDELSE
  FLUSH,bad
ENDFOR
CLOSE,bad
FREE_LUN,bad
PRINT,badname ,' written
mod_datp,datp,'x',indgen(1600)
mod_datp,datp,'par_txt',['First Numor:  ','Last Numor:   ']
help,n1,numor
mod_datp,datp,'p',[n1,numor-1]
mod_datp,datp,'e',error
mod_datp,datp,'n',0
mod_datp,datp,'y',y
mod_datp,datp,'w_tit','Deviation from expected counting rate'
mod_datp,datp,'other_tit','Numors from'+STRING(n0)+' to'+STRING(n2)
mod_datp,datp,'y_tit','factor'
mod_datp,datp,'x_tit','cell'
give_datp,datp

return,deviation
END

;+
; NAME:
;	BAD
;
; PURPOSE:
;	Detection of bad cells in a range of numors.
;
; CATEGORY:
;	Instrument
;
; CALLING SEQUENCE:
;	BAD, N1, N2
;
; INPUTS:
;	N1:	first numor.
;
; OPTIONAL INPUTS:
;	N2: last numor..
;	
; KEYWORD PARAMETERS: See also psplot!
;	SENSITIVITY:	sensitivity in multiples of sigma (default 3).
;	ZERO: will contain zero-counting cells.
;	LOW: will contain low-counting cells.
;	HIGH: will contain high-counting cells.
;	M: will contain "M"-counting cells.
;	W: will contain "W"-counting cells.
;
; OUTPUTS:
;	onscreen plot
;	relative deviation from expected counting rate
;
; OPTIONAL OUTPUTS:
;	cell numbers by keywords, see above.
;
; COMMON BLOCKS:
;	Calibration
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;	no foobar superfloatation method.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by:	Thomas Hansen, April 1998.
;	May, 1998	Documentation
;-
