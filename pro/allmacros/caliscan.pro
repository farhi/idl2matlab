function mean_and_var, x

on_error, 2
nx = n_elements(x)
if nx le 1 then $
  begin
    if nx eq 0 then BEGIN
		  mean=0
			var=0 
		ENDIF else BEGIN
		  mean=x
      var=x*x/(x+1)
		ENDELSE
  endif else begin 
    mean = total(x) / nx
    resid = x - mean
    r2 = total(resid^2)  
    var1 = r2 / (nx-1.0)
    var2 = (r2 - (total(resid)^2)/nx)/(nx-1.0)
    var =  (var1 + var2)/2.0
  endelse
return, [mean, var]
end


;******************************************************************************
FUNCTION caliscan,w,eff=eff,frac=frac,noplot=noplot,noprint=noprint,steps=steps,$
                    low=low,high=high,bad=bad,moreoutput=moreoutput
;+
; NAME:
;	caliscan
;
; PURPOSE:
;	This procedure creates the relative efficiencies of a PSD and a reconstituted powder pattern from a detector scan on a vanadium sample
;
; CATEGORY:
;	Instrument.
;
; CALLING SEQUENCE:
;	CALISCAN,w,eff=eff,frac=frac,noplot=noplot,noprint=noprint,steps=steps,low=low,high=high,bad=bad,moreoutput=moreoutput
;
; INPUTS:
;   w		:	Workspace (2D array, nd * steps elements, as defined in LAMP, normally a 2theta scan over a vanadium)
;
; OPTIONAL INPUTS:
;	none
;	
; KEYWORD PARAMETERS:
;	eff			:	relative efficiencies of the detector cells, only output parameter, becomes a 1D vector of nd elements
;	frac		:	?
;	noplot		:	must be set if no plotting output is wanted
;	noprint		:	must be set if no output in the output file caliscan.out is wanted
;	steps:		:	?
;	low			:	lower limit of counting rates to be taken into account for the computing
;	high		:	higher limit of counting rates to be taken into account for the computing
;	bad			:	?
;	moreoutput	:	produces more output (relative efficiencies) in caliscan.out if set

; OUTPUTS:
;	The reconstituted powder pattern
;
; OPTIONAL OUTPUTS:
;	none
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;	First loop: compare for each cell count rate with all other cells having counted at same positions
;	Second loop: choose the best correlation
;
; EXAMPLE:
;	RDSET,inst='D20',base="Loc Cycle 005" 
;	flag,/nor,/noint,/nobad,/flp,/noeff,/noang
;	w1=RDRUN(19512)
;   w2=CALISCAN(w1,eff=w3,low=22,high=32,/moreoutput)
;
; MODIFICATION HISTORY:
; 	Written by:		Thomas Hansen,	1997.
;	Modified by:	Thomas Hansen,	November 2000	(more output options, plot in window 1)
;
;-

TAKE_DATP,datp
IF NOT KEYWORD_SET(noplot) THEN window,1,xsize=1600,ysize=640
IF NOT KEYWORD_SET(noprint) THEN openw,output,'caliscan.out',/get_lun
IF NOT KEYWORD_SET(noprint) THEN printf,output
IF NOT KEYWORD_SET(noprint) THEN printf,output,'Calibration Scan - Version 2.1.0 beta'
IF NOT KEYWORD_SET(steps) THEN steps=N_ELEMENTS(UNIQ(datp.y))
IF NOT KEYWORD_SET(frac) THEN frac =N_ELEMENTS(w(0,*))/steps
frac=ROUND(frac)
IF frac LE 0 THEN frac=1
steps=N_ELEMENTS(w(0,*))/frac
IF NOT KEYWORD_SET(noprint) THEN printf,output,steps,' steps a ' ,frac,' acquisitions'
w=w(*,0:frac*steps-1)                 ; truncates interrupted scans
cells=N_ELEMENTS(w(*,0))
IF NOT KEYWORD_SET(noprint) THEN  flush,output

IF NOT KEYWORD_SET(noprint)  THEN printf,output
IF NOT KEYWORD_SET(noprint)  THEN printf,output,"************** Counting error calculation (standard deviations) *********"
IF NOT KEYWORD_SET(noprint)  THEN  flush,output
IF frac GT 1 THEN BEGIN
  sdeviation=FLTARR(cells,steps,2)
  FOR i=0,cells-1 DO BEGIN
    FOR j=0,steps-1 DO sdeviation(i,j,*)=mean_and_var(w(i,j*frac:(j+1)*frac-1)) 
  ENDFOR                             ; i=cells
  ave =     sdeviation(*,*,0)
  sdeviation=sqrt(sdeviation(*,*,1))
ENDIF ELSE BEGIN
  ave=w
  sdeviation=datp.e
  IF N_ELEMENTS(datp.e) NE N_ELEMENTS(W) THEN sdeviation=float(w)/SQRT(w+1.)
ENDELSE

NbOfBadCells = 0
IF KEYWORD_SET(bad) THEN BEGIN
  IF N_ELEMENTS(bad) LE 1 THEN BEGIN
	  IF STRING(ROUND(bad)) EQ STRING(ROUND(1)) THEN BEGIN
		  bad=P_LAMBDA()+'/BAD_CELLS/d20.bad' 
		ENDIF ELSE BEGIN
		  bad=STRING(bad)
		ENDELSE
		printF,output,'Reading bad cells from ',bad
		OPENR,badfile,bad,/get_lun
		READF,badfile,NbOfBadCells
		NbOfBadCells=ROUND(NbOfBadCells)
		printF,output,NbOfBadCells,' bad cells'
		IF NbOfBadCells GE 1 THEN BEGIN
		  bad=INTARR(NbOfBadCells)
			READF,badfile,bad
    ENDIF ELSE NbOfBadCells=0
  ENDIF ELSE NbOfBadCells=N_ELEMENTS(bad)
	IF NbOfBadCells GE 1 THEN PRINTF,output,'Bad Cells:  ',bad
ENDIF

plotflag=0
IF NOT KEYWORD_SET(noprint) THEN printf,output
IF NOT KEYWORD_SET(noprint) THEN printf,output,"************** Calculation of relative Efficiencies *******************"
IF NOT KEYWORD_SET(noprint) THEN flush,output
celleff   =fltarr(cells,cells)
cellefferr=fltarr(cells,cells)
cellrelerr=fltarr(cells,cells)
IF NOT KEYWORD_SET(low) THEN low=min(ave)>1.
IF NOT KEYWORD_SET(high) THEN high=max(ave)
FOR i=0,cells-1 DO BEGIN
	;IF NOT KEYWORD_SET(noprint) AND KEYWORD_SET(moreoutput) THEN printf,output,"--> Cell ",i
	;IF NOT KEYWORD_SET(noprint) AND KEYWORD_SET(moreoutput) THEN flush,output
	;print,i
	;  tmp=   min([cells,i+steps])
	;  thrdind=indgen(tmp)
	;  thrdind=thrdind(max([0,i-steps+1]):tmp-1)
	;  thrdind=thrdind(WHERE(thrdind NE i,thrdcnt))
	;  FOR jj=0,thrdcnt-1 DO BEGIN
	;    j=thrdind(jj)
  	FOR j=((i-steps+1)>0),i-1 DO BEGIN
    	kmin=max([j-i,0])
   	 	kmax=min([steps,steps+j-i])-1
   		eff=fltarr(steps)
    	err=fltarr(steps)
    	index=INDGEN(kmax-kmin+1)+kmin-j+i
    	secind=(WHERE((ave(j,index) GT low AND ave(j,index) LT high) AND (ave(i,index) GT low AND ave(i,index) LT high),count)) ; only valid steps for i AND j
		;IF NOT KEYWORD_SET(noprint) AND KEYWORD_SET(moreoutput) THEN printf,output,"---> step ",j,count
		;IF NOT KEYWORD_SET(noprint) AND KEYWORD_SET(moreoutput) THEN flush,output
    	IF count GT 0 THEN BEGIN
      		index=index(secind)                                   ; only valid steps for i AND j
      		eff(index)=eff(index)+ ave(i,index-i+j)/ ave(j,index)        ; ######### here was the great mistake! ########
      		err(index)=err(index)+(ave(i,index-i+j)*sdeviation(j,index)+ave(j,index)*sdeviation(i,index-i+j))/ave(j,index)/ave(j,index)
      		index=index(SORT(err(index)/eff(index)))
      		newerr=fltarr(count)
      		neweff=fltarr(count)
      		ind=indgen(count)
      		FOR k=0,count-1 DO BEGIN
        		neweff(k)=TOTAL(eff(index(0:k)))/FLOAT(k+1)
        		newerr(k)=TOTAL(err(index(0:k)))/FLOAT(k+1)/(FLOAT(k+1)/SQRT(k+1))
      		ENDFOR
      		minerr=min(newerr/neweff,k)
      		cellrelerr(i,j) =minerr
      		cellrelerr(j,i) =minerr
      		celleff(i,j)    =neweff(k)
      		cellefferr(i,j) =newerr(k)
      		IF neweff(k) NE 0 THEN celleff(j,i)=1./neweff(k) ELSE print,i,j,k,neweff(k)
      		cellefferr(j,i) =celleff(j,i)*minerr
			IF NOT KEYWORD_SET(noprint) AND KEYWORD_SET(moreoutput) THEN printf,output,i,j,celleff(i,j),cellefferr(i,j)
    	ENDIF 
		;IF NOT KEYWORD_SET(noprint) AND KEYWORD_SET(moreoutput) THEN printf,output,i,celleff(i,(i-2)>0:(i-1)>0),celleff(i,(i+1)<(cells-1):(i+2)<(cells-1))
  	ENDFOR                            ; j=min(cells,i+steps) 
  	plotindex=WHERE(celleff(i,*) GT 0,plotcount)
  	IF NOT KEYWORD_SET(noplot) AND plotcount GT 0 THEN BEGIN
  		IF plotflag EQ 0 THEN PLOT, plotindex,celleff(i,plotindex),xr=[0,1600],yr=[0.8,1.2] ELSE PLOT, plotindex,celleff(i,plotindex),xr=[0,1600],yr=[0.8,1.2],/NOERASE
  		plotflag=1
  	ENDIF
ENDFOR                            ; i=cells

PRINTF,output,'Cells out of limits:',ROUND(WHERE(TOTAL(cellefferr,1) EQ 0))
IF NbOfBadCells GE 1 THEN BEGIN
  celleff(bad,*)=celleff(bad,*)*0
  cellefferr(bad,*)=cellefferr(bad,*)*0
  cellrelerr(bad,*)=cellrelerr(bad,*)*0
  celleff(*,bad)=celleff(*,bad)*0
  cellefferr(*,bad)=cellefferr(*,bad)*0
  cellrelerr(*,bad)=cellrelerr(*,bad)*0
ENDIF
PRINTF,output,'Cells bad or out of limits: ',ROUND(WHERE(TOTAL(cellefferr,1) EQ 0))

;openw,effdat,'celleff.out',/get_lun
;printf,effdat,cells,cells
;printf,effdat,celleff
;printf,effdat,cellefferr
;printf,effdat,cellrelerr
;close,effdat
;free_lun,effdat

IF NOT KEYWORD_SET(noprint)  THEN printf,output
IF NOT KEYWORD_SET(noprint)  THEN printf,output,"*******************************************************************"
IF NOT KEYWORD_SET(noprint)  THEN printf,output,"************* Looking up best efficiency correlation **************"
minerr=fltarr(cells)
minind=intarr(cells)
med =REFORM(ave,cells*steps)
med=med(SORT(med))
med=med(cells*steps/2)
;print,'med',med
totave=TOTAL(ave(*,*),2)/N_ELEMENTS(ave(0,*))
FOR i=0,cells-1 DO BEGIN
  index=where(cellrelerr(i,*),count)          ; AND (ABS(celleff(i,*)-1.) LT 0.1),count)
  IF count GE 1 THEN BEGIN
    cellrelerr(i,index)=cellrelerr(i,index)*(ABS(celleff(i,index)-1.)+1.)*(ABS(totave(index)-med)*ABS(totave(i)-med)+med)
    minerr(i)=min(cellrelerr(i,index),j)
    ;print,i,totave(i),index(j),totave(index(j)),cellrelerr(i,index(j)),celleff(i,index(j)),cellefferr(i,index(j))
    j=index(j)
    minind(i)=j
  ENDIF 
ENDFOR
index=where(minerr,count)
minerr=min(minerr(index),i)
i=index(i)
j=minind(i)
;print,"min. error ",minerr, " for pair ",i,j
IF NOT KEYWORD_SET(noprint)  THEN flush,output
printf,output
printf,output,"************* Looked up best efficiency correlation ***************"
printf,output,"*******************************************************************"

printf,output,"********* Powder pattern reconstitution ***************"
pattern=fltarr(cells+steps-1)
error=  fltarr(cells+steps-1)
overlap=fltarr(cells+steps-1)
twotheta=findgen(cells+steps-1)*.1+datp.x(0,0)
newpattern=fltarr(cells+steps-1)
newerror=  fltarr(cells+steps-1)
patterncell=fltarr(cells+steps-1,steps)
eff=fltarr(cells)
err=fltarr(cells)
num=fltarr(cells)
start=i
n=0
oldeff=1
olderr=0
preceff=1
precerr=0
flag=intarr(cells)
flagref=intarr(1600)
refcount=0
count=cells
k=j
WHILE (n LT cells) AND (count GT 0) DO BEGIN
  eff(i)   =oldeff*preceff 
  err(i)   =olderr*preceff+oldeff*precerr  
  printf,output
  printf,output,'################################################################################'
	 printf,output,i,k,celleff(i,k),celleff(k,i),preceff
  printf,output,i,eff(i),err(i),ROUND(num(i)+1),k,preceff
  flush,output
  num(i)=num(i)+1
; ########## look for further references #############
  tmp=(1 EQ 1)
	 tmparr=fltarr(2*(steps-1))
	 tmparr(0)=eff(i)
	 tmpcounter=1
  WHILE refcount GT 0 AND tmp DO BEGIN   
    index=WHERE((cellefferr(i,*) NE 0.) AND (flag(*) EQ 1) AND (flagref(*) EQ 0),count)
    minerr=min(cellrelerr(i,index),k)
    k=index(k)
    refcount=count-1
    flagref(k)=1
    preceff=eff(k)
    precerr=err(k)                                          
    oldeff=celleff(i,k)
    olderr=cellefferr(i,k)
    neweff   = celleff(i,k)    * eff(k)
    neweff   =(neweff + num(i) * eff(i))/(num(i)+1)
    tmparr(tmpcounter)=neweff
		  newerr=mean_and_var(tmparr(0:tmpcounter))
		  newerr=SQRT(newerr(1)/(num(i)+1.))	
;   newerr   = cellefferr(i,k) * eff(k) + celleff(i,k) * err(k) 
;   newerr   =(newerr + num(i) * err(i))/(num(i)+1)/((num(i)+1)/sqrt((num(i)+2)))
    tmp=    ((newerr/neweff) LT (err(i)/eff(i)))
    nottmp= ((newerr/neweff) GE (err(i)/eff(i)))
    err(i)=tmp*newerr+(NOTtmp)*err(i)
    eff(i)=tmp*neweff+(NOTtmp)*eff(i)
    num(i)=num(i)+tmp*1
    printf,output,i,neweff,newerr,ROUND(num(i)),k,eff(k)
		  tmpcounter=tmpcounter+1
  ENDWHILE
; ########################################### Powder Pattern Reconstitution #######################################################
  index=indgen(steps)+i
  newpattern(index)  =(overlap(index)*pattern(index)+ ave(i,*)/eff(i))/(overlap(index)+1)
  tmp=patterncell(index,*) ;######### already ##############
 	tmp=[[tmp],[REFORM(ave(i,*)/eff(i),N_ELEMENTS(ave(i,*)))]]  
  tmparr=FLTARR(2,steps)
	 FOR tmpcounter=0,steps-1 DO BEGIN
		  tmptmp=WHERE(tmp(tmpcounter,*) NE 0,tmptmptmp)
	   IF tmptmptmp GT 0 THEN BEGIN
				  tmpvec=tmp(tmpcounter,tmptmp) 
	  	  tmpvec=REFORM(tmpvec,N_ELEMENTS(tmpvec))
	     tmparr(*,tmpcounter)=mean_and_var(tmpvec)
				ENDIF ELSE BEGIN
				  print,'Alert: Cell',i,' has no contribution for step',tmpcounter 
				ENDELSE
  ENDFOR
  newerror(index)=SQRT(tmparr(1,*)/(overlap(index)+1.))
; newerror  (i:i+steps-1)  =(overlap(i:i+steps-1)*  error(i:i+steps-1)+(err(i)*ave(i,*)+eff(i)*sdeviation(i,*))                 /eff(i)/eff(i))/(overlap(i:i+steps-1)+1)/((overlap(i:i+steps-1)+1)/sqrt((overlap(i:i+steps-1)+2)))
; newerror  (i:i+steps-1)  =(overlap(i:i+steps-1)*  error(i:i+steps-1)+(ABS(newpattern(i:i+steps-1)-pattern(i:i+steps-1)))/eff(i)/eff(i))/(overlap(i:i+steps-1)+1)/((overlap(i:i+steps-1)+1)/sqrt((overlap(i:i+steps-1)+2)))
; index =i+WHERE((ave(i,*) GT low AND ave(i,*) LT high) AND ((pattern(i:i+steps-1) EQ 0) OR ((newerror(i:i+steps-1)/newpattern(i:i+steps-1)) LE (error(i:i+steps-1)/pattern(i:i+steps-1)))),count)
  index =i+WHERE(newerror(index) NE 0 AND ((pattern(index) EQ 0) OR ((newerror(index)/newpattern(index)) LE (error(index)/pattern(index)))),count)
  printf,output,newpattern(i:i+(steps-1<5))
  printf,output,newerror  (i:i+(steps-1<5))
  printf,output,   pattern(i:i+(steps-1<5))
  printf,output,   error  (i:i+(steps-1<5))
  IF count GT 0 THEN BEGIN
    overlap(index)=overlap(index)+1
    pattern(index)=newpattern(index)
    error(index)  =newerror(index)
    patterncell(index,index-i)=pattern(index)
    printf,output,'Using',i,index(0:N_ELEMENTS(index)-1<4)
    IF NOT KEYWORD_SET(noplot) THEN BEGIN
  		PLOT,twotheta(WHERE(pattern)),pattern(WHERE(pattern)),xrange=[min(twotheta),max(twotheta)],yrange=[low,high],psym=3
	  	OPLOTERR,twotheta(WHERE(pattern)),pattern(WHERE(pattern)),error(WHERE(pattern)),3
    ENDIF
  ENDIF ELSE printf,output,'Not using',i
  out=pattern
  flag(i)=1
  n=n+1
  IF n LT cells THEN BEGIN
    ; #################### look for successor of cell i #######################################################
    index=WHERE((cellefferr(i,*) NE 0.) AND (flag(*) NE 1),count)
    ;print,n,count,' good successors'
    IF count EQ 0 THEN BEGIN
      index=WHERE(TOTAL(cellefferr,2) AND (flag(*) NE 1),count)
      ;PRINT,n,count,' -',index
      IF count NE 0 THEN BEGIN
        flagsum=intarr(N_ELEMENTS(index))
        for l=0,N_ELEMENTS(index)-1 DO flagsum(l)=TOTAL(flag ((index(l)-steps)>0: ( (index(l)+steps)<cells) -1)) 
        secind=(WHERE(flagsum,count))
        IF count NE 0 THEN BEGIN
          maxref=max(flagsum,j)
          j=index(j)
          ;PRINT,n,count,j
        ENDIF ELSE BEGIN
          n=(count EQ 0)*cells + (count NE 0)*n
          PRINTf,output,n,count,' The End - Not considered cells:',WHERE(flag(*) NE 1,count)
        ENDELSE
      ENDIF ELSE BEGIN
        n=(count EQ 0)*cells + (count NE 0)*n
        PRINTf,output,n,count,' Another End - Not considered cells:',WHERE(flag(*) NE 1,count)
      ENDELSE
    ENDIF ELSE BEGIN
      minerr=min(cellrelerr(i,index),j)
      j=index(j)
    ENDELSE
    ; ############## look for first and best reference ################
    IF count NE 0 THEN BEGIN
      flagref=intarr(1600)
      index=WHERE((cellefferr(j,*) NE 0.) AND (flag(*) EQ 1),count)
      IF count GT 0 THEN BEGIN
        minerr=min(cellrelerr(j,index),k)
        k=index(k)
        refcount=count-1
        flagref(k)=1
        oldeff=celleff(j,k)
        olderr=cellefferr(j,k)
        preceff=eff(k)
        precerr=err(k)
        i=j
      ENDIF
    ENDIF
  ENDIF
ENDWHILE

w_tit=datp.w_tit
mod_datp,datp,'w_tit',w_tit+": resulting diffractogram"
mod_datp,datp,'y_tit',"counting rate per single ACQ"
mod_datp,datp,'x',twotheta
mod_datp,datp,'e',error
mod_datp,datp,'n',overlap
give_datp,datp
openw,pat,'pattern.out',/get_lun
printf,pat,cells+steps-1
printf,pat,twotheta,pattern,error,overlap
printf,pat,w_tit+": resulting diffractogram"
printf,pat,datp.other_tit
printf,pat,datp.x_tit,"counting rate per single ACQ"
close,pat
free_lun,pat
;ploterr,twotheta,pattern,error
IF KEYWORD_SET(eff) THEN BEGIN
  mod_datp,datp,'w_tit',w_tit+": resulting efficiencies"
  mod_datp,datp,'x_tit',"cell number"
  mod_datp,datp,'y_tit',"efficiency 1/alpha"
  mod_datp,datp,'x',indgen(cells)
  mod_datp,datp,'e',err
  mod_datp,datp,'n',num
  give_datp,datp,/third
ENDIF
openw,effdat,'eff.out',/get_lun
printf,effdat,cells
printf,effdat,indgen(cells),eff,err,num
printf,effdat,w_tit+": resulting efficiencies"
printf,effdat,datp.other_tit
printf,effdat,"cell number"
printf,effdat,"efficiency 1/alpha"
close,effdat
free_lun,effdat
IF NOT KEYWORD_SET(noprint) THEN close,output
IF NOT KEYWORD_SET(noprint) THEN free_lun,output
RETURN,out
END
