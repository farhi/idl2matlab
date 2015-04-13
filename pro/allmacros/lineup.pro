function LINEUP, win ,elas ,goodx ,exclu ,decl ,TOLOW ,ERRORS=wer
;******* ******
;**
;** aline  elastic peaks having a shift less that TOBAD canals  (D. Richard).
;** Output:ELAS  for the peaks position.
;**        GOODX for a list of spectra having a good intensity and placement peak.
;**        EXCLU for a list of spectra having a bad  intensity or  placement peak.

wout=0
siz =size(win)
if   siz (0) eq 2 then begin

;  ********    				      *************
   TOBAD=60  & if n_elements(TOLOW) eq 0 then TOLOW=15./100 else TOLOW=TOLOW>0.01
;  ********    				      *************
   sy  =siz(2)
   wout=win
   cm  =fltarr(sy)
   eldx=intarr(sy)

   for  i=0,sy-1 do begin
        tmp      =win(*,i)                                         ;make one peak
	wout(*,i)=smooth(smooth(median(tmp,3),3),4)	           ;
	cm  (  i)=max   (tmp,elas)                                 ;get peak index
	eldx(  i)=elas                                             ;
   endfor

   a   =max(eldx,min=b) & elas=eldx(0)
   if a ne b then begin   his =histogram(eldx)                     ;elastic peak
   			  tmp =max(his  ,c)                        ;
   			  elas=min(eldx)+c        		   ;position
			  endif
   exclu =-1 & idg=0

   for  i=0,sy-1 do begin					   ;verify index
	if abs(elas-eldx(i)) ge TOBAD  then begin		   ;
		    eldx(i)=elas & exclu=[exclu,i]		   ;
	endif else  idg   =[idg,i]				   ;
   endfor

   decl=elas - eldx						   ;how  to  shift

   for  i=0,sy-1 do wout(*,i) =shift(win(*,i),decl(i))		   ;do the work
   if n_elements(wer) eq n_elements(wout) then $		   ;** *** ****
   for  i=0,sy-1 do wer (*,i) =shift(wer(*,i),decl(i))		   ;** *** ****
	
   idg =idg(1:*)						   ;minimum peak
   ng  =n_elements(idg)						   ;
   off =ng/10                                                      ;
   pri =cm(idg)                                                    ;
   pri =pri(sort(pri))                                             ;
   c   =total  (pri(off:ng-off-1))/(ng-2*off)                      ;
   c1  =c*TOLOW  &  c2=c/TOLOW                                     ;

   pry =total (wout(*,idg)>0,2)                                    ;noisy spectra
   pidx=sort  (pry)                                                ;
   pidx=pidx  (4:(n_elements(pidx)/40)>5)                          ;
   tmp =wout  (*,idg)>0                                            ;
   tmp =total (tmp(pidx,*),1) / n_elements(pidx)                   ;
   c   =total (tmp)/ng/TOLOW                                       ;
   for i=0,ng-1 do if tmp(i) gt c+1 then cm(idg(i))=0              ;

   goodx =-1
   for  i=0,ng-1 do if (cm(idg(i)) ge c1) and (cm(idg(i)) le c2) $ ;make goodx
		 then goodx=[goodx,idg(i)] $			   ;
		 else exclu=[exclu,idg(i)]			   ;

   if n_elements(goodx) gt 1 then goodx =goodx(1:*)
   if n_elements(exclu) gt 1 then begin
				  exclu =exclu(1:*)
   				  exclu =exclu(sort(exclu))
   endif
endif else P_MUS,'mus_cannon'

return,wout
end
