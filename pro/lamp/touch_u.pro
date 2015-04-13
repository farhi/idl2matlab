;**
;** Most importante modules since LAMP exists.
;** **** ********** ******* ***** **** ******
;**

function TOUCH_ATTENT , LABT, DESK ,whatdo ,weit
;******* ************
;**
res=0
	  
;**	     Be attentive for an event
;**	     -- --------- --- -- -----
	     if DESK gt 0 then begin
	        if weit eq 1 then evv=widget_event(/nowait) else begin  evv=widget_event([LABT,DESK],bad_id=ii)
	        							if ii ne 0 then res=1 & endelse
;		NOT TIMER,MANUAL,M TOUCH
;		************************
	  	if (evv.id  ne LABT) then $
	  	if (evv.top eq DESK) and (whatdo ne 3) and (whatdo ne 7) then begin
    		   text='Touch Update is stopped ...'
    		   if LABT gt 0 then widget_control,bad_id=ii,LABT,set_value=text else print,text
	  	   res=1
	  	endif else whatdo=-5
		widget_control,/hourglass
	     endif
	     
return,res
end

pro TOUCH_U , CYC , LAB1 , LAB2 , DESK
;** *******
;**

common touch_u,T_ALL
common desk,whatdo,b1
@lamp.cbk

T_BASE=lamp_touch

;TOUCH_X, lamp_ins ,T_BASE+lamp_dvd ,1
 TOUCH_X, nothing  ,T_BASE+lamp_dvd ,1

if n_elements(LAB1)   le 0  then LAB1=0
if n_elements(LAB2)   le 0  then LAB2=0
if n_elements(DESK)   le 0  then DESK=0
whatdo=-5

T_BASE=lamp_touch
WI    =1 & WS='w1'
fdat  = 'DATE'    & bid=sys_dep      ('POT',fdat)
fcat  = 'catalog' & bid=sys_dep      ('POT',fcat)
ferr  = 'iLAMP'   & bid=sys_dep      ('POT',ferr)
fdef  = 'touch.up'

if DESK gt 0 then begin LABT=widget_label(widget_base(map=0))
			widget_control,LABT,group_leader=DESK,/realize & endif

if n_elements(T_ALL) eq 0 THEN T_ALL =[''] & tb0=['']
if n_elements(T_DEF) eq 0 THEN T_DEF =''
T_FLAG=-1 & DOITK=0 & F_RUNK=0 & L_RUNK=0

;******************
;START of TIME LOOP
;******************
WHILE (1) DO BEGIN

	  
res=TOUCH_ATTENT (LAB1, DESK ,whatdo ,1)
if res eq 1 then return
	     	     
IF T_FLAG le 0 THEN BEGIN
   tb2=sys_dep       ('DIRD',T_BASE+lamp_dvd)
   idx=where(strpos(tb2,fdef) ge 0) & idx=idx(0)
   IF idx ge 0 THEN if tb2(idx) eq T_DEF then T_FLAG=1 else T_DEF=tb2(idx)
ENDIF

IF T_FLAG le 0 THEN BEGIN
  ;********************
  ;READ INSTRUMENT LIST
  ;********************
   TOUCH_R,t_up,t_inst,t_fnu,t_lnu,c_y,y_e,t_sd,t_st,t_ip,exist

   if (exist ne 1) and (T_FLAG eq -1) then begin
   	print,string(7b) & text=T_BASE+lamp_dvd+fdef+' not found ...!'
   	if LAB1 gt 0 then widget_control,bad_id=ii,LAB1,set_value=text else print,text
   	return & endif
   T_FLAG=1
   IF exist eq 1 THEN BEGIN
    YEAR  =strmid (systime(),20,4) ;or YEAR=y_e
    CYC   =c_y
    CYCLS =strtrim(string(CYC),2)

    INST  =t_inst(1:*)
    NNT   =n_elements(INST)
    SUB   =t_sd(1:*)*9
    GROUP =strarr(NNT) & FOR i=0,NNT-1 DO BEGIN idx=where(lamp_ins eq INST(i)) & idx=idx(0)
					        if idx ge 0 then GROUP(i)=lamp_grp(idx) & ENDFOR
    KPTN  =t_st(1:*)
    t	  =t_up(1:*)
    DOITK =t - DOITK	& idx=where(DOITK eq 1)
    DOIT  =t 		& if idx(0) ge 0 then DOIT(idx)= DOIT(idx)*2
    DOITK =t
    t	  =long(t_fnu(1:*))
    F_RUNK=t - F_RUNK	& idx=where(F_RUNK ne 0)
    F_RUN =t 		& if idx(0) ge 0 then DOIT(idx)=(DOIT(idx)*2)<2
    F_RUNK=t
    t	  =long(t_lnu(1:*))
    L_RUNK=t - L_RUNK	& idx=where(L_RUNK ne 0)
    L_RUN =t 		& if idx(0) ge 0 then DOIT(idx)=(DOIT(idx)*2)<2
    L_RUNK=t
    C_BASE=t_ip(1:*)+lamp_dvd
   ENDIF
ENDIF ELSE DOIT=DOITK

if (n_elements(LASTR) eq 0) or (n_elements(LASTR) ne NNT) then LASTR=lonarr(NNT)
if (n_elements(LASTT) eq 0) or (n_elements(LASTT) ne NNT) then LASTT=lonarr(NNT)

;**	Find directories that changed.
;**	---- ----------- ---- -------
IF total(DOIT) gt 0 THEN BEGIN
	idx=where (DOIT gt 0)
	bbb=C_BASE(idx) & bbb=bbb(sort(bbb))
	ppp='???'
	FOR i=0,n_elements(bbb)-1 DO BEGIN
	    if bbb(i) ne ppp then begin
	       ppp=bbb(i)
	       tb2=sys_dep       ('DIRD',ppp)  &  tb2=strlowcase(tb2)+' '
	       if i eq 0 then tb1=tb2 else tb1=[tb1,tb2]
	    endif
	ENDFOR

	ins=strlowcase(INST)
	res=sys_dep      ('POT' ,ins) & if ins(0) eq strlowcase(INST(0)) then ins=ins+' '
	tb0=['']
	FOR i=0,NNT-1 DO IF (SUB(i) eq 0) and (DOIT(i) gt 0) THEN BEGIN
					idx=where(strpos(tb1,ins(i)) ge 0)
					IF idx(0) ge 0 THEN tb0=[tb0,tb1(idx(0))]
					ENDIF
	FOR i=0,NNT-1 DO IF (SUB(i) gt 0) and (DOIT(i) gt 0) THEN BEGIN
        				bas=sys_dep      ('INSUB',C_BASE(i),strlowcase(INST(i)))
					tb1=sys_dep      ('DIRD' ,bas)  & tb1=strlowcase(tb1)+' '
					tb0=[tb0,tb1]
					ENDIF
	tb1=['']
	FOR i=0,n_elements(tb0)-1 DO BEGIN
					idx=where(strpos(T_ALL,tb0(i)) ge 0)
					IF idx(0) lt 0 THEN tb1=[tb1,tb0(i)]
					ENDFOR
	FOR i=0,NNT-1 DO IF DOIT(i) eq 1 THEN BEGIN
					idx=where(strpos(tb1,strlowcase(INST(i))) ge 0)
					IF idx(0) lt 0 THEN DOIT(i)=0
					ENDIF
ENDIF					
IF total(DOIT) gt 0 THEN BEGIN

 stat=0 & iii=1
;catch,stat
 IF (stat eq 0) and (iii eq 1) then begin

    XBASE=sys_dep      ('NEWSUB',T_BASE,YEAR)

    out=-1 & on_ioerror,mis_year
    openw,out,XBASE+fdat,/get_lun
    on_ioerror,mis_io
    printf,out,!stime
    mis_year:if out gt 0 then free_lun,out $
    	     else begin	 bid=sys_dep      ('MKDIR',XBASE)
    		  	 on_ioerror,mis_io & openw,out,XBASE+fdat,/get_lun
	          	 printf,out,!stime & free_lun,out & endelse
    
    text ='-> '
    for i=0,NNT-1 do if DOIT(i) gt 0 then text=text+INST(i)+' '
    if LAB1 gt 0 then widget_control,bad_id=ii,LAB1,set_value=text else print,text
    
    on_ioerror,mis_io
    
;** Loop for Instruments
;** ---- --- -----------
    FOR si=0,NNT-1 DO BEGIN i=si
    
     IF DOIT(i) gt 0 THEN BEGIN

      text='Updating '+INST(i)+' '
      ins		=strlowcase(INST(I))	   
      ptin		=strpos (strlowcase(C_BASE(i)),ins)
      CYCLE		='Cycle'
;     **************************
      INST_VALUE        =INST(i)
      INST_GROUP	=GROUP(i)
      PATH_FOR_ONLINE   =C_BASE(i)
      if ptin lt 0  then if CYCLE ne 'Cycle' then ptin=0
      if ptin lt 0  then $
      PATH_FOR_ONLINE   =sys_dep      ('INSUB',PATH_FOR_ONLINE,ins ) else CYCLE=''
      O_BASE		=sys_dep      ('INSUB',XBASE    ,ins +'_'+CYCLS)
      D_BASE		=sys_dep      ('INSUB',XBASE    ,ins +'_'+CYCLS+'d')

      NO=0
      FOR kk=0,SUB(i) DO BEGIN
        
        if ptin lt 0 then I_BASE=sys_dep ('INSUB',C_BASE(i),ins ) else I_BASE=C_BASE(i)
        idx=[1] & EXTD=ins & txts='_'+strtrim(string(kk),2)
	if SUB(i) gt 0  then  begin  EXTD  =EXTD + txts
        			     I_BASE=sys_dep      ('INSUB',I_BASE,EXTD)
        			     IF DOIT(I) eq 2 THEN idx=where(strpos(tb0,EXTD) ge 0) $
        			     		     ELSE idx=where(strpos(tb1,EXTD) ge 0)
        		      endif
	if (idx(0) ge 0) and  $
	   ((LASTR(i) lt L_RUN(i)) or (LASTR(i) le 0)) then begin
	   stat=0  &  catch,stat
	   if stat eq 0 then begin
	     if LAB2 gt 0 then widget_control,bad_id=ii,LAB2,set_value=text+txts+' FindFiles'
	     cd,current=mee
	     cd,I_BASE
	     OCAT=1
	     if (LASTR(i) eq 0) or ((SUB(i) gt 0) and (abs(LASTT(i)) ne kk)) then begin
				 I_FILE=findfile(count=NI)
;	   			 bid=sys_dep('DIR',I_FILE ,NI)
	   			 LASTT(i)=kk
	   			 if NI gt 0 then begin cd,current=you
	   			 			ln=strpos(strupcase(I_FILE(0)),strupcase(you))
	   			 			if ln ge 0 then ln=ln+strlen(you)
	   			 			I_FILE=strmid(I_FILE,ln>0,lamp_6)
	   			 			endif
	     endif else begin
	   			 frs = LASTR(i) & I_FILE=[flto6(frs)] & cnt=1
	   			 while  cnt gt 0  do begin
	   			 	frs= frs+1 & fll= flto6(frs)     &   bid=FINDFILE(fll,count=cnt)
	   			 	if (cnt eq 0) and (SUB(i) gt 0) then bid=FINDFILE(fll+'.Z',count=cnt)
	   			 	if  cnt gt 0 then begin I_FILE=[I_FILE,fll] & OCAT=0 & endif
	   			 endwhile
	   			 NI=n_elements(I_FILE)-1
	   			 if NI gt 0 then begin I_FILE=I_FILE(1:*)  & LASTT(i)=kk & endif $
	   			 else   if LASTT(i) eq kk then   LASTT(i)=(-kk) <(-1) $
	   			 	else begin LASTR(i)=0  & LASTT(i)=  kk & endelse
	     endelse
	   endif else begin NI=0 & print,INST_VALUE+' '+!err_string & endelse
	   catch,/cancel
	   cd,mee
      
	   if NI gt 0  then if NO eq 0 then begin

      	   	if LAB2 gt 0 then widget_control,bad_id=ii,LAB2,set_value=text+txts+' ('$
				 +strtrim(string(NI),2)+ 'f)->Get Catalog' else print,text+string(NI)
		O_FILE=[''] & LINE=[''] & NO=1 & nxd=500
		if OCAT eq 1  then begin
      		  on_ioerror,end_f
      		  in=-1 & openr,in,O_BASE+INST_VALUE+fcat,/get_lun
		  while(1) do begin
		    LINE=strarr(nxd) & readf,in,LINE
		    for n=0,nxd-1 do LINE(n)=strmid(LINE(n),strpos(LINE(n),'#RUN#')+6,lamp_6)
		    O_FILE =[O_FILE,LINE]
		  endwhile
      		  end_f: if in gt 0 then begin
      			    free_lun ,in & idx=where(LINE ne '')
      			    if idx(0) ge 0 then begin
			     LINE=LINE(idx)
			     for n=0,n_elements(LINE)-1 do $
			     	     LINE(n)=strmid(LINE(n),strpos(LINE(n),'#RUN#')+6,lamp_6)
		    	     O_FILE =[O_FILE,LINE]
			    endif
		    	    NO=n_elements(O_FILE)
			    O_FILE=O_FILE(sort(O_FILE))
      			 endif else bid=sys_dep      ('MKDIR',O_BASE)
      		  on_ioerror,mis_io
		endif
		W_FILE=O_FILE
           endif
	endif  else NI=0
        
;** 	Loop for Runs
;** 	---- --- ----
	idx=[1]
	if NI gt 0 then begin
	   if (NI ne NO) or (OCAT eq 0) then idx=[-1]
	   on_ioerror,mislong  & lastn=L_RUN(i)    & frstn=F_RUN(i)
	   frsti=I_FILE(0)     & frstn=long(frsti)
	   lasti=I_FILE(NI-1)  & lastn=long(lasti)
	   if  (F_RUN(i) gt 0) or (L_RUN(i) lt 999999) then $
	   	if (frstn gt L_RUN(i)) or (lastn lt F_RUN(i)) then begin idx=[1] & LASTR(i)=lastn & endif
	   mislong:on_ioerror,mis_io
	endif

	if idx(0) lt 0  then begin

    	if LAB2 gt 0 then widget_control,bad_id=ii,LAB2,set_value=text+txts+' Looping'
	limit=2000 & j=0
        WHILE j lt NI do begin
        
	  status=0
	  on_ioerror,misloon
	  RUN=I_FILE(j) & RNN=long(RUN)
	  on_ioerror,mis_io
	  idx=[1]
	  if (RNN ge F_RUN (i)) and (RNN le L_RUN (i))    then $
	  if (RUN lt O_FILE(0))  or (RUN gt O_FILE(NO-1)) then idx=[-1] $
							  else begin idx=where(W_FILE eq RUN)
								     if idx(0) ge 300 then $
									W_FILE=W_FILE(idx(0)-1:*) & endelse
	  if idx(0) lt 0 then begin

	     res=TOUCH_ATTENT (LAB1, DESK ,whatdo ,1)
	     if res eq 1 then return

	     limit=limit-1 & if limit le 0 then begin
				idx=where(strpos(tb0,EXTD) ge 0)
				if idx(0) ge 0 then tb0(idx)=''
				j=NI
				endif

;**	     Read input
;**	     ---- -----
	     iii=execute(WS+'=0' )
	     		      
	     text=INST(i)+' ---> '+RUN +' (->'+lasti+' )'
    	     if LAB2 gt 0 then widget_control,bad_id=ii,LAB2,set_value=text else print,text

	     catch,stat & if stat eq 0 then p_did_getrun,RNN,WI,status else catch,/cancel
;					    **************************
	     LASTR(i)=RNN
	     
	     if status eq 0 then begin
	     	auto=-1
	     
;**	     	Write into touch_base if necessary
;**	     	----- ---- ---------- -- ---------
		mini=0 & maxi=0 & szw=0
		iii=execute( 'maxi=max('+WS+',min=mini)' )
		iii=execute( 'szw=size('+WS+')' )
		
		if mini ne maxi then begin
		   w_min(WI)=mini & w_max(WI)=maxi
		   limtxt   =[WS+': min='+strtrim(string(mini),2) $
			        + ' max='+strtrim(string(maxi),2) ]
		   histxt   =[WS+': '+INST_VALUE+'('+RUN+')']
		
		   if  (szw(szw(0)+1)*4 le 192.^2) and (KPTN(i) eq 0) then begin
			XS='S' & PP='.' & auto=-2
		   endif else begin
			XS='X' & PP=' ' & p_did_save_auto, WI, O_BASE,RUN ,auto
		   endelse
	       
		   if auto le -1 then begin
	     	
;**		      Write data file
;**		      ----- ---- ----
		      DR='##'
		      if (auto ne -2) and (KPTN(i) gt 0) then begin
		          
			  lrun=RUN & bid=sys_dep      ('POT',lrun)
			  
		          out=-1 & on_ioerror,mis_dirdat
		          openw,out,D_BASE+lrun  ,/get_lun,/XDR
		          
		          mis_dirdat:on_ioerror,mis_io
		          if out le 0 then begin
		             bid=sys_dep      ('MKDIR',D_BASE)
		             openw,out,D_BASE+lrun  ,/get_lun,/XDR
		          endif
		          
		          ii=execute('writeu,out,' + WS)
		          free_lun,out
			  bid=sys_dep      ('DO_Z',D_BASE+lrun,lamp_dir)
		          DR='#'+lrun+'#'
		      endif
		      
;**	     	      Update catalogue
;**	     	      ------ ---------
		      if auto eq -2 then RR='R' else RR=' '
		      DATE=strmid(head_tit(WI,4),0,9)
		      if DATE      eq '' then DATE     ='!!'+strmid(!stime,2,5)+'!!'
		      if w_tit(WI) eq '' then w_tit(WI)='                 '
		      LINE=DATE+PP +w_tit(WI)+' #RUN# '+RUN $
		   			     +' #MIN# '+strtrim(string(w_min(WI)),2)$
		   			     +' #MAX# '+strtrim(string(w_max(WI)),2)$
		   			     +' #FMT# '+XS+RR+'     '+DR
		      openw   ,out,O_BASE+INST_VALUE+fcat,/get_lun,/append
		      printf  ,out,LINE
		      free_lun,out
		   
		   endif else status=-2
		endif else status=-1
	     endif
;**	     Notes problems
;**	     ----- --------
	     if status ne 0 then begin
	     	      openw   ,out,O_BASE+RUN+ferr,/get_lun
		      if status gt  0 then printf  ,out,  ' Problem reading data  file ...'
		      if status lt  0 then printf  ,out,  ' Identical min & max counts ...'
		      if status eq -2 then printf  ,out,  ' Problem writing snapshot   ...'
		      free_lun,out
		      openw   ,out,O_BASE+INST_VALUE+fcat,/get_lun,/append
		      printf  ,out,'Bad runs ......... #RUN# '+RUN
		      print,INST_VALUE+' Bad run '+RUN
		      free_lun,out
	     endif
	  endif
	  j=j+1 
        ENDWHILE
        endif
        misloon:
      ENDFOR
     ENDIF
    ENDFOR
    
    text='End pass of Touch Update ... '+!stime
    if LAB1 gt 0 then widget_control,bad_id=ii,LAB1,set_value=text else print,text

 endif else begin
    catch,/cancel
    if LAB1 gt 0 then widget_control,bad_id=ii,LAB1,set_value=!err_string
    print,INST_VALUE+' '+!err_string
 endelse
    
ENDIF ELSE BEGIN sec=120.
      		 text='Sleeping for '+strtrim(string(sec),2)+' secondes ...'+!stime
      		 if LAB1 gt 0 then widget_control,bad_id=ii,LAB1,set_value=text else print,text
		 if LAB1 gt 0 then begin
		 	 step =sec/30.
		 	 for i=step,sec,step do begin
				   res=TOUCH_ATTENT (LAB1, DESK ,whatdo ,1) & if res eq 1 then return
		 		   widget_control,bad_id=ii,LABT,timer=step
				   res=TOUCH_ATTENT (LABT, DESK ,whatdo ,0) & if res eq 1 then return
			 endfor
      		 endif else WAIT,sec
      		 T_FLAG=0
		 ENDELSE
T_ALL=tb0

ENDWHILE
;****************
;END of TIME LOOP
;****************

print,string(7b)
return

mis_io: print,string(7b)
    if LAB1 gt 0 then widget_control,bad_id=ii,LAB1,set_value=!err_string else print,!err_string
return

end

