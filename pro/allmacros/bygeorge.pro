;------------------------------------------------------------------------------
; This code is used to run a Dial speaking to the instrument without GEORGE guy
;------------------------------------------------------------------------------

; FUNCTION DialNewValue   GET  A VALUE FROM THE INSTRUMENT
; FUNCTION DialControl 	  SEND A COMMAND TO THE INSTRUMENT
; FUNCTION DialOn 	  NORMALY USED FOR INTERRUPT IN GEORGE
; PRO      DialTag 	  SET OR GET A DIAL PROPERTY
; PRO      DialStart	  TIE A DIAL TO THE CLOCK
; PRO      DialStop	  DETACH A DIAL FROM THE CLOCK
; PRO      DialModValue	  ADD or CHANGE DIMENSION OF A DIAL PROPERTY
; PRO      DialInit	  READ   A DIAL FROM FILE AND PUT IN MEMORY
; PRO      DialMacro	  EXECUTE THE MACRO OF A NON-ACTIVE DIAL
; PRO      DialClear	  REMOVE A DIAL FROM MEMORY
; PRO      DialWset       NORMALY USED TO SELECT MAIN-PLOT-WINDOW IN GEORGE
; PRO      DialsFrequency NORMALY USED TO GIVE THE SAME FREQUENCY TO A GROUP OF DIALS

; MATLAB tips: DD=CALL_FUNCTION('dial_'+DD.origin+'_macro',DD)
;             ii =EXECUTE('DD='+dial)
;             mat= 1 in by_timer
;             value='                                               ',/dynamic_resize
;--------------------------------------------------------------------------------------
FUNCTION DialNewValue, tope,NAME=name,d=dnum,SETVALUE=setvalue, COMMENT=ctxt, TYPE=tipe
;******* ************
;**	 GET A VALUE FROM THE INSTRUMENT
	 common noGeorge,DD, Widg
	 
CATCH,stat
if stat ne 0 then begin catch,/cancel & print,!err_string & return,'error' & endif
if n_elements(tope) ne 1 then if n_elements(tipe) ne 1 then tipe=DD.type
if n_elements(tope) eq 1 then if n_elements(tipe) ne 1 then tipe=tope
IF DD.generic eq 'lamp'  then begin
   CASE strlowcase(tipe) of
   'flagus':	V=0
   'status':	V='Idle'
   'log':	V=''
   ELSE:	V=0
   ENDCASE
ENDIF else $
   V= CALL_FUNCTION('dial_'+DD.generic+'_read',tipe)    ;...GET A VALUE FROM INSTRUMENT
if keyword_set(setvalue) then DialModValue, V, tag='VALUE'
return,V
END

;--------------------------------------------------------------------------------------
FUNCTION DialControl, command, d=dnum ,check=check ,name=nome
;******* ***********
;**	 SEND A COMMAND TO THE INSTRUMENT
	 common noGeorge,DD, Widg

CATCH,stat
if stat ne 0 then begin catch,/cancel   & print,!err_string  & return,'error' & endif
if n_elements(check) ne 1 then check=0. & V=0
IF DD.generic ne 'lamp' then $
  V= CALL_FUNCTION('dial_'+DD.generic+'_send','',check,command)$;SEND COMMAND TO INSTRU
ELSE ii=EXECUTE(command(0))
return,V
END

;--------------------------------------------------------------------------------------
FUNCTION DialOn, dial, d=dnum
;******* ******
;**	 NORMALY USED FOR INTERRUPT IN GEORGE
return,1
END

;--------------------------------------------------------------------------------------
PRO DialTag, name, d=dnum, TAG=tag, SET=setv, GET=getv
;** *******
;** SET OR GET A DIAL PROPERTY
    common noGeorge,DD, Widg

BY_SWITCH,/STORE, NEWNUM=dnum,NEWNAME=name
getv=0
IF n_elements(tag)  eq 0 then getv=DD else ii=execute('getv=DD.'+tag)           ;...GET
IF n_elements(setv) gt 0 then DialModValue, setv, TAG=tag                       ;...SET
BY_SWITCH,/RESTORE
END

;--------------------------------------------------------------------------------------
PRO DialStart, name, d=dnum
;** *********
;** TIE A DIAL TO THE CLOCK
    common noGeorge,DD, Widg

BY_SWITCH,/STORE, NEWNUM=dnum,NEWNAME=name
DialModValue, systime(1), TAG='BYSTART'
DD.onoff=1 & widget_control,bad_id=ii,Widg(DD.number+3),TIMER=.1     ;...SET TIME EVENT
BY_SWITCH,/RESTORE
END

;--------------------------------------------------------------------------------------
PRO DialStop, name, d=dnum
;** ********
;** DETACH A DIAL FROM THE CLOCK
    common noGeorge,DD, Widg
    
DialTag, name, d=dnum, TAG='ONOFF', SET=0
END

;--------------------------------------------------------------------------------------
PRO DialModValue, val, NAME=name, d=dnum ,tag=TAG
;** ************
;** ADD or CHANGE DIMENSION OF A DIAL PROPERTY
    common noGeorge,DD, Widg
    
      sv =SIZE(val)
      if n_elements(TAG) ne 1 then TAG='VALUE' else TAG=strupcase(TAG)
      tlist=strupcase(tag_names(DD))
      idx=WHERE(tlist eq TAG) & sz =[0,0,0]
      if idx(0) ge 0 then ii=execute('sz=SIZE(DD.'+TAG+')')
      if (sz(sz(0)+1) ne sv(sv(0)+1)) or (sz(sz(0)+1) eq 8) or $
         (sz(0) ne sv(0)) or (sz(sz(0)+2) ne sv(sv(0)+2)) then begin
	 elsa = ''
	 for k=0,n_elements(tlist)-1 do begin
          CASE tlist(k) of
           TAG:
           ELSE:      elsa =elsa+','+tlist(k)+':DD.'+tlist(k)
          ENDCASE
	 endfor
                 ii=execute('DD={'+TAG+':val'+ elsa +'}') ;...RECONSTRUCT THE STRUCTURE
      endif else ii=execute('DD.' +TAG+'=val')            ;...OR SIMPLY MODIFY THE  TAG
END

;--------------------------------------------------------------------------------------
PRO DialInit, name, d=dnum, path=pth, restore=rest, new=newed, herits=diaH, nostart=nos
;** ********
;** READ A DIAL FROM FILE AND PUT IN MEMORY
    common noGeorge,DD, Widg

IF n_tags(DD) ge 3 then begin if n_elements(name) ne 1 then name=DD.name
			kp=DD.number & endif else kp=0
DIAL_BYGEORGE, name, nostart=nos, new=newed                            ;...GET THE DIAL
BY_SWITCH,LOAD=kp
END

;--------------------------------------------------------------------------------------
PRO DialMacro, name, d=dnum, Si=di
;** *********
;** EXECUTE THE MACRO OF A NON-ACTIVE DIAL
    common noGeorge,DD, Widg

BY_SWITCH,/STORE, NEWNUM=dnum,NEWNAME=name
call_procedure,'dial_' +DD.origin+'_macro',DD                     ;...EXECUTE THE MACRO
BY_SWITCH,/RESTORE
END

;--------------------------------------------------------------------------------------
PRO DialClear, name, d=dnum
;** *********
;** REMOVE A DIAL FROM MEMORY
    common noGeorge,DD, Widg

BY_SWITCH,/CLEAR, NEWNUM=dnum,NEWNAME=name
END

;--------------------------------------------------------------------------------------
PRO DialWset, Activity=activity
;** ********
;** NORMALY USED TO SELECT MAIN PLOT WINDOW IN GEORGE
    Wset,0L
END

;--------------------------------------------------------------------------------------
PRO DialsFrequency, GET=getv, SET=setv, STOP=stop, START=stort, DURATION=lim, SENS=sens
;** **************
;** NORMALY USED TO GIVE THE SAME FREQUENCY TO A GROUP OF DIALS IN GEORGE

getv=0.
END
;-------------------------------- End of User library ---------------------------------
;-------------------------------- End of User library ---------------------------------

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

;--------------------------------  Internal routines  ---------------------------------
;--------------------------------  Internal routines  ---------------------------------

; PRO      by_switch	  SWITCH ROUND THE DIALS WHEN INVOLVED
; PRO      by_timer	  THE CLOCK HANDLER
; PRO      by_stop	  STOP ALL DIALS
; PRO      dial_byGeorge  CALLED BY "DialInit" TO READ A DIAL FILE


PRO by_switch,LOAD=lod ,SAVE=sav,STORE=sto ,RESTORE=res,NEWNUM=dnu,NEWNAME=nam,$
              CLEAR=clr,STOP=stp,GETNUM=gnu
;** *********
;** SWITCH ROUND THE DIALS WHEN INVOLVED
    common noGeorge,DD, Widg
    common bygeorge,d0 ,d1 ,d2 ,d3 ,d4 ,d5 ,d6 ,d7 ,d8 ,d9 ,d10,$
                    d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,names,pile

IF n_elements(names) eq 0 then begin d0={ONOFF:0,NUMBER:0,NAME:''}   & d1 =d0 & d11=d1
	d2 =d1 & d3 =d1 & d4 =d1 & d5 =d1 & d6 =d1 & d7 =d1 & d8 =d1 & d9 =d1 & d10=d1
	d12=d1 & d13=d1 & d14=d1 & d15=d1 & d16=d1 & d17=d1 & d18=d1 & d19=d1 & d20=d1
	names=strarr(21) & pile='' & ENDIF

IF n_elements(nam)  eq 1 then begin
		    idx=where(names eq strlowcase(nam))
		    if idx(0) gt 0 then lod=idx(0) else IF keyword_set(sto) then lod=0
ENDIF
IF n_elements(dnu)  eq 1           then lod=dnu

if n_tags    (DD)   lt 3 then DD=d0 & sd=strtrim(string(DD.number),2)

same=0 & if n_elements(lod) ne 1 then same=1 else if lod eq DD.number then same=1

;---SWITCHES---
IF keyword_set(clr) then begin if same then idx=DD.number else idx=lod
		    r=EXECUTE('d'+strtrim(string(idx),2)+'.ONOFF=0')
		    if same then DD.onoff=0 & names(idx)='' & same=1    & ENDIF
 
IF keyword_set(stp) then FOR k=1,20 do r=EXECUTE('d'+strtrim(string(k),2)+'.ONOFF=0')
IF keyword_set(stp) then DD.onoff=0

IF keyword_set(gnu) then begin if n_elements(lod) eq 1 then gnu=lod $
		               else begin idx=where(names(1:*) eq '')+1
			                  if idx(0) eq 0 then gnu=20 else gnu=idx(0)
			       endelse  & same=1 & ENDIF

IF keyword_set(sto) then pile=[pile,sd]

IF keyword_set(sav) then begin names(DD.number)= DD.name
			       r=EXECUTE('d'+sd+'=DD') & ENDIF

IF     not same     then begin names(DD.number)= DD.name
			       r=EXECUTE('d'+sd+'=temporary(DD)')
			       r=EXECUTE('DD=d'+strtrim(string(lod),2)) & ENDIF

IF keyword_set(res) then BEGIN nn=n_elements(pile)-1
		    IF nn gt 0 then BEGIN sg=pile(nn) & pile=pile(0:nn-1)
		       IF sg ne sd then BEGIN
		               names(DD.number)= DD.name
		               r=EXECUTE('d'+sd+'=temporary(DD)')
			       r=EXECUTE('DD=d'+sg)    & ENDIF & ENDIF & ENDIF
END



PRO by_timer, event
;** ********
;** THE CLOCK HANDLER
    common noGeorge,DD, Widg
BYs=0
IF Widg(0) gt 0 then begin widget_control,event.id,get_uvalue=uv
		MAT=0 ;MAT=0 for Idl, mat=1 for Matlab
		if (DD.number ne uv(0)) then $
		 if MAT then begin BY_SWITCH,/STORE,NEWNUM=uv(0) & BYs=1
		 endif  else BY_SWITCH,LOAD=uv(0)
ENDIF
WHILE ((DD.frequency gt 0) and (DD.onoff)) do begin T1=systime(1)

	CALL_PROCEDURE,'dial_'+DD.origin+'_macro',DD

	if n_elements(DD.value) eq 1 then if (DD.value(0) ne '') then $
			 val=' Val=' + string(DD.value) else val='' else val=''

	if (DD.history   eq 1) then if (DD.value(0) ne '')   then begin
		openw,u,'dial_'+DD.name+'.his',/get_lun,/APPEND
		printf,u,DD.value & free_lun,u & endif
	  
	T2=systime(1)
	if  DD.duration  gt 0. then if (T2-DD.bystart) gt DD.duration then DD.onoff=0
       
	T1=(DD.frequency-float((T2-T1)))>0.01
	if (DD.frequency gt 0.) and (DD.onoff) then $
		if Widg(0) gt 0 then begin
		   widget_control,Widg(1),bad_id=ii,set_value=strmid(!stime,12,11)$
		                             +' Freq='+strtrim(string(DD.frequency),2)+val
		   widget_control,Widg(2), set_value=strupcase(DD.name)
		   widget_control,Widg(uv(0)+3),timer=T1
		   if BYs then    BY_SWITCH,/RESTORE
		   BYs=0 & return
		endif else wait,T1
ENDWHILE
if BYs then BY_SWITCH,/RESTORE
END

PRO by_stop, event
;** *******
;** STOP ALL DIALS
    common noGeorge,DD, Widg

BY_SWITCH,/STOP
if n_elements(event) gt 0 then widget_control,event.top,/destroy
END

PRO map	   & common noGeorge,DD, Widg & widget_control,Widg(0),bad_id=ii,map=1 & END
PRO nomap  & common noGeorge,DD, Widg & widget_control,Widg(0),bad_id=ii,map=0 & END
;** *****

PRO dial_byGeorge, name, nostart=nostart, new=newed
;** *************
;** CALLED BY "DialInit" TO READ A DIAL FILE
    common noGeorge,DD, Widg
    
!quiet=1 & dnum=1 & if n_elements(name) eq 0 then name='bygeorge'
IF n_elements(newed) eq 0 then newed=name

BY_SWITCH,/SAVE
BY_SWITCH, GETNUM=dnum, NEWNAME=newed
dial ='dial_'+name
DD   ={INIT:0,ONOFF:0}

IF name ne 'bygeorge' then ii =EXECUTE('DD='+dial+'()') else II=1
                                IF (not II) then RETURN

tags =['NAME','VALUE','UNIT','GENERIC','TYPE','ORIGIN','PATH']
defs =[ newed,   ''  ,  ''  ,  'mad'  ,'data', name   ,  ''  ]
tagi =['NUMBER','ONOFF','PLOT','WUPDATE','HISTORY','INIT']
defi =[ dnum   ,   1   ,   1  ,    0    ,    0    ,  0   ]
tagr =['ERROR','UPPERLIM','LOWERLIM','FREQUENCY','DURATION']
defr =[   0.  ,    0.    ,    0.    ,     1.    ,    0.    ]

tlist= strupcase(tag_names(DD))
FOR i= 0,n_elements(tags)-1 do begin  idx=WHERE(tlist eq tags(i))
    if idx(0) eq -1 then DialModValue,defs(i),tag=tags(i) & ENDFOR
FOR i= 0,n_elements(tagi)-1 do begin  idx=WHERE(tlist eq tagi(i))
    if idx(0) eq -1 then DialModValue,defi(i),tag=tagi(i) & ENDFOR
FOR i= 0,n_elements(tagr)-1 do begin  idx=WHERE(tlist eq tagr(i))
    if idx(0) eq -1 then DialModValue,defr(i),tag=tagr(i) & ENDFOR

DD.name=newed

if (!D.flags and 65536) ne 0 then begin
  if xregistered('ByGeorge') le 0 then begin Widg=lonarr(24)
  	Widg(0)  =widget_base  (Title="By George !",/column,event_pro='by_timer',map=0)
	bod      =widget_base  (Widg(0),/row)
	Widg(2)  =widget_label (bod    ,value='----- '+strupcase(newed)+' -----' )
	bid      =widget_button(bod    ,value='stop'       ,event_pro='by_stop' )
	for i=0,20 do Widg(3+i)=widget_label (bod    ,value='', uvalue=i)

	bod      =widget_base  (Widg(0),/row)
	Widg(1)  =widget_label (bod    ,value=' ',/dynamic_resize)

	widget_control,Widg(0),/realize
	XMANAGER,'ByGeorge',Widg(0),event_handler='by_timer',/no_block
  endif
  IF keyword_set(nostart) then DD.onoff=0 else if DD.onoff then DialStart
  BY_SWITCH,/SAVE

endif else begin Widg=0 & BY_TIMER & BY_STOP & endelse
END

PRO  byGeorge, name, nostart=nostart, new=newed
dial_byGeorge, name, nostart=nostart, new=newed
END
