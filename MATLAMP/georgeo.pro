pro DialCommons
;** ***********
;**
@lamp.cbk
common c_geo, geo_stat, geo_w  , geo_vis, geo_alp, geo_freq, geo_isw, geo_onbas,$
              geo_bxy , geo_cur, geo_ran, geo_seq, geo_info, geo_lim, geo_timon,$
              geo_par2, geo_lead,geo_act

common c_geweb, geo_web,gew_act, gew_pth, gew_snd, gew_err, gew_pwd, gew_r,gew_g,gew_b,gew_v
end

function N2S, number
;******* ***
;**
return, strtrim(string(number),2)
end

function DialNameToNumber, name, find=find
;******* ****************
;**
;** Given the name of a dial, return its number
;** If keyword_set find then  return a free number if no name match

common dialshare2
common c_geo

named=strupcase(name) & nome=''
N=0   & Pn=(size(geo_w))(2)-1
k=1   & while k le Pn do begin di=strtrim(string(k),2)
		    ii=execute ('if n_tags(d'+di+') gt 1 then nome=d'+di+'.NAME')
		    if strupcase(nome) eq named then begin N=k & k=Pn & endif & k=k+1
        endwhile

if N eq 0 then if keyword_set(find) then begin
			k=1 & while k le Pn do begin di=strtrim(string(k),2)
			      ii=execute ('if n_tags(d'+di+') le 1 then N=k')
			      ii=execute ('if n_tags(d'+di+') gt 1 then if d'+di+'.NAME eq "" then N=k')
			      if N gt 0 then   k=Pn & k=k+1
				endwhile
			if N eq 0 then $
			k=1 & while k le Pn do begin di=strtrim(string(k),2) & onoff=1
			      ii=execute ('if (n_tags(d'+di+') gt 1) then onoff=d'+di+'.ONOFF')
			      if onoff ne 1 then begin N=k & k=Pn & endif & k=k+1
				endwhile
			endif
return,N
end

;******* **************************************************************************
;******* **************************************************************************
;******* **************************************************************************
pro DialTag, name, d=dnum, TAG=tag, SET=setv, GET=getv
;** *******
;**
;** Set or Get the tag value of named dial
common c_geo
common dialshare2
	getv=0
	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if dnum gt 0 then if n_elements(tag) eq 1 then begin
	   if n_elements(setv) gt 0 then begin
	      if strupcase(tag) eq "ONOFF"   then if setv gt 0 then DialStart, d=dnum $
	                                                       else DialStop , d=dnum else $
	      if strupcase(tag) eq "HISTORY" then DialHistory, d=dnum ,ONOFF=setv     else $
	      if strupcase(tag) eq "VALUE"   then DialModValue,d=dnum ,      setv     else $
	      if strupcase(tag) eq "ERROR"   then DialModValue,d=dnum ,      setv, tag='ERROR' $
		  else DialModValue,d=dnum ,      setv, tag=strupcase(tag)
	     ;else    ii=execute('d'+strtrim(string(dnum),2)+'.'+tag+'=setv')

	   endif else ii=execute('getv=d'+strtrim(string(dnum),2)+'.'+tag)
	endif else if n_elements(setv) eq 0 then ii=execute('getv=d'+strtrim(string(dnum),2))  $
	                                    else ii=execute('d'+strtrim(string(dnum),2)+'=setv')
end

;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
pro DialsFrequency, GET=getv, SET=setv, STOP=stop, START=start, DURATION=lim, SENS=sens
;** **************
;**
;** Settings of the Timer

common c_geo
	if n_elements(setv) eq 1 then begin
	   on_ioerror,miset & geo_freq=setv & v=strtrim(string(setv),2)
	   if geo_onbas(0) gt 0 then widget_control,geo_onbas(0),bad_id=ii,set_value=v
	   miset:
	endif
	if n_elements(lim)  eq 1 then begin
	   on_ioerror,mislm & geo_lim =lim  & v=strtrim(string(lim),2) & if lim le 0 then v=" "
	   if geo_onbas(2) gt 0 then widget_control,geo_onbas(2),bad_id=ii,set_value=v
	   mislm:
	endif
	if keyword_set(stop)  then begin
	   if geo_onbas(1) gt 0 then widget_control,geo_onbas(1),bad_id=ii,set_button=0
	   geo_stat=0 & geo_w(8,*)=0 & endif
	if keyword_set(start) then begin
	   if geo_onbas(1) gt 0 then widget_control,geo_onbas(1),bad_id=ii,set_button=1
	   geo_stat=1 & geo_timon(0)=systime(1)*1000 & P_GEO_TIMER, {id:geo_onbas(3)}
	   geo_w(8,*)=0 & endif

	if keyword_set(sens)  then if geo_stat  then   P_GEO_TIMER, {id:geo_onbas(3)}

	getv=geo_freq
end

;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
function DialNewValue, tope, NAME=name, d=dnum, SETVALUE=setvalue, COMMENT=ctxt, TYPE=tipe
;******* ************
;**
;** Get next value of the named dial by calling dial_"generic"_read interface function
;** (called from dial_macros)

common dialshare2
common c_lamp_par
common c_geo
val='0'
	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if n_elements(ctxt) eq 0 then ctxt=""
	if n_elements(tope) eq 1 then if n_elements(tipe) ne 1 then tipe=tope

	 di=strtrim(string(dnum),2)

	   P_GEO_STATUS, dnum, "Get"

	   generic='' & nome='' & prox=-1
	   if n_elements(tipe) eq 1 then nome=tipe else $
	   ii=execute('nome   =d' +di+'.TYPE')
	   ii=execute('generic=d' +di+'.GENERIC')
	   ii=execute('prox   =d' +di+'.PROX')
	   now=systime(1)
	   val=3.14

	   if generic eq 'lamp' then begin val=' '
	      CASE strlowcase(nome) of
	      'flagus':	val=0
	      'status':	val='Idle'
	      'log':	DID_WRITE_JOURNAL, val
	      ELSE:
	      ENDCASE
	   endif else $
	   if prox(0) ge 0 then begin par1=nome & par2=now     & par3=ctxt & par4=val
	                              COMMCA, proxcod ,prox    & val =par4 ; GET THE VALUE !!!!!!!!!!!!
	   endif else begin
	    ii=execute('val=dial_'+generic+'_read(nome, now, ctxt)')       ; GET THE VALUE !!!!!!!!!!!!
	    if not ii then begin val='0' & DialErrMes & DialStop & endif
	   endelse

	   geo_w(9,dnum)=geo_w(9,dnum)+((systime(1)-now)*1000)
	   sv=SIZE(val)
	   if sv(sv(0)+1) eq 4 then if val(0) eq 3.14 then P_GEO_STATUS, dnum, "GetNone" $
	                                              else P_GEO_STATUS, dnum, "GetOk"   $
	                                              else P_GEO_STATUS, dnum, "GetOk"

	   if keyword_set(setvalue) then DialModValue, val, d=dnum
return, val
end

;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
pro DialModValue, val, NAME=name, d=dnum ,tag=TAG
;** ************
;**
;** Used when dial.VALUE changes its type or dimension

common dialshare2
common c_geo

    if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
    if n_elements(dnum) ne 1 then dnum=geo_cur
    if dnum gt 0 then begin
      di=strtrim(string(dnum),2)
      sv=SIZE   (val)
      if n_elements(TAG) ne 1 then TAG='VALUE' else TAG=strupcase(TAG)
      sz=[0,0,0] & ii=execute('sz=SIZE(d'+di+'.'+TAG+')')
      if (sz(sz(0)+1) ne sv(sv(0)+1)) or (sz(sz(0)+1) eq 8) or  $
         (sz(0) ne sv(0)) or (sz(sz(0)+2) ne sv(sv(0)+2)) then begin
	 elsa = ''
	 tlist=[''] & ii=execute('tlist=strupcase(tag_names(d'+di+'))')
	 for k=0,n_elements(tlist)-1 do begin
          CASE tlist(k) of
           TAG:
           ELSE:      elsa =elsa+','+tlist(k)+':d'+di+'.'+tlist(k)
          ENDCASE
	 endfor
                 ii=execute('d'+di+'={'+TAG+':val'+ elsa +'}')
      endif else ii=execute('d'+di+'.' +TAG+'=val')
    endif
end

pro DialMix, A,B
;** *******
;**
;** Mixe Dial B into Dial A

    nA=n_tags(A)  & nB=n_tags(B)
    if nB lt 1 then return
    if nA lt 1 then begin A=B  & return & endif
    elsa=''
    lA=strupcase(tag_names(A))
    for k=0,nA-2 do  elsa =elsa    + lA(k)   +':A.'+lA(k)+','
                     elsa =elsa    + lA(nA-1)+':A.'+lA(nA-1)
    lB=strupcase(tag_names(B))
    for k=0,nB-1 do  if (where(lA eq lB(k)))(0) eq -1 then $
                     elsa =elsa+','+ lB(k)   +':B.'+lB(k)

    ii=execute('A={'+ elsa +'}')
end

pro DialHistory, name, d=dnum, ONOFF=v
;** ***********
;**
;** A short for DialTag procedure

common dialshare2
common c_geo
	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if dnum gt 0 then begin  ii=execute('d'+strtrim(string(dnum),2)+'.HISTORY=v')
	                   if v then lab ='History is on' else lab ='History is off'
				 if geo_isw then widget_control,bad_id=ii,geo_w(7,dnum),set_value=lab
				 endif
end

;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
pro DialStop, name, d=dnum
;** ********
;**
;** A short for DialTag procedure

common c_lamp_info
common dialshare2
common c_geo
common c_geweb
	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if dnum gt 0 then begin  di=strtrim(string(dnum),2) & wupd=0
				 ii=execute('d'+di+'.ONOFF=0')
				 ii=execute('name=d'+di+'.NAME')
				 ii=execute('wupd=d'+di+'.WUPDATE')
				 if geo_isw then widget_control,bad_id=ii,geo_w(3,dnum),set_button=0
				 geo_w(8,dnum)=0
				 if l_message gt 0 then $
				 widget_control,bad_id=ii,l_message,set_value='Dial '+name+' stopped'
				 if wupd gt 0 then $
				      TO_DON_HISTORY,dnum,0,'W'+di+'=dial_'+name+'_macro result ;Dial stopped' $
				 else TO_DON_HISTORY,-1  ,0,'Dial '+name+' stopped'
				 if geo_web then WebDo,'val',dnum,dnum
				 endif
end
;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
pro DialStart, name, d=dnum
;** *********
;**
;** A short for DialTag procedure

common c_lamp_info
common dialshare2
common c_geo
common c_geweb
	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if dnum gt 0 then begin  di=strtrim(string(dnum),2) & wupd=0
				 ii=execute('d'+di+'.ONOFF=1')
				 ii=execute('name=d'+di+'.NAME')
				 ii=execute('wupd=d'+di+'.WUPDATE')
				 if geo_isw then widget_control,bad_id=ii,geo_w(3,dnum),set_button=1
				 if geo_isw then widget_control,bad_id=ii,geo_w(2,dnum),sensitive =1
				 freq=0 & ii=execute('freq=d'+di+'.FREQUENCY')
				 geo_w(8,dnum)=0
				 geo_timon(dnum)=systime(1)*1000
				 if freq  gt 0  then  P_GEO_TIMER, {id:geo_w(11,dnum)} $
	                                  else  DialsFrequency, /SENS
				 if l_message gt 0 then $
				 widget_control,bad_id=ii,l_message,set_value='Dial '+name+' started'
				 if wupd gt 0 then $
				      TO_DON_HISTORY,dnum,0,'W'+di+'=dial_'+name+'_macro result ;Dial started' $
				 else TO_DON_HISTORY,-1  ,0,'Dial '+name+' started'
				 if n_elements(gew_pth) eq 0 then WEBON
	endif
end

;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
function DialControl, command, d=dnum ,check=check ,name=nome
;******* ***********
;**
;** Back control to the instrument

common dialshare2
common c_lamp_par
common c_geo
common c_geweb

if n_elements(dnum)  ne 1 then dnum =geo_cur
if n_elements(nome)  eq 1 then dnum =DialNameToNumber(nome)
if n_elements(check) ne 1 then check=0.

di     =strtrim(string(dnum),2)
generic='' & ii=execute('generic=d' +di+'.GENERIC')
if generic eq 'lamp' then ctrl='' else ctrl='Ctrl:'

comm=[strtrim(command,2)]
comh= comm
for i=0,n_elements(comm)-1 do begin

	if strpos(strupcase(comm(i)),'CTRL:') eq 0 then comm(i)=strmid(comm(i),5,85)
	if i eq 0 then comh(i)= ctrl  +comm(i)+" ;; "+!stime $
	          else comh(i)='----:'+comm(i)
	to_don_history,-1,0,comh(0)
endfor
if geo_web then WebDo,'snd',comh,dnum

if generic ne "lamp" then begin
	   P_GEO_STATUS,dnum, "Send"

	   nome='' & prox=-1 & name=""
	   ii=execute('nome   =d' +di+'.TYPE')
	   ii=execute('name   =d' +di+'.NAME')
	   ii=execute('prox   =d' +di+'.PROS')
	   now=systime(1)
	   val=3.14

	   if prox(0) ge 0 then begin par1=nome & par2=check & par3=comm & par4=val & par5=name
	                              COMMCA, proxcod ,prox  & val =par4     ; SEND THE COMMAND !!!!!!!!!!!!

	   endif else begin CoCo='val=dial_'+generic+'_send(nome, check, comm, name)'
				      ii=EXECUTE(CoCo)                       ; SEND THE COMMAND !!!!!!!!!!!!

	    if not ii then begin val=0 & DialErrMes & DialStop & endif
	   endelse

	   geo_w(10,dnum)=geo_w(10,dnum)+((systime(1)-now)*1000)

	   if val ne 3.14 then P_GEO_STATUS,dnum, "SendOk" $
                        else P_GEO_STATUS,dnum, "SendNone"

endif else begin val=0 & XICUTE,comm(0) & endelse
return,val
end

pro DialClear, name, d=dnum
;** *********
;**
;** Remove the named dial

common dialshare2
common c_geo
	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if dnum gt 0 then begin
				 DialStop,d=dnum
				 DialTag ,d=dnum, tag="NAME", set=""
				;DialTag ,d=dnum, tag="PWD",  set=""
				 geo_w(6,dnum)=0
				 geo_w(8,dnum)=0
				 if geo_isw then widget_control,bad_id=ii,geo_w(2,dnum),sensitive =0
				 if geo_w(1,dnum) gt 0 then begin
				    di=strtrim(string(dnum),2)
				    P_GEO_DISPLAY, 'd'+di, geo_w(1,dnum) ,-1 ,DNUM=dnum
	endif		&	 endif
end

pro DialMacro, name, d=dnum, Si=di
;** *********
;**
;** Execute the macro of the named dial
;** Display the result

@lamp.cbk
common c_geo
common c_geowks, W71,W72,W73,W74,W75,W76,W77,W78,W79,W80,W81,W82,W83,W84,W85,W86,W87,W88,W89,W90, $
		 E71,E72,E73,E74,E75,E76,E77,E78,E79,E80,E81,E82,E83,E84,E85,E86,E87,E88,E89,E90
common c_geowkc, X71,X72,X73,X74,X75,X76,X77,X78,X79,X80,X81,X82,X83,X84,X85,X86,X87,X88,X89,X90, $
		 Y71,Y72,Y73,Y74,Y75,Y76,Y77,Y78,Y79,Y80,Y81,Y82,Y83,Y84,Y85,Y86,Y87,Y88,Y89,Y90

	if n_elements(name) eq 1 then dnum=DialNameToNumber(name)   else name=''
	if n_elements(dnum) ne 1 then dnum=geo_cur
	if dnum gt 0 then begin
	   if n_elements(di) ne 1 then di = strtrim(string(dnum),2)

	   geo_w(5,dnum)=geo_cur & geo_cur= dnum
	   geo_w(9,dnum)=0       & geo_w(10,dnum)=0
	   now=systime(1)

	   ii=execute('name =d'+di+'.NAME')
	   ii=execute('orig =d'+di+'.ORIGIN')
	   ii=execute('prox =n_elements(dt'+di+')') & jj=1

	   CATCH,stat & if stat ne 0 then begin catch,/cancel & DialErrMes & DialStop & return & endif

	   if prox gt 1 then begin DialFromD,keepd
	                     ii=execute('DialToD,  d'+di)
	                     jj=execute('COMMCA,  dt'+di)           ;EXECUTE THE MACRO OF THE DIAL from .prox
	                     ii=execute('DialFromD,d'+di)
	                                 DialToD  ,keepd
	   endif        else jj=execute('dial_'+orig+'_macro,d'+di) ;EXECUTE THE MACRO OF THE DIAL from .pro

	   if jj eq 0 then begin DialErrMes &  DialStop & endif

	   geo_w(8,dnum)=((systime(1)-now)*1000)>1

	   sz  =0  & ii=execute('sz   = SIZE (d'+di+'.VALUE)')
	   wupd=0  & ii=execute('wupd =d'+di+'.WUPDATE')
	   hiss=0  & ii=execute('hiss =d'+di+'.HISTORY')
	   if hiss then begin u=-1
		ii=execute('val=d'+di+'.VALUE(0)')
		if (val ne '') or (sz(sz(0)+2) gt 1) then begin
			on_ioerror,misopn & openw,u,"dial_"+name+".his",/append,/get_lun
			ii=execute('printf,u,d'+di+'.VALUE')
			misopn: if u gt 0 then free_lun,u
	   endif  & endif
	   val  =0
	   plt  =0      &         ii=execute('plt =       d'+di+'.PLOT')
	   histo=0

	   dw=di
	   if wupd le 0 then dw=strtrim(string(dnum+70),2)

	   if (sz(sz(0)+1) eq 7) and (plt gt -2) then begin
		ii=execute('val =  ";"+ d'+di+'.VALUE')
		if sz(sz(0)+2) gt 1 then begin
			WebDo,'log',val,dnum
			form_out,val & print,val & val='... '+val(n_elements(val)-1)
		endif else begin
			if val(0) le "; " then plt=-2
		endelse
		if plt ne -2 then begin ii=execute('w'+dw+' = val') & plt=-1 & endif

	   endif else if plt eq -1 then ii=execute('w'+dw+' = d'+di+'.VALUE') $

	   else if plt ge 0 then begin
		   sze=0.       &   ii=execute('sze  = d'+di+'.ERROR')
		   if n_elements(sze) ne sz(sz(0)+2) then sze=0.
				    alltag=[' '] & ii=execute('alltag = strupcase(TAG_NAMES(d'+di+'))')
				    xtl='' & ytl=''
				    idx=where(alltag eq 'X_TIT')  & idy=where(alltag eq 'Y_TIT')
				    if idx(0) ge 0 then  ii=execute ( 'xtl     = d'+di+'.X_TIT')
				    if idy(0) ge 0 then  ii=execute ( 'ytl     = d'+di+'.Y_TIT')
				    if wupd   gt 0 then  x_tit(dnum) = xtl    & !X.title=xtl
				    if wupd   gt 0 then  y_tit(dnum) = ytl    & !Y.title=ytl
				    if wupd   gt 0 then  w_tit(dnum) = name
				    if wupd   gt 0 then  other_tit(dnum) =  '(Dial)'
		   if sz(sz(0)+2) gt 1 then begin
				    ii=execute('w'+dw+' = d'+di+'.VALUE')
				    ii=execute('e'+dw+' = sze')
				    idx=where(alltag eq 'XVALUE') & idy=where(alltag eq 'YVALUE')
				    if idx(0) ge 0 then  ii=execute ( 'x'+dw+' = d'+di+'.XVALUE')
				    if idy(0) ge 0 then  ii=execute ( 'y'+dw+' = d'+di+'.YVALUE')
		   endif else begin
		     if plt gt 1 then  begin val=0 & ii=execute('val    = d'+di+'.VALUE')
			                             ii=execute('sz     = n_elements(w'+dw+')')
			if sz lt plt then begin      ii=execute('w'+dw+'=[(lonarr(plt-sz)+1)*val,w'+dw+']')
			           if sze gt 0. then ii=execute('e'+dw+'=  fltarr(plt)')
			endif
			if sz gt plt then begin      ii=execute('w'+dw+'= w'+dw+'(sz-plt:sz-1)')
			           if sze gt 0. then ii=execute('e'+dw+'=  fltarr(plt)')
			endif
			                 ii=execute('w'+dw+'=[w'+dw+'(1:plt-1),val]')
			if sze gt 0 then ii=execute('e'+dw+'=[e'+dw+'(1:plt-1),sze]')
			histo=1
		     endif          else ii=execute('w'+dw+'= d'+di+'.VALUE')
		   endelse
	   endif
	   if plt ge -1 then if (geo_w(1, dnum) gt 0) or (geo_w(13, dnum) gt 0) then begin
			ii=execute('P_GEO_DISPLAY, D'+di+', W'+dw+', error=E'+dw+', X=X'+dw+', Y=Y'+dw+', histo=histo')
	   endif
	   if geo_act eq 2 then WebDo,'dws',0,0,0

	   geo_cur=geo_w(5,dnum) & geo_w(5,dnum)=0
	   nel=n_elements(geo_timon)-1
	   if now - geo_timon(nel-dnum) gt 3. then begin
	   	if (dw eq di) and (plt gt -2) then TO_DON_HISTORY ,dnum,0,'W'+di+'=dial_'+name+'_macro result' ,/nojournal
		geo_timon(nel-dnum)=now
	   endif
	endif
end

pro DialFromD, Dial
;** *********
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
if n_elements(d) gt 0 then Dial=d
end
pro DialToD, Dial
;** *******
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
d=Dial
end
pro DialErrMes
;** **********
@lamp.cbk
common c_geo
  if l_message gt 0 then widget_control,bad_id=ii,l_message,set_value=strmid(!err_string,0,65)+' ...'
  print, !err_string
  WebDo,'err',!err_string
end
pro DialWSet, Activity=activity
;** ********
@lamp.cbk
common c_geo
  if keyword_set(activity) then begin
	P_GEO_DISPLAY,did_win0,1,0
  	geo_act=0
  endif else begin
  	if geo_act ne 2 then P_GEO_DISPLAY,did_win0,-1,0
  	geo_act=2
  	p_did_setwin0,/map
  endelse
end

pro DialInit, name, d=dnum, path=pth, restore=restore, new=newed, herits=diaH, nostart=nostart
;** ********
;**
;** the dial_name.pro file is compiled
;** and dial_name function is used to initiate the Dial.
;** Check for dial consistency then put it in D'dnum'.
;** /restore is used when restore saved session.
;** If new is defined then after loaded, give it the name "newed".

@lamp.cbk
common c_geo
common c_geowks, W71,W72,W73,W74,W75,W76,W77,W78,W79,W80,W81,W82,W83,W84,W85,W86,W87,W88,W89,W90, $
		 E71,E72,E73,E74,E75,E76,E77,E78,E79,E80,E81,E82,E83,E84,E85,E86,E87,E88,E89,E90
common c_geowkc, X71,X72,X73,X74,X75,X76,X77,X78,X79,X80,X81,X82,X83,X84,X85,X86,X87,X88,X89,X90, $
		 Y71,Y72,Y73,Y74,Y75,Y76,Y77,Y78,Y79,Y80,Y81,Y82,Y83,Y84,Y85,Y86,Y87,Y88,Y89,Y90

if n_elements(name)   eq 1 then begin
 if n_elements(newed) ne 1 then newed =strlowcase(name)
 if n_elements(dnum)  ne 1 then dnum  =DialNameToNumber( newed, /find)
 if (dnum ge 1) and (dnum lt (size(geo_w))(2)) then begin
  di=strtrim(string(dnum),2)
  if l_message gt 0 then widget_control,l_message,bad_id=ii,set_value=" "
  if n_elements(pth) ne 1 then pth =""
  CD,current=mee
  if pth gt " " then begin catch,stat & if stat eq 0 then CD,pth else catch,/cancel & endif
  fname="dial_"+name
  dial =''
  proc =findfile(fname+".pro" ,count=nn)
  if nn eq 0 then begin
     prox =findfile(fname+".prox",count=nn)
     if  nn gt 0 then begin DialFromD,keepd
                         COMMSI,fname+".prox", /EXEC             & DialFromD,dial
                         COMMSI,fname+"_macro.prox", MACRO=extxt & ii=execute('dt'+di+'=extxt')
                         DialToD  ,keepd  &  nn=-2
     endif & CD,mee
  endif
  if  nn ne -2 then begin
       if newed eq strlowcase(name) then if  sys_dep ('VERSION') ge 4.0 then $
       if (not sys_dep("EMBEDDED")) and (not sys_dep("RUNTIME")) then $
       ii  =execute('RESOLVE_ROUTINE,fname,/is_f')
       rout=[strupcase(fname)]
       if  sys_dep ('VERSION') ge 5.1 then ii=execute('rout=routine_info(/functions)')
       idx=where(rout eq strupcase(fname))
       if idx(0) ge 0 then ii=execute('dial='+fname+'()')	;INITIATE THE DIAL
       CD,mee
       if not ii then DialErrMes
  endif

  if n_elements(restore) eq 0 then begin

   if n_tags(dial) lt 1 then dial={init:0}

	tlist=strupcase(tag_names(dial))
	idx=where(tlist eq 'INHERIT')
	if idx(0) ge 0 then if dial.inherit gt ' ' then begin    diaG=''
	    DialInit , string (dial.inherit) , d=dnum, path=pth, herits=diaG
	    DialMix  , dial ,  diaG
	    tlist=strupcase(tag_names(dial)) & endif

	if n_elements(diaH) gt 0 then begin
	    DialMix  ,diaH,dial  &   return  & endif

	named    =newed
	generic  ='mad'
	type     =name
	value    =""
	error    =0.
	onoff    =0
	number   =dnum
	frequency=0.
	wupdate  =0. & if GEORGE eq 1 then wupdate=1
	upperlim =0.
	lowerlim =0.
	plot     =50
	history  =0
	duration =0
	init     =0
	unit     =''
	elsa     =''
	for k=0,n_elements(tlist)-1 do begin

	    CASE tlist(k) of
	    'NAME':
	    'NUMBER':
	    'PATH':
	    'ONOFF':     onoff    =fix   (dial.onoff)
	    'VALUE':     value    =dial.value
	    'ERROR':     error    =float (dial.error)
	    'PLOT':      plot     =fix   (dial.plot)
	    'WUPDATE':   wupdate  =fix   (dial.wupdate)
	    'INIT':      init     =fix   (dial.init)
	    'UNIT':      unit     =string(dial.unit)
	    'UPPERLIM':  upperlim =float (dial.upperlim)
	    'LOWERLIM':  lowerlim =float (dial.lowerlim)
	    'GENERIC':   generic  =string(dial.generic)
	    'TYPE':      type     =string(dial.type)
	    'FREQUENCY': frequency=float (dial.frequency)
	    'HISTORY':   history  =fix   (dial.history)
	    'DURATION':  duration =float (dial.duration)
	     ELSE:       elsa=elsa+','+tlist(k)+':dial.'+tlist(k)
	     ENDCASE
	endfor

	if keyword_set(nostart) then onoff=0
	DialClear,named
	ii=execute('d'+di+'={NAME:named,ORIGIN:name,GENERIC:generic,TYPE:type,INIT:init'+ $
	                   ',PLOT:plot ,UPPERLIM:upperlim,LOWERLIM:lowerlim,WUPDATE:wupdate'    + $
	                   ',ONOFF:onoff,VALUE:value,ERROR:error,FREQUENCY:frequency,DURATION:duration' +$
	                   ',HISTORY:history,PROX:[-1L,0],PROS:[-1L,0],NUMBER:number,UNIT:unit,PATH:pth'+$
	                     elsa +'}')

	if (wupdate gt 0) then begin
		ii=execute('w'+di+'=0')
		CLEARPAR, dnum,di
		w_tit    (dnum)=named  +' dial'
		other_tit(dnum)=type   +' (type) <- '+generic
		y_tit    (dnum)=unit
	endif
	ii=execute('w'+strtrim(string(dnum+70),2)+'=0')
	ii=execute('x'+strtrim(string(dnum+70),2)+'=0')
	ii=execute('y'+strtrim(string(dnum+70),2)+'=0')
	ii=execute('e'+strtrim(string(dnum+70),2)+'=0')

	if pth gt " " then begin catch,stat & if stat eq 0 then CD,pth else catch,/cancel & endif
	SetDuduch, "_read", generic, PROX & ii=execute('d'+di+'.PROX=PROX')
	SetDuduch, "_send", generic, PROX & ii=execute('d'+di+'.PROS=PROX')
	CD,mee

  endif
  if geo_isw then begin
			widget_control,bad_id=ii,geo_w(2,dnum),sensitive =1
			P_GEO_DISPLAY, newed, dnum ,-2

			DialTag, d=dnum, TAG='ONOFF'  , GET=onoff
			DialTag, d=dnum, TAG='ONOFF'  , SET=onoff
			DialTag, d=dnum, TAG='HISTORY', GET=history
			DialTag, d=dnum, TAG='HISTORY', SET=history
			endif

  if geo_w(1,dnum) gt 0 then P_GEO_DISPLAY, newed, geo_w(1,dnum) ,-1 ,DNUM=dnum
  
  if geo_cur le 0 then geo_cur=dnum ; To prevent problems in DialControl & DialNewValue
  geo_w(6,dnum)=1
 endif
endif
end

pro SetDuduch, proo, generic, PROX
;** *********
;**
common dialshare2

PROX=[-1L,0]
	  proc  =findfile ("dial_"+generic+proo+".pro" ,count=nn)
	  if nn eq 0 then begin
	   prof =findfile ("dial_"+generic+proo+".prox",count=nn)
	   if nn gt 0 then begin
		idx=where(duduch3 eq generic+proo) & idx=idx(0)
		if idx eq -1 then begin              idx=n_elements(proxcod)
		   COMMSI,"dial_"+generic+proo+".prox",  MACRO=tmpcod
		   nn     =n_elements(tmpcod)
		   duduch3=[duduch3,generic+proo,string(idx),string(idx+nn-1)]
		   proxcod=[proxcod,tmpcod]
		   PROX   =[idx,idx+nn-1L]
		endif else PROX=[long(duduch3(idx+1)),long(duduch3(idx+2))]
	   endif
	  endif
end

;** *******************************************************************************
;** *******************************************************************************
;** *******************************************************************************
function DialOn, dial, d=dnum
;******* ******
;**
;** Check for a User interrupt

common dialshare2
common c_geo

R=1
if geo_isw then begin
	if geo_onbas(1) gt 0 then begin
	   evv=widget_event(geo_onbas(1),/nowait,bad_id=ii)
	   if evv.id    eq  geo_onbas(1) then begin geo_stat =evv.select & R=geo_stat
	                                            geo_timon(0)=systime(1)*1000  &  endif
	endif
	if R then begin
	   if n_elements(dnum) ne 1 then dnum=geo_cur
	   if n_tags(dial)     gt 1 then dnum=dial.number
	   if (dnum ge 1) and (dnum le (size(geo_w))(2)) then begin
	     evv=widget_event(geo_w(3,dnum),/nowait,bad_id=ii)
	     if evv.id    eq  geo_w(3,dnum) then DialTag, d=dnum, tag='ONOFF', set=evv.select
	     DialTag ,d=dnum,tag='ONOFF', get=R
	   endif
	endif
endif
return,R
end

;************************************** WEB MODULES ***************************
;************************************** WEB MODULES ***************************
;************************************** WEB MODULES ***************************
pro WebFTP, file ,open=opn ,ifopn=ifopn ,check=check
;** ******
common c_geweb
common c_geUnit, gew_unit, gew_tftp ,gew_chk

;TEST FOR FTP AND BARNS ENABLE
if n_elements (gew_unit)  eq 0 then begin
   if sys_dep ('MACHINE') eq 'unix' then gew_unit= 0 else gew_unit=-1
   if sys_dep ('VERSION') lt 5.2    then gew_unit=-1
   bb=findfile(expand_path('~/.ssh')+'/*',count=n)  & if n lt 1 then gew_unit=-1
   gew_chk=0 & gew_tftp= dblarr(2)
endif else if keyword_set(opn) then begin if gew_unit lt 0 then gew_unit=0 & gew_chk=0 & endif

gew_unit=-1 ;Barns now is inside ILL

if gew_unit ge 0 then begin line=' ' & u=0
   on_ioerror,misftp
   if gew_unit gt 0 then begin now=systime(1) & delt=now-gew_tftp(0)
;   TEST TIME-TO-TIME FOR PIPE CONNECTION (OR INPUT COMMAND)
    if keyword_set(check) or (delt gt 7.) then begin
       if delt le 7. then begin if check eq 2 then gew_chk=0 & ok=1 & endif else ok=0
       s=fstat(gew_unit) & six=6
       if s.size gt six then begin ok=1
          while s.size gt six do begin readf,gew_unit,line & s=fstat(gew_unit)
	   if strpos(line,'Timeout')  ge 0 then begin s.size=0 & ok=0 & WebFTP_close & endif else $
	   if strpos(line,'by peer')  ge 0 then begin s.size=0 & ok=0 & WebFTP_close & endif else $
	   if strpos(line,gew_pth(3)) ge 0 then if (strpos(line,'Fetching') lt 0) and (strpos(line,'Removing') lt 0) then begin
	                                        printf, gew_unit,'get '+gew_pth(1)+gew_pth(3)+' '+gew_pth(1)+gew_pth(3)
                                                printf, gew_unit,'rm  '+gew_pth(1)+gew_pth(3) & gew_chk=2 & endif
          endwhile
          if ok eq 1 then begin if keyword_set(check) then begin
	                           if check eq 2 then gew_chk=0 $  ;RESET by webon_macro
				   else begin check=gew_chk        ;WILL BE THERE NEXT TIME IF gew_chk eq 2
	                                      if gew_chk eq 2 then begin gew_chk=1 & check=0 & endif
                                              if gew_chk eq 0 then printf, gew_unit,'dir '+gew_pth(1) & endelse
	                        endif
				printf, gew_unit,'pwd' & gew_tftp(0)=now & endif
       endif else check=gew_chk
       if ok eq 0 then if now-gew_tftp(1) gt 60*6. then gew_tftp(1)=now else $
                       if now-gew_tftp(1) gt 60*4. then WebFTP_close    else $
		       if now-gew_tftp(1) lt 20*1. then begin print,'No response from Ftp '+!stime & gew_tftp(1)=now-20 & endif
    endif
   endif
;  OPEN FIRST OR BROKEN CONNECTION
   if gew_unit eq 0 then begin
      if keyword_set(ifopn) then return
;     openr ,u,gew_pth(2),/get_lun & readf ,u,line & free_lun,u
;     openw ,u,'~/.netrc',/get_lun & printf,u,line & free_lun,u
;     spawn,'chmod 600 ~/.netrc' & openr ,u,'~/.netrc',/get_lun,/DELETE
      spawn,['sftp','arthur@barns'],unit=gew_unit,/noshell
      printf, gew_unit,'cd '+ gew_pth(0)
      printf, gew_unit,'pwd' & wait,2 & s=fstat(gew_unit) & six=6
      ok=1
      if gew_tftp(0) eq 0 then begin jj=5 & ok=0 ;FIRST TIME OPENED
         while (s.size le six) and (jj gt 0) do begin wait,1 & jj=jj-1 & s=fstat(gew_unit) & endwhile
         if s.size gt six then begin
            while s.size gt six do begin readf,gew_unit,line & s=fstat(gew_unit) & endwhile
            if strpos(line,strmid(gew_pth(0),0,strlen(gew_pth(0))-1)) ge 0 then begin
               printf, gew_unit,'pwd' & print,'Ftp reactivated '+!stime & ok=1 & endif
         endif
      endif                                      ;-----------------
      gew_tftp(0:1)=systime(1)
      if not ok then begin WebFTP_close & gew_unit=-1 & endif
      if u gt 0 then free_lun,u & u=0
   endif

;  SEND THE FILE
   if n_elements(file) eq 1 then printf, gew_unit,'put '+gew_pth(0)+file+ ' '+file
   return
   
   misftp: WebFTP_close & if u gt 0 then free_lun,u & if gew_tftp(0) eq 0 then gew_unit=-1
endif
end

pro WebFTP_close
;** ************
common c_geUnit
if n_elements(gew_unit) eq 1 then if gew_unit gt 0 then begin
   print,'Ftp canceled '+!stime
   if sys_dep('VERSION') lt 5.4 then free_lun,gew_unit else ii=execute('free_lun,gew_unit,/force')
   gew_unit=0
endif
end

pro dial_webon_macro,D
;** ****************
;**
nn =1 & WebFtp,CHECK=nn
if  nn eq 1 then bid=FINDFILE(D.webpth,count=nn)
IF  nn gt 0 then begin wline=[''] & value=['']
    on_ioerror,misopn
    OPENR,u,D.webpth,/GET_LUN,/DELETE
	on_ioerror,misread
	line=''
	WHILE (not EOF(u)) do begin READF,u,line & wline=[wline,line] & ENDWHILE
	misread:  on_ioerror,misopn
	FREE_LUN ,u
	WebFtp,CHECK=2
	IF n_elements(wline) gt 2 then if wline(1) eq D.pwd then begin
		wline=wline(2:*)
		XICUTER,' ;--->WebOn '+!stime+' {'+wline(0)+'...}'
		CATCH,stat & if stat ne 0 then begin print,!err_string & return & endif
		FOR i=0,n_elements(wline)-1 do $
			IF wline(i) ne D.pwd then begin line=strtrim(wline(i),2)
			
			   if strpos(line,'ii=barns_i') ge 0 then jj=EXECUTE(line) $
			   else if line gt ' '	then begin XICUTER,(line+'') & value=[value,line]
			        endif
			ENDIF
		XICUTER,' ;---> '
		if n_elements(value) gt 1 then value=value(1:n_elements(value)-1)
		WebDo,'log',value,12
	ENDIF
	DialModValue,value, TAG='VALUE'
    misopn:
endif else if D.value(0) ne '' then DialModValue,[''], TAG='VALUE'
end

function dial_webon
;******* **********
;**
return,{NAME:"webon",PLOT:0,HISTORY:0,VALUE:[''],FREQUENCY:3,WEBPTH:'',PWD:'',WUPDATE:-1}
end

pro WebOn, PATH=wpth ,PASSWORD=pass, PASSWD=pasw
;** *****
common c_geo
common c_geweb

catch,stat & if stat ne 0 then return

if n_elements(gew_pwd) ne 1 then       gew_pwd= ''
if n_elements(   pass) eq 1 then begin gew_pwd=pass & ii=execute('geokey,gew_pwd') & endif
if n_elements(   pasw) eq 1 then begin gew_pwd=pasw & ii=execute('geokey,gew_pwd') & endif
Mach=strupcase(getenv('HOST')) & id=strpos(Mach,'.')
if id gt 0 then Mach=strmid(Mach,0,id)
dvd=sys_dep('DIVIDER')
if n_elements(gew_pth) lt 2 then begin gew_pth=['','','','.geo_webon.txt']
		if Mach eq "" then begin F='C:\User\Didier\geoport\' & Mach='pcRichard'
		endif         else       F='/home/cs/lambda/geoport/'
		T=FINDFILE(F+'*',count=n)
		if n gt 0 then gew_pth(0)=F+Mach
		gew_pth(2)=F+'.bArns'
		F=F+'WebGeorge'+dvd
		T=FINDFILE(F+'*',count=n)
		if n gt 0 then gew_pth(1)=F+Mach+dvd else gew_pwd=''
endif

if n_elements(wpth) eq 1 then gew_pth(0)=  wpth
nld=strlen(gew_pth(0))-1
if nld gt 0 then if strmid(gew_pth(0),nld,1) eq dvd then gew_pth(0)=strmid(gew_pth(0),0,nld)

if gew_pth(0) gt ' ' then begin
 gew_pth(0)=gew_pth(0)+dvd
 on_ioerror,misopn
 openw,u,gew_pth(0)+'geo_d_0.web',/get_lun & free_lun,u

    list=findfile(gew_pth(0)+'geo_d_*',count=nn)
    if  nn gt 0 then bid=sys_dep('DELIST',list)

    par1 =''
    if Mach ne '' then begin
	catch,stat
	if stat eq 0 then ii=execute('par1=dial_pad_init_'+(Mach)+'(dummy)') else catch,/cancel
    endif
    
    WebFTP,/open
    
    sz=SIZE(par1)
    if  sz(0) lt 2 then ii=execute("par1=dial_pad_init()") & sz=SIZE(par1)
    if (sz(0) eq 2) and (sz(1) eq 5) then begin
	  openw,u,gew_pth(0)+'dial_pad.web',/get_lun
	  printf,u,'# George Instrument PAD'
	  printf,u,'# ',strtrim(string(sz(2)),2),' * 5 entries (label command flag program check)'
	  printf,u,'# First 5 lines for input text, others for buttons'
	  printf,u,'# '
	  for j=0,sz(2)-1 do for i=0,sz(1)-1 do printf,u,par1(i,j)
	  free_lun,u
	  WebFTP, 'dial_pad.web' & endif

 if sys_dep('MACHINE')  eq  'vms' then gew_v=';1' else gew_v=''
 gew_act=100 & gew_snd=['-'] & gew_err='-'
 tvlct,gew_r,gew_g,gew_b,/get
 if    gew_pwd gt ' ' then begin list=findfile(gew_pth(1)+gew_pth(3),count=nn)
				 if  nn gt 0 then bid=sys_dep('DELIST',list)
				 DialInit ,"webon",d=12
				 DialTag  ,"webon",tag="WEBPTH",set=gew_pth(1)+gew_pth(3)
				 DialTag  ,"webon",tag="PWD"   ,set=gew_pwd
				 DialStart,"webon" & endif
 geo_web=1
 WebDo,"sta","on",0
 return
 misopn: ;DialErrMes
endif
gew_pth='' & gew_pwd= ''
end

pro WebOff
;** ******
common c_geo
common c_geweb
if geo_web gt 0 then WebDo,"sta","off",0
geo_web=0
gew_pth='' & gew_pwd= '' & DialClear,"webon" & WebFTP_close
end

pro WebMess, val
;** *******
WebDo, 'log', val, 9
end

pro WebDo, flag, val, dnum, matx
;** *****
common c_geo
common dialshare2
common c_geweb

if geo_web eq 0 then return
on_ioerror,misopn
flg=flag
case flg of
'act':gew_act=100 + 100-val
'err':gew_err=val
'snd':gew_snd=val
'pth':begin   val=gew_pth(0) & return & end
'fil':WebFTP, val
'log':begin fifi='geo_d_'+strtrim(dnum,2)+'Lhtm.web'+gew_v
		openw,u,gew_pth(0)+fifi,/get_lun
		printf,u,'<html><head></head><body><b>'
		if dnum eq 12 then printf,u,'<h3>Command executed:</h3><br>'
		for i=0,n_elements(val)-1 do printf,u,'<nobr>'+val(i)+'<br>'
		printf,u,'</body></html>'  & printf,u,''
		flush,u & free_lun,u
		WebFTP, fifi
      end
;'dws':return ;nfs problem for big files
 else:begin ds=strtrim(dnum,2)			;'gif' 'dws' ('val' 'wks')
       now=systime(1)
       nel=n_elements(geo_timon)-1
       onf=1     &  if dnum gt 0 then ii=execute('onf =d'+ds+'.onoff')
       if (now-geo_timon(nel-dnum) ge 3.) or (flg eq 'val') or (not onf) then begin ;Care 3. le that for history!!!!!!

	freq=0   &  if dnum gt 0 then ii=execute('freq=d'+ds+'.frequency')
	dur =0   &  if dnum gt 0 then ii=execute('dur =d'+ds+'.duration')
	his =0   &  if dnum gt 0 then ii=execute('his =d'+ds+'.history')
	nam ='z' &  if dnum gt 0 then ii=execute('nam =d'+ds+'.name')
	if (not onf) then nam=nam+'_stopped'
	ifopn=0
	dws =flg
	if flg eq 'dws' then begin flg='gif' & if not val then begin DialWSet & geo_act=1 & endif else ifopn=1 & endif
	if flg eq 'gif' then begin r=0       & matx=tvrdd(r,g,b) & endif
	if flg eq 'val' then flg='gif'
	if flg eq 'gif' then if sys_dep('VERSION') ge 5.4 then  flg='png'

	img=n_elements(matx)
	if img gt 1 then $
	   if flg ne 'wks' then begin fifi='geo_d_'+ds+'.'+flg+gew_v
	    if n_elements(r) le 1 then $
	         WRITE_KIF,gew_pth(0)+fifi,matx,gew_r,gew_g,gew_b,transparent=0 $
	    else WRITE_KIF,gew_pth(0)+fifi,matx, r,g,b,transparent=0
	    WebFTP, fifi, IFOPN=ifopn
	   endif
	if dws eq 'dws' then flg=dws

	fifi='geo_d_'+ds+'.web'+gew_v
	openw,u,gew_pth(0)+fifi,/get_lun
	printf,u, nam,gew_act,geo_freq,geo_lim
	printf,u, freq,dur,his,' '+flg
	printf,u, val
	if gew_err    ne '' then begin printf,u,'err:'+gew_err & gew_err=''   & endif
	if gew_snd(0) ne '' then begin printf,u,'snd:'+gew_snd & gew_snd=[''] & endif
	flush,u & free_lun,u
	WebFTP, fifi, IFOPN=ifopn
	
       endif
      end
endcase
misopn:
end
;************************************ END WEB MODULES *************************
;************************************ END WEB MODULES *************************

pro P_GEO_EVENT, ev, uv
;** ***********
;**
;** Widget events handler
@lamp.cbk
common c_geo

if uv(1) eq 667 then begin geo_stat  =ev.select  & uv(1)=666                        ;on/off General
                           geo_w(8,*)=0 & endif
case uv(1) of
	614: begin                                                                  ;Mad command
		widget_control,uv(3),bad_id=ii,get_value=comm
		comm=strtrim(comm(0),2)
		if strpos(strupcase(comm),'CTRL:') ne 0 then comm='Ctrl:'+comm
		XICUTER,comm
	     end
	660: if geo_act ne ev.select then begin geo_act=ev.select                   ;plot Activity
	        if geo_act gt 0 then P_GEO_DISPLAY,did_win0,0,0 $
			        else P_GEO_DISPLAY,did_win0,1,0
	     endif
	665: DialTag, d=uv(2), tag='ONOFF', set=ev.select                           ;on/off on Dials
	666: begin                                                                  ;Change frequency ?
		widget_control,uv(2),get_value=bid
		on_ioerror,mis666
		geo_freq=(float(bid(0))>0.) & duduch1=geo_freq
		bid=0
		widget_control,uv(4),get_value=bid,bad_id=ii                        ;Change duration  ?
		on_ioerror,misbid
		geo_lim =(float(bid(0))>0.) & duduch2=geo_lim & misbid:
                geo_timon(0)=systime(1)*1000
		P_GEO_TIMER, {id:uv(3)}
		mis666:
	     end
	670: begin case uv(2) of
	     1:  begin dnum=uv(3)                                                   ;Reset
	               pth   =''   & DialTag,d=dnum,tag="PATH"  ,get=pth
	               named =''   & DialTag,d=dnum,tag="NAME"  ,get=named
	               named =''   & DialTag,d=dnum,tag="NAME"  ,get=named
	               origin=''   & DialTag,d=dnum,tag="ORIGIN",get=origin
	               DialInit, origin, d=dnum, path=pth ,new=named
	                            DialTag,d=dnum,tag="ONOFF",set=onoff
	               end
	     2:  DialTag,d=uv(3),tag="HISTORY",set=0                                ;History off
	     3:  DialTag,d=uv(3),tag="HISTORY",set=1                                ;History on
	     4:  begin DialTag,d=uv(3),tag="NAME",get=named                         ;Clear history file
	               bid=sys_dep('DELET',"dial_"+named+".his")
	               end
	     5:  begin dn=uv(3) & dw=geo_w(13,dn)                                   ;large dial
		       keep_w=!D.window
	               if dw gt 0 then begin ii=execute('wset,dw')
	                                     if ii ne 1  then dw=0 else wshow,dw & endif
	               DialTag,d=uv(3),tag="WUPDATE",get= wupd
	               DialTag,d=uv(3),tag="WUPDATE",set=(wupd<1)
		       
	               if dw le 0 then begin DialTag,d=dn,tag="NAME" ,get=named
		                               if geo_w(1,dn) gt 0 then P_GEO_DISPLAY, 'Large '+named,geo_w(1,dn),-1
	                                     WINDOW,xsize=geo_bxy(0)*2,ysize=geo_bxy(1)*2,title=named,/FREE
	                                     dw=!D.Window & endif
	               geo_w(13,dn)=dw
		       if keep_w gt 0 then wset,keep_w
	         end
	     6:  begin di=strtrim(string(uv(3)),2)                                  ;Properties list
	               tlist=[''] & ii=execute('tlist=strupcase(tag_names(d'+di+'))')
		       if geo_lead gt 0 then begin b0=widget_base(title="Dial "+di+" properties",resource_name="lamp")
						   b1=widget_base(b0,/column,resource_name="geo") & endif
	               for k=0,n_elements(tlist)-1 do begin
			   DialTag,d=uv(3),tag=tlist(k),get=V & tag='d'+di+'.'+tlist(k)
	                   CASE tlist(k) of
	                   'ERROR':
	                   'PROX':
	                   'PROS':
	                   'PWD':
	                    ELSE:   if geo_lead gt 0 then begin sz=SIZE(V)
			    		if sz(sz(0)+1) eq 8 then V='is a structure' $
			    		else if sz(sz(0)+2) gt 1 then begin  V=strtrim(string(sz(1)),2)
							if sz(0) gt 1 then V=V+','+strtrim(string(sz(2)),2)
							if sz(0) gt 2 then V=V+','+strtrim(string(sz(3)),2)
							if sz(0) gt 3 then V=V+',..'
							V='size('+V+')'
					endif else      V= strtrim(string(V(0)),2)
			    		b2=widget_base (b1,/row)
					bb=widget_label(b2,value=tag+"=",font=ft_b_normal)
					bb=widget_label(b2,value=V      ,font=ft_b_normal)
				    endif else print, tag+'= ',V
	                   ENDCASE
	               endfor
		       if geo_lead gt 0 then widget_control,b0,group_leader=geo_lead,/realize
	         end
	     7:  if ev.TYPE eq 0 then begin
	               di=strtrim(string(uv(3)),2) & N=0 & wupd=0				;plot in main window
	               geo_info(5)=did_win0							;case main window changed
	               DialTag,d=uv(3),tag="WUPDATE",get=wupd
	               DialTag,d=uv(3),tag="WUPDATE",set=2
	               ii=execute('N=n_elements(D'+di+'.VALUE)')
	               if N  gt 1 then begin
	                  DialTag,d=uv(3),tag="XVALUE",get=XX
	                  DialTag,d=uv(3),tag="YVALUE",get=YY
	                  ii=execute('P_GEO_DISPLAY, D'+di+', D'+di+'.VALUE, error=D'+di+'.ERROR, X=XX, Y=YY')
	               endif
	               if ev.PRESS eq 2 then DialTag,d=uv(3),tag="WUPDATE",set=1 ;middle
	               if ev.PRESS eq 1 then DialTag,d=uv(3),tag="WUPDATE",set=0 ;left
		 endif
	     8:        DialClear,d=uv(3)                                            ;Remove the dial
	     else:
	     endcase
	     if (uv(2) ge 80) and (uv(2) le 95) then begin
			widget_control,ev.id,get_value=fq & DialTag,d=uv(3),tag="FREQUENCY",set=float(fq(0))
			;widget_control,uv(4),set_value='Own Frequency='+fq
			DialTag,d=uv(3),tag="ONOFF",get=V & if V eq 1 then DialTag,d=uv(3),tag="ONOFF",set=1
	     endif
	     end
	671: begin  DialTag,d=uv(3),tag="PLOT",set=uv(2)
	            if uv(2) eq -2 then if geo_w(1,uv(3)) gt 0 then P_GEO_DISPLAY,'no Plot',geo_w(1,uv(3)),-1, DNUM=uv(3)
	     end
	else:
endcase
end

pro P_GEO_TIMER, ev
;** ***********
;**
;** Timer handler

common dialshare2
common c_geo

begtim=systime(1)
if ev.id eq geo_onbas(3) then begin
;  GENERAL TIMER
   if geo_lim gt 0  then if (begtim*1000 - geo_timon(0) gt geo_lim*1000) then DialsFrequency,/STOP
   if geo_stat      then begin
	 P_GEO_RETIM ,ev.id, geo_freq
	 Pn=((size(geo_w))(2))-1
	 dnum =0
	 R    =1
	 for i=1,Pn do if geo_w(6,i) eq 1 then begin

	   di   =strtrim(string(i),2)
	   onoff=0 & ii=execute('if n_tags(d'+di+') gt 1 then onoff=d'+di+'.ONOFF else geo_w(6,i)=0')
	   if onoff then begin
	    freq=0 & ii=execute('freq=d'+di+'.FREQUENCY')
	    if   freq le 0 then begin
	      if dnum eq 0 then P_GEO_STATUS, 0,"Loop",begtim  else   R=DIALON(d=(i+0))

	      if geo_stat  then if R then begin dnum=i
	                                        P_GEO_STATUS, dnum, "This" ,2
		                                DIALMACRO,  d=dnum, Si=di
	    					freq=0 & ii=execute('freq=d'+di+'.FREQUENCY')
	    					if freq gt 0 then P_GEO_RETIM ,geo_w(11,dnum), freq
	      endif
	    endif else if abs(geo_w(12,i)) eq 2 then  DialStart,d=(i+1-1)
	   endif else geo_w(8,i)=0
	 endif
	 if   dnum  gt 0   then P_GEO_STATUS, (geo_freq+0), "EndL" ,begtim
	 totim=geo_freq-(systime(1)-begtim)
	 if totim lt 0 then if geo_freq gt 0 then P_GEO_RETIM ,ev.id, totim ;GIVE TIME FOR OTHERS.
   endif
endif else begin

;  SPECIFIC TIMER
   widget_control,ev.id,get_uvalue=dnum
   di   = strtrim(string(dnum),2)
   freq = 0 & ii=execute('freq =d'+di+'.FREQUENCY')
   durat= 0 & ii=execute('durat=d'+di+'.DURATION')
   if durat gt 0 then if (begtim*1000 - geo_timon(dnum) gt durat*1000) then DialStop, d=dnum
   onoff= 0 & ii=execute('onoff=d'+di+'.ONOFF')
   if freq gt 0 then if onoff then begin
		P_GEO_RETIM ,ev.id,  freq
		P_GEO_STATUS, 0   , "Loop", begtim
		P_GEO_STATUS, dnum, "This" ,1
		DIALMACRO  ,d=dnum, Si=di
		P_GEO_STATUS, freq, "EndL" ,begtim
		fruq=freq & ii=execute('freq =d'+di+'.FREQUENCY')
		if freq ne fruq then P_GEO_RETIM ,ev.id,  freq $
		else begin
		  totim=(geo_freq*geo_stat)+freq - (systime(1)-begtim)
		  if totim lt 0 then P_GEO_RETIM ,ev.id, totim              ;GIVE TIME FOR OTHERS.
		endelse
   endif    else geo_w(8, dnum)=0
endelse
end

pro P_GEO_RETIM, id,freq
;** ***********
;**
;** RE-TIMER
			widget_control,bad_id=ii,id,/CLEAR_EVENTS
if freq gt 0 then	widget_control,bad_id=ii,id, TIMER=freq	else $
if freq lt 0 then	widget_control,bad_id=ii,id, TIMER=0.05<(-freq)
end

pro P_GEO_STATUS, dnum, way , flgtim
;** ************
;**
;** Display a scheme for the timer process

common dialshare2
common c_lamp_info
common c_geo
common c_geweb
common c_geostat,kpstr

if way eq "Loop" then       geo_seq=flgtim
if way eq "This" then       geo_w(12,dnum)=flgtim
if geo_isw then begin

   if geo_act eq 0 then begin keep_w=!D.window & wset,did_win0
   case way of

	"Loop":
	"This":     begin if geo_info(4) ne dnum then begin
			      device,copy=[0,dnum*25,geo_bxy(0),geo_bxy(1)/2,   20      ,geo_info(3),geo_alp(3)]
			      geo_info(4)=dnum & endif
	            end

	"Get":      begin if geo_info(4) ne dnum then begin
			      device,copy=[0,dnum*25,geo_bxy(0),geo_bxy(1)/2,   20      ,geo_info(3),geo_alp(3)]
			      geo_info(4)=dnum & endif
	                  plots,[geo_bxy(0)/2+20,geo_info(0)/3] , [geo_info(3)-10,40],thick=2.,/device,color=120
	            end

	"GetOk":          plots,[geo_bxy(0)/2+20,geo_info(0)/3] , [geo_info(3)-10,40],thick=2.,/device,color=0

	"GetNone":  begin if l_message gt 0 then widget_control,l_message,bad_id=ii, $
	                     set_value='Dial'+string(dnum)+' got a Bad new Value !!!'
			  plots,[geo_bxy(0)/2+20,geo_info(0)/3] , [geo_info(3)-10,40],thick=2.,/device,color=0
	            end

	"Send":     begin if dnum gt 0 then $
			   if geo_info(4) ne dnum then begin
			      device,copy=[0,dnum*25,geo_bxy(0),geo_bxy(1)/2,   20      ,geo_info(3),geo_alp(3)]
			      geo_info(4)=dnum & endif
			  plots,[geo_bxy(0)/2+20,geo_info(0)*2/3],[geo_info(3)-10,70],thick=2,/device,color=120
	            end

	"SendOk":   plots,[geo_bxy(0)/2+20,geo_info(0)*2/3],[geo_info(3)-10,70],thick=2,/device,color=0

	"SendNone": begin if l_message gt 0 then widget_control,l_message,bad_id=ii, $
	                     set_value='Dial'+string(dnum)+' got a Send Control error-code !!!'
			   plots,[geo_bxy(0)/2+20,geo_info(0)*2/3],[geo_info(3)-10,70],thick=2,/device,color=0
	            end

	"EndL":     begin device,copy=[0,0      ,geo_bxy(0),geo_bxy(1)/2,   20      ,geo_info(3),geo_alp(3)]
	                  geo_info(4)= 0
				idx=where(geo_w(8,*) gt 0)
				totim= ((systime(1)-flgtim)*1000+geo_w(9,0))>1
				now  =   systime(1)
				if idx(0) ge 0 then begin
				   for i= 0,n_elements(idx)-1 do if geo_w(12,idx(i)) gt 0 then begin

				       j=idx(i)
				       geo_w(8 ,j)=(geo_w(8 ,j)*100./totim)>1
				       geo_w(9 ,j)=(geo_w(9 ,j)*100./totim)
				       geo_w(10,j)=(geo_w(10,j)*100./totim)
				       geo_w(12,j)=-geo_w(12,j)

				       endif else geo_w(8:10)=1

				   P_GEO_DISPLAY, idx ,dnum,totim
				   if geo_web then begin
				      if dnum gt 0 then ppm=long(dnum*1000*100./totim)<100 else ppm=100
				      WebDo,'act',ppm
				      endif
				endif
				device,copy=[0,0,geo_info(0)/2+30,geo_info(1)*2/3,geo_info(0)/2-32,geo_info(1)/3-2,geo_alp(4)]
				geo_w(9,0) =(systime(1)-now)*1000
	            end
	else:
	endcase
	if keep_w gt 0 then wset,keep_w

   endif else if geo_act gt 0 then begin di=string(dnum)
   case way of

	"Loop":      kpstr=''
	"This":      begin ok=1 & if n_elements(b_labins) ge 6 then ok=b_labins(5)
	             if ok then begin
	                    di=strtrim(di,2) &  name="" & ii=execute('name=d'+di+'.NAME')
						wupd="" & ii=execute('wupd=d'+di+'.WUPDATE')
	             if wupd gt 0 then sws='W' else sws='D'
	             if wupd ge 0 then begin kpstr=kpstr+' ('+sws+di+')'+name
	             widget_control,l_message,bad_id=ii,set_value='............ Dial '+name+' running ...'
		     endif & endif
		     end
	"Get":      ;widget_control,l_message,bad_id=ii,set_value='............ Dial'+di+' requesting ...'
	"GetOk":    ;widget_control,l_message,bad_id=ii,set_value='............ Dial'+di+' running ...'
	"GetNone":  ;widget_control,l_message,bad_id=ii,set_value='............ Dial'+di+' bad request !!!'
	"Send":     ;widget_control,l_message,bad_id=ii,set_value='............ Dial'+di+' sending control ...'
	"SendOk":   ;widget_control,l_message,bad_id=ii,set_value='............ Dial'+di+' running ...'
	"SendNone": ;widget_control,l_message,bad_id=ii,set_value='............ Dial'+di+' bad control !!!'
	"EndL":      begin ok=1 & if n_elements(b_labins) ge 6 then ok=b_labins(5)
	             if ok then begin
	               if kpstr gt ' ' then begin
	                  totim= ((systime(1)-flgtim)*1000)>1
	                  ppm  =long(dnum*1000*100./totim)
	                  if ppm lt 100 then miss=' mis:'+string(long(totim-dnum*1000)/1000.)+' sec' else miss=''
	                  widget_control,l_message,bad_id=ii,set_value=kpstr+string(ppm<100)+'%'+miss
	                  if geo_web then WebDo,'act',ppm<100
                       endif
	             endif
		     end
	else:
   endcase
   endif
endif
end

pro P_GEO_DISPLAY, D, W ,totim  ,error=E ,X=XXX ,Y=YYY ,histo=histo ,DNUM=dnum
;** *************
;**
;** Dial   display -> D=dial      ,  W=workspace
;** Scheme display -> D=dial index,  W=frequence,  totim= time
;** Backgr display -> D=did_win0  ,  W=big arrow,  totim= 0
;** Backgr reset   -> D=did_win0  ,  W=1        ,  totim= 0
;** Backgr erase   -> D=did_win0  ,  W=0        ,  totim= 0
;** di     xyouts  -> D=di        ,  W=winID    ,  totim=-1
;** Dial big name  -> D=name      ,  W=dnum     ,  totim=-2
;** c_did  info    -> D=return [] ,  W=0        ,  totim=-3

common c_trap, trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
common c_draw ; rx,rz,nlv,styles(0,0),styles(1,0)
common c_did  ;[did_x,did_y,did_wd]
common c_geo
common c_geweb

	keep_w=!D.window
	if n_elements(totim)  eq 0 then begin

			dw  =geo_w(13,D.NUMBER)
			if  (D.WUPDATE eq 2)  then begin if dw gt 0 then              ii=execute('wdelete,dw')
							 dw=geo_info(5)
							 geo_w(13,D.NUMBER)=0       & ii=execute('wset   ,dw')
			endif else if dw gt 0 then begin ii= execute('wset, dw')    & if ii ne 1  then    dw=0
							 geo_w(13,D.NUMBER)=dw      & endif
			trap_current=dw
			stime=strmid(!stime,12,8)
			sz =SIZE(W) & upl=D.UPPERLIM & lwl=D.LOWERLIM
			if sz(sz(0)+2) eq 1 then begin
			   if (D.PLOT le 0) or (upl le lwl) or (dw gt 0) then begin
				if  (dw le 0) or (geo_web) then begin
				     wset,geo_alp(1) & erase
				     xyouts,3,3,strtrim(string(W(0)),2), charsize=1.,/device
				     xyouts,3,geo_bxy(1)-10,D.NAME     , charsize=1.,/device
				     wset  ,geo_w(1,D.NUMBER)
				     device,copy=[0,0,geo_bxy(0),geo_bxy(1),0,0,geo_alp(1)]
				     if geo_web then WebDo,'gif',W(0),D.number
				     endif
				if  (dw gt 0) then begin
				     if geo_web then wset,dw & erase
				     xyouts,9,9,strtrim(string(W(0)),2), charsize=3.,charthick=3,font=-1,/device
				endif
			   endif else begin
				wset,geo_w(1,D.NUMBER)
				on_ioerror,notan & res=0
				res=fix(float(upl-W(0))/(upl-lwl) * geo_bxy(0))
				device,copy=[res,0,geo_bxy(0),geo_bxy(1)-3,0,3,geo_alp(0)]
				notan: if res eq 0 then erase
				xyouts,3,geo_bxy(1)-23,strtrim(string(W(0)),2),charsize=1.,/device
			        xyouts,3,geo_bxy(1)-10,D.NAME,charsize=.6,/device
				if geo_web then WebDo,'gif',W(0),D.number
			   endelse

			endif else if sz(0) eq 1 then begin
			   if n_elements(xxx) ne sz(1) then xxx=indgen(sz(1))
			   if keyword_set(histo) then velu=       strtrim(string(W(sz(1)-1)),2) $
			   		   else begin mox =max(W) & velu= 'Max:'  +strtrim(string(mox),2)     + $
					   				  ' at X:'+strtrim(string(xxx(!C)),2)
					   endelse
			   if (dw le 0) or (geo_web)   then begin
			      wset,geo_alp(1)
			      if upl le lwl then plot,xxx,W,xmargin=[0,0],ymargin=[0,2],charsize=.7,font=-1
			      if upl gt lwl then plot,xxx,W,xmargin=[0,0],ymargin=[0,2],charsize=.7,font=-1,yrange=[lwl,upl]
			      if (not geo_web) then xyouts,3,geo_bxy(1)-10,D.NAME+'='+velu+'  ',charsize=.6,/device $
			                       else xyouts,3,geo_bxy(1)-10,velu+'      '       ,charsize=.6,/device
			      wset,geo_w(1,D.NUMBER)
			      device,copy=[0,0,geo_bxy(0),geo_bxy(1),0,0,geo_alp(1)]
				if geo_web then WebDo,'gif',W(n_elements(W)-1),D.number
				endif
			   if (dw gt 0)  then begin
				if geo_web then wset,dw
			      erase
			      velu=velu+' Updated:'+stime
			      if n_elements(E) eq n_elements(W) then sym=4 else sym=10
			      if upl le lwl then plot,xxx,W,xmargin=[10,1],ymargin=[4,2],title=velu,psym=sym,$
			                              yticks=4,yticklen=1.,ygridstyle=1,thick=1.4,charthick=1.4,charsize=1.2,$
						      font=-1
			      if upl gt lwl then plot,xxx,W,xmargin=[10,1],ymargin=[4,2],title=velu,psym=sym,yrange=[lwl,upl],$
			                              yticks=4,yticklen=1.,ygridstyle=1,thick=1.4,charthick=1.4,charsize=1.2,$
						      font=-1
			      if n_elements(E) eq n_elements(W) then errplot,xxx,W-E,W+E
			   endif

			endif else if sz(0) eq 2 then begin
			   if (n_elements(xxx) ne sz(1)) and (n_elements(xxx) ne sz(1)*sz(2)) then xxx=indgen(sz(1))
			   if (n_elements(yyy) ne sz(2)) and (n_elements(yyy) ne sz(1)*sz(2)) then yyy=indgen(sz(2))
			   congw='w' & congx=',xxx' & congy=',yyy' & qq=128L
			   if upl gt lwl then congw='w>lwl<upl'
			   if (sz(sz(0)+2) gt qq*qq) and (D.PLOT ne 0) then begin
			   	                                 congw= 'congrid('+congw+',qq<sz(1),qq<sz(2))'
			   	if n_elements(xxx) eq sz(1) then congx=',congrid(xxx,qq<sz(1))'          else $
								 congx=',congrid(xxx,qq<sz(1),qq<sz(2))'
			   	if n_elements(yyy) eq sz(2) then congy=',congrid(yyy,qq<sz(2))'          else $
								 congy=',congrid(yyy,qq<sz(1),qq<sz(2))'
			   endif
			   if D.PLOT eq 0 then mmx=max(W)
			   if (dw le 0) or (geo_web)  then begin
			      wset,geo_alp(1)
			      if D.PLOT eq 0 then begin 
				if mmx gt 10^3 then ii=execute('tvscl,ALOG((CONGRID('+congw+',geo_bxy(0),geo_bxy(1))+1)>1)') $
					       else ii=execute('tvscl,      CONGRID('+congw+',geo_bxy(0),geo_bxy(1))      ')
			      endif else $
			      if D.PLOT ne 2 then ii=execute($
				'shade_surf,'+congw+congx+congy+',xstyle=4,ystyle=4,zstyle=4,xmargin=[0,0],ymargin=[0,0],az=rz,ax=rx') $
			                      else ii=execute($
				'contour,'   +congw+congx+congy+',xstyle=4,ystyle=4,/fill   ,xmargin=[0,0],ymargin=[0,0],nlevels=nlv')
			      if (not geo_web) then xyouts,3,geo_bxy(1)-10,    D.NAME,    charsize=1.,/device
			      wset,geo_w(1,D.NUMBER)
			      device,copy=[0,0,geo_bxy(0),geo_bxy(1),0,0,geo_alp(1)]
			      if geo_web then WebDo,'gif',0,D.number
			      endif
			   if (dw gt 0) then begin
			   	titil=D.NAME+' updated:'+stime
				if geo_web then wset,dw
			      ;sxx=[1,round(sz(1)/4),round(sz(1)/2),round(sz(1)*3/4),sz(1)] & sxx=strtrim(string(sxx),2)
			      ;syy=[1,round(sz(2)/4),round(sz(2)/2),round(sz(2)*3/4),sz(2)] & syy=strtrim(string(syy),2)
			      if D.PLOT eq 0 then begin
				dxsiz=!D.x_size & xo=40 & xf=10
				dysiz=!D.y_size & yo=40 & yf=20
				if (sz(1) eq sz(2)) and (D.WUPDATE eq 2) then begin
				   if dxsiz gt dysiz then dxsiz=dysiz else dysiz=dxsiz
				endif
				erase
				if mmx gt 10^3 then ii=execute('tvscl,ALOG((CONGRID('+congw+',dxsiz-xo-xf,dysiz-yo-yf)+1)>1),xo,yo') $
					       else ii=execute('tvscl,     (CONGRID('+congw+',dxsiz-xo-xf,dysiz-yo-yf))     ,xo,yo')
				ii=execute('plot,W,charsize=1.2 ,xrange=[xxx(0),xxx(sz(1)-1)],yrange=[0,sz(2)-1]'+$
				           ',/nodata,position=[xo,yo,dxsiz-xf-1,dysiz-yf-1],/noerase,/device,title=titil')
			      endif else $
			      if D.PLOT ne 2 then begin
				surfbody=congw+congx+congy+',az=rz,ax=rx,xticks=4,yticks=4,zticks=1,title=titil'
				;surfbody=surfbody+',xtickname=sxx,ytickname=syy'
			 	surfkey ='' & surf='surface,'
				if styles(0,0) eq 4 then surfkey =''             else $
				if styles(0,0) eq 5 then surfkey =',/lego'       else $
				if styles(0,0) eq 6 then surfkey =',/horizontal'      $
				                    else surf    ='shade_surf,'
				ii=execute( surf+surfbody+surfkey )
			      endif else begin
				surfbody=congw+congx+congy+',xticks=4,yticks=4,zticks=2,xmargin=[10,1],ymargin=[4,2],title=titil'
				;surfbody=surfbody+',xtickname=sxx,ytickname=syy'
				surf    ='contour,'
				if styles(1,0) ne 1 then surfkey =',/fill,nlevels=nlv' $
				                    else surfkey =',c_colors=(indgen(nlv)+1)*(180/nlv)+50,nlevels=nlv'
				ii=execute( surf+surfbody+surfkey )
			      endelse
			   endif
			endif

	endif else if totim gt 0 then begin

		if W gt 0 then ppm=long(W*1000*100./totim)<100 else ppm=100
		wset,geo_alp(4) & trap_current=geo_alp(4)
		nk =n_elements(D)
		YYt=reform(geo_w(8 ,D))
		YYl=reform(geo_w(10,D))
		YYm=reform(geo_w(9 ,D))+YYl

		if  nk eq 1 then begin
		    YYt=[YYt,0] & YYm=[YYm,0] & YYl=[YYl,0] & D=[D,0] & endif
		sx ='d'+strtrim(string(D),2)
		if  nk eq 1 then begin nk =2  & sx(1)='  '  & endif
		ttl=    strtrim(string(round(totim/10.)/100.),2)+' Elapse sec.'

		if nk gt 8 then begin
		plot, YYl,xmargin=[2,2],ymargin=[2,2],charsize=.8,font=0,yrange=[0,100],$
		          psym=10,xticks=nk-1,xtickname=sx,title=ttl,ystyle=4,xthick=2,linestyle=1

		oplot,YYm,psym=10,linestyle=2
		oplot,YYt,psym=10,linestyle=0
		if ppm lt 100 then begin YYl(*)=ppm & oplot,YYl,thick=2 & endif

		endif else begin
		YP =[[YYl],[YYm],[YYt]]
		sy =['Command    ','Request  ','Macro']
		sz =[ ' ' , strtrim(string(ppm),2)+'%' ]
		w4d=bytscl(YP,min=0,max=100)>20
		surface,YP,xmargin=[4,2],ymargin=[2,2],zrange=[0,100],xticks=nk-1,xtickname=sx,shades=w4d,$
		        title=ttl,/lego,charsize=1.2,font= 0,yticks=2,ytickname=sy,zticks=1,ztickname=sz

		if ppm lt 100 then begin
		YP(*)=ppm
		surface,YP,xmargin=[4,2],ymargin=[2,2],zrange=[0,100],/lego,/noerase,$
		        xstyle=4,ystyle=4,zstyle=4,charsize=1.2,font= 0
		endif
		endelse

	endif else if totim eq 0 then begin
		if n_elements(w) gt 1 then begin
		    wset,geo_alp(4) & erase
		    wset,geo_alp(3) & erase
		    wset,geo_alp(0) & erase & tv,W
		endif else begin
		 wset,D
		 if (n_elements(w) eq 1) and (w(0) le 0) then begin
		 ;if w(0) eq 0 then did_fu=0 else did_fu=1
		 endif else begin
		  erase
		 ;did_fu=1
		  inf=2.4
		  eng=sys_dep('MACHINE') & if (eng eq 'win') then inf=3. else if (eng eq 'mac') then inf=1.5
		  geo_info(3)= geo_info(1)-geo_bxy(1)-10
		  geo_info(2)= geo_info(0)-geo_bxy(0)-10
		  wdt=20 & cz=(geo_info(0)/341.33*2) & ct=round(cz) & cz=cz<inf
		  xyouts,wdt-4,14,"Values REQUEST ",font=-1,charsize=cz,charthick=1 ,/device,color=100
		  xyouts,wdt-3,13,"Values REQUEST ",font=-1,charsize=cz,charthick=1 ,/device,color=100
		  xyouts,wdt-0,10,"Values REQUEST ",font=-1,charsize=cz,charthick=ct,/device,color=255,width=wdt
		  wdt=geo_info(0)*wdt
		  wdt=geo_info(0)-1-wdt-20
		  xyouts,wdt-4,44,"COMMAND Control",font=-1,charsize=cz,charthick=1 ,/device,color=100
		  xyouts,wdt-3,43,"COMMAND Control",font=-1,charsize=cz,charthick=1 ,/device,color=100
		  xyouts,wdt-0,40,"COMMAND Control",font=-1,charsize=cz,charthick=ct,/device,color=255

		  xf1=(geo_info(0)*2/3) - ((geo_info(0)*2/3)-(geo_bxy(0)/2+20))/4
		  xf2=(geo_info(0)  /3) - ((geo_info(0)  /3)-(geo_bxy(0)/2+20))/4
		  yf1=70 + ((geo_info(3)-10)-(70))/4
		  yf2=40 + ((geo_info(3)-10)-(40))/4
		  plots,[xf1,geo_info(0)*2/3], [yf1,70],thick=2.,/device,color=255
		  plots,[xf2,geo_info(0)  /3], [yf2,40],thick=2.,/device,color=255
		 endelse
		endelse
	endif else if totim eq -1 then begin

		wset,W   &  erase
		xyouts,3,geo_bxy(1)-9,D,charsize=1,/device
		if n_elements(dnum) eq 1  then dw  =geo_w(13,dnum) else dw=0	    ;Delete the large Dial plot.
		if dw gt 0 then begin ii=execute('wdelete,dw')  & geo_w(13,dnum)=0 & endif

	endif else if totim eq -2 then begin

		wset, geo_alp(3)
		device,copy=[0,0, geo_bxy(0),geo_bxy(1)/2, 0,W*25]
		xyouts,0,W*25+1, D ,font=-1,charsize=1.5,charthick=2,/device,color=255

	endif else if totim eq -3 then D=[did_x,did_y,did_wd]

	if keep_w gt 0 then wset,keep_w
end

function P_GEO_Ndials & return,20 & end
;******* ************

pro GEORGEO, init=init, construct=base, timer=bs1, freq=freq, command=intxt, nowin=nowin, lim=lim, duduch=prox
;** *******
;**
;** Construct the base then initiate george (lamp startup)
;** handle a command line for instrument control (from dons)

@lamp.cbk
common c_geo
common c_geweb

	if n_elements(intxt) eq 1 then begin
	   comm=strtrim(intxt,2)
	   if strpos(strupcase(comm),'CTRL:') eq 0 then begin intxt=';' & ln=strlen(comm)
	   		comm=strmid(comm,5,ln)
			typ='PAD' & gen=geo_par2(3)
			if (geo_par2(4) eq '1') then check=1 else check=0
			if strpos(comm,'G:')  eq 0 then begin	comm =strmid(comm,2,ln) & jj=strpos  (comm,':')
								gen  =strmid(comm,0,jj) & comm=strmid(comm,jj+1,ln)
								typ  ='WEB'        & endif
			if strpos(comm,'T:')  eq 0 then begin	comm =strmid(comm,2,ln) & jj=strpos  (comm,':')
								typ  =strmid(comm,0,jj) & comm=strmid(comm,jj+1,ln)
								endif
			if strpos(comm,'K:')  eq 0 then begin	comm =strmid(comm,2,ln) & jj=strpos  (comm,':')
								check=strmid(comm,0,jj) & comm=strmid(comm,jj+1,ln)
								on_ioerror,mischk  & check=float(check)
								mischk:            & endif
			comm=str_sep(comm, '<br>') & if n_elements(comm) eq 1 then comm=comm(0)

			d0={GENERIC:gen,NAME:geo_par2(0),TYPE:typ,PROS:long(geo_par2(5:6))} & R=0
			R = DialControl(comm,   d=0, check=check)
			if (R lt 0) or (R gt 1)    then begin print,'error-code '+string(R)
					if geo_web then WebDo,'err','error-code '+string(R) & ENDIF
	   endif
	   return
	endif else $
	if n_elements(base) eq 1 then begin
	   Pn     =P_GEO_Ndials()
	   geo_w  =lonarr(14,Pn+1)
	   geo_vis=Pn/2 -Pn/10
	   geo_bxy=[100,50]
	   if n_elements(geo_freq) eq 0 then begin geo_freq=0 & geo_lim=0 & endif
	   bid=widget_button(base,font=ft_biggest,value='....GET DIALS....',uvalue=[-88,380,0],resource_name="geo")
	   bas=widget_base (base,/column,y_scroll=fix(lamp_siz/2.1)<395)
	   for i=1,((size(geo_w))(2))-1 do begin
		di=strtrim(string(i),2)
		if sys_dep('VERSION') lt 4.0 then bod=widget_base(bas,/column) $
		else bod  =widget_base  (bas,/column,xsize=110)
		bud       =widget_base  (bod,/row)

		if i le geo_vis+4 then begin
	 	  byd     =widget_button(bud,value='d'+di                       ,font=ft_b_normal,menu=2,resource_name="geo")
			bed3   =widget_button(byd ,value='Large Dial'		,font=ft_normal,uvalue=[-88,670,5,i])
			bed1   =widget_button(byd ,value='Reset'		,font=ft_normal,uvalue=[-88,670,1,i])
			bed4   =widget_button(byd ,value='Properties'		,font=ft_normal,uvalue=[-88,670,6,i])
			bed8   =widget_button(byd ,value='Remove'		,font=ft_normal,uvalue=[-88,670,8,i])
			bed2   =widget_button(byd ,value='History is off'	,font=ft_normal,menu=2)
			  bed21=widget_button(bed2,value='Stop recording'	,font=ft_normal,uvalue=[-88,670,2,i])
			  bed22=widget_button(bed2,value='Start recording'	,font=ft_normal,uvalue=[-88,670,3,i])
			  bed23=widget_button(bed2,value='Clear History file'	,font=ft_normal,uvalue=[-88,670,4,i])
			if i le geo_vis  then begin
			bed5   =widget_button(byd ,value='Change Plot'		,font=ft_normal,menu=2)
			  bed51=widget_button(bed5,value='-2 (no plot)'         ,font=ft_normal,uvalue=[-88,671,-2 ,i,bed5])
			  bed51=widget_button(bed5,value='-1 (in W'+di+')'      ,font=ft_normal,uvalue=[-88,671,-1 ,i,bed5])
			  bed51=widget_button(bed5,value=' 0 (scalar,image)'    ,font=ft_normal,uvalue=[-88,671,0  ,i,bed5])
			  bed51=widget_button(bed5,value=' 1 (arrow,surface)'   ,font=ft_normal,uvalue=[-88,671,1  ,i,bed5])
			  bed51=widget_button(bed5,value=' 2 (contour)'         ,font=ft_normal,uvalue=[-88,671,2  ,i,bed5])
			  bed51=widget_button(bed5,value='10 (vector)'          ,font=ft_normal,uvalue=[-88,671,10 ,i,bed5])
			  bed51=widget_button(bed5,value='20 (  ..  )'          ,font=ft_normal,uvalue=[-88,671,20 ,i,bed5])
			  bed51=widget_button(bed5,value='30'                 ,font=ft_normal,uvalue=[-88,671,30 ,i,bed5])
			  bed51=widget_button(bed5,value='50'                 ,font=ft_normal,uvalue=[-88,671,50 ,i,bed5])
			  bed51=widget_button(bed5,value='100'                ,font=ft_normal,uvalue=[-88,671,100,i,bed5])
			  bed51=widget_button(bed5,value='200'                ,font=ft_normal,uvalue=[-88,671,200,i,bed5])
			  bed51=widget_button(bed5,value='500'                ,font=ft_normal,uvalue=[-88,671,500,i,bed5])
			bed6   =widget_button(byd ,value='Own Frequency'      ,font=ft_normal,menu=2)
			  bed61=widget_button(bed6,value='0.00'               ,font=ft_normal,uvalue=[-88,670,80,i,bed6])
			  bed61=widget_button(bed6,value=' .1 '               ,font=ft_normal,uvalue=[-88,670,81,i,bed6])
			  bed61=widget_button(bed6,value=' .5 '               ,font=ft_normal,uvalue=[-88,670,82,i,bed6])
			  bed61=widget_button(bed6,value='1   '               ,font=ft_normal,uvalue=[-88,670,83,i,bed6])
			  bed61=widget_button(bed6,value='2   '               ,font=ft_normal,uvalue=[-88,670,84,i,bed6])
			  bed61=widget_button(bed6,value='4   '               ,font=ft_normal,uvalue=[-88,670,85,i,bed6])
			  bed61=widget_button(bed6,value='10  '               ,font=ft_normal,uvalue=[-88,670,86,i,bed6])
			  bed61=widget_button(bed6,value='30  '               ,font=ft_normal,uvalue=[-88,670,87,i,bed6])
			  bed61=widget_button(bed6,value='60  '               ,font=ft_normal,uvalue=[-88,670,88,i,bed6])
			  bed61=widget_button(bed6,value='240 '               ,font=ft_normal,uvalue=[-88,670,89,i,bed6])
			  bed61=widget_button(bed6,value='600 '               ,font=ft_normal,uvalue=[-88,670,90,i,bed6])
			  bed61=widget_button(bed6,value='1800'               ,font=ft_normal,uvalue=[-88,670,91,i,bed6])
			  bed61=widget_button(bed6,value='3600'               ,font=ft_normal,uvalue=[-88,670,92,i,bed6])
			endif
		endif
		bidof=0
		if (i eq geo_vis+5) and (GEORGE eq 2) then begin
		 cap=0
		 eng=sys_dep('MACHINE') & if eng eq 'win' then cap=3 else if eng eq 'mac' then cap=-2

		 bs1_t =widget_label (bod,font=ft_b_normal,value ='General Freq.' , event_pro='P_GEO_TIMER')
		 bs1_f =widget_text  (bod,font=ft_propor  ,value=strtrim(string(geo_freq),2),xsize=4+cap,ysize=1,/editable  ,resource_name="geo")
		 bs1_1 =widget_button(widget_base(bod,/nonexclusive),font=ft_b_normal,value='on/off',resource_name="geo")
		 bid   =widget_label (bod,font=ft_b_normal,value='Duration:')
		 bs1_d =widget_text  (bod,font=ft_propor  ,value=strtrim(string(geo_lim ),2),xsize=4+cap,ysize=1,/editable  ,resource_name="geo")
		 bact  =widget_button(widget_base(bod,/nonexclusive),font=ft_b_normal,value='no plot,resource_name="geo"')
		 widget_control,bact ,set_uvalue=[-88,660,0,0],set_button=1
		 widget_control,bs1_1,set_uvalue=[-88,667,bs1_f,bs1_t]
		 widget_control,bs1_f,set_uvalue=[-88,666,bs1_f,bs1_t,bs1_d]
		 widget_control,bs1_1,set_uvalue=[-88,667,bs1_f,bs1_t,bs1_d],set_button=1
		 widget_control,bs1_d,set_uvalue=[-88,666,bs1_f,bs1_t,bs1_d]
		 geo_onbas=[bs1_f , bs1_1, bs1_d , bs1_t]
		endif
		if i le geo_vis+4 then $
		   bidof  =widget_button(widget_base(bud,/nonexclusive),uvalue=[-88,665,i],$
						     value='on/off',font=ft_smaller)

		bedou     =widget_label (bud,value="", event_pro='P_GEO_TIMER',uvalue=i)

		geo_w(2 ,i)=bud
		geo_w(3 ,i)=bidof
		geo_w(4 ,i)=byd	;not used at this time!
		geo_w(7 ,i)=bed2
		geo_w(11,i)=bedou
		widget_control,bud  ,sensitive=0
		if i le geo_vis then geo_w(0,i)=widget_draw (bod,retain=2,xsize=geo_bxy(0),ysize=geo_bxy(1),$
							     /button_events,uvalue=[-88,670,7,i])
	   endfor

	endif else $
	if n_elements(bs1)  eq 1 then begin
		eng=sys_dep('MACHINE')
		if eng eq 'win' then begin cap= 3 & scheme=' SCHEME'        & reffre=' frequency:' & dur='Duration:'
		endif           else $
		if eng eq 'mac' then begin cap=-2 & scheme='SCHEME'         & reffre='freq:'      & dur='for:'
		endif           else begin cap= 0 & scheme=' SCHEME'        & reffre='frequency:' & dur='Duration:' & endelse

		if n_elements(lim)  eq 1 then geo_lim =lim  else geo_lim =0
		if n_elements(freq) eq 1 then geo_freq=freq else geo_freq=0
		vaf=strtrim(string(geo_freq),2) & vaf =" 0 "  &  geo_freq=0
		vad=strtrim(string(geo_lim ),2) & if geo_lim le 0 then vad=" "
		geo_onbas=[0L,0L,0L,0L]
		if GEORGE eq 1 then begin
		 bs1_1 =widget_label (bs1,font=ft_biggest ,value = scheme)
		 bs1_t =widget_label (bs1,font=ft_b_normal,value = reffre , event_pro='P_GEO_TIMER')

		 bs1_f =widget_text  (bs1,font=ft_propor  ,value=vaf,xsize=5+cap,ysize=1,/editable  ,resource_name="geo")
		 bs1_1 =widget_label (bs1,font=ft_smallest,value='seconds')
		 bs1_1 =widget_button(widget_base(bs1,/nonexclusive),font=ft_b_normal,value='on/off',resource_name="geo")
		 bid   =widget_label (bs1,font=ft_normal  ,value='Duration:')
		 bs1_d =widget_text  (bs1,font=ft_propor  ,value=vad,xsize=5+cap,ysize=1,/editable  ,resource_name="geo")

		 widget_control,bs1_1,set_uvalue=[-88,667,bs1_f,bs1_t]
		 widget_control,bs1_f,set_uvalue=[-88,666,bs1_f,bs1_t,bs1_d]
		 widget_control,bs1_1,set_uvalue=[-88,667,bs1_f,bs1_t,bs1_d],set_button=1
		 widget_control,bs1_d,set_uvalue=[-88,666,bs1_f,bs1_t,bs1_d]

		 geo_onbas=[bs1_f , bs1_1, bs1_d , bs1_t]
		endif
	endif else $
	if n_elements(prox)  gt 1 then begin
		geo_par2=prox
		SetDuduch,"_send", geo_par2(3,0), prox
		geo_par2=[geo_par2,string(prox)]
	endif else $
	if keyword_set(init) then begin
		setcol,27
		geo_info =lonarr(6)
		P_GEO_DISPLAY, info,0 ,-3                      ;return [did_x,did_y,did_wd]
		geo_info(0)  = info(0) & geo_info(1)=info(1)   ;r=widget_info(did_wd,/geometry)
		geo_info(5)  = did_win0
		P    =geo_bxy(0)   & N=geo_bxy(1)/2
		for i=1,((size(geo_w))(2))-1 do   begin
		    if geo_w(0,i) gt  0  then begin
			   widget_control,bad_id=ii,geo_w(0,i),get_value=j
			   geo_w(1,i)=j
			   di=strtrim(string(i),2)
			   P_GEO_DISPLAY, 'd'+di, j ,-1
		    endif
		endfor
		geo_alp =lonarr(5)
		bid  =widget_base(map=0)
	        geo_alp(0)=widget_draw(bid,retain=2,xsize=2*P,ysize=2 *N)
	        geo_alp(1)=widget_draw(bid,retain=2,xsize=  P,ysize=2 *N)
	        geo_alp(2)=widget_draw(bid,retain=2,xsize=  P,ysize=2 *N)
	        geo_alp(3)=widget_draw(bid,retain=2,xsize=  P,ysize=21*N)
	        geo_alp(4)=widget_draw(bid,retain=2,xsize=geo_info(0)/2+30,ysize=geo_info(1)*2/3)
		widget_control,bid,group_leader=lamp_b1,/realize
		widget_control,geo_alp(0),get_value=j & geo_alp(0)=j
		widget_control,geo_alp(1),get_value=j & geo_alp(1)=j
		widget_control,geo_alp(2),get_value=j & geo_alp(2)=j
		widget_control,geo_alp(3),get_value=j & geo_alp(3)=j
		widget_control,geo_alp(4),get_value=j & geo_alp(4)=j
		bid=bindgen(P)*2+55
		bis=bytarr(P,N)  & for i=0,n-1,2 do bis(0,i)=bid
		bid=reverse(bid) & for i=1,n-2,2 do bis(0,i)=bid
		bis=bytscl (bis)
		for i=0,N-1 do bis(P-1-abs((N-1)/2-i)/2:P-1,i)=253*0

		P_GEO_DISPLAY, did_win0, bis ,0

		if GEORGE eq 1 then geo_act=0 else geo_act=1
		if GEORGE eq 1 then P_GEO_DISPLAY, did_win0, 1 ,0

		geo_stat=1 & geo_isw=1 & geo_cur=0 & geo_seq=.0D & geo_timon=dblarr(((size(geo_w))(2))*2)
		geo_lead=lamp_b1       & geo_web=0

		if dial_ini then begin
		 for i=1,((size(geo_w))(2))-1 do begin    di=strtrim(string(i),2)
		    named=''
		    ii=execute ('if n_tags(d'+di+') gt 1 then named=d'+di+'.NAME')
		    if named ne '' then begin
			 dnum =i
			 pth  ='' & DialTag,d=dnum, tag="PATH",get=pth
			 DialInit , named,  d=dnum, path=pth, /restore
		    endif
		 endfor
		endif  else dial_ini=1
	endif else $
	if keyword_set(nowin) then begin
		Pn       =P_GEO_Ndials()    ;
		geo_freq =0                 ;General Timer
		geo_timon=dblarr((Pn+1)*2)  ;Time on Timer start process
		geo_lim  =0                 ;General Timer limit
		geo_par2 =['','','','','','-1','0'] ;Contains "dial_pad_init" table
		dial_ini =1
		geo_stat =0                 ;General Timer is on or off
		geo_act  =0                 ;0:plot Activity  1:output strings
		geo_web  =0                 ;1:outputs for web mirror
		geo_alp  =[0L,0L,0L,0L,0L]  ;BaseId  for pixmap plotting
		geo_vis  =0                 ;Visible short Dials
		geo_w    =lonarr(14,Pn+1)   ;0:drawBase    1:drawId  2:sensBase  3:OnOffButton
		                            ;4:HiLightBase 5:previous active dial (geo_cur) 6:has a dial
		                            ;7:HistoryBase 8:time in Macro 9:time in Get 10:time in Send
		                            ;11:Specific timer base  12:in the general or spec loop(2,1)
		                            ;13:large drawId
		geo_info =lonarr(6)         ;0:draw.xsize  1:draw.ysize  4:last dial scheme  5:did_win0
		geo_bxy  =[0,0]             ;short drawBase size
		geo_isw  =0                 ;0 for noWindow
		geo_cur  =0                 ;Current active Dial
		geo_onbas=[0L,0L,0L,0L]     ;BaseId for text & on/off Timer button & limit & Timer
		geo_lead =lamp_b1           ;Group Leader
		geo_seq  =.0D               ;Current Time (double flt)
	endif
end
