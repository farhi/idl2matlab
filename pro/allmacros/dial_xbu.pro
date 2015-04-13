pro xbu
;** ***
FORWARD_FUNCTION DialControl, DialNewValue, DialOn

if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

;bygeorge,'xbu' ;UN-COMMENT when standalone use.
END

pro dial_xbu_macro, DD
;** **************
;**
common for_users, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

widget_control, DD.wbase,bad_id=ii,get_uvalue=basl				;**Get BASL
IF n_elements(basl) le 1 then begin
   IF xregistered('Dial_'+DD.name) eq 0 then xbu_window,DD $			;**Make GUI
   ELSE begin xbu_help,DD.number,ret  & DD.wbase=ret & ENDELSE & RETURN & ENDIF
   
cur=basl(0,1)									;**Cur line#
IF  basl(0,0) eq -1 then begin	basl(0,0)=-2					;**Canceled ???
		widget_control,DD.wbase,set_uvalue=basl
		IF cur gt 0 then begin
		  xbuline, line, cur, basl, casof
		  IF (casof eq 'CTRL') and (DD.generic ne 'lamp') then begin
				rr=DialControl('stop') & WAIT,1 & ENDIF		;**STOP !!!
		ENDIF
ENDIF
		
act=basl(0,0) & nxt=basl(0,2)
            
VV=DialNewValue(TYPE='status') & DialModValue,VV				;**Get  status

IF (act ne 0) and (cur gt 0) then begin IF VV ne 'Idle'  then begin
		txt='   '+VV+'{'+strtrim(string(cur),2)+'} '+DD.cmd
		widget_control,basl( 1 ,2),set_value=txt			;**Show status
		widget_control,basl(cur,1),set_value='   '+VV & endif else $	;**Show status
		widget_control,basl(cur,1),set_value='...done...'		;**Cmd  done
endif else begin
   if nxt gt 0 then begin txt='   Stopped{'+strtrim(string(nxt),2)+'} '		;**Tell stopped
		commd='' & xbuline, commd, nxt, basl
                widget_control,basl( 1 ,2),set_value=txt+commd
   endif  else  widget_control,basl( 1 ,2),set_value='   '+VV & endelse		;**Inactive

LL=DialNewValue(TYPE='log')							;**Get  Logs

IF LL(0) le ' ' then DialModValue,DD.cmd+'['+VV+']' $				;**Show logs
ELSE begin DialModValue,LL  & LL=[DD.log,LL]  & nl=n_elements(LL)
	   if cur eq 1 then mx=15 else mx=100
           if nl gt mx then LL=LL(nl-mx-1:nl-1)   & DialModValue,LL,TAG='log'
           widget_control,basl(9,2),set_value=LL & nl=n_elements(LL)
           widget_control,basl(9,2),set_text_top_line=nl-16 & ENDELSE

IF  DD.lines ne (size(basl))(1)-1 then XBU_MORE, DD.lines ,basl			;**More lines

IF  VV ne 'Idle' then RETURN							;**Running

IF  act eq -2   then begin basl(0,0)=0 & act=0 & xbu_sensitive,basl,1		;**Just stopped
                           basl(0,1)=nxt
                           widget_control,DD.wbase,set_uvalue=basl  & ENDIF

IF  act eq 0    then IF DD.generic eq 'lamp' then DD.onoff=0			;**Stopped
IF  act eq 0    then begin DD.frequency=3 &  RETURN				;**Stopped
		ENDIF ELSE DD.frequency=DD.Xfreq

IF  nxt gt 0    then begin cur=nxt-1 & basl(0,2)=0                  & ENDIF	;**Resume

command=''
WHILE (cur le DD.lines) and (command eq '') do begin  cur=cur+1			;**Next Cmd
    if cur le DD.lines then xbuline, command, cur, basl, casof
ENDWHILE

IF (cur gt 0) and (cur le DD.lines) then begin Lo=cur

  CATCH,stat & IF stat ne 0 then begin xbuErrors, basl,cur,DD      & RETURN  & ENDIF

  !Error=0
  DD.cmd=command
  widget_control,basl( 1 ,2),set_value='   Active{'+strtrim(string(cur),2)+'} '+DD.cmd
  widget_control,basl(cur,1),set_value= '<'  & txt=' '
  CASE casof of
  'IF':      begin coco='o='+strmid(command,2,100)
  		idx=strpos(strlowcase(coco),' then')
		IF idx gt 0 then coco=strmid(coco,0,idx)
  		ii=EXECUTE(coco)
		IF !Error ne 0 then begin xbuErrors, basl,cur,DD   & RETURN  & ENDIF
		ok=0
		IF not o then widget_control,basl(cur,1),set_value= 'No'
		IF not o then $
		   WHILE (Lo le DD.lines) and (casof ne 'ENDIF') do begin
		    Lo=Lo+1
		    if Lo le DD.lines then xbuline,command,Lo,basl,casof
		    IF casof eq 'IF' then ok=ok+1
		    IF casof eq 'ENDIF' then IF ok gt 0 then begin casof='' & ok=ok-1 & ENDIF
		   ENDWHILE
		IF Lo gt DD.lines then begin xbuErrors, basl,cur,DD,'No ENDIF!'
		                             RETURN  & ENDIF
		cur=Lo & END

  'IDL':     begin idx=1
  		WHILE idx gt 0 do   begin idx=strpos(command,'$')
  		   IF idx gt 0 then begin var=''
  		    ii=execute('var=strtrim(string( '+strmid(command,idx+1,1)+'),2)')
  		    command=strmid(command,0,idx)+var+strmid(command,idx+2,100)
  		   ENDIF
		ENDWHILE
		widget_control,basl(cur,1),set_value= 'Executing'			;**Execute Idl
  		IF DD.lampok then XICUTE,command else ii=EXECUTE(command)		;*************
		IF !Error ne 0 then begin xbuErrors, basl,cur,DD   & RETURN  & ENDIF
		txt='Executed' & END

  'CTRL':      if strpos(command,'! ') ne 0 then begin
  		idx=1
  		WHILE idx ge 0 do   begin idx=strpos(command,'$')
  		   IF idx ge 0 then begin var=''
  		    ii=execute('var=strtrim(string( '+strmid(command,idx+1,1)+'),2)')
  		    command=strmid(command,0,idx)+var+strmid(command,idx+2,100)
  		   ENDIF
		ENDWHILE
  		rr= DialControl(command)						;**Execute Mad
		if DD.generic eq 'lamp' then txt='Executed'  else txt='Sent' & ENDIF	;*************

  'ENDFOR':  begin ok=0
		WHILE (Lo gt 0) and (casof ne 'FOR') do begin
		   Lo=Lo-1 & if Lo ge 1 then xbuline,command,Lo,basl,casof
		   IF casof eq 'ENDFOR' then ok=ok+1
		   IF casof eq 'FOR' then IF ok gt 0 then begin casof=''    & ok=ok-1 & ENDIF
		ENDWHILE
		IF Lo lt 1 then begin xbuErrors, basl,cur,DD,'No FOR!'      & RETURN  & ENDIF
		END

  'ENDWHILE':begin ok=0
		WHILE (Lo gt 0) and (casof ne 'WHILE') do begin
		   Lo=Lo-1 & if Lo ge 1 then xbuline,command,Lo,basl,casof
		   IF casof eq 'ENDWHILE' then ok=ok+1
		   IF casof eq 'WHILE' then IF ok gt 0 then begin casof=''  & ok=ok-1 & ENDIF
		ENDWHILE
		IF Lo lt 1 then begin xbuErrors, basl,cur,DD,'No WHILE!'    & RETURN  & ENDIF
		END

  'STOP':    begin widget_control,basl(cur,3),set_value='<<-'
		   basl(0,0)=-2 & basl(0  ,2)=cur & END
   ELSE:
  ENDCASE
  
  CASE casof of
  'FOR':begin good=0									;**For loop
	bid=strmid(command,3,100)
	je =strpos(bid,'=')
	IF je gt 0 then begin a1=0. & a2=-1. & a3=1.
	 ;**GET LOOP VARIABLES IN A1 A2 A3**
	  jd =strpos (strlowcase(bid),' do') & if jd lt je then jd=100
	  var=strupcase(strtrim(strmid(bid,0,je),2))
	  bid=strmid (bid,je+1,jd-je-1)
	  tab=str_sep(bid+',1',',')
	  ii=Execute ('a1='+tab(0)+' & a2='+tab(1)+' & a3='+tab(2))
	  on_ioerror,misloop   & a1=float(a1) & a2=float(a2)  & a3=float(a3)
	  if long(a1) eq a1 then a1=long (a1) & if long (a3) eq a3 then  a3=long(a3)
	 ;**GET NOW STEP IN A5**
	  jn =strpos (command,';NOW=') & txt=' '
	  IF  jn  lt 0  then jn = strlen (command)
	  IF  cur ne Lo then ii = Execute(var+'=a3+'+var) $
	  		else ii = Execute(var+'=a1')
	  ii =Execute('a5='+var)
	  IF long(a5) eq a5 then a5=long(a5)
	 ;**TEST END OF LOOP**
	  IF a3 gt 0  then if a5 gt a2 then txt='Loop complete'
	  IF a3 lt 0  then if a5 lt a2 then txt='Loop complete'
	 ;**LOOP IS OK?
	  cur=Lo  & ok=0
	  IF txt ne ' ' then begin now=''
	    WHILE (cur le DD.lines) and (casof ne 'ENDFOR') do begin
		cur=cur+1 & if cur le DD.lines then xbuline,command,cur,basl,casof
		IF casof eq 'FOR' then ok=ok+1
		IF casof eq 'ENDFOR' then IF ok gt 0 then begin casof='' & ok=ok-1 & ENDIF
	    ENDWHILE
	  ENDIF else now=';NOW='+strtrim(string(a5),2)
	  newcmd='FOR ' +var+'='+strmid(bid,0,jn)+' do begin '+now
	  widget_control,basl(Lo,0),set_value=newcmd
	  good=1
	  misloop:IF not good then begin xbuErrors, basl,cur,DD,' '  & RETURN &  ENDIF
	ENDIF & END

  'WHILE':begin	txt=' ' & coco='o='+strmid(command,5,100)				;**While loop
  	  idx=strpos(strlowcase(coco),' do') & IF idx gt 0 then coco=strmid(coco,0,idx)
	  ii=EXECUTE(coco)
	  IF !Error ne 0 then begin xbuErrors, basl,cur,DD & RETURN &  ENDIF
	  cur=Lo & ok=0
	  IF not o then begin txt='Loop complete'
	    WHILE (cur le DD.lines) and (casof ne 'ENDWHILE') do begin
		cur=cur+1 & if cur le DD.lines then xbuline,command,cur,basl,casof
		IF casof eq 'WHILE' then ok=ok+1
		IF casof eq 'ENDWHILE' then IF ok gt 0 then begin casof='' & ok=ok-1 & ENDIF
	    ENDWHILE
  	  ENDIF & END
   ELSE:
  ENDCASE
  widget_control,basl(cur,1),set_value= txt
ENDIF

IF cur gt DD.lines then begin xbu_sensitive, basl,1 & basl(0,*)=0 & cur=0		;**All done
	command='done'
	widget_control,basl(1,1),set_value='...All is done...'
ENDIF

DD.cmd=command
basl(0,1)=cur & widget_control,DD.wbase,set_uvalue=basl
end

pro xbuComp, new_macro, bat
;** *******
;**
	resol =0
	if float(!version.release) ge 4.0 then resol=2 else resol=1
	poc='lamp_tmp.pro'
	pac='lamp_tmp'
	if resol gt 0 then begin out1=-1
	   ON_IOERROR,mis_cmp
	   OPENW ,out1,poc,/get_lun
	   if bat ne '.pro' then PRINTF,out1,'pro '+pac
	   for i=0,n_elements(new_macro)-1 do PRINTF,out1,new_macro(i)
	   PRINTF,out1,''
	   if bat ne '.pro' then PRINTF,out1,'end' $
	   else begin	PRINTF,out1,'pro '+pac & PRINTF,out1,'end'
			endelse
	   PRINTF  ,out1,''
 	   FREE_LUN,out1 & out1=-1

	  if resol eq 1 then iii=EXECUTE   ( pac )
	  if resol eq 2 then RESOLVE_ROUTINE,pac

	  mis_cmp:if out1 gt 0 then free_lun,out2
	 ;OPENR ,out1,poc,/get_lun,/DELETE & FREE_LUN,out1 & out1=-1
	endif
end


pro xbuErrors, basl,cur,DD, error
;** *********
;**
basl(0,0)=0  & DD.onoff=0
basl(0,1)=cur
basl(0,2)=cur
IF n_elements(error) eq 0 then error=strmid(!err_string,0,52)
widget_control,DD.wbase   ,set_uvalue=basl
widget_control,basl(cur,1),set_value ='<-Error!'
widget_control,basl(1,2)  ,set_value =   error
widget_control,basl(cur,3),set_value ='<<-'
xbu_sensitive,basl,1
end

pro xbuline, line, cur, basl, casof
;** *******
;**
  widget_control,basl(cur,0),get_value =comd
  ctrl=basl(cur,5)
  line=strtrim  (comd(0),2)
  COMD=strupcase(line)
  IF ctrl   then casof='CTRL'  else casof='IDL'
  if strpos(COMD,'! ')    eq 0 then casof='CTRL'
  IF strpos(COMD,'CTRL:') eq 0 then begin casof='CTRL'
  	line=strmid (line,5,100)
  	line=strtrim(line,2) & COMD=strupcase(line) & endif else $
  	
  IF strpos(COMD,'IDL:')  eq 0 then begin casof='IDL'
  	line=strmid (line,4,100)
  	line=strtrim(line,2) & COMD=strupcase(line) & ENDIF
  
  IF casof eq 'IDL' then begin
	if (strpos(COMD,'IF(') eq 0) or (strpos(COMD,'IF ') eq 0) then casof='IF' else $
	if (strpos(COMD,'ENDIF')    eq 0) then casof='ENDIF'    else $
	if (strpos(COMD,'FOR ')     eq 0) then casof='FOR'      else $
	if (strpos(COMD,'ENDFOR')   eq 0) then casof='ENDFOR'   else $
	if (strpos(COMD,'WHILE')    eq 0) then casof='WHILE'    else $
	if (strpos(COMD,'ENDWHILE') eq 0) then casof='ENDWHILE' else $
	if (strpos(COMD,'STOP')     eq 0) then casof='STOP'
  ENDIF
end

pro xbucontrol  ,flag ,LINES=L ,COMMAND=command ,FILE=fifi ,NAME=xbu ,ctrl=ctrl
;** **********
;**

IF n_elements(xbu) ne 1 then xbu='XBU'
dialTag,xbu ,tag='WBASE',get=top

CASE strlowcase(flag) of

'read':		if (n_elements(fifi) eq 1) then $
		xbu_event, {TOP:top,ID:0,UV:[2 ,0]} ,FILE=fifi
		
'write':	if (n_elements(fifi) eq 1) then $
		xbu_event, {TOP:top,ID:0,UV:[3 ,0]} ,FILE=fifi
		
'start':	xbu_event, {TOP:top,ID:0,UV:[4 ,0]}
'break':	xbu_event, {TOP:top,ID:0,UV:[5 ,0]}
'resume':	xbu_event, {TOP:top,ID:0,UV:[6 ,0]}
'stop':		xbu_event, {TOP:top,ID:0,UV:[7 ,0]}
'back':		xbu_event, {TOP:top,ID:0,UV:[8 ,0]}
'next':		xbu_event, {TOP:top,ID:0,UV:[9 ,0]}
'erase':	xbu_event, {TOP:top,ID:0,UV:[10,0]}

'put':		if (n_elements(L) eq 1) and (n_elements(command) eq 1) then begin
		if n_elements(ctrl) ne 1 then ctrl=0
		xbu_event, {TOP:top,ID:0,UV:[11,0,L,ctrl]} ,COMMAND=command & endif
		
'show':		xbu_event, {TOP:top,ID:0,UV:[12,0]}

'more':		begin if (n_elements(L) ne 1) then L=1
		xbu_event, {TOP:top,ID:0,UV:[14,0,L]} & end

'map':		widget_control,top, bad_id=ii,map=1
'nomap':	widget_control,top, bad_id=ii,map=0

ELSE:
ENDCASE
end

pro xbu_DYING, id
;** *********
;**
widget_control, id, get_uvalue=basl
DialStop ,D=basl(13,2)
DialClear,D=basl(13,2)
end

pro xbu_event, ev, command=command ,file=fifi
;** *********
;**
widget_control, ev.top, get_uvalue=basl & sz=(size(basl))(1)
IF ev.id gt 0 then widget_control, ev.id, get_uvalue=uv ELSE uv=ev.uv
IF n_elements(uv) lt 2 then RETURN
uv(1)=basl(1,2) & widget_control, uv(1) , set_value =string(replicate(byte(32),52))

dnum=basl(13,2)

CASE uv(0) of
0:begin										;**toggle Log/Xbu
	if uv(4) eq 0 then begin widget_control, uv(2),map=0
	                         widget_control, uv(3),map=1
	endif else         begin widget_control, uv(3),map=0
	                         widget_control, uv(2),map=1 & endelse
	uv(4)=abs(uv(4)-1)   &   widget_control, ev.id,set_uvalue=uv
	end
1:	basl(uv(2),5)=ev.select							;**toggle Ctrl/Idl

2:begin	if n_elements(fifi) ne 1 then widget_control, uv(2), get_value=fifi	;**read XBU file
	fifi=strtrim(fifi(0),2) & on_ioerror,misread & u=-1 & tline=''
	if fifi ne '' then begin
	 openr,u,fifi,/get_lun  & line=''
	 while (not EOF(u)) do begin readf,u,line & tline=[tline,line] & endwhile
	 free_lun,u
	endif
	nel=n_elements(tline)
	XBU_MORE, nel ,basl  &   sz=(size(basl))(1)
	if strpos(fifi,'.cmd') gt 0  then bstat=1 else bstat=0
	for i=1,sz-1 do begin  basl(i,5)= bstat
		if basl(i,4) gt 0 then widget_control, basl(i,4), set_button=bstat
	  if i ge nel then widget_control , basl(i,0), set_value=' ' $
	  else begin  line=tline(i)
		widget_control,  basl(i,0), set_value=line
		xbuline,  linc, (i), basl, casof
		if casof eq 'CTRL' then begin
		  widget_control,basl(i,0), set_value=strmid (line,5,100) & basl(i,5)=1
		  if basl(i,4) gt 0 then widget_control,basl(i,4),set_button=1
		endif
	  endelse
	  widget_control, basl(i,1), set_value=string(replicate(byte(32),15))
	  widget_control, basl(i,3), set_value=string(replicate(byte(32),3 ))
	endfor
	widget_control, uv(1),  set_value =fifi+' is loaded'
	xbu_sensitive, basl,1 & basl(0,*)=0
	misread:
	if u lt 0 then widget_control,uv(1),set_value ='!!! un-readable '+fifi
	end
3:begin	if n_elements(fifi) ne 1 then widget_control, uv(2), get_value=fifi	;**write XBU file
	fifi=fifi(0) & on_ioerror,miswrite & tline='' & u=-1 & ttick=0
	for i=1,sz-1 do begin
	    widget_control, basl(i,0), get_value =line & line=line(0) & linv=strtrim(line,2)
	    bid =  basl(i,5)
	    if linv ne '' then if bid then line='CTRL:'+line
	    if linv ne '' then tline=[tline,line]
	endfor
	if  n_elements(tline) gt 1 then begin
	    openw,u,fifi,/get_lun
	    for i=1,n_elements(tline)-1 do printf,u,tline(i)
	    free_lun,u
	    widget_control, uv(1), set_value =fifi+' is saved'
	endif
	miswrite:
	if u lt 0 then widget_control,uv(1),set_value ='!!! un-writeable '+fifi
	end
4:if basl(0,0) eq 0 then begin							;**Start
	xbu_sensitive,basl,0 & basl(0,*)=0 & basl(0,0)=1
	for i=1,sz-1  do begin widget_control,basl(i,1),set_value=' '
	              widget_control,basl(i,3),set_value=' ' & endfor & endif

5:if basl(0,0) eq 1 then begin
	basl(0,0)=-2 & nxt=basl(0,2) & basl(0,2)=(basl(0,1)+1)<(sz-1)		;**Stop next
	if nxt gt 0 then widget_control,basl(nxt,3),set_value=' '
	widget_control,basl(basl(0,2),3),set_value='<<-'              & endif

6:if basl(0,0) eq 0 then begin nxt=basl(0,2)					;**Resume
	if nxt gt 0 then begin xbu_sensitive,basl,0 & basl(0,0)=1     & endif
	if nxt gt 0 then widget_control,basl(nxt,3),set_value=' '     & endif

7:if basl(0,0) eq 1 then begin							;**Cancel
	basl(0,0)=-1 & nxt=basl(0,2) & basl(0,2)=basl(0,1)>1
	if nxt gt 0 then widget_control,basl(nxt,3),set_value=' '
	widget_control,basl(basl(0,2),3),set_value='<<-'              & endif

8:begin if basl(0,2) eq 0 then basl(0,2)=basl(0,1)				;**Previous
	widget_control,basl(basl(0,2)>1,3),set_value=' '
	basl(0,2)=(basl(0,2)-1)>1
	if basl(0,0) eq 0 then txt='<<-' else txt='Brk'
	widget_control,basl(basl(0,2),3),set_value= txt               & end

9:begin if basl(0,2) eq 0 then basl(0,2)=basl(0,1) & if basl(0,2) gt 0 then $	;**Next
	widget_control,basl(basl(0,2)>1,3),set_value=' '
	basl(0,2)=(basl(0,2)+1)<(sz-1)
	if basl(0,0) eq 0 then txt='<<-' else txt='Brk'
	widget_control,basl(basl(0,2),3),set_value= txt               & end

10:for i=1,sz-1 do begin widget_control, basl(i,0), set_value=' ' & basl(0,*)=0	;**Erase
			 widget_control, basl(i,1), set_value=' '
			 widget_control, basl(i,3), set_value=' '     & endfor

11:begin i=uv(2)>1<(sz-1)
	 widget_control, basl(i,0),set_value=command & basl(i,5)=uv(3)		;**Put
	 if basl(i,4) gt 0 then widget_control,basl(i,4),set_button=uv(3)
   end
12:										;**Show !!!!!

13:begin tline=[''] & line=''							;**Check
	for i=1,sz-1 do begin
	    widget_control, basl(i,0), get_value =line & line=line(0)
	    bid =  basl(i,5)
	    if bid then    line=';;;'+line
	    if line gt ' ' then tline=[tline,line]
	endfor
	!Error=0 & xbuComp, tline ,'.xbu'
	IF !Error ne 0 then  widget_control,uv(1),set_value =!err_string
	IF !Error ne 0 then  txt='Found error !!!' else txt='Seems ok !!!'
	widget_control,basl(1,1),set_value =txt
	END

14:	XBU_MORE, sz-1+uv(2) ,basl						;**More

15:	IF uv(2) eq 1 then DialTag,D=dnum,tag='XFREQ',set=.1 else $		;**Speed
	IF uv(2) eq 2 then DialTag,D=dnum,tag='XFREQ',set=.8 else $
	IF uv(2) eq 3 then DialTag,D=dnum,tag='XFREQ',set=1.5

16:	XBU_SHIFT ,basl, ev.value						;**Slider

17:     XBU_INSERT,basl, uv(2)							;**Inset new line
else:
ENDCASE
widget_control, ev.top, set_uvalue=basl
IF basl(0,0) eq 1 then begin DialTag,D=dnum,tag='ONOFF',get=onoff
                             if onoff eq 0 then DialStart,D=dnum & ENDIF
end

pro xbu_sensitive, basl,a
;** *************
;**
b=abs(a-1)
	widget_control, basl(2,2), sensitive=a ;start
	widget_control, basl(3,2), sensitive=a ;read
	widget_control, basl(4,2), sensitive=b ;stop nx
	widget_control, basl(5,2), sensitive=a ;resume
	widget_control, basl(6,2), sensitive=b ;cancel
	widget_control, basl(7,2), sensitive=a ;prev
	widget_control, basl(8,2), sensitive=a ;next
end

pro xbu_help, dnum,ret
;** ********
;**
common xbu_kp,basec0
ret=basec0(dnum)
end

pro xbu_window,DD
;** **********
;**
@lamp.cbk
common xbu_kp,basec0

if xregistered('Dial_'+DD.name) eq 0 then begin DD.lines=DD.lines>14
   lampok=n_elements(lamp_b1)
   DialModValue,lampok,tag='LAMPOK'
   if n_elements(basec0)  eq 0 then basec0=lonarr(21)
   basl=lonarr(DD.lines+1,6)
   basec0_0=widget_base  (title='Dial_'+DD.name,/column ,resource_name='lamp',kill_notify='xbu_DYING')
   basec0(DD.number)=basec0_0
   basecf1 =widget_base  (basec0_0 ,/row,/frame,resource_name='don')
   basecmap=widget_base  (basec0_0 ,/frame,resource_name='did')
   basec2  =widget_base  (basecmap ,map=1,/row)
   basecf2g=widget_base  (basecmap ,map=0)
   basecf2s=widget_slider(basec2   ,xsize=15,ysize=350,max=1,min=DD.lines,/vertical,/drag,value=1,/suppress_value,title='')
   basecf2 =widget_base  (basec2   ,/column)
   baselog =widget_text  (basecf2g ,xsize=60,ysize=18 ,font=ft_b_normal,/scroll)
   basecf3 =widget_base  (basec0_0 ,/frame,resource_name='don')
   
   if DD.lampok then put_logo,basecf1
   bid     =widget_label (basecf1  ,value='  ')
   baselogx=widget_button(basecf1  ,value='Log/'+DD.name  ,font=ft_b_normal)
   bid     =widget_label (basecf1  ,value='  ')
   bid     =widget_label (basecf1  ,value=DD.name+' file:',font=ft_b_normal)
   basefil =widget_text  (basecf1  ,xsize=15,ysize=1,/editable, font=ft_propor)
   baseread=widget_button(basecf1  ,value='Read'     ,font=ft_b_normal)
   basewrit=widget_button(basecf1  ,value='Write'    ,font=ft_b_normal)
   bid     =widget_button(basecf1  ,value='Help'     ,font=ft_b_normal,menu=2)
     bid1 =widget_button(bid       ,value='Use only variables A->Z and Workspaces'             ,font=ft_b_normal)
     bid1 =widget_button(bid       ,value='Prefix A->Z with "$" for substitutions'      ,font=ft_b_normal)
     bid1 =widget_button(bid       ,value='Tick left for Instrument control lines'      ,font=ft_b_normal)
     bid1 =widget_button(bid       ,value='WHILE  command'				,font=ft_b_normal,menu=2)
      bid2=widget_button(bid1      ,value='- 1  K=55. & T=800 & N=10'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='* 2  Set temperature $K'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 3  R=my_temp_macro()'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 4  WHILE (T eq R) and (N gt 0) do begin'	, font=ft_propor)
      bid2=widget_button(bid1      ,value='* 5      Count $T seconds'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 6      R=my_temp_macro()'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 7      N=N-1'				, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 8  ENDWHILE'				, font=ft_propor)
     bid1 =widget_button(bid       ,value='FOR  command'				,font=ft_b_normal,menu=2)
      bid2=widget_button(bid1      ,value='- 1  N=10'					, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 2  FOR J=N,0,-2 do begin'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='* 3      Set position $J'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='* 4      Count 800 seconds'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 5  ENDFOR'					, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 6  print,"End"'                             , font=ft_propor)
     bid1 =widget_button(bid       ,value='IF  command'					,font=ft_b_normal,menu=2)
      bid2=widget_button(bid1      ,value='- 1  IF (K ne 55.) then begin'		, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 2      print,"on pompe!"'			, font=ft_propor)
      bid2=widget_button(bid1      ,value='* 3      Set temperature 55.0'		, font=ft_propor)
      bid2=widget_button(bid1      ,value='- 4  ENDIF'					, font=ft_propor)
     bid1 =widget_button(bid       ,value='STOP command does a break'                   ,font=ft_b_normal)
     bid1 =widget_button(bid       ,value='------'					, font=ft_propor)
     check=widget_button(bid       ,value='Check for syntax now!'			, font=ft_propor)
     bid1 =widget_button(bid       ,value='------'					, font=ft_propor)
     bid1 =widget_button(bid       ,value='To insert an empty line, type <Enter>'       ,font=ft_b_normal)
     bid1 =widget_button(bid       ,value='To delete a line, erase and type <Enter>'    ,font=ft_b_normal)
     bid1 =widget_button(bid       ,value='------'					, font=ft_propor)
      run1=widget_button(bid       ,value='Run fast'					, font=ft_propor)
      run2=widget_button(bid       ,value='Run medium'					, font=ft_propor)
      run3=widget_button(bid       ,value='Run slow'					, font=ft_propor)
   
   basec2_0=widget_base  (basecf2  ,/row)
   basestar=widget_button(basec2_0 ,value='Start',font=ft_b_normal)
   basemess=widget_label (basec2_0 ,font=ft_propor,$
   	                  value=string(replicate(byte(45),52)))
   
   basec3_0=widget_base  (basecf3  ,/row)
   basestop=widget_button(basec3_0 ,value='Break'    ,font=ft_b_normal)
   bid     =widget_label (basecf1  ,value=' '        ,font=ft_propor)
   baseresu=widget_button(basec3_0 ,value='Resume'   ,font=ft_b_normal)
   bid     =widget_label (basecf1  ,value=' '        ,font=ft_propor)
   b1      =widget_button(basec3_0 ,value='<-'       ,font=ft_b_normal)
   b2      =widget_button(basec3_0 ,value='->'       ,font=ft_b_normal)
   bid     =widget_label (basec3_0 ,value='         ',font=ft_propor)
   basecanc=widget_button(basec3_0 ,value='!Stop-Instrument'    ,font=ft_b_normal)
   
   more=-basecf3
   basl(1:14,2)=[basemess,basestar,baseread,basestop,baseresu,basecanc,b1,b2, $
                 baselog ,basecf2 ,more    ,basec0_0,DD.number,basecf2s]
   XBU_MORE, DD.lines ,basl

   widget_control, basec0_0, set_uvalue=basl
   widget_control, baselogx, set_uvalue=[0 ,basemess,basec2,basecf2g,0]
   widget_control, basefil , set_uvalue=[2 ,basemess,basefil]
   widget_control, baseread, set_uvalue=[2 ,basemess,basefil]
   widget_control, basewrit, set_uvalue=[3 ,basemess,basefil]
   widget_control, basestar, set_uvalue=[4 ,basemess]
   widget_control, basestop, set_uvalue=[5 ,basemess]
   widget_control, baseresu, set_uvalue=[6 ,basemess]
   widget_control, basecanc, set_uvalue=[7 ,basemess]
   widget_control, b1      , set_uvalue=[8 ,basemess]
   widget_control, b2      , set_uvalue=[9 ,basemess]
   widget_control, check   , set_uvalue=[13,basemess]
   widget_control, run1    , set_uvalue=[15,basemess,1]
   widget_control, run2    , set_uvalue=[15,basemess,2]
   widget_control, run3    , set_uvalue=[15,basemess,3]
   widget_control, basecf2s, set_uvalue=[16,basemess,1]
   widget_control, basestop, sensitive = 0
   widget_control, baseresu, sensitive = 0
   widget_control, basecanc, sensitive = 0
   widget_control, b1      , sensitive = 0
   widget_control, b2      , sensitive = 0
   
   res=0 & ii=execute('res=LMGR(/EMBEDDED)<1 + LMGR(/RUNTIME)<1 ')
   if res ne 0 then widget_control,check,sensitive=0
   
   DialModValue,basec0_0,tag='WBASE'

   if DD.nowin eq 1 then widget_control, basec0_0, map=0
   widget_control, basec0_0, group_leader=lamp_b1, /realize  & if DD.lampok then put_logo
   widget_control, basemess, set_value= DD.value
   XMANAGER,'Dial_'+DD.name , basec0_0, event_handler='xbu_event',/just_reg

endif
end

pro xbu_more, nb2 ,basl
;** ********
;**
@lamp.cbk

   basemess= basl(1 ,2) & basecf2= basl(10,2) & more= basl(11,2) & basec0_0=basl(12,2)
   nb=(size(basl))(1)-1
   DialTag,D=basl(13,2),tag='LINES',set=nb2>nb
   if(nb ge  nb2) and (more gt 0) then RETURN
   if nb lt  nb2 then begin mo=nb2-nb & basl=[[basl],lonarr(mo,6)]
   			    widget_control,basl(14,2),set_slider_max=1,set_slider_min=nb2
   endif
   visible=8
   nb1= nb+1
   if more lt 0 then begin nb1=1 & more=abs(more) & basl(11,2)=more & endif
   if nb1  gt 1 then begin widget_control,/hourglass
                     IF !version.release gt '4.0' THEN widget_control, basec0_0, update=0
   endif
   rep15=string(replicate(byte(45),15))
   rep3 =string(replicate(byte(45),3 ))
   for i=nb1,nb2  do  begin
	if i le visible then begin
   	   si=strtrim(string(i),2) & if i le 9 then si='0'+si & if i le 99 then si='0'+si
	   bid      =widget_base  (basecf2,/row)
	   bod      =widget_base  (bid,/nonexclusive)
	   basl(i,4)=widget_button(bod,value=si,font=ft_propor          ,uvalue=[1 ,basemess,i])
	   basl(i,0)=widget_text  (bid,xsize=31,/editable,font=ft_propor,uvalue=[17,basemess,i])
	   basl(i,1)=widget_label (bid,value=rep15,font=ft_propor,uvalue=bid)
	   basl(i,3)=widget_label (bid,value=rep3 ,font=ft_propor)
	endif else begin
	   bid      =widget_base(more,/row,map=0)
	   basl(i,0)=widget_text  (bid,xsize=31,/editable,font=ft_propor)
	   basl(i,1)=widget_label (bid,value=rep15,font=ft_propor,uvalue=bid)
	   basl(i,3)=widget_label (bid,value=rep3 ,font=ft_propor) & endelse
   endfor
   if nb1 eq 1 then begin bosl=lonarr(visible,4) ;Extras for keeping
      for i=0,visible-1 do begin
           bid      =widget_base(more,/row,map=0)
	   bod      =widget_base  (bid,/nonexclusive)
	   bosl(i,3)=widget_button(bod,value='000',font=ft_propor,uvalue=[1,basemess,i+1])
	   bosl(i,0)=widget_text  (bid,xsize=31,/editable,font=ft_propor)
	   bosl(i,1)=widget_label (bid,value=rep15,font=ft_propor,uvalue=bid)
	   bosl(i,2)=widget_label (bid,value=rep3 ,font=ft_propor)
      endfor
      widget_control,more,set_uvalue={id:bosl,vis:visible}
   endif else IF !version.release gt '4.0' THEN widget_control, basec0_0, update=1
   widget_control, basec0_0, set_uvalue=basl
end

pro xbu_insert,basl, cur
;** **********
;**

nb=(size(basl))(1)-1
widget_control,basl(cur,0),get_value=txt  & txt=strtrim(txt(0),2)
widget_control,basl(11,2) ,get_uvalue=kp  & vis=kp.vis & go=nb-vis+1

if txt gt ' ' then begin          ;Insert a new line
	widget_control,/hourglass
	widget_control,basl(nb  ,0),get_value=t1
	widget_control,basl(nb-1,0),get_value=t2
	widget_control,/hourglass
	if cur gt go then XBU_SHIFT,basl,go
	if strcompress(t1(0)+t2(0)) gt ' ' then XBU_MORE ,nb+1,basl
	XBU_SHIFT,basl,cur, decal= 1
	if cur ge nb-1 then begin
	   widget_control,basl(14,2),get_uvalue=sluv
	   XBU_SHIFT,basl,sluv(2)+1 & endif

endif else if nb gt 20 then begin ;Suppress an empty line
	XBU_SHIFT,basl,cur, decal=-1
endif
end

pro xbu_shift, basl,to, decal=decal
;** *********
;**
nb =(size(basl))(1)-1
 widget_control,basl(11,2),get_uvalue=kp   & visible=kp.vis

;DECAL AFTER LINE INSERT
;***********************
if n_elements(decal) eq 1 then begin cur=to
 if decal gt 0 then begin
 	for i=nb,cur+2,-1 do begin
 	  widget_control,basl(i-1  ,0), get_value=tmp
 	  widget_control,basl(i    ,0), set_value=tmp(0)
 	  basl(i,5)=basl(i-1,5)
	  if basl(i,4) gt 0 then widget_control,basl(i,4),set_button=basl(i,5)
 	endfor
 	  widget_control,basl(cur+1,0), set_value=''  &  basl(cur+1,5)=0
	  if basl(cur+1,4) gt 0 then widget_control,basl(cur+1,4),set_button=0
 endif else begin
 	for i=cur,nb-1    do begin
 	  widget_control,basl(i+1  ,0), get_value=tmp
 	  widget_control,basl(i    ,0), set_value=tmp(0)
 	  basl(i,5)=basl(i+1,5)
	  if basl(i,4) gt 0 then widget_control,basl(i,4),set_button=basl(i,5)
 	endfor
 	  widget_control,basl(nb   ,0), set_value=''  &  basl(nb,5)=0
 	  if (nb gt 20) and (cur lt nb-visible) then begin
 	  	widget_control,basl(nb,1),get_uvalue=bid
 	  	widget_control,bid,/destroy
 	  	basl=basl(0:nb-1,*)
 	  	DialTag,D=basl(13,2),tag='LINES',set=nb-1 & endif
 endelse

endif else begin
;SLIDER HAS MOVED
;****************
 widget_control,basl(14,2),get_uvalue=sluv & from=sluv(2) & sluv(2)=to
 widget_control,basl(14,2),set_uvalue=sluv,set_value=to
 onu=from<(nb-visible+1)  &  num=to<(nb-visible+1)

 IF onu ne num then begin

 v=visible-1
 
;GET TEXT VALUES
;***************
 txt=strarr(visible) & oxt=txt & lb1=txt & lb2=txt & ob1=txt & ob2=txt
 for i=0,visible-1 do begin
 	widget_control,basl(num+i,0), get_value=tmp  & txt(i)=tmp(0)
	widget_control,basl(num+i,1), get_value=tmp  & lb1(i)=tmp(0)
	widget_control,basl(num+i,3), get_value=tmp  & lb2(i)=tmp(0)
	widget_control,basl(onu+i,0), get_value=tmp  & oxt(i)=tmp(0)
	widget_control,basl(onu+i,1), get_value=tmp  & ob1(i)=tmp(0)
	widget_control,basl(onu+i,3), get_value=tmp  & ob2(i)=tmp(0)
 endfor
;EXCHANGE WIDGET ID's
;********************
 tmp=basl(onu:onu+v,[0,1,3,4])  & basl(onu:onu+v,[0,1,3,4])=kp.id & kp.id=basl(num:num+v,[0,1,3,4])
 basl(num:num+v,[0,1,3,4])=tmp  & widget_control,basl(11,2),set_uvalue=kp

;SET TEXT VALUES
;***************
 for i=0,visible-1 do begin
	widget_control,basl(onu,0), set_value=oxt(i)
	widget_control,basl(onu,1), set_value=ob1(i)
	widget_control,basl(onu,3), set_value=ob2(i) & onu=onu+1
 endfor
 for i=0,visible-1 do begin
   	si=strtrim(string(num),2) & if num le 9 then si='0'+si & if num le 99 then si='0'+si
	widget_control,basl(num,4), set_value=si,set_button=basl(num,5),set_uvalue=[1 ,0,num]
	widget_control,basl(num,0), set_value=txt(i)                   ,set_uvalue=[17,0,num]
	widget_control,basl(num,1), set_value=lb1(i)
	widget_control,basl(num,3), set_value=lb2(i) & num=num+1
 endfor
 ENDIF
endelse
end

function dial_xbu
;******* ********

;**
return,{value:'Idle',wbase:0L,cmd:'',log:[''],wupdate:-1, nowin:0, lines:45, onoff:1, frequency:1., Xfreq:.8}
end
