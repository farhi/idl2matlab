pro DialCommons
;** ***********
;** Global variables
;@lamp.cbk ;Replace this line by next 3 lines for standalone use (No lamp!)
	common c_lamp_info, did_win0
	common c_lamp_w,w0
	common dialshare2,dd,d0,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10
	common c_dialet, c_base, c_name, c_output, c_stat, c_freq, c_lim,$
	                 c_timon,c_current,c_input,c_basd,c_seq,c_que
	common c_webweb, geo_web,gew_act, gew_pth, gew_snd, gew_err, $
	                 gew_pwd,gew_r,gew_g,gew_b,gew_v
end

function N2S, number
;******* ***
;**
return, strtrim(string(number),2)
end

;************************************************************ ;+++++++++++
pro DialTag, name ,D=dnum, Si=di, TAG=tag, SET=setv, GET=getv ;+++++++++++
;** *******
;**
;** Set or get the tag value of named Dial
    common dialshare2
getv=0
IF n_elements(name) eq 1 then DialNameToNumber,name,D=dnum
IF n_elements(di)   ne 1 then di=strtrim(string(dnum),2)
IF dnum ge 0  THEN  $
IF n_elements(setv) gt 0 then BEGIN
   IF strupcase(tag) eq "ONOFF" then if setv gt 0 then $
               DialStart,D=dnum else DialStop,    D=dnum      ELSE $
   IF strupcase(tag) eq "VALUE" then DialModValue,D=dnum,setv ELSE $
   IF strupcase(tag) eq "ERROR" then DialModValue,D=dnum,setv,tag="ERROR"$
   ELSE    ii=EXECUTE("d"+di+"."+tag+"=setv")
ENDIF ELSE ii=EXECUTE("getv=d"+di+"."+tag)
end

;************************************************** ;+++++++++++++++++++++
pro DialsFrequency, GET=getv, SET=setv, STOP=stop,$ ;+++++++++++++++++++++
                    START=start, DURATION=lim
;** **************
;**
;** Settings for the main Timer
    common c_dialet

IF n_elements(setv) eq 1 then c_freq=setv
IF n_elements(lim)  eq 1 then c_lim =lim
IF keyword_set(stop)     then c_stat=0
IF keyword_set(start)    then begin c_timon(0)=systime(1)*1000
                              c_stat=1 & Dialet_event,{id:c_base} & endif
getv=c_freq 
end

;**************************************************************** ;+++++++
function DialNewValue, SETVALUE=setvalue, COMMENT=ctxt ,TYPE=tipe ;+++++++
;******* ************
;**
;** Get value of the running dial by calling dial_"generic"_read function
    common dialshare2
    common c_dialet

di= strtrim(string(c_current),2)
IF n_elements(ctxt) eq 0 then ctxt=""

if n_elements(tipe) eq 1 then nome=tipe else $
DialTag,D=c_current,Si=di,tag="TYPE"   ,get=nome
DialTag,D=c_current,Si=di,tag="GENERIC",get=generic
now=systime(1)
val=3.14
ii=EXECUTE('val=dial_'+generic+'_read(nome, now, ctxt)')
IF not ii then begin  DialMessage,4,di+":"+nome  & DialStop
ENDIF ELSE IF keyword_set(setvalue) then  DialModValue,val,D=c_current
RETURN,val
end

;************************************************ ;+++++++++++++++++++++++
pro DialModValue, val, NAME=name, d=dnum ,tag=TAG ;+++++++++++++++++++++++
;** ************
;**
;** Used when dial.VALUE changes its type or dimension
    common dialshare2
    common c_dialet

    IF n_elements(name) eq 1 then DialNameToNumber,name,D=dnum
    IF n_elements(dnum) ne 1 then dnum=c_current
      di=strtrim(string(dnum),2)
      sv=SIZE(val)
      IF n_elements(TAG) ne 1 then TAG='VALUE' else TAG=strupcase(TAG)
      sz=0 & ii=EXECUTE('sz=SIZE(d'+di+'.'+TAG+')')
      IF (sz(sz(0)+1) ne sv(sv(0)+1)) or  $
         (sz(0) ne sv(0)) or (sz(sz(0)+2) ne sv(sv(0)+2)) then BEGIN
          elsa = ''
          tlist=[''] & ii=EXECUTE('tlist=strupcase(tag_names(d'+di+'))')
          FOR k=0,n_elements(tlist)-1 DO BEGIN CASE tlist(k) OF
              TAG:
              ELSE: elsa =elsa+','+tlist(k)+':d'+di+'.'+tlist(k)
              ENDCASE
          ENDFOR
                 ii=EXECUTE('d'+di+'={'+TAG+':val'+ elsa +'}')
      ENDIF ELSE ii=EXECUTE('d'+di+'.' +TAG+'=val')
end

;************************** ;+++++++++++++++++++++++++++++++++++++++++++++
pro DialStop,  name, D=dnum ;+++++++++++++++++++++++++++++++++++++++++++++
;** ********
;** Stop the Dial
    common dialshare2
    common c_dialet
IF  n_elements(name) eq 1 then DialNameToNumber,name,D=dnum
IF  n_elements(dnum) ne 1 then dnum=c_current
IF  dnum gt 0 then BEGIN 
    di= strtrim(string(dnum),2) & ii= EXECUTE('d'+di+'.ONOFF=0')
    DialMessage,6,di            & ENDIF
end

;************************** ;+++++++++++++++++++++++++++++++++++++++++++++
pro DialStart, name, D=dnum ;+++++++++++++++++++++++++++++++++++++++++++++
;** *********
;** Start or resume the Dial
    common dialshare2
    common c_dialet
IF  n_elements(name) eq 1 then DialNameToNumber,name,D=dnum
IF  n_elements(dnum) ne 1 then dnum=c_current
IF  dnum gt 0 then BEGIN
    di= strtrim(string(dnum),2)    & freq=0
    ii= EXECUTE('d'+di+'.ONOFF=1') & ii=EXECUTE('freq=d'+di+'.FREQUENCY')
    c_timon(dnum)=systime(1)*1000  & DialMessage,7,di
    IF freq gt 0 THEN Dialet_event,{id:c_basd(dnum,0)} $
                 ELSE Dialet_event,{id:c_base}
ENDIF & end

;************************************************* ;++++++++++++++++++++++
function DialControl, command, D=dnum, CHECK=check ,NAME=nome;++++++++++++
;******* ***********
;**
;** Back control to the instrument
    common dialshare2
    common c_dialet
    common c_webweb

IF n_elements(dnum)  ne 1 then dnum =c_current
IF n_elements(nome)  eq 1 then dnum =DialNameToNumber(nome)
IF n_elements(check) ne 1 then check=0.
di     =strtrim(string(dnum),2)
generic='' & DialTag,D=dnum,Si=di,tag="GENERIC",get=generic
nome=''    & DialTag,D=dnum,Si=di,tag="TYPE"   ,get=nome
name=''    & DialTag,D=dnum,Si=di,tag="NAME"   ,get=name
val=3.14
if geo_web then WebDo,'snd',command,dnum
ii=EXECUTE('val=dial_'+generic+'_send(nome, check, command, name)')
IF not ii then BEGIN val=0 & DialMessage,5,di+":"+name & DialStop & ENDIF
RETURN,val
end

;************************** ;+++++++++++++++++++++++++++++++++++++++++++++
pro DialClear, name, D=dnum ;+++++++++++++++++++++++++++++++++++++++++++++
;** *********
;** Remove the Dial
    common c_dialet
IF  n_elements(name) eq 1 then DialNameToNumber,name,D=dnum
IF  n_elements(dnum) ne 1 then dnum=c_current
IF  d_num gt 0 then BEGIN DialStop ,D=dnum & c_name(d_num)="" & ENDIF
end

;********************************* ;++++++++++++++++++++++++++++++++++++++
pro DialMacro, name, D=dnum, Si=di ;++++++++++++++++++++++++++++++++++++++
;** *********
;**
;** Execute the macro of named Dial
    common dialshare2
    common c_dialet
    common c_lamp_w
    common c_webweb
IF n_elements(name) eq 1 then DialNameToNumber,name,D=dnum else name=''
IF n_elements(dnum) ne 1 then dnum=c_current
CATCH,stat & if stat ne 0 then begin catch,/cancel & DialStop
                                     DialMessage,0 & return & endif
IF dnum gt 0 then BEGIN
   c_basd(dnum,1)=c_current    & c_current=dnum  & orig=""
   IF n_elements(di) ne 1 then di= strtrim(string(dnum),2)
   DialTag,D=dnum,Si=di,tag="NAME"  ,get=name
   DialMessage,8,name
   DialTag,D=dnum,Si=di,tag="ORIGIN",get=orig
   ii = EXECUTE('dial_'+orig+'_macro,d'+di)
   IF ii eq 0 then begin DialMessage,9,name & DialStop
   ENDIF ELSE BEGIN
	sz=0 & vl=0   & ii=EXECUTE('sz=size(d'+di+'.VALUE)') & nl=sz(sz(0)+2)
	IF nl le 5 then ii=EXECUTE('vl=d'+di+'.VALUE') $
	           else ii=EXECUTE('vl=d'+di+'.VALUE(0:4)')
	str=name+"="+string(vl,/print) & IF nl gt 5 then str=str+" ->"
	IF n_elements(w1) gt 0 then ii=EXECUTE('w'+di+'=d'+di+'.VALUE');Lamp

	if geo_web then begin
		if (sz(sz(0)+1) eq 7) or (nl eq 1) then begin
			if nl    gt 1  then ii=EXECUTE('vl=d'+di+'.VALUE')
			if nl    gt 1  then WebDo,'log',vl ,dnum
			if vl(0) ne '' then WebDo,'val',str,dnum
		endif else ii=EXECUTE('WebDo,"wks",str,dnum,d'+di+'.VALUE')
	endif

	IF (vl(0) ne '') or (nl gt 1) then begin
	  DialMessage ,10,str
	  hiss=0  & DialTag, D=dnum,Si=di,tag="HISTORY",get=hiss
	  IF hiss then begin u=-1
	   on_ioerror,misopn & openw,u,"dial_"+name+".his",/append,/get_lun
	   ii=EXECUTE('printf,u,d'+di+'.VALUE')
	   misopn: IF u gt 0 then free_lun,u
	ENDIF  &  ENDIF
   ENDELSE
   c_current=c_basd(dnum,1)
ENDIF & end

;*********** ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro DialWSet ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;** ********
;**
;** Select the main draw window
    common c_lamp_info
IF  n_elements(did_win0) eq 1 then Wset,did_win0 ;Lamp
end

;**************************** ;+++++++++++++++++++++++++++++++++++++++++++
function DialOn, Dial, D=dnum ;+++++++++++++++++++++++++++++++++++++++++++
;******* ******
;**
;** Check fo a user input
    common c_dialet
R=1
P_DIA_WIDGET, "GET_ev",c_input,resp
IF resp then BEGIN     Dialet_event,{id:c_input}
   IF n_tags(Dial)     gt 1 then dnum=Dial.number
   IF n_elements(dnum) ne 1 then dnum=c_current
   DialTag,D=dnum,TAG="ONOFF",GET=R
   IF R then BEGIN
      DialTag,D=dnum,TAG="FREQUENCY",GET=freq
      IF freq eq 0 then BEGIN 
         DialsFrequency, GET=freq
         IF (freq le 0) or (c_stat eq 0) then R=0
      ENDIF
   ENDIF
ENDIF
RETURN,R
end

;*********************************************************** ;++++++++++++
pro DialInit, name, D=dnum, NEW=newed, PATH=pth, herits=diaH ;++++++++++++
;** ********
;**
;** Initiates a Dial from its file
    common dialshare2
    common c_dialet

IF n_elements(c_name) eq 0 then Dialet, /MAP
IF n_elements(name)   ne 1 then name  = "NoName"
IF n_elements(newed)  ne 1 then newed = strlowcase(name)
IF n_elements(pth)    ne 1 then pth   = ""
IF n_elements(dnum)   ne 1 then DialNameToNumber,newed,/find,D=dnum
dial=""
ii  =EXECUTE ("dial=dial_"+name+"()")
if n_tags(dial) lt 1 then dial={init:0} & ii=1
IF ii eq 1 then BEGIN
   tlist   =strupcase(tag_names(dial))

   idx     =where    (tlist eq 'INHERITS')
   IF idx(0) ge 0 then IF dial.inherits gt ' '  then  BEGIN  diaG=''
          DialInit , string (dial.inherits) , d=dnum, herits=diaG
          DialMix  , dial ,  diaG
          tlist=strupcase(tag_names(dial)) & ENDIF
   IF n_elements(diaH) gt 0 then BEGIN DialMix,diaH,dial & RETURN & ENDIF

   named   =newed  & onoff    =0  & generic ='mad'  & type     =name
   value   =0.     & error    =0. & elsa    =""     & frequency=0.
   history =0      & duration =0. & plot    =0      & unit     =""
   upperlim=0.     & lowerlim =0. & init    =0

   FOR k=0,n_elements(tlist)-1 DO CASE tlist(k) OF
                     "NAME":
                     "PATH":
                     "NUMBER":
                     "ONOFF":    onoff    =fix   (dial.onoff)
                     "VALUE":    value    =       dial.value
                     "ERROR":    value    =       dial.error
                     "PLOT":     plot     =fix   (dial.plot)
                     "INIT":     init     =fix   (dial.init)
                     "UNIT":     unit     =string(dial.unit)
                     "UPPERLIM": upperlim =float (dial.upperlim)
                     "LOWERLIM": lowerlim =float (dial.lowerlim)
                     "GENERIC":  generic  =string(dial.generic)
                     "TYPE":     type     =string(dial.type)
                     "FREQUENCY":frequency=float (dial.frequency)
                     "HISTORY":  history  =fix   (dial.history)
                     "DURATION": duration =float (dial.duration)
                      ELSE:      elsa=elsa+","+tlist(k)+":dial."+tlist(k)
                      ENDCASE
   di=strtrim(string(dnum),2)
   ii=execute('d'+di+'={NAME:named,ORIGIN:name,GENERIC:generic'+$
             ',TYPE:type,PLOT:plot,VALUE:value,ERROR:error,INIT:init'+$
             ',UNIT:unit,UPPERLIM:upperlim,LOWERLIM:lowerlim'  +$
             ',FREQUENCY:frequency,DURATION:duration,PATH:pth' +$
             ',ONOFF:onoff,HISTORY:history,NUMBER:dnum'+elsa   +'}')
   IF ii eq 1 then BEGIN
      c_name(dnum)=named
      DialMessage,11,di+":"+named
      DialTag,D=dnum,Si=di,TAG="ONOFF", SET=onoff
      if c_current le 0 then c_current=dnum
   ENDIF ELSE DialMessage,3,named

ENDIF ELSE DialMessage,2,name
end

;********************************** ;+++++++++++++++++++++++++++++++++++++
pro WebOn, PATH=wpth ,PASSWORD=pass ;+++++++++++++++++++++++++++++++++++++
;** *****
common c_dialet
common c_webweb

if n_elements(gew_pth) ne 1 then begin
		Mach=strupcase(getenv('HOST')) & id=strpos(Mach,'.')
		if id gt 0 then Mach=strmid(Mach,0,id)
		gew_pth='/home/cs/lambda/geoport/'+Mach  & endif
if n_elements(gew_pwd) ne 1 then gew_pwd=''
if n_elements(   wpth) eq 1 then gew_pth=  wpth
if n_elements(   pass) eq 1 then gew_pwd=  pass

nld=strlen(gew_pth)-1 & dvd=sys_dep('DIVIDER')
if nld gt 0 then if strmid(gew_pth,nld,1) ne dvd then gew_pth=gew_pth+dvd
on_ioerror,misopn
openw,u,gew_pth+'geo_d_0.web',/get_lun & free_lun,u
list=findfile(gew_pth+'geo_d_*',count=nn)
if  nn gt 0 then bid=sys_dep('DELIST',list)

    par1='' & ii=execute('par1=dial_pad_init()') & sz=SIZE(par1)
    if (sz(0) eq 2) and (sz(1) eq 5) then begin
	  openw,u,gew_pth+'dial_pad.web',/get_lun
	  printf,u,'# George Instrument PAD'
	  printf,u,'# ',strtrim(string(sz(2)),2),$
	           ' * 5 entries (label command flag program check)'
	  printf,u,'# First 5 lines for input text, others for buttons'
	  printf,u,'# '
	  for j=0,sz(2)-1 do for i=0,sz(1)-1 do printf,u,par1(i,j)
	  free_lun,u & endif

if sys_dep ("MACHINE") eq  "vms" then gew_v=";1" else gew_v=""
gew_act=100 & gew_snd=['-'] & gew_err='-'
if  gew_pwd gt ' ' then begin DialInit ,"webon"
                              DialTag  ,"webon",tag="WEBPTH",set=gew_pth+$
							"geo_webon.txt"
                              DialTag  ,"webon",tag="PWD"   ,set=gew_pwd
                              DialStart,"webon" & endif
WebDo,"sta","on",0
geo_web=1 & return
misopn: DialErrMes
end

;********* ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro WebOff ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;** ******
common c_dialet
common c_webweb
if geo_web gt 0 then WebDo,"sta","off",0
geo_web=0
gew_pwd='' & DialStop,"webon"
end

;****************************** ;+++++++++++++++++++++++++++++++++++++++++
pro WebDo, flg, val, dnum, matx
;** *****
common c_dialet
common dialshare2
common c_webweb

if geo_web eq 0 then return
case flg of
'act':gew_act=100 + 100-val
'err':gew_err=val
'snd':gew_snd=val
'log':begin openw,u,gew_pth+'geo_d_'+strtrim(dnum,2)+'.txt'+gew_v,/get_lun
	      printf,u, val
	      free_lun,u & end
 else:begin di=strtrim(dnum,2)			;'gif' 'val' 'wks'
	freq=0 & dur=0 & his=0
	if dnum gt 0 then DialTag,D=dnum,Si=di,tag="FREQUENCY",get=freq
	if dnum gt 0 then DialTag,D=dnum,Si=di,tag="DURATION" ,get=dur
	if dnum gt 0 then DialTag,D=dnum,Si=di,tag="HISTORY"  ,get=his

	img=n_elements(matx)
	on_ioerror,misopn
	if img gt 1 then $
	   if flg ne 'wks' then WRITE_KIF,gew_pth+'geo_d_'+di+'.gif'+gew_v,$
						    matx,gew_r,gew_g,gew_b
	openw,u,gew_pth+'geo_d_'+di+'.web'+gew_v,/get_lun
	printf,u, gew_act,c_freq,c_lim
	printf,u, freq,dur,his,' '+flg
	printf,u, val
	if gew_err    ne '' then begin printf,u,'err:'+gew_err & gew_err=''
					 endif
	if gew_snd(0) ne '' then begin printf,u,'snd:'+gew_snd & gew_snd=['']
					 endif
	free_lun,u & end
endcase
misopn:
end

pro DialMix, A,B ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;** *******
;** Mixe Dial B into Dial A

    nA=n_tags(A)  & nB=n_tags(B)
    IF nB lt 1 then RETURN & IF nA lt 1 then BEGIN A=B & RETURN & ENDIF
    elsa=''
    lA=strupcase(tag_names(A))
    FOR k=0,nA-2 DO  elsa =elsa    + lA(k)   +':A.'+lA(k)+','
                     elsa =elsa    + lA(nA-1)+':A.'+lA(nA-1)
    lB=strupcase(tag_names(B))
    FOR k=0,nB-1 DO  IF (where(lA eq lB(k)))(0) eq -1 then $
                     elsa =elsa+','+ lB(k)   +':B.'+lB(k)
    ii=EXECUTE('A={'+ elsa +'}')
end

pro DialMessage ,flg, str ;+++++++++++++++++++++++++++++++++++++++++++++++
;** ***********
;**
;** Message output
    common c_dialet
    common c_webweb
msg=""
CASE flg OF
0:   msg=!err_string
1:   msg="Executed: "+str
2:   msg="Dial file dial_"+str+".pro ->"+!err_string
3:   msg="Dial "+str+" failed setting attributes ->"+!err_string
4:   msg="Dial "+str+" failed reading new value ->"+!err_string
5:   msg="Dial "+str+" failed sending control ->"+!err_string
6:   P_DIA_WIDGET, "SET_va",c_basd(long(str),0)," "+str
7:   P_DIA_WIDGET, "SET_va",c_basd(long(str),0),"+"+str
8:   msg="Dial "+str
9:   msg="Dial "+str+" failed executing macro ->"+!err_string
10:  msg=str
11:  msg="Dial "+str+" is loaded ..."
12:  msg="Error-code"+str+" ->"+!err_string
ELSE:
ENDCASE
IF   c_basd(0,0)  eq 1 then P_DIA_WIDGET, "SET_va",c_output,msg $
ELSE BEGIN	IF flg eq 6 then msg="Dial "+str+" is stopped ..."
		IF flg eq 7 then msg="Dial "+str+" is started ..."
		IF flg ne 8 then begin print,msg
		 IF geo_web then IF flg ne 10 then WebDo,'err',msg
		ENDIF
ENDELSE  & end

pro DialNameToNumber, name, FIND=find, D=knam ;++++++++++++++++++++++++++++
;** ****************
;**
;** Given the name of a dial, return its number
;** If keyword_set find then  return a free number if no name match
    common c_dialet
    common dialshare2

kfree= 1 & knam=-1

FOR i= n_elements(c_name)-1 , 1 ,-1 DO IF c_name(i) eq name then knam =i $
                                  ELSE IF c_name(i) eq ""   then kfree=i

IF (knam lt 0) and (keyword_set(find)) then knam=kfree
end

pro Dialet_event, event, uv ;+++++++++++++++++++++++++++++++++++++++++++++
;** ************
;**
;** Event handler
    common dialshare2
    common c_dialet

IF n_elements(uv) eq 0 then P_DIA_WIDGET, "GET_uv", event.id, uv
CATCH,stat
IF stat ne 0 then DialMessage,0 ELSE BEGIN
CASE uv(1) OF
690: BEGIN P_DIA_WIDGET, "GET_va", event.id, comd
           ii=EXECUTE(comd(0))      ;COMMAND INPUT -----------------------
           IF ii ne 1 then DialMessage,0 ELSE DialMessage,1,comd(0)
           comd="                         ;"+strtrim(comd(0),2)
           P_DIA_WIDGET, "SET_va", event.id, strmid (comd,0,100)
     END
691: BEGIN c_seq=systime(1)
           IF uv(2) lt 0 then BEGIN ;GENERAL  TIMER ----------------------
            IF c_lim gt 0 then IF c_seq*1000-c_timon(0) gt c_lim*1000 $
                          then DialsFrequency,/STOP
            IF c_stat     then BEGIN
             P_DIA_RETIM ,event.id, c_freq
             dnum =0 & R  =1
             FOR i=1,n_elements(c_name)-1 DO IF c_name(i) gt "" THEN BEGIN
              di  =strtrim(string(i),2) & i0=i
              onoff=0 & DialTag,D=i0,Si=di,tag="ONOFF"    ,get=onoff
              IF onoff then BEGIN
               freq=0 & DialTag,D=i0,Si=di,tag="FREQUENCY",get=freq
                IF freq le 0 then BEGIN
                   IF dnum gt 0 then R=DIALON(d=i0)
                   IF c_stat then if R then BEGIN dnum =i
		        DIALMACRO,D=dnum, Si=di
                        DialTag  ,D=dnum, Si=di,tag="FREQUENCY",  get=freq
			IF freq gt 0 then P_DIA_RETIM ,c_basd(dnum,0),freq
	           ENDIF
                ENDIF
              ENDIF
             ENDIF
             tot=c_freq-(systime(1)-c_seq)
             IF tot lt 0 then if c_freq gt 0 then P_DIA_RETIM,event.id,tot
            ENDIF
           ENDIF ELSE BEGIN         ;SPECIFIC TIMER ----------------------
            dnum =uv(2) & di=strtrim(string(dnum),2)
            freq = 0    & DialTag ,D=dnum,Si=di,tag="FREQUENCY",get=freq
            durat= 0    & DialTag ,D=dnum,Si=di,tag="DURATION" ,get=durat
            IF durat gt 0 then IF  c_seq*1000-c_timon(dnum) gt durat*1000 $
                     then DialStop,D=dnum
            onoff= 0    & DialTag ,D=dnum,Si=di,tag="ONOFF"    ,get=onoff
            IF freq  gt 0 then IF  onoff then  BEGIN
		P_DIA_RETIM , event.id , freq
		DIALMACRO   , D=dnum   , Si=di
		fruq=freq & DialTag ,D=dnum,Si=di,tag="FREQUENCY",get=freq
		if freq ne fruq then P_DIA_RETIM ,event.id, freq $
		else begin
		  totim=(c_freq*c_stat)+freq - (systime(1)-c_seq)
		  if totim lt 0 then P_DIA_RETIM ,event.id, totim
		endelse
            ENDIF
           ENDELSE
     END
694: P_PAD_EVENT, event, uv
ELSE:
ENDCASE
ENDELSE
end

pro P_DIA_RETIM, id,freq ;++++++++++++++++++++++++++++++++++++++++++++++++
;** ***********
;** RE-TIMER
			widget_control,bad_id=ii,id,/CLEAR_EVENTS
if freq gt 0 then	widget_control,bad_id=ii,id, TIMER=freq	else $
if freq lt 0 then	widget_control,bad_id=ii,id, TIMER=0.05<(-freq)
end

pro P_DIA_WIDGET, flg, id, val ;++++++++++++++++++++++++++++++++++++++++++
;** ************
;** Miscelaneous widget controls
CASE flg OF
'GET_ev':BEGIN evv=widget_event(id,/nowait,bad_id=ii)
               IF evv.id eq id then val=1 ELSE val=0 & END
'SET_va':widget_control,id,set_value =val ,bad_id=ii
'GET_va':widget_control,id,get_value =val ,bad_id=ii
'GET_uv':widget_control,id,get_uvalue=val
ELSE:
ENDCASE
end

pro P_PAD_EVENT, event, uv ;++++++++++++++++++++++++++++++++++++++++++++++
;** ***********
;**
common dialshare2

widget_control,uv(3),get_uvalue=PadTab
ncomm=-1

if (uv(4) ne 100) and ((PadTab(2,uv(8)) eq "t") or $
			(PadTab(2,uv(8)) eq "c")) then begin
	k=uv(8)
	str8="PAD_" +strtrim  (string(k),2)
	if xregistered(str8) le 0 then begin
	   padr=str_sep    (strlowcase(PadTab(1,k)),"<cr>")
	   padb=widget_base(title=PadTab(0,k),resource_name='lamp',/column)
	   for r=0,n_elements(padr)-1 do begin
		padt=str_sep      (Padr(r),"~")
		n   =n_elements   (padt)
		if (n/2)*2 ne n then padt=[padt,' ']
		n   =n_elements   (padt) & biti =lonarr(n)
		padg=widget_base(padb,/row,resource_name='geo',/frame)
		for i=0,n-1,2 do begin
		 bid=widget_base (padg,/column)
		 bil=widget_label(bid ,value=padt(i))      & biti(i)  =bil
		 bit=widget_text (bid ,value=padt(i+1),/editable, $
		                xsize=strlen(padt(i+1))+3) & biti(i+1)=bit
		endfor
		if r eq 0 then bito=biti else bito=[bito,-1,-1,biti]
	   endfor
	   ivv  =uv & ivv(4)=100
	   padg =widget_base  (padb,/row,resource_name='geo')
	   bid  =widget_button(padg,value='SEND ->',uvalue=ivv)
	   err  =widget_label (padg,value='       ',xsize =250)
	   bito =[bito,err]
	   widget_control,padb,set_uvalue=bito,/realize
	   XMANAGER, str8,padb,event_handler="Dialet_event",/just_reg
	endif
endif else begin
	ncomm=uv(8)
	if (uv(4) ne 100) then  comm =PadTab(1,uv(8)) $	;from button
	else begin    como='' & comm =[''] & r=0 & sep=""	;from GUI
		widget_control, event.top, get_uvalue=bito
		nbito= n_elements(bito)-1
		for j=1,nbito-1,2 do begin	   comi=''
		  if bito(j) gt 0 then begin
			widget_control,bad_id=ii,bito(j-1),get_value=labi
			widget_control,bad_id=ii,bito(j)  ,get_value=comi
			comi   = strtrim (comi(0),2)
			comm(r)= comm(r) +comi+' '
			if comi eq '' then ncomm=-1
			como   = como+sep+labi(0)+"~"+comi & sep="~"
		  endif else begin r=r+1  & sep="" 
			comm   =[comm,''] & como=como+"<cr>" & endelse
		endfor
		PadTab(1,uv(8))=como
		widget_control,uv(3),set_uvalue=PadTab
		if ncomm lt 0	then txt ='!!! Cmd is incomplete.' $
				else txt ='Sending ...'
		widget_control, bito(nbito),set_value =txt
	endelse
endelse

if ncomm ge 0 then begin pros=[uv(5),uv(6)]
   d0={GENERIC:PadTab(3,ncomm),NAME:PadTab(0,ncomm),TYPE:'PAD',PROS:pros}
   on_ioerror, mischk & check=0. & check=float(PadTab(4,ncomm)) & mischk:
   R=0 & ii=execute('R = DialControl(comm,  d=0, check=check)')
   if (R lt 0) or (R gt 1) then txt="err-code "+string(R) else txt='Sent'
   DialMessage,12,txt
endif
end

pro P_PAD_CREATE, aque,bido,cque ;++++++++++++++++++++++++++++++++++++++++
;** ************
;**
	par1='' & ii=execute('par1=dial_pad_init()')
	sz=SIZE(par1)
	if (sz(0) eq 2) and (sz(1) eq 5) then begin

	   cque = widget_base  (aque ,/row,frame=3)

	   widget_control,bido,set_value=par1(0,0)
	   PROX=[-1L,0]
	   uvv =[-88,694,0,cque]
	   widget_control,cque,set_uvalue=par1

	   i=1 & k=0 & n_e=n_elements(par1)/5
	   par1=[[par1],['','','','','']]
	   while i  lt n_e do begin
		val=par1(0,i)
		j=k/2 & if j*2 eq k then dque=widget_base  (cque ,/column)
		eque  = widget_base  (dque ,/row)
		k=k+1
;**	level1
		if par1(2,i) eq '-' then begin
		  bid1= widget_button(eque ,value=val ,menu=2)
		  i=i+1
		  while strpos(par1(0,i),'-') eq 0 do begin
		   val =strmid(par1(0,i),1,15)
;**	level2
		   if par1(2,i) eq '-' then begin
			bid2=widget_button(bid1 ,value=val ,menu=2)
			i=i+1
			while strpos(par1(0,i),'--') eq 0 do begin
			  val =strmid(par1(0,i),2,15)
;**	level3
			  if par1(2,i) eq '-' then begin
				bid3=widget_button(bid2 ,value=val ,menu=2)
				i=i+1
				while strpos(par1(0,i),'---') eq 0 do begin
				  val =strmid(par1(0,i),3,15)
				  bid4=widget_button(bid3 ,value=val,uvalue=$
					[uvv,i,PROX,bido,i]) & i=i+1
				endwhile
			  endif else begin
				bid3=widget_button(bid2 ,value=val,uvalue=$
				     [uvv,i,PROX,bido,i]) & i=i+1 & endelse
			endwhile
		   endif else begin
			bid2=widget_button(bid1 ,value=val,uvalue=$
				[uvv,i,PROX,bido,i]) & i=i+1 & endelse
		  endwhile
		endif else begin
		  bid = widget_button(eque ,value=val,uvalue=$
			 [uvv,i,PROX,bido,i]) & i=i+1 & endelse
	   endwhile
	endif
end

pro Dialet, MAP=map ,PAD=pad ;++++++++++++++++++++++++++++++++++++++++++++
;** ******
;** Initiate the User Interface (Visible if map=1)
;**
;** c_name is a string array containing the Dial names
;** c_basd  is a baseID array used to keep the timer events & c_current
;** c_output is a baseID for message output
;** c_input  is a baseID for command input
;** c_current is the running Dial (saved in c_basd for recurrent calls)
;** c_stat, c_freq, c_lim are the onoff,frequency,duration of main timer
;** c_timon is a double array containing start time for each dial
;** c_seq   is the start time for current loop (used in DialNewValue)
;** c_base  is the main base
    common c_dialet
    common c_lamp_info
    common c_webweb

IF n_elements (c_name)   eq 0 then begin c_name =STRARR(11) & c_current=0
                                         c_stat =1          & c_freq=0D
                                         c_timon=DBLARR(11) & c_lim =0
                                         c_seq  =0D         & geo_web  =0
                                         c_basd =LONARR(11,2) & endif
IF not keyword_set(pad)  then pad= 0
IF not keyword_set(map)  then map=pad &  c_basd(0,0)=map
IF XREGISTERED("DIALET") eq 0 then begin c_que=0
   c_base  = widget_base   (title="Dialet Control",MAP=0,/column,$
				resource_name="lamp"   ,uvalue=[-88,691,-1])
   c_input = widget_text   (c_base,xsize=52,/editable,uvalue=[-88,690, 1])
   c_set   = widget_base   (c_base,/row)
   FOR i=1,n_elements(c_name)-1 DO c_basd(i,0)= $
             widget_label  (c_set ,value="    ",uvalue=[-88,691, i])
   c_output= widget_label  (c_base,xsize=52*10 , value=" ")
             widget_control,c_base,/REALIZE
          XMANAGER,"DIALET",c_base,/just_reg
ENDIF
IF (pad and (c_que eq 0)) then P_PAD_CREATE, c_base, c_input, c_que
widget_control,c_base,MAP=map
IF map then XMANAGER
end
