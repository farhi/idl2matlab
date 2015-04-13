;=============================================================================================================
PRO CHRIS_EVENT, event
;=============================================================================================================
common chr1 ,comment,listd,listp,newdata,newproc,newpath,mnemo,lindex,lindex1 $
            ,last_list,lirinst,lirpath,ttinst,ttgroup,ttproc,ttpath,ttsymbol  $
            ,ttouch,ttmacro,touch,macros,access,site,actif,pth,base0,potn,magn,npar,wall
common did1 ,WoR,worb,wort,txtr,txtw,deffont

		 stat=0 & catch,stat
	         if stat  ne 0  then begin catch,/cancel
	         		widget_control,bad_id=i,comment,set_value=strmid(!err_string,0,55)
	         		return & endif
	         		
widget_control,event.id,get_uvalue = uv                                          ;  WIDGET UVALUE ? 
widget_control,comment,set_value   = ''                                          ;  COMMENT area set with blank 

IF n_elements(uv) gt 1 then IF uv(1) eq 391 then begin  p_did_mvlog, event,uv
							return & endif
;-----------------------EXAMPLE------------------------------------------------------------------------------

IF (uv eq 'TEMPLATE')  then begin
	bib=widget_base (title = 'Lamp  template read_tmp.pro' ,resource_name='lamptouch')
	if n_elements(deffont) eq 1 then widget_control,bib,default_font=deffont 
	bob=widget_text (bib,value=txtr,xsize=95,ysize=30,/scroll)
	widget_control,  bib,group_leader=base0,/realize & endif
IF (uv eq 'TEMPLATE2') then begin
	bib=widget_base (title = 'Lamp  template write_tmp.pro',resource_name='lamptouch')
	if n_elements(deffont) eq 1 then widget_control,bib,default_font=deffont 
	bob=widget_text (bib,value=txtw,xsize=95,ysize=30,/scroll)
	widget_control,  bib,group_leader=base0,/realize & endif

IF (uv eq 'WoRtoggle') then begin wort=abs(wort-1) & widget_control,worb,set_value=WoR(wort) & endif
;---------------------- CARRIAGE RETURN ? --------------------------------------------------------------------

IF (uv eq 'CR')    then return                                                   ;  IF <CR> in input fields RETURN 

;---------------------- BUTTON ABORT -------------------------------------------------------------------------

IF (uv eq 'ABORT') then begin

    widget_control,event.top,/destroy                                            ;  ABORT SESSION?..destroy bases 
    return

ENDIF

;-------------INDEX IN DATATYPE LIST-------------------------------------------------------------------------- 

IF (uv eq 'INDEX_DATA') then begin

     widget_control,listp,set_list_select = -1                                   ;  SUPRESS INDEX IN OTHER LIST
     widget_control,newpath,set_value = ''                                       ;  PUT BLANK IN OTHER FIELDS
     widget_control,mnemo,set_value   = ''
     lindex = event.index                                                        ;  INDEX SELECTED ? 
     last_list = 1                                                               ;  LAST LIST SELECTED ? 
     IF (lindex eq 0) then return                                                ;  TOTAL SELECTED ? RETURN
     widget_control,NEWDATA,set_value = strmid(lirinst(lindex),0,10)             ;  SHOW SELECTED DATATYPE  
     widget_control,NEWPROC,set_value = strmid(lirinst(lindex),24,20)            ;  SHOW SELECTED PROCEDURE 

ENDIF

;-------------INDEX IN PATHLIST ------------------------------------------------------------------------------ 

IF (uv eq 'INDEX_PATH') then begin

     widget_control,listd,set_list_select = -1                                   ; EQUAL ABOVE..............
     widget_control,newdata,set_value = ''
     widget_control,newproc,set_value = ''
     lindex1 = event.index
     last_list = 2
     IF (lindex1 eq 0) then return
     widget_control,NEWPATH,set_value = strmid(lirpath(lindex1),20,50)
     widget_control,MNEMO,  set_value = strmid(lirpath(lindex1),0,14)

ENDIF

;-------------BUTTON REMOVE DATA------------------------------------------------------------------------------

IF (uv eq 'REM_DATA') then BEGIN
                                                                                 
     IF(lindex eq -1 or last_list eq 2 or last_list eq 0)then begin              ; if nothing selected    
        widget_control,comment,set_value = 'Select an Item before Remove...'     ; or concerns other list -> return
        return  
     ENDIF               

     IF (lindex eq 0) then return
     IF (lindex eq 1) then begin                                                 ; line 0 or DEMO cannot be removed.....
        widget_control,comment,set_value='DEMO cannot be removed...'
        widget_control,newdata,set_value = ''                             
        widget_control,newproc,set_value = ''
        return
     ENDIF

     widget_control,comment,set_value = strmid(lirinst(lindex),0,12)$
                  +'...Removed...' ;remove from list
     lirinst(lindex) = 'DELETED'                                                 ; flags items to be deleted
     ttinst(lindex)  = 'DELETED'
     ttproc(lindex)  = 'DELETED'
     ttgroup(lindex) = 'DELETED'

     lirinst = lirinst(where(lirinst ne 'DELETED'))                              ; does same lists minus item selected
     ttinst  = ttinst (where(ttinst ne  'DELETED'))     
     ttgroup = ttgroup(where(ttgroup ne 'DELETED')) 
     ttproc  = ttproc (where(ttproc ne  'DELETED'))     
                                                                                
     widget_control,listd,set_value = strmid(lirinst(0:*),0,32)                  ; Show new list until 30 car.
                                                                                 
     widget_control,newdata,set_value = ''                                       ; set entry fields with blank                            
     widget_control,newproc,set_value = ''

ENDIF

;------------BUTTON REMOVE PATH--------------------------------------------------------------------------------

IF (uv eq 'REM_PATH') then begin                                                 ; EQUAL ABOVE..... 

     IF(lindex1 eq -1 or last_list eq 1or last_list eq 0)then begin     
        widget_control,comment,set_value = 'Select an Item before Remove...'
        return
     ENDIF

     IF (lindex1 eq 0) then return

     IF (lindex1 eq 1) then begin                                    
        widget_control,comment,set_value = 'CURRENT PATH cannot be removed...'
        return
     ENDIF

     widget_control,comment,set_value = strmid(lirpath(lindex1),0,10)$
                                        + '...Removed...'
     lirpath (lindex1) = 'DELETED'
     ttpath  (lindex1) = 'DELETED'
     ttsymbol(lindex1) = 'DELETED'

     lirpath  = lirpath (where(lirpath  ne  'DELETED'))
     ttpath   = ttpath  (where(ttpath   ne  'DELETED'))
     ttsymbol = ttsymbol(where(ttsymbol ne  'DELETED'))

     widget_control,listp,set_value   = strmid(lirpath(0:*),0,50)                ; SHOW NEW LIST
     widget_control,newpath,set_value = ''
     widget_control,mnemo,set_value   = ''

ENDIF

;-----------BUTTON ADD DATA------------------------------------------------------------------------------------
IF (uv eq 'ADD_DATA') then BEGIN

      widget_control,newdata,get_value = ndata                                   ; GET DATATYPE 
      widget_control,newproc,get_value = prodata                                 ; GET PROCEDURE
      ndata   = ndata(0) 
      prodata = prodata(0)
      prodata = strlowcase(prodata);                                             ; SET PROCEDURE IN LOWERCASE                                        

      blanc   = strpos(prodata,'.pro')                                           ; ".PRO" EXISTS ?                    
      IF (blanc ne -1) then begin
         prodata=strtrim(strmid(prodata,0,blanc),2)                              ; REMOVE ".PRO"  
      ENDIF

      ndata   = strtrim(strcompress(ndata),2)                                    ; SUPRESS BLANKS IN 2 FIELDS 
      prodata = strtrim(strcompress(prodata),2)                                   
      IF (ndata eq '' or prodata eq '') then begin                               ; IF ONE NOT FILLED ? MESSAGE...
         widget_control,comment,set_value = $
           'COMPLETE fields DATATYPE and READ(write) BY before adding...' 
         return
      ENDIF

      blanc   = strpos(ndata,' ')                                                ; EXISTS GROUP ?
      tgroup  = " "
      IF (blanc ne -1) then begin
         tinst =  strtrim(strmid(ndata,0,blanc),2)                               ; EXTRACT DATATYPE & GROUP
         tgroup = strtrim(strmid(ndata,blanc,strlen(ndata)),2)  
      ENDIF else begin
         tinst  = strtrim(strmid(ndata,0,strlen(ndata)),2)
      ENDELSE
      IF wort then IF strpos(tinst,'.') ne strlen(tinst)-1 then tinst=tinst+'.'
      IF strpos(tinst,'.') eq strlen(tinst)-1 then rb='--Write_by> ' else rb='--Read_by-> '

      tproc   = strtrim(strmid(prodata,0,strlen(prodata)),2)                     ; EXTRACT PROCEDURE
      ndata   = tinst+' '+tgroup

      ex1 = 0 & ex2 = 0 & ex3 = 0 & ipos = 0 
      FOR i1  = 1,n_elements(ttinst)-1 DO begin                                  ; DATATYPE ALREADY EXISTS ?
         IF strupcase(ttinst(i1))eq strupcase(tinst) then begin
                              ex1 = 1 & ipos = i1
                              IF strupcase(ttgroup(i1))eq strupcase(tgroup) then ex2 = 1
                              IF strupcase(ttproc(i1))eq strupcase(tproc) then   ex3 = 1
         ENDIF
      ENDFOR

      IF (strlen(ndata) lt 12) then ndata = ndata+$                               ; DATATYPE MAXIMUM 12 CAR.
                      string(replicate(32b,12 -strlen(ndata)))  
      IF (strlen(ndata) ge 12) then ndata = strmid(ndata,0,12)           
      IF (strlen(tproc) ge 20) then tproc = strmid(tproc,0,20)          

      IF ex1 eq 0 and ex2 eq 0 and ex3 eq 0 then begin                            ; DATATYPE DOES NOT EXIST ?
          widget_control,comment,set_value = ndata+'...Added...'           
          lirinst = [lirinst,ndata+rb+tproc]                 	        	 ; INCREASE ARRAYS.....
          ttinst  = [ttinst,tinst]
          ttproc  = [ttproc,tproc]
          ttgroup = [ttgroup,tgroup]
          position= n_elements(lirinst)-1
          widget_control,listd,set_value    = strmid(lirinst(0:*),0,32)           ; <SHOW NEW LIST....
          widget_control,listd,set_list_top = n_elements(lirinst)-1               ; TOP OF LIST WITH NEW....
      endif

      IF ex1 eq 1 then begin                                                      ; DATATYPE EXISTS BUT MODIFIED
          widget_control,comment,set_value = ndata+'...Modified...'          
          lirinst (ipos)= ndata+rb+tproc                              		; MODIFY ARRAYS
          ttinst(ipos)  = tinst
          ttproc(ipos)  = tproc
          ttgroup (ipos)= tgroup
          widget_control,listd,set_value    = strmid(lirinst(0:*),0,32)           ; <SHOW NEW LIST....
          widget_control,listd,set_list_select = ipos                             ; SET POSITION WITH NEW....
      endif

      widget_control,newdata,set_value = ''                                       ; 2 FIELDS ARE FILLED WITH BLANKS...
      widget_control,newproc,set_value = ''

ENDIF 


;-----------BUTTON ADD PATH-------------------------------------------------------------------------------------
IF (uv eq 'ADD_PATH') then begin

  widget_control,newpath,get_value = pathn					  ; GET DATABASE 
  widget_control,mnemo,  get_value = symbol					  ; GET FULL PATH
  symbol = symbol(0) & pathn = pathn(0)
  symbol = strtrim(strcompress(symbol),2) & pathn = strtrim(strcompress(pathn),2)
      IF (symbol eq '' or pathn eq '') then begin                                 ; ONE FIELD NOT FILLED ? 
         widget_control,comment,set_value = $
            'COMPLETE fields DATABASE and PATH before adding...' 
         return
      ENDIF


      symbol  = strtrim(strmid(symbol,0,strlen(symbol)),2)
      pathn   = strtrim(strmid(pathn ,0,strlen(pathn)),2)      

      ex1 = 0 & ex2 = 0 & ipos = 0
      FOR i1  = 1,n_elements(ttpath)-1 DO begin                                   ; DATABASE ALREADY EXISTS ?
         IF strupcase(ttsymbol(i1))eq strupcase(symbol) then begin
                ex1 = 1 & ipos = i1
                IF strupcase(ttpath(i1))eq strupcase(pathn) THEN ex2 = 1
         ENDIF
      ENDFOR

      IF (strlen(symbol) lt 14) then symbol = symbol+$
           string(replicate(32b,14 -strlen(symbol)))                              ; COMPLETE DATABASE -> 20 CAR.  
      IF (strlen(symbol) ge 14) then symbol = strmid(symbol,0,14)


      IF ex1 eq 0 and ex2 eq 0  then begin                                        ; DATABASE NOT EXISTS ?
          widget_control,comment,set_value = symbol+'...Added...'          
          ttdata   = symbol+'----> '+pathn
          lirpath  = [lirpath,ttdata]                                             ; INCREASE ARRAYS
          ttpath   = [ttpath,strtrim(pathn,2)]
          ttsymbol = [ttsymbol,strtrim(symbol,2)]
      widget_control,listp,set_value   = strmid(lirpath(0:*),0,50)                ; SHOW NEW LIST
      widget_control,listp,set_list_top = n_elements(lirpath)-1                   ; TOP LIST WITH NEW
      endif

      IF ex1 eq 1  then begin                                                     ; DATABASE EXISTS BUT PATH MODIFIED
          widget_control,comment,set_value = symbol+'...Modified...'          
          lirpath (ipos)= symbol+'----> '+pathn                                   ; MODIFY ARRAYS
          ttpath(ipos)  = strtrim(pathn,2)
          ttsymbol(ipos)= strtrim(symbol,2)
      widget_control,listp,set_value   = strmid(lirpath(0:*),0,50)                ; SHOW NEW LIST
      widget_control,listp,set_list_select = ipos                                 ; HIGHTLIGHT NEW
      endif

      widget_control,newpath,set_value = ''
      widget_control,mnemo,  set_value = ''

ENDIF

;---------------------- BUTTON DONE (WRITES FUNCTION READ_PAR.PRO)----------------------------------------------------------
IF (uv eq 'DONE')  then begin;             

  valid = 0
  on_ioerror, err_write                                                           ;  ERROR ?

  openw,in ,pth+'read_par.pro',/get_lun                                        ;  WRITE NEW FILE
  Printf,in,"FUNCTION READ_PAR , inst ,path, filename, status, datp"
  Printf,in,';-----------------------------------------------------'
  Printf,in,''
  Printf,in,'CASE inst OF ';              <   
  Printf,in,''
  IF (n_elements(ttinst)gt 1) then begin                                          ;  WRITE "CASES OF" DATATYPES,PROCEDURES ....      
    FOR i = 1,n_elements(ttinst)-1 DO begin
     IF strpos(ttinst(i),'.') le 0 then begin
      n=fix(2-strlen(ttproc(i))/8) 
      if n eq 0 then n=1
      bl=string(replicate(9b,n))

      n=fix(1-strlen(ttinst(i)+ttgroup(i))/8)
      if n gt 0 then bl1=string(replicate(9b,n)) else bl1='' 
      Printf,in,"'",ttinst(i),"'",string(9b),": ","RETURN,", ttproc(i),bl,"(['",ttinst(i),"','",ttgroup(i),"']",bl1,",path,filename,status,datp)"
     ENDIF
    ENDFOR
  ENDIF
  Printf,in,"'init'",string(9b),": BEGIN  Status=0"                                         ;  WRITE "CASE OF INIT"........
  Printf,in,''
;.........................................................................................................                                                                                  
  IF (n_elements(ttinst)eq 1) then Printf,in,"  ttinst   = ['demo']'",string(replicate(9b,4)),";exec"        ;  Concatenates DATATYPES
  IF (n_elements(ttinst)ge 1) then begin ;         
      FOR i = 1,n_elements(ttinst)-1 DO begin  
          IF( i eq 1) then Printf,in,"  ttinst   = ['",ttinst(i),"']",string(replicate(9b,2)),";exec"  $
             else Printf,in,"  ttinst   = [ttinst,","'",ttinst(i),"']",string(9b),";exec"
      ENDFOR
  ENDIF
;.........................................................................................................                                                                                  
  Printf,in,''
  IF (n_elements(ttinst)eq 1) then Printf,in,"  ttproc   = ['read_tmp']",string(9b),";exec";  Concatenates PROCEDURES
  IF( n_elements(ttproc)ge 1) then begin
      FOR i = 1,n_elements(ttproc)-1 DO begin                 
        IF(i eq 1)then Printf,in,"  ttproc   = ['",ttproc(i),"']",string(9b),";exec" $
          else Printf,in,"  ttproc   = [ttproc,","'",ttproc(i),"']",string(9b),";exec"                             
      ENDFOR
  ENDIF
;.........................................................................................................                                                                                  
  Printf,in,''
  IF (n_elements(ttinst)eq 1) then Printf,in,"  ttgroup  = [' ']",string(replicate(9b,2)),";exec"            ;  Concatenates GROUPS
  IF (n_elements(ttgroup)ge 1) then begin 
      FOR i = 1,n_elements(ttgroup)-1 DO begin                    
        IF(i eq 1)then Printf,in,"  ttgroup  = ['",ttgroup(i),"']",string(replicate(9b,2)),";exec" $
          else Printf,in,"  ttgroup  = [ttgroup,","'",ttgroup(i),"']",string(9b),";exec"
      ENDFOR
  ENDIF
;.........................................................................................................                                                                                  
  Printf,in,''
      Printf,in,"  ttsymbol = ['Current Path']",string(replicate(9b,3)),";exec"                              ;  Concatenates SYMBOLS
  IF (n_elements(ttsymbol)ge 2) then begin
      FOR i = 2,n_elements(ttsymbol)-1 DO begin
      if strlen(ttsymbol(i)) ge 14 then tabul=string(9b) else tabul=string(replicate(9b,2))
            Printf,in,"  ttsymbol = [ttsymbol,","'",ttsymbol(i),"']",tabul,";exec"     
      ENDFOR
  ENDIF
;.........................................................................................................                                                                                  
  Printf,in,''
      Printf,in,"  ttpath   = ['.']",string(replicate(9b,4)),";exec"                                         ;  Concatenates PATHS
  IF (n_elements(ttpath)ge 2) then begin
      FOR i = 2,n_elements(ttpath)-1 DO begin
      if strlen(ttpath(i)) ge 10 then tabul=string(replicate(9b,2)) else tabul=string(replicate(9b,3))
            Printf,in,"  ttpath   = [ttpath,","'",ttpath(i),"']",tabul,";exec"                             
      ENDFOR
  ENDIF
;.........................................................................................................                                                                                  
  widget_control,touch,get_value=touch_v                                          ;  Get TOUCH_BASE location (if empty then default)     
  touch_v = touch_v(0)
  if(touch_v eq '')then touch_v="/home/cs/TOUCH_BASE"
  touch_v = strtrim(strcompress(touch_v))
  Printf,in,''                                             
      ;if strlen(touch_v) ge 16 then tabul=string(replicate(9b,3)) else tabul=string(replicate(9b,2))
       Printf,in,"  ttouch   = '"+touch_v+"' ;exec"              
;.........................................................................................................                                                                                  
  widget_control,macros,get_value=macro_v                                         ;  Get USER_MACROS location (if empty then default)  
  macro_v = macro_v(0)
  macro_v = strtrim(strcompress(macro_v),2)        
  Printf,in,'';                                                    
      
     ;if strlen(macro_v) gt 16 then tabul=string(replicate(9b,2)) else tabul=string(replicate(9b,3)) 
      Printf,in,"  ttmacro  = '"+macro_v+"' ;exec"            
;.........................................................................................................                                                                                  
  widget_control,access,get_value=access_v                                        ;  Get DATA_ACCESS location (if empty then default)   
  access_v  = access_v(0)
  access_v  = strlowcase(access_v)                                                ;  SET IN LOWERCASE
      blanc = strpos(access_v,'.pro');                    
      IF (blanc ne -1) then begin
         access_v = strtrim(strmid(access_v,0,blanc),2)                           ;  REMOVE ".PRO"  
      ENDIF
  access_v = strtrim(strcompress(access_v))       
  Printf,in,'';                                                    
      if strlen(access_v) gt 16 then tabul=string(replicate(9b,2)) else tabul=string(replicate(9b,4)) 
      Printf,in,"  ttaccess = '",access_v,"'",tabul,";exec"            
;.........................................................................................................                                                                                  
  widget_control,site,get_value = site_v                                          ;  Get SITE_DISPLAY function (if empty then default)  
  site_v = site_v(0)
  site_v = strlowcase(site_v)                                                     ;  SET IN LOWERCASE  
  if(site_v eq '')then site_v = " "
      blanc = strpos(site_v,'.pro')                                               ;  REMOVE ".PRO"                   
      IF (blanc ne -1) then begin
         site_v = strtrim(strmid(site_v,0,blanc),2)                               
      ENDIF
  if(site_v ne ' ')then site_v =  strtrim(strcompress(site_v))                          
  Printf,in,'';                                                    
 
      if strlen(site_v) gt 12 then tabul=string(replicate(9b,2)) else tabul=string(replicate(9b,4)) 
      Printf,in,"  ttsite   = '",site_v,"'",tabul,";exec"            
  Printf,in,''; 
;.........................................................................................................                                                                                  
  widget_control,magn,get_value = magic
  magic  = strtrim(magic(0),2)
  if (magic lt '2') or (magic gt '9') then magic='6'
      Printf,in,"  ttmagi   = '",magic ,"' ",";exec" 
      Printf,in,''      
;.........................................................................................................                                                                                  
  widget_control,wall,get_value = fwall
  fwall  = strtrim(fwall(0),2)
      Printf,in,"  ttwall   = '",fwall ,"' ",";exec" 
      Printf,in,''      
;.........................................................................................................                                                                                  
  widget_control,npar,get_value =   npars
  on_ioerror,mispar   & npars =long(npars(0)) & mispar: on_ioerror,err_write
  npars=npars>40<10000 & npars=strtrim(string(npars),2)
      Printf,in,"  ttpars   = '",npars ,"' ",";exec" 
      Printf,in,''      
;.........................................................................................................                                                                                  
      Printf,in,"  datp     = {a:ttinst,  b:ttproc,  c:ttgroup,  $"               ; CREATE STRUCTURE DATP
      Printf,in,"              d:ttsymbol,e:ttpath,  f:ttouch,   $"
      Printf,in,"              g:ttmacro, h:ttaccess,i:ttsite,j:ttmagi,k:ttwall,l:ttpars}"
;.........................................................................................................                                                                                  
  Printf,in,''
  printf,in,"  return,0"
  Printf,in,''
  Printf,in,'         END'
  Printf,in,''
  Printf,in,'ELSE :'
  Printf,in,''
  Printf,in,'ENDCASE'
  Printf,in,''
  Printf,in,"Status = 14"
  Printf,in,''
  Printf,in,'return,0'
  Printf,in,''
  Printf,in,'END'
  Printf,in,''
  valid = 1
  FREE_LUN,in                                                                     ; END WRITE READ_PAR.PRO.......... 
  
  if actif eq 1 then begin P_NEWCUST, /fromcust
  			   n=n_elements(ttinst)
;  			   if n gt potn then i=sys_dep      ('POT','',n-1)
			   potn=n & endif
 
   err_write: if (valid eq 0) then widget_control,comment,set_value=$              ; TEST OF WRITING:
                pth+"READ_PAR.PRO : unable to write !!!"
  if(valid eq 1)then WIDGET_CONTROL,event.top,/DESTROY
  
ENDIF

END
;===============================================================================================================
;                                 END OF EVENT
;===============================================================================================================






;***************************************************************************************************************
;                                 BEGIN CUSTOMIZ
;***************************************************************************************************************

PRO CUSTOMIZ ,inter

@lamp.cbk
common chr1 ;comment,listd,listp,newdata,newproc,newpath,mnemo,lindex,lindex1 $
            ;last_list,lirinst,lirpath,ttinst,ttgroup,ttproc,ttpath,ttsymbol  $
            ;ttouch,ttmacro,touch,macros,access,site,actif,pth,base0,potn,magn,npar,wall
common did1 ;WoR,worb,wort,txtr,txtw,deffont

if xregistered('CUSTOMIZ') gt 0 then widget_control,bad_id=i,base0,map=1 $
else begin

actif = n_elements(inter)
if n_elements(lamp_asite) gt 0 then if lamp_asite eq 'customiz' then actif=1
if n_elements(lamp_siz)   eq 0 then lamp_siz=800

if(actif ne 0) then pth=sys_dep      ('NEWSUB',lamp_dir,'lamp_mac') else pth=''
if(actif ne 0) then deffont=ft_propor

lirinst   =  ' '                                                
lirpath   =  ' '
ttinst    = [' ','demo']
ttgroup   = [' ',' ']
ttproc    = [' ','read_tmp']
ttsymbol  = [' ','Current Path']
ttpath    = [' ','.']                                                             ; CURRENT PATH IS SET....
ttouch    =  ' '
if sys_dep("MACHINE") eq "unix" then ttouch = '/home/cs/TOUCH_BASE'            ; DEFAULT TOUCH_BASE IS SET 
if sys_dep("MACHINE") eq "vms"  then ttouch = 'dka0:[lamp.demo.TOUCH_BASE]'
ttmacro   =  ''  	                                                    ; DEFAULT MACROS IS SET
if sys_dep("MACHINE") eq "unix" then ttmacro= '~lambda/macros'
if sys_dep("MACHINE") eq "vms"  then ttmacro= 'dka0:[macros]'
if sys_dep("MACHINE") eq "mac"  then ttmacro= 'disk:macros'
if sys_dep("MACHINE") eq "win"  then ttmacro= 'c:\lambda\macros'
ttaccess  =  'rdfilter'
WoR       = ['Read  by','Write by'] & wort=0
ttsite    =  'language_help'
ttmagi	  =  '6'
ttwall	  =  ''
ttpars	  =  '120'

last_list =  0                                                                    ; LAST LIST SELECTED
lindex    = -1                                                                    ; INDEX NEVER TOUCHED
lindex1   = -1
valid     =  0                                                                    ; PARAMETER I/O ERRORS

;--------------------READ FILE READ_PAR.PRO --------------------------------------
 
t = findfile (pth + 'read_par.pro',count = exist)
ptth=pth
if (exist eq 0) then begin pth=!dir    +sys_dep("DIVIDER")
t = findfile (pth + 'read_par.pro',count = exist) & endif
if (exist eq 0) then begin status=23 & datp=0
		if ptth ne '' then begin  t = findfile (ptth + '*',count = cnt)
		                          if cnt gt 0 then pth=ptth & endif
                ii=execute('bid=read_par("init","","",status,datp)')
                if status eq 0 then begin ttinst  =datp.a & ttproc  =datp.b & ttgroup=datp.c
                                          ttsymbol=datp.d & ttpath  =datp.e & ttouch =datp.f
                                          ttmacro =datp.g & ttaccess=datp.h & ttsite =datp.i
                                          ttmagi  =datp.j & ttwall  =datp.k & ttpars =datp.l
endif     &     endif
valid=2
on_ioerror, no_file & in=-1						; FLAG ERROR
   OPENR,in,pth+'read_par.pro',/get_lun                                        ; OPEN...                              
	ligne=' '
   	on_ioerror, end_file
        WHILE (1) DO begin                                
            readf,in,ligne                                                        ; READ UNTIL END_OF_FILE...
            IF (strpos(ligne,';exec')   ge 0) THEN r=execute(ligne)               ; CAN TRANSLATE LINE IN ARRAY    
        ENDWHILE                                                                  ; END READ
        end_file:FREE_LUN,in                                                           ; CLOSE FILE

   valid=3
   
   on_ioerror, no_write & in=-1
   OPENW,in,pth+'read_par.pro',/APPEND,/get_lun
   	valid=1
   no_write:if in gt 0 then FREE_LUN,in

no_file:

IF (ttinst(0) ne ' ') then begin                                                  ; FIRST ELEMENT OF ARRAYS
       ttinst  = [' ',ttinst]                                                  ; IS ALWAYS A BLANK
       ttproc  = [' ',ttproc]
       ttgroup = [' ',ttgroup]
ENDIF

IF (ttpath(0) ne ' ') then begin                                                  ;   "   "    "                                         
       ttpath  = [' ',ttpath]                                               
       ttsymbol= [' ',ttsymbol]
ENDIF

if (ttouch(0)   ne ' ') then ttouch  = ttouch(0)                                  ;   "   "    "
if (ttmacro(0)  ne ' ') then ttmacro = ttmacro(0)
if (ttaccess(0) ne ' ') then ttacces = ttaccess(0)
if (ttsite(0)   ne ' ') then ttsite  = ttsite(0)

potn=n_elements(ttinst)
IF (potn ge 1) then begin                                           ; MAKES LIST OF DATATYPES
   FOR ij = 1,n_elements(ttinst)-1 DO begin
       l1 = strlen(ttinst(ij))
       l2 = strlen(ttgroup(ij))
       l3 = l1+l2+1
       if strpos(ttinst(ij),'.') ne strlen(ttinst(ij))-1 then rb='--Read_by-> ' else rb='--Write_by> '
       if l3 lt 12 then t1 = ttinst(ij)+' '+ttgroup(ij)+string(replicate(32b,12-(l3)))+$
                             rb+ttproc(ij) else t1 = ttinst(ij)+' '+ttgroup(ij)
       lirinst = [lirinst,t1]
   ENDFOR
ENDIF

IF (n_elements(ttpath) ge 1) then begin                                           ; MAKES LIST OF PATHNAMES
   FOR ij = 1,n_elements(ttpath)-1 DO begin
       l1 = strlen(ttsymbol(ij))
    if(l1 lt 14)then lirpath=[lirpath,ttsymbol(ij)+string(replicate(32b,14-l1))+$
          '----> '+ttpath(ij)]
    if(l1 eq 14)then lirpath=[lirpath,ttsymbol(ij)+'----> '+ttpath(ij)]
   ENDFOR
ENDIF


;-------------------TEXTS OF HELP------------------------------------------------------
explain1  = 'Enter label for datatype  to appear in LAMP menus in the field DATATYPE '   
explain1b = '      you   may enter  an optional  group   for datatype (label group)'   
explain2  = 'Enter name  of  procedure to read   datatype      in the field READ  BY'                                         
explain22 = 'Enter name  of  procedure to write  datatype      in the field WRITE BY'                                         
explain3  = 'Enter label for database  to appear in LAMP menus in the field DATABASE'
explain4  = 'Enter Path  to  database  to appear in LAMP menus in the field FULL PATH'

explain5  = 'Enter full path to the directory containing user macros and to the Catalog'
explain6  = 'Enter the name of procedure mapped to the Self... button '
explain7  = 'and one of the DISPLAY FUNCTIONS button'

;--------------------WIDGETS------------------------------------------------------------

base0        =  widget_base  (/column,title = 'Lamp  CUSTOMIZE',/frame,$
				      resource_name='lamptouch')

if actif ne 0 then widget_control,bad_id = ii,base0,default_font = deffont                       ; INSTALL FONT 

 base01      =  widget_base  (base0  ,/row)
  txt1       =  widget_label (base01 ,value = explain1)
 base01      =  widget_base  (base0  ,/row)
  txt1       =  widget_label (base01 ,value = explain1b)

 base01      =  widget_base  (base0  ,/row)
  txt2       =  widget_label (base01 ,value = explain2)
  template   =  widget_button(base01, value='(see  read_tmp)',uv='TEMPLATE')
 base01      =  widget_base  (base0  ,/row)
  txt2       =  widget_label (base01 ,value = explain22)
  template   =  widget_button(base01, value='(see write_tmp)',uv='TEMPLATE2')

 base01      =  widget_base  (base0  ,/row)
  txt3       =  widget_label (base01 ,value = explain3)  
 base01      =  widget_base  (base0  ,/row)
  txt4       =  widget_label (base01 ,value = explain4)  
  
base001      =  widget_base  (base0  ,/row)
  BUT_ABORT  =  widget_button(base001,value = 'ABORT' ,uvalue = 'ABORT')
		if (actif ne 0) then put_logo     ,base001
  BUT_DONE   =  widget_button(base001,value = 'APPLY ',uvalue = 'DONE',/frame)
  nul0       =  widget_label (base001,value = '   ')
  COMMENT    =  widget_label (base001,value = '   ',xsize=lamp_siz/2 + 30)
  nul0       =  widget_label (base001,value = ' ')

base1	     =  widget_base  (base0  ,/row)
  base11     =  widget_base  (base1  ,/column,/frame)
  LISTD      =  widget_list  (base11 ,value = lirinst,uvalue = 'INDEX_DATA',ysize = 7)
  base12     =  widget_base  (base1  ,/column,/frame)
  LISTP      =  widget_list  (base12 ,value = strmid(lirpath(0:*),0,50),$
                                           uvalue = 'INDEX_PATH',ysize = 7)
base2        =  widget_base  (base11 ,/row)
  n1         =  widget_label (base2  ,value = 'Datatype :')
  NEWDATA    =  widget_text  (base2  ,/editable,value = '',uvalue = 'CR',xsize = 10)         
  REM_DATA   =  widget_button(base2  ,value = 'Remove',uvalue = 'REM_DATA')
  ADD_DATA   =  widget_button(base2  ,value = 'Update',uvalue = 'ADD_DATA')
base21       =  widget_base  (base11 ,/row)
  worb       =  widget_label (base21 ,value = WoR(wort)+' :')
  NEWPROC    =  widget_text  (base21 ,/editable,value = '',uvalue = 'CR',xsize = 20)           
  toggle     =  widget_button(base21 ,value ='R<-->W', uvalue='WoRtoggle')

base3        =  widget_base  (base12 ,/row)
  n4         =  widget_label (base3  ,value = 'Database :')
  MNEMO      =  widget_text  (base3  ,/editable,value = '',xsize = 14,uvalue = 'CR')
  REM_PATH   =  widget_button(base3  ,value = 'Remove',uvalue = 'REM_PATH')
  ADD_PATH   =  widget_button(base3  ,value = 'Update',uvalue = 'ADD_PATH')

base31       =  widget_base  (base12 ,/row)
  n3         =  widget_label (base31 ,value = 'Full Path:')
  NEWPATH    =  widget_text  (base31 ,/editable,value = '',xsize = 40,uvalue = 'CR')

base350	     =  widget_base  (base0  ,/row)
bid	     =  widget_label (base350,value = 'Magic_number:')
magn	     =  widget_text  (base350,value =  ttmagi,xsize=4,ysize=1,/editable,uvalue = 'CR')
bid	     =  widget_label (base350,value = '(Char.lenght: accessing data by numor means'$
					     +' numor is in the filename)')
base350	     =  widget_base  (base0  ,/row)
bid	     =  widget_label (base350,value = 'FTP proxy (Firewall):')
wall	     =  widget_text  (base350,value =  ttwall,xsize=8,ysize=1,/editable,uvalue = 'CR')

bid	     =  widget_label (base350,value = '  -----  ')
bid	     =  widget_label (base350,value = 'Data Parameters maxi_length:')
npar	     =  widget_text  (base350,value =  ttpars,xsize=6,ysize=1,/editable,uvalue = 'CR')

base400      =  widget_base  (base0  ,/column,/frame)
base411      =  widget_base  (base400,/row)
  n24        =  widget_label (base411,value = 'User macros location:')
  MACROS     =  widget_text  (base411,/editable,xsize = 20,value = ttmacro(0),uvalue = 'CR')
  n25        =  widget_label (base411,value = 'TOUCH_BASE location :')
  TOUCH      =  widget_text  (base411,/editable,xsize = 20,value = ttouch(0) ,uvalue = 'CR')
  txt5       =  widget_label (base400,value = explain5)

base500      =  widget_base  (base0 ,/column,/frame)
base511      =  widget_base  (base500 ,/row)
  n26        =  widget_label (base511,value = 'Procedure for Self... button:')
  ACCESS     =  widget_text  (base511,/editable,xsize = 10,value = ttaccess(0),uvalue = 'CR')
  n27        =  widget_label (base511,value = 'Procedure for site display function:')
  SITE       =  widget_text  (base511,/editable,xsize = 10,value = ttsite(0),uvalue   = 'CR')

  txt6       =  widget_label (base500,value = explain6)
  txt7       =  widget_label (base500,value = explain7)

   		   if actif ne 0 then bid=sys_dep      ('DYNLAB',BASE0,0)
		   WIDGET_CONTROL,BASE0,/REALIZE & if (actif ne 0) then put_logo
leader=0
if  n_elements(lamp_b1) eq 1 then leader=lamp_b1
if (actif ne 0) and (leader gt 0) then WIDGET_CONTROL,BASE0,GROUP_LEADER=leader

one_error:
IF (valid eq 0)THEN widget_control,comment,set_value = 'problem reading '+pth+'READ_PAR.PRO'
IF(valid eq 1) THEN widget_control,comment,set_value = 'OK Reading '     +pth+'READ_PAR.PRO'
IF(valid eq 2) THEN widget_control,comment,set_value = pth+'READ_PAR.PRO is a new file....'
IF(valid eq 3) THEN widget_control,comment,set_value = pth+'READ_PAR.PRO access denied !!!'

XMANAGER,'CUSTOMIZ', BASE0,EVENT_HANDLER = 'CHRIS_EVENT',/just_reg

txtw=[$
"pro write_tmp, FileName , Data , XC=x, YC=y , ZC=z ,E=e , N=n      $",$
"                               , PR=p, PV=pv, PAR_TXT=p_txt        $",$
"                               , W_tit=wt   , X_tit=xt  , Y_TIT=yt $",$
"                               , Z_tit=zt   , OTHER_TIT=ot",$
";** *********",$
";**",$
";**	Standard call for a data-write procedure called by LAMP.",$
" ",$
";**	Keywords:",$
";**           XC       = [vector of x coordinates.]",$
";**           YC       = [vector of y coordinates.]",$
";**           ZC       = [vector of z coordinates.]",$
";**           W_TIT    =   main title",$
";**           X_TIT    = x axis title",$
";**           Y_TIT    = y axis title",$
";**           Z_TIT    = z axis title",$
";**           OTHER_TIT=    sub title",$
";**           N        = monitors",$
";**           PR       = vector of parameter values",$
";**           PAR_TXT  = string array of text associated to PR (same size)",$
";**           PV       = an array of any dimensions containing other parameter values",$
";**           E        = the errors associated to DATA (same size) or 0",$
" ",$
"CATCH,stat & IF stat ne 0 then begin print,!err_string & return & endif",$
"ON_IOERROR,mis",$
" ",$
"OPENW ,unit, FileName, /get_lun",$
" ",$
"WRITEU,unit, Data    ;Write the Data (and parameters in the way you want).",$
" ",$
"FREE_LUN,unit",$
"return",$
" ",$
"mis:print,!err_string",$
"end"]

txtr=[$
"function read_tmp, INST , PATH , FILENAME , STATUS , DATP",$
";******* ********",$
";**",$
";**	Standard call for a data-read function interfacing LAMP.",$
"",$
";**	Return of the function",$
";**	 DATA     is an array of any dimensions and type containing the data values (spectra).",$
"",$
";**	Input  parameters:",$
";**	 INST(0)  is the file_type  (or instrument_name ) (string defined in customize tables).",$
";**	 INST(1)  is the file_group (or instrument_group) (string defined in customize tables).",$
";**	 INST(2)  is '1' if raw  button is set.",$
";**	 PATH     is the full path where to find the data (string defined in customize tables).",$
";**	 FILENAME is the name of the data file.",$
";**             if FILENAME(1) exists, this is the requested image number in the file",$
";**                                    entered as  file.ext{3} (for image 3).",$
";**	Output parameters:",$
";**	 STATUS   is the returned error code you can choose from the following list:",$
";**           0 =' Successfull read'",$
";**           1 =' Client/server on local  node not established'",$
";**           2 =' Client/server on router node not established'",$
";**           3 =' The local  node cannot access the server node'",$
";**           4 =' The router node cannot access the server node'",$
";**           5 =' VME memory read error'",$
";**           7 =' Sequence error in data transfer'",$
";**           9 =' Parameter error'",$
";**           10=' Router is busy with other transfer'",$
";**           11=' Cant open the file or file not found'",$
";**           13=' Data file incomplete'",$
";**           14=' Bad instrument data definition'",$
";**           24=' Cant read the file'.",$
";**",$
";**	 DATP     is a structure defined as follow: (all tags are OPTIONAL)",$
";**           DATP.X        = vector of x coordinates.",$
";**           DATP.Y        = vector of y coordinates.",$
";**           DATP.Z        = vector of z coordinates.",$
";**           DATP.W_TIT    =   main title",$
";**           DATP.X_TIT    = x axis title",$
";**           DATP.Y_TIT    = y axis title",$
";**           DATP.Z_TIT    = z axis title",$
";**           DATP.OTHER_TIT=    sub title",$
";**           DATP.N        = monitors",$
";**           DATP.P        = vector of parameter values, max dim is defined in customize.",$
";**           DATP.PAR_TXT  = string array of text associated to DATP.P (same size)",$
";**           DATP.PV       = an array of any dimensions containing other parameter values",$
";**           DATP.E        = the errors associated to DATA (same size)",$
";**           DATP.TIME     = string date of the experiment.",$
"",$
" DATA  =0",$
" STATUS=7",$
" CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif",$
"",$
" ON_IOERROR, no_file",$
" OPENR,unit, PATH+FILENAME,/get_lun                   ;Open the data file",$
"",$
"      ON_IOERROR, read_err",$
"      STATUS=13",$
"      DATA  =FLTARR(40,40)                            ;Make a floating array",$
"      READU,unit,DATA                                 ;Read the data",$
"      STATUS=0                                        ;Status is ok",$
";     ********",$
"          xv       =  INDGEN(40)*2 +15                ;Make X coordinates",$
"          wt       =' Template_read test '",$
"          xt       =' This is the X axis '",$
"          par      = [        3.0       ,        5.5        ]",$
"          ptxt     = ['First  parameter','Second parameter' ]",$
"          pall     =  INDGEN(40,6)",$
"",$
"      DATP={X:      xv,    $                          ;Pass into the DATP structure",$
"            W_TIT:  wt,    $                          ;those variables which were read-in",$
"            X_TIT:  xt,    $",$
"            P:      par,   $",$
"            PAR_TXT:ptxt,  $",$
"            PV:     pall   }",$
";     **********************",$
"",$
" read_err:  FREE_LUN,unit                             ;Free the unit number",$
" no_file:",$
"",$
" RETURN, DATA                                         ;Return the data values",$
";************",$
" END"]

IF ACTIF EQ 0 THEN XMANAGER
endelse
return
END
;***************************************************************************************************************
;                                       END CUSTOMIZ 
;***************************************************************************************************************
