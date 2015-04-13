;                  FILE : DESCRIPT.PRO
;                 --------------------           

;PARAMETERS RECIEVED ---->    NAMEFILE, TBL (lonarr(8))
                                                           
;tbl(0) = x
;tbl(1) = y
;tbl(2) = z
;tbl(3) = type
;2        1  Byte               
;4        2  Integer 2          
;5       -2  Integer 2 unsigned 
;16       3  Integer 4 long      
;8        4  Floating point    
;32       5  complex             
;64       6  double precision   
;tbl(4) = format
;5        0  unformatted fortran       
;3        1  stream Vms binary          
;2        2  Tiff gp uncompressed
;6        3  CCP4                     (unsensitive)
;0        4  stream unix, fixed vms
;7        5  Mar image plate
;1        6  Formatted ascii
;4        7  Formatted ix,iy,value    (unsensitive)
;8        8  Formatted value,ix,iy,iz (unsensitive)
;tbl(5) = swap
;         0   no swap
;         1   swap
;tbl(6) = record_size
;tbl(7) = start_record or Byte offset

;PARAMETERS RETURNED in P_ICK_RETURN : ok,namefile,tbl 
;                                      ok = 1 open file 
;                                      ok = 0 CANCEL)

;****************************************************************************
PRO DESCRIPT_EVENT, event
;****************************************************************************

common widg3,wstrec,getfil,xs,ys,zs,w_typ,w_swap,w_form,rsize,strec,apply,$
            typfil,formdata,rs_typfil,rs_formdata,ids_type,ids_form,ids_swap,$
            button_type,button_form,comment,val_type,val_form,base0
            
common values,namefile,tbl

		 stat=0 & catch,stat
	         if stat  ne 0  then begin catch,/cancel
	         		widget_control,bad_id=i,comment,set_value=strmid(!err_string,0,50)
	         		return & endif
	         		
widget_control,event.id,get_uvalue  =  uv               ; WIDGET UVALUE 

IF n_elements(uv) gt 1 then IF uv(1) eq 391 then begin  p_did_mvlog, event,uv
							return & endif
;-----------------------------------------------------------------------
;                     BUTTONS "FORMATS"
;-----------------------------------------------------------------------
if(uv eq 'BUTTON_FORM')then begin               
   tbl(4)  =  val_form(event.value)
   MOD_TITTLE,tbl(4)
endif
;------------------------------------------------------------------------
;                     BUTTONS type 
;------------------------------------------------------------------------
if(uv eq 'BUTTON_TYPE')then begin
   tbl(3) = val_type(event.value)
endif
;------------------------------------------------------------------------
;                     BUTTON  BYTE SWAP
;------------------------------------------------------------------------
if(uv eq 'SWAP') then begin
   widget_control,w_swap(0),get_value = swap
   tbl(5) = swap
endif         
;------------------------------------------------------------------------
;                    <CARRIAGE RETURN> IN FIELD NAMEFILE ?
;------------------------------------------------------------------------
if(uv eq 'CR') then return
;-------------------------------------------------------------------------
;                     BUTTON CANCEL
;-------------------------------------------------------------------------
if(uv eq 'CANCEL') then begin   
   widget_control,event.top,/destroy                                            
   P_ICK_RETURN,0,namefile,tbl
   return
endif
;-------------------------------------------------------------------------
;                     BUTTON READ
;-------------------------------------------------------------------------
if(uv eq 'READ')then begin;         
     widget_control,getfil,get_value = namefile         ; GET NAMEFILE
     namefile = strcompress(namefile(0))                                  
     VERIF_NAME,namefile,exist                          ; File exist and not blank ?
     if exist eq 0 then return
;........................ GET XSIZE
     widget_control,xs,get_value = ix                   ; GET XSIZE
     ix = ix(0)                                     
     ix = strtrim(ix(0),2)
     if(ix eq '')then ix ='1'                           ; Blank ?
     field = 'Dim X' & good = 0
     VERIF_CHAR,ix,field,good                           ; Alphabetic in filed ?
     if good eq 1 then return
     tbl(0) = long(ix)
;........................ GET YSIZE
     widget_control,ys,get_value = iy                   ; GET YSIZE                                   
     iy = iy(0)
     iy = strtrim(iy(0),2)
     if(iy eq '')then iy = '1'                          ; Blank ?                                                                                         
     field = 'Dim Y' & good = 0                            
     VERIF_CHAR,iy,field,good                           ; Alphabetic in filed ?
     if good eq 1 then return
     tbl(1) = long(iy)
;........................ GET ZSIZE
     widget_control,zs,get_value = iz                   ; GET ZSIZE                                   
     iz = iz(0)
     iz = strtrim(iz(0),2)
     if(iz eq '')then iz = '1';                         ; Blank ?                                              
     field = 'Dim Z' & good = 0    
     VERIF_CHAR,iz,field,good                           ; Alphabetic in filed ?
     if good eq 1 then return
     tbl(2) = long(iz)
;.........................GET RECORD SIZE 
  widget_control,rsize,get_value = rs                   ; GET RECORD SIZE                                     
  rs = strtrim(rs(0),2)
  if(rs eq '')then rs = '0';                            ; Blank ?                                                                                             
     field = 'Record' & good =0                         
     VERIF_CHAR,rs,field,good                           ; Alphabetic in filed ?
     if good eq 1 then return
  tbl(6) = long(rs)                  
;........................ GET START RECORD 
  widget_control,strec,get_value = st                   ; GET START RECORD                                    
  st = strtrim(st(0),2)
  if(st eq '')then st = '1'                             ; Blank ?                                           
     field = 'Offset' & good =0
     VERIF_CHAR,st,field,good                           ; Alphabetic in filed ?
     if good eq 1 then return
     tbl(7) = long(st)

;......................... ALL IS GOOD ........ THEN
  widget_control,event.top,/destroy                     ; destroy base
  P_ICK_RETURN,1,namefile,tbl                          ; call p_ick_return
  return

ENDIF

END
;*********************************************************************************************************
;                           END EVENT PROCEDURE
;*********************************************************************************************************



;^^^^^^^^^^^^^^^^^^^^^^^^^^ CONTROL PROCEDURES ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PRO VERIF_NAME,name,exist           ;EXIST FILE AND NOT BLANK ?
common widg3
if(name eq '')then begin;                                       
   widget_control,comment,set_value='fill up name of file area...'
   return
endif
b = findfile(name,count = exist);                                     
if(exist eq 0)then widget_control,comment,set_value = '****** FILE DOES NOT EXIST...... ' else $
                   widget_control,comment,set_value = '****** FILE EXISTS................'
return
end
;-----------------------------------------------------------------------------
PRO VERIF_CHAR,name,field,good      ;ALPHABETICS CHARACTERS IN NUMERICS FIELDS ?
common widg3
fl='!!!...Non Numeric character in '+field+' area...!!!'
  for i1 = 0,strlen(name)-1 do begin                                
       car = (strmid(name,i1,1))
       if ((car ge 'A') and (car le 'Z')) or ((car ge 'a') and (car le 'z')) then begin
          widget_control,comment,set_value=fl
          good=1
          return
       endif            
  endfor
  return
end
;-----------------------------------------------------------------------------
PRO MOD_TITTLE,num
common widg3
if (num ne 5) and (num ne 2) then $
   widget_control,wstrec,set_value = 'Byte offset 1->n:' else $
   widget_control,wstrec,set_value = 'Starting record'
   
   if (num eq 0) 		then widget_control,bad_id=i,rsize,set_value='0'
   if (num eq 2) or (num eq 3)  then widget_control,bad_id=i,rsize,set_value='512'
   if (num eq 5) 		then widget_control,bad_id=i,rsize,set_value='-1'
   if (num eq 6) 		then widget_control,bad_id=i,rsize,set_value='1024'
   if (num eq 7) then begin	     widget_control,bad_id=i,xs   ,set_value='1200'
   				     widget_control,bad_id=i,ys   ,set_value='1200'
   				     widget_control,bad_id=i,zs   ,set_value='1'
   				     widget_control,bad_id=i,rsize,set_value='0'
   				     widget_control,bad_id=i,strec,set_value='2401'
   				     tbl(3)=5 & tbl(4)=0  &  endif
return
end
;^^^^^^^^^^^^^^^^^^^^^^^^^^ END CONTROL PROCEDURES ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^






;**********************************************************************************************************
PRO DESCRIPT,xnamefile,xtbl
;**********************************************************************************************************
@lamp.cbk
common widg3
common values
                              
if xregistered('descript') gt 0 then widget_control,bad_id=i,base0,map=1 $
else begin


namefile=xnamefile; 
tbl=xtbl

typfil   = strarr(7)
typfil   = ['Byte','Integer 2','Integer 2 unsigned','Integer 4 long',$
'Floating_point','Double_precision floating','Complex floating']
ids_type = intarr(7) ;                                                       ID type
but_type = intarr(7) ;                                                       
val_type = [2,4,5,16,8,32,64];                                               Values returned by type 
val_form = [5,3,2,6,0,7,1,4,8];						     Values returned by format
formdata = strarr(9)
formdata = ['Unformatted Fortran','Stream VMS binary','Tiff g.p Uncompressed ',$
'Ccp4 (.map binary)','Stream Unix , Fixed VMS','Mar image plate','Formatted Ascii',$
'Formatted ix,iy,value','Formatted val,ix,iy,iz']

ids_form = intarr(9) ;                                                       ID form
ids_swap = intarr(1) ;                                                       ID swap

;^^^^^^^^^^^^^^^^^^^^^^^^^ WIDGETS DESCRIPTION ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

base0   = widget_base (/column,title = 'Data description',resource='lampdid')

widget_control,bad_id = ii,base0,default_font = ft_propor;                  Install FONT 

bid	= widget_base (base0 ,/row)
	  put_logo    ,bid
COMMENT = widget_label(bid   ,value =string(replicate(32b,50)) )

base01  = widget_base (base0 ,/row)
chf     = widget_label(base01,value = 'File name :')
GETFIL  = widget_text (base01,/editable,value = strcompress(xnamefile),xsize = 45,uvalue = 'CR')

base02  = widget_base (base0 ,/row)
bid     = widget_label(base02,value = 'Dimensions:   X=')
XS      = widget_text (base02,/editable,value = strcompress(string(tbl(0))),xsize = 5,uvalue = 'CR')
bid     = widget_label(base02,value = 'Y=')
YS      = widget_text (base02,/editable,value = strcompress(string(tbl(1))),xsize = 5,uvalue = 'CR')
bid     = widget_label(base02,value = 'Z=')
ZS      = widget_text (base02,/editable,value = strcompress(string(tbl(2))),xsize = 5,uvalue = 'CR')

base03  = widget_base (base0, column = 2)

base031 = widget_base (base03 , /column,/frame)
W_TYP   = cw_bgroup   (base031, /column,/exclusive,typfil,uvalue = 'BUTTON_TYPE',$
				/no_release,ids = ids_type,label_top='DATA Type')

base032 = widget_base (base031, /column)
W_SWAP  = cw_bgroup   (base032, /row,/nonexclusive,'swap byte for Integers',$
					    ids = ids_swap,uvalue = 'SWAP',/frame)

base033 = widget_base (base03 , /column,/frame)
W_FORM  = cw_bgroup   (base033, /column,/exclusive,formdata,uvalue = 'BUTTON_FORM',$
				/no_release,ids = ids_form,label_top='DATA Format')


base4   = widget_base (base0,/row)
wstrec  = widget_label(base4, value = 'Byte offset 1->n:')
STREC   = widget_text (base4,/editable,xsize = 5,uvalue = 'CR',value = strcompress(string(tbl(7))))
w_rsize = widget_label(base4,value  = '[byte_recl](vms)')
RSIZE   = widget_text (base4,/editable,xsize = 5,uvalue = 'CR',value = strcompress(string(tbl(6))))

base41  = widget_base  (base0,/row)
CANCEL  = widget_button(base41,value = 'Cancel',uvalue = 'CANCEL')
bid     = widget_label (base41,value = 'or confirm characteristics pressing')
APPLY   = widget_button(base41,value = '    READ    ',uvalue = 'READ',/frame)

;^^^^^^^^^^^^^^^^^^^^^^^^^ END WIDGETS DESCRIPTION ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 

bid=sys_dep      ('DYNLAB',base0,0)
WIDGET_CONTROL,group_leader=lamp_b1,/REALIZE,BASE0 & put_logo;               CREATE WIDGETS

widget_control,ids_form(3),sensitive = 0;                                    SET UNSENSITIVE BUTTONS FORM
widget_control,ids_form(7),sensitive = 0
widget_control,ids_form(8),sensitive = 0

but_type = where(val_type eq tbl(3)) & but_type=but_type(0)>0 ;              Corresponding buttons type
but_form = where(val_form eq tbl(4)) & but_form=but_form(0)>0 ;              Corresponding buttons type

widget_control,ids_type(but_type),set_button = 1   ;                         Set Buttons pushed with form and type recieved
widget_control,ids_form(but_form),set_button = 1
widget_control,ids_swap(0),       set_button = tbl(5)

VERIF_NAME,namefile

MOD_TITTLE,but_form

xmanager,'descript', base0,event_handler='descript_event',/just_reg

endelse
return
END

