;**************************
;HOW TO RUN THIS DIAL !!!
;------------------------
;START idl THEN ENTER:  
;Idl>.run dial_d22kin
;Idl> d22kin    
;**************************


pro d22kin
;** *********
;Used to run with "byGeorge"
FORWARD_FUNCTION DialControl, DialNewValue, DialOn
if strpos(!path,'/home/cs/lambda/macros') lt 0 then $
    !path=!path+':'+expand_path('+/home/cs/lambda/macros')

dial_bygeorge,'d22kin'   ;<-------- Name of your Dial !!!
END                      ;          ~~~~~~~~~~~~~~~~~ !!!



pro d22kin_gui_event,ev
;** ************
;**

widget_control,ev.id,get_uvalue=uv
case uv(0) of

;*****CONSTANT*****
1:begin call_procedure,'d22kin_const'

  end

;*****LOGARITHMIC*****
2:begin call_procedure,'d22kin_log'
	
  end

;*****PERSONEL*****
3:begin call_procedure,'d22kin_pers'

  end

;*****ADDITION*****
4:begin call_procedure,'d22kin_add'
	
  end

;*****READING FILE*****
5:begin call_procedure,'d22kin_read'
	
  end

;*****EXIT*****
6:begin widget_control,ev.top,/destroy
	DialClear, 'd22kin'
;	DialClear, 'd22kin_const'
;	DialClear, 'd22kin_log'
;	DialClear, 'd22kin_pers'
;	DialClear, 'd22kin_add'
;	DialClear, 'd22kin_read'
	
  end

;*****HELP*****
7:begin ;xdisplayfile,'.hlp',title='D22 KIN help',group=ev.top,height=20,width=65
	
  end

else:

endcase

end

pro d22kin_gui
;** ******
;**

if xregistered('d22kin_gui') eq 0 then begin

base = widget_base (title='D22 KIN',/column,frame=5)

base0 = widget_base   (base,/column,frame=5)

base1 = widget_base   (base0,/row)
const = widget_button (base1,value='Do',uvalue=1)
label = widget_label  (base1,value='Constant acquisition time ')

base2 = widget_base   (base0,/row)
log   = widget_button (base2,value='Do',uvalue=2)
label = widget_label  (base2,value='Logarithmic acquisition time ')

base3 = widget_base   (base0,/row)
pers  = widget_button (base3,value='Do',uvalue=3)
label = widget_label  (base3,value='Personal acquisition time ')

base4 = widget_base   (base0,/row)
add   = widget_button (base4,value='Do',uvalue=4)
label = widget_label  (base4,value='Concatenation of files ')

base5 = widget_base   (base0,/row)
rf    = widget_button (base5,value='Do',uvalue=5)
label = widget_label  (base5,value='Reading file ')

label = widget_label  (base,value='For information :')
label = widget_label  (base,value='-Time is converted in ticks (1 tick=90.9 ns)')
label = widget_label  (base,value='-Max preset per frame is 390.4 s(=2^32*90.9)',/align_left)
label = widget_label  (base,value='-The max number of frames is 450',/align_left)

base6 = widget_base   (base,/row)
exit  = widget_button (base6,value='Exit',uvalue=6)
help  = widget_button (base6,value='Help',uvalue=7)

widget_control,base,/realize
xmanager,'d22kin_gui',base,/just_reg
endif
end

pro dial_d22kin_macro, D
;** ********************
;**

if D.init eq 0 then begin
	D.init=1
	D.frequency=0.
	d22kin_gui

endif else print,'ok'

end

function dial_d22kin
;******* **************
;**

return, {init:0}
end
