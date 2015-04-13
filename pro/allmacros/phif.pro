;pro phif


PRO fpol,dbns,dbs,fns,nfns,fs,nfs,trans,etrans,phi,ephi,f,ef,r,er

COMMON local,nparams,whatwid

dbns=float(dbns)
dbs=float(dbs)
fns=float(fns)
nfns=float(nfns)
fs=float(fs)
nfs=float(nfs)

print,dbns,dbs,fns,nfns,fs,nfs

;now calculate shim transmission

itrans=dbns/dbs
edbs=sqrt(dbs)/dbs
edbns=sqrt(dbns)/dbns
eitrans=itrans*sqrt(edbs^2+edbns^2)
efitrans=sqrt(edbs^2+edbns^2)/itrans

trans=1./itrans
etrans=trans*efitrans

; calculate phi
phi=nfns/(itrans*((nfs+fs))) 

enfns=sqrt(nfns)/nfns
eb=sqrt(nfs+fs)/(nfs+fs)

ephi=phi*sqrt(enfns^2+eb^2+efitrans^2)

;now calculate flipper efficiency

top=fns+nfns-(nfs+fs)*itrans

fetop=sqrt(fns+nfns+nfs+fs)/top

bot=2.*nfns-(nfs+fs)*itrans
febot=sqrt(2.*nfns+nfs+fs)/bot

f=top/bot
ef=f*sqrt(fetop^2+febot^2)

f=1-f

; calculate flipping ratio

r=nfns/fns

er=r*sqrt((1/nfns)+1/(fns))

print,'transmission= ',trans,'+/- ',etrans
print,'phi= ',phi,' +/- ',ephi
print,'f= ',f,' +/- ',ef
print,'ratio= ',r,' +/- ',er


end

pro startmono_event,event
;** ************
;**
	COMMON local,nparams,whatwid
	con='x'
	par1=lonarr(6)
        
      	wWidget =  Event.top

        WIDGET_CONTROL,event.id,get_uvalue=gv

        if(gv eq 'quit')then widget_control,event.top,/destroy

        if(gv eq 'do')then begin
	FOR i=0,nparams-1 DO BEGIN
          Widget_Control, whatwid(i), GET_VALUE=gv
;	  print,i,gv
	  if(i lt 6) then  par1(i)=STRTRIM(gv,2)	  
        ENDFOR
;	print,par1

	fpol,par1(0),par1(1),par1(2),par1(3),par1(4),par1(5)$
        ,trans,etrans,phi,ephi,f,ef,r,er

	
	Widget_Control, whatwid(nparams-4), SET_VALUE=strtrim(string(trans),2)+' +/- ' $
	+strtrim(string(etrans),2)
	Widget_Control, whatwid(nparams-3), SET_VALUE=strtrim(string(phi  ),2)+' +/- ' $
	+strtrim(string(ephi),2)
	Widget_Control, whatwid(nparams-2), SET_VALUE=strtrim(string(f    ),2)+' +/- ' $
	+strtrim(string(ef),2)
	Widget_Control, whatwid(nparams-1), SET_VALUE=strtrim(string(r    ),2)+' +/- ' $
	+strtrim(string(er),2)
	
	
	
	
endif	
end


pro send_command,event,i
;** ************
;**
        COMMON local,nparams,whatwid
	Widget_Control, event.id, GET_VALUE=gv
	gv=STRTRIM(gv,2)
	command=fix(gv)
;	  
;	print, gv
;
end

PRO phif
COMMON local,nparams,whatwid
nparams=10
whatwid=intarr(nparams)
result='xxx'
param=strarr(nparams)
                param_text=STRARR(nparams)
		param_text(0) ='Direct beam no shim'
		param_text(1) ='Direct beam with shim'
		param_text(2) ='Reflection flipper on no shim'
		param_text(3) ='Reflection flipper off no shim'
		param_text(4) ='Reflection flipper on with shim'
		param_text(5) ='Reflection flipper off with shim'
		
		
		
		param_text(6) ='Shim transmission'
		
		param_text(7) ='Polariser+analyser efficiency'
		param_text(8) ='Flipper Efficiency'
                param_text(9) ='Flipping ratio'
		
		
;               defaults
                		
		param(0)='106780'
		param(1)='107507'
		param(2)='4425'
		param(3)='157423'
		param(4)='76173'
		param(5)='76290'
		param(6)='----'
		param(7)='----'
		param(8)='----'
		param(9)='----'
		
		
		
		
                D17S_BASE_4=Widget_Base(UNAME='D17S_BASE_4',TITLE="Enter Values" $
			,SPACE=3,XPAD=5,YPAD=3,/COLUMN)
		D17S_BASE_0=Widget_Base(D17S_BASE_4,UNAME='D17S_BASE_0' $
			,SPACE=3,XPAD=3,YPAD=3,/ROW)
		D17S_BASE_1=Widget_Base(D17S_BASE_0,UNAME='D17S_BASE_1' $
			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)




		D17S_BASE_2=Widget_Base(D17S_BASE_0,UNAME='D17S_BASE_2' $
			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)
		D17S_BASE_3=Widget_Base(D17S_BASE_0,UNAME='D17S_BASE_3' $
			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)

                gbutton = WIDGET_BUTTON(D17S_BASE_0,UNAME='gbutton' $
			, value='Do it',uvalue='do')
		fbutton = WIDGET_BUTTON(D17S_BASE_0,UNAME='fbutton' $
			, value='Quit',uvalue='quit')
			
		
				
			
		FOR i=0,nparams-1 DO BEGIN 
  			name= 'D17S_LABEL_'+STRTRIM(STRING(i+1),2) 
  			dummy = Widget_Label(D17S_BASE_2, UNAME=name,XOFFSET=3   $ 
     	 		,YOFFSET=3, SCR_YSIZE=33,/ALIGN_LEFT ,VALUE=param_text(i) $
			,FONT=ft_smaller)
  	  	ENDFOR
		

		
		FOR i=0,nparams-1 DO BEGIN 
  			name='D17S_VALUE_'+STRTRIM(STRING(i+1),2) 
  			 whatwid(i)= Widget_Text(D17S_BASE_3, UNAME=name ,FRAME=1  $ 
     			,XOFFSET=3 ,YOFFSET=3 ,SCR_XSIZE=150 ,SCR_YSIZE=29 $
			,/EDITABLE ,VALUE=STRTRIM(STRING(param(i)),2) $
			,FONT=ft_smaller) 
  	  	ENDFOR


		Widget_Control,/REALIZE,D17S_BASE_3
		loadct, 5
		XMANAGER, 'startmono',D17S_BASE_0,/JUST_REG

end
