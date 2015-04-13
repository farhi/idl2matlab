;pro calth
pro th,n1,n2,tt

data_read,n1,w1,mon,y,m,q,th,san,d0,det,nx


data_read,n2,w2,mon,y,m,q,th,san,dr,det,nx


a=max(total(w1,2),p0)

b=max(total(w2,2),pr)

step=5

k=0.
kk=0.
print,'com direct :',p0-step,p0+step
for i=p0-step,p0+step do begin
  k=k+total(w1(i,*),2)
  kk=kk+total(w1(i,*),2)*float(i)

endfor

pp0=kk/k


l=0.
ll=0.
for i=pr-step,pr+step do begin
  l=l+total(w2(i,*),2)
  ll=ll+total(w2(i,*),2)*i
endfor
ppr=ll/l


dpr=180./!pi

pcen=135.79/nx
mmpp=1.04*nx

print,'det: ',det,' X grouping: ',nx
print,'detector centre: ',pcen,' mmpp: ',mmpp
print,'reflection dan: ',dr,' direct beam dan: ',d0
print,'reflection pixel:',ppr,' direct pixel:',pp0

print,'ref th= ',(dr+dpr*atan((pcen-ppr)*mmpp/det))/2-(d0+dpr*atan((pcen-pp0)*mmpp/det))/2
tt=(dr+dpr*atan((pcen-ppr)*mmpp/det))/2-(d0+dpr*atan((pcen-pp0)*mmpp/det))/2
return
end


pro calth_event,event
;** ************
;**
	COMMON local,nparams,whatwid
	
	pars=intarr(nparams)
        
      	wWidget =  Event.top

        WIDGET_CONTROL,event.id,get_uvalue=gv

        if(gv eq 'quit')then widget_control,event.top,/destroy

        if(gv eq 'do')then begin
	FOR i=0,nparams-1 DO BEGIN
          Widget_Control, whatwid(i), GET_VALUE=gv
	  pars(i)=STRTRIM(gv,2)
	  
        ENDFOR
	
	th,pars(0),pars(1),tt
	Widget_Control, whatwid(nparams-1), SET_VALUE=strtrim(string(tt),2)
	endif

end

pro send_command, event,i
;** ************
;**
	Widget_Control, event.id, GET_VALUE=gv
	gv=STRTRIM(gv,2)
;	command=fix(gv)
	  
	print, gv

end

PRO calth
COMMON local,nparams,whatwid
nparams=3
whatwid=intarr(nparams)
result='xxx'
param=intarr(nparams)
                param_text=STRARR(nparams)
		param_text(0) ='Direct beam numor'
		param_text(1) ='Reflection numor'
		param_text(2) ='Calculated theta'
		
		
		
                D17S_BASE_4=Widget_Base(UNAME='D17S_BASE_4',TITLE="Enter Values" $
			,SPACE=3,XPAD=3,YPAD=3,/COLUMN)
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
     			,XOFFSET=3 ,YOFFSET=3 ,SCR_XSIZE=100 ,SCR_YSIZE=29 $
			,/EDITABLE ,VALUE=STRTRIM(STRING(param(i)),2) $
			,FONT=ft_smaller) 
  	  	ENDFOR


		Widget_Control,/REALIZE,D17S_BASE_3
		loadct, 5
		XMANAGER, 'calth',D17S_BASE_0,/JUST_REG

end
