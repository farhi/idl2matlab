;pro startmono


PRO mon,lam,qmin,qmax,dan0,sw0,fac,slit,pol,polfac,mt,$
a,b,c,d,db,dbtime,filey,totalp,totalt,totalh,totalm,con





slitmax=4.

; formula for counting times  t=a(b+cq^d)

con=' t'
if (mt eq 1)then con=' t'
if (mt eq 0)then con=' m'

lres=0.04
ss=3440.
san=fltarr(1000)
dan=fltarr(1000)
s2=fltarr(1000)
s3=fltarr(1000)
q=fltarr(1000)
sw=fltarr(1000)
dq=fltarr(1000)
t=fltarr(1000)

thmin=180.*asin(lam*qmin/(4*!pi))/!pi
thmax=180.*asin(lam*qmax/(4*!pi))/!pi

print,' thmin= ',thmin,' thmax= ',thmax
print,' dth/th= ',((sw0/ss)*180/!pi)/thmin
dtth=((sw0/ss)*180/!pi)/thmin

i=0
qq=0
san(0)=thmin
q(0)=qmin
sw(0)=sw0

close,3
close,10
path='/users/d17/'
openw,3,path+filey+'.cmd'
print,'opening....',path+filey+'.cmd'
printf,3,'!',lam,qmin,qmax,fac,dan0,sw0
printf,3,' att 0'
printf,3,' s2w 0'
printf,3,' s3w 0'

openw,10,path+filey+'_db.cmd'
print,'opening....',path+filey+'_db.cmd'
printf,10,'!',lam,qmin,qmax,fac,dan0,sw0
printf,10,'! direct beam calibration runs'
printf,10,'dan ',dan0
printf,10,'trs 10'
printf,10,' s2w 0'
printf,10,' s3w 0'
printf,10,'san 0'



while (qq le qmax) do begin

  i=i+1  
  dq(i)=q(i-1)*sqrt(lres^2+dtth^2)  
  q(i)=q(i-1)+dq(i)/fac
  qq=q(i)
  san(i)=(180/!pi)*asin(lam*q(i)/(4*!pi)) 
  dan(i)=2.*san(i)+dan0
  sw(i)=(san(i)/san(0))*sw0
  if (sw(i) gt slitmax) then sw(i)=slitmax
  t(i)=a*(b+c*q(i)^d)
;  print,san(i),dan(i),sw(i),q(i),dq(i)

   printf,3,'par sub q=',q(i)   
   printf,3,'dan ',dan(i)
   printf,3,'san ',san(i)
   printf,3,'dan ',dan(i)
if (slit eq 1)then begin
   printf,3,'s2w ',sw(i)
   printf,3,'s3w ',sw(i)
endif
   printf,3,'run ',t(i),con

if (pol eq 1) then begin
   printf,3,'par sub q=',q(i),' Flipper on'
   printf,3,'b1 on'
   printf,3,'b2 on'
   printf,3,'run ',t(i)*polfac,con
   printf,3,'b1 off'
   printf,3,'b2 off'
endif



   sws=sw(i)*sw(i)
; attenuators for direct beam run

   if (sws lt 0.03) then begin
     printf,10,'att 0'
      printf,10,'par sub att 0 db q=',q(i)
   endif 
   if (sws ge 0.03 and sws lt 0.078) then begin
     printf,10,'att 1'
     printf,10,'par sub att 1 db q=',q(i)
   endif
   if (sws ge 0.078 and sws lt 0.18) then begin
     printf,10,'att 2'
     printf,10,'par sub att 2 db q=',q(i)
   endif
   if (sws ge 0.18 and sws lt 0.46) then begin
     printf,10,'att 3'
     printf,10,'par sub att 3 db q=',q(i)
   endif
   if (sws ge 0.46 and sws lt 1.21) then begin
     printf,10,'att 4'
     printf,10,'par sub att 4 db q=',q(i)
   endif
   if (sws ge 1.21 and sws lt 2.89) then begin
     printf,10,'att 5'
     printf,10,'par sub att 5 db q=',q(i)
   endif
   if (sws ge 2.89 and sws lt 7.84) then begin
     printf,10,'att 6'
     printf,10,'par sub att 6 db q=',q(i)
   endif
   if (sws ge 7.84 and sws lt 25.) then begin
     printf,10,'att 7'
     printf,10,'par sub att 7 db q=',q(i)
   endif
   if (sws ge 25.) then begin
     printf,10,'ose in'
   endif
   printf,10,'sol in'
   printf,10,'fl1 in'

   
  
   printf,10,'s2w ',sw(i)
   printf,10,'s3w ',sw(i)
   printf,10,'run ',dbtime,' t'


endwhile
qq=q(1:i)
s=sw(1:i)
printf,10,'s2w 0'
printf,10,'s3w 0'
printf,10,'att 0'
printf,3,'s2w 0'
printf,3,'s3w 0'
printf,3,'att 0'



print,'number of points in the scan = ',i
print,'number of lines in the file = ',i*6
print,'total reflection time = ',total(t)/3600.,' hours'
if (pol eq 1) then begin 
 print,'plus flipper time of: ',total(t)*polfac/3600.,' hours'
 print,'gives a grand total of: ',(total(t)*polfac/3600)+(total(t)/3600)
endif
if (db eq 1)then print,'total direct beam time = ',(dbtime*i)/3600.,' hours'
totalt=pol*(total(t)*polfac/3600.)+(total(t)/3600.)+db*(dbtime*i)/3600.

totalh=floor(totalt)
totalm=floor((totalt-floor(totalt))*60.)
print,totalt,totalh,totalm
totalp=i
close,3
close,10
print,!stime

end

pro startmono_event,event
;** ************
;**
	COMMON local,nparams,whatwid
	con='x'
	par1=fltarr(6)
        par2=intarr(10)
	par3=strarr(1)
      	wWidget =  Event.top

        WIDGET_CONTROL,event.id,get_uvalue=gv

        if(gv eq 'quit')then widget_control,event.top,/destroy

        if(gv eq 'do')then begin
	FOR i=0,nparams-1 DO BEGIN
          Widget_Control, whatwid(i), GET_VALUE=gv
;	  print,i,gv
	  if(i lt 6) then  par1(i)=STRTRIM(gv,2)
	  if(i ge 6 and i lt 16) then par2(i-6)=STRTRIM(gv,2)
	  if(i eq 16) then par3(i-16)=STRTRIM(gv,2)
        ENDFOR
;	print,par1,par2,par3

	mon,par1(0),par1(1),par1(2),par1(3),par1(4),par1(5)$
,par2(0),par2(1),par2(2),par2(3),par2(4),par2(5),par2(6),par2(7),par2(8)$
,par2(9),par3(0),totalp,totalt,totalh,totalm,con

	
	Widget_Control, whatwid(nparams-2), SET_VALUE=strtrim(string(totalp),2)
	
	if (par2(3) eq 0)then begin
	  Widget_Control, whatwid(nparams-1), SET_VALUE=strtrim(string(floor(totalt*3600.)),2)
	endif
	
	if (par2(3) eq 1)then begin
	  Widget_Control, whatwid(nparams-1), SET_VALUE=strtrim(string(totalh),2)+' h'$
	  +strtrim(string(totalm),2)+' m'
	endif
	
endif	
end


pro send_command,event,i
;** ************
;**
	Widget_Control, event.id, GET_VALUE=gv
	gv=STRTRIM(gv,2)
	command=fix(gv)
;	  
;	print, gv
;
end

PRO startmono
COMMON local,nparams,whatwid
nparams=19
whatwid=intarr(nparams)
result='xxx'
param=strarr(nparams)
                param_text=STRARR(nparams)
		param_text(0) ='Wavelength (A)'
		param_text(1) ='q min (A^-1)'
		param_text(2) ='q max (A^-1)'
		param_text(3) ='Detector position (deg)'
		param_text(4) ='Starting value for s2w & s3w (mm)'
		param_text(5) ='Points inside dq'
		
		param_text(6) ='Open slits? 0/1'
		param_text(7) ='Polarized neutrons? 0/1'
		param_text(8) ='Extra counting factor for I-'
		param_text(9) ='Count on monitor (0) or time (1)?'
		param_text(10) ='Counting formula a(b+cq^d) a='
		param_text(11) ='Counting formula a(b+cq^d) b='
		param_text(12) ='Counting formula a(b+cq^d) c='
		param_text(13) ='Counting formula a(b+cq^d) d='
		param_text(14) ='Make direct beam calibration? 0/1'
		param_text(15) ='Direct beam counting'
		
		param_text(16) ='Output filename (no extention)'
		
		param_text(17) ='Calculated number of points in q'
		param_text(18) ='Calculated total duration monitor or time'
		
		
;               defaults
                		
		param(0)='5.0'
		param(1)='0.008'
		param(2)='0.2'
		param(3)='2.2'
		param(4)='0.1'
		param(5)='2.0'
		param(6)='1'
		param(7)='0'
		param(8)='3'
		param(9)='1'
		param(10)='1'
		param(11)='20'
		param(12)='20000'
		param(13)='2'
		param(14)='1'
		param(15)='60'
		param(16)='xxx'
		param(17)='-------'
		param(18)='-------'
		
		
		
		
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
		XMANAGER, 'startmono',D17S_BASE_0,/JUST_REG

end
