pro qs,w,minl,th,pmin,pmax,sizex,sizez,file
common data,w8,x8,y8
print,'readback',nn,th,pmin,pmax,sizex,sizez,file
close,3
y=size(w)
print,y
TAKE_DATP,P
help,p
if (y(0) eq 3) then begin
   print,y
   
   xsize=y(1)
   ysize=y(2)
   tsize=y(3)

   print,'Three dimensions x,y,t =',xsize,ysize,tsize
   print,'Will sum over the y dimension'
   ww=lonarr(xsize,tsize)
   ww=total(w,2)
   
   
endif else begin
   
   tsize=y(2)
   xsize=y(1)
   print,'Two dimensions x,t =',xsize,tsize
   ww=lonarr(xsize,tsize)
   ww=w
      
endelse

print,'hello world'
print,'no of chans= ',p.p(1),' chan width= ',p.p(6),' tof delay= ',p.p(7)
print,'x1= ',p.p(2),' x2= ',p.p(3),' y1= ',p.p(4),' y2= ',p.p(5)
nx=p.p(8)
ny=p.p(9)
print,'nx= ',nx,' ny= ',ny
print,'chop 1 speed req= ',p.p(10),' chop 1 phase req= ',p.p(11)
print,'chop 2 speed req= ',p.p(12),' chop 2 phase req= ',p.p(13)
print,'chop 1 speed act= ',p.p(14),' chop 1 phase act= ',p.p(15)
print,'chop 2 speed act= ',p.p(16),' chop 2 phase act= ',p.p(17)

; useful chopper variables
openr=45.-(p.p(13)-p.p(11))
opena=45.-(p.p(17)-p.p(15))
opena=opena-1.1
period=60./p.p(14)
dela=(285.-opena-3.3776)/2.
delt=(dela/360.)*period
chopsam=4.1135-(85.e-3)/2.
tofd=chopsam+(p.p(23)/1000.)
chanpa=(tofd/3956.)/(p.p(6)*1e-6)
chanpam=(chopsam/3956.)/(p.p(6)*1e-6)
delchan=delt/(p.p(6)*1e-6)
delechan=p.p(7)/p.p(6)
print,'TOF distance    = ',tofd, ' period chans    =',period/(p.p(6)*1e-6)
print,'channels/Ang    = ',chanpa,' chop del chans  = ',delchan
print,'chopper period  = ',period*1000.,' elec del chans  = ',delechan
print,'opening      req= ',openr,' opening      act= ',opena
print,'chop delay angle= ',dela,' chop delay  time= ',delt*1000.,' ms'
print,'san= ',p.p(24),' deg.','dan= ',p.p(25),' deg.'
san=p.p(24)
dan=p.p(25)
print,' 27A TOF        = ',27.*chanpa,' 4.5A TOF        = ',4.5*chanpa
time=p.p(26)/10.
print,'run time= ',time,' s',' det= ',p.p(23)
det=p.p(23)


print,'well done chaps...'
;make lambda array
yy=indgen(tsize)
;larr=(yy+delechan-delchan)/chanpa
larr=(yy)/chanpa
m=(yy+delechan-delchan)/chanpam



lfirst=(delchan-delechan+0.5)
print,'channel shift = ',lfirst

; loose nn channels at the beginning to avoid infinities in reciprocal space

;nn=1
aa=shift(ww,1,-round(lfirst))

neww1=aa(*,nn:tsize-1)
newlarr=larr(nn:tsize-1)

q=fltarr(xsize,tsize-nn)
th=fltarr(xsize,tsize-nn)
qx=fltarr(xsize,tsize-nn)
qz=fltarr(xsize,tsize-nn)

 

;  conversion to reciprocal space

dpr=180./!pi

pcen=135.79/nx
mmpp=1.04*nx
;pmin=150./nx
;pmax=233./nx


print,'pcen= ',pcen,' mmpp= ',mmpp
print,'ref th= ',(dr+dpr*atan((pcen-pr)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
lmax=max(newlarr)
lmin=min(newlarr)

;expt information d0=dan for diect beam p0=pixel for direct beam
;dr=dan for reflection pr=pixel for reflection



;find limits in reciprocal space

for i=pmin,pmax do begin
  for j=0,tsize-nn-1 do begin
  
;   l=newlarr(j)
   th(i,j)=(dr+dpr*atan((pcen-i)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2.0
   h1=th(i,j)
   h2=(h1-san)/dpr
   q(i,j)=4*!pi*sin(h1/dpr)/newlarr(j)
   h3=q(i,j)
   qx(i,j)=h3*sin(h2)
   qz(i,j)=h3*cos(h2)

;   l=newlarr(j)
;   th(i,j)=(dr+dpr*atan((pcen-i)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
;   q(i,j)=4*!pi*sin(th(i,j)/dpr)/newlarr(j)
;   qx(i,j)=q(i,j)*sin((th(i,j)-san)/dpr)
;   qz(i,j)=q(i,j)*cos((th(i,j)-san)/dpr)

  
   endfor
 endfor

   
  
   print,'limits....'
   print,'thmax= ',max(th),'thmin= ',min(th),'qmin= ',min(q),'qmax= ',max(q)
   print,'qxmax= ',max(qx),'qxmin= ',min(qx),'qzmax= ',max(qz),'qzmin= ',min(qz)



;nfac=1
;sizex=120.*nfac
;sizez=300.*nfac
print,min(qx),max(qx),min(qz),max(qz),sizex,sizez

gs=[(max(qx)*1.001-min(qx))/sizex,(max(qz)*1.001-min(qz))/sizez]

print,gs(0),gs(1),max(qz)/gs(1)

;**********************
;  nicks bit

;  bin neww1 into a square array qx qz of size sizex X sizez


x8=indgen(sizex)*(max(qx)-min(qx))/(sizex)+min(qx)
y8=indgen(sizez)*(max(qz)-min(qz))/(sizez)+min(qz)

xpixelsize=gs(0)
zpixelsize=gs(1)


print,tsize-nn
w8=fltarr(sizex,sizez)


; loop about output array w9
;for i=0,sizex-1 do begin
;	for j=0,sizez-1 do begin
;		w8(i,j)=0
;	endfor
;endfor	


minz=min(qz)
minx=min(qx)
;      loop about input array neww1
          for k=0,xsize-1 do begin
;             print,k,xsize-1

               
;		i=floor((qx(k,*)-min(qx))/xpixelsize)
;               j=floor((qz(k,*)-min(qz))/zpixelsize)

		i=floor((qx(k,*)-minx)/xpixelsize)
                j=floor((qz(k,*)-minz)/zpixelsize)
              
		w8(i,j)=w8(i,j)+neww1(k,*)

          endfor



print,'done'



;*********************




return

end
pro qit_event,event
;** ************
;**
        common data,w8,x8,y8
	COMMON local,nparams,whatwid
	con='x'
	par1=fltarr(2)
        par2=intarr(4)
	par3=strarr(1)
      	wWidget =  Event.top

        WIDGET_CONTROL,event.id,get_uvalue=gv

        if(gv eq 'quit')then widget_control,event.top,/destroy

        if(gv eq 'do')then begin
	FOR i=0,nparams-1 DO BEGIN
          Widget_Control, whatwid(i), GET_VALUE=gv
;	  print,i,gv
	  if(i le 1) then  par1(i)=STRTRIM(gv,2)
	  if(i gt 1 and lt 6) then  par2(i)=STRTRIM(gv,2)
	  if(i eq 6) then par3(i)=STRTRIM(gv,2)
        ENDFOR
	print,par1,par2,par3

	qs,par1(0),par1(1),par2(0),par2(1),par2(2),par2(3),par3(0)


	
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

PRO qit,w8,x8,y8
COMMON local,nparams,whatwid
common data,w8,x8,y8
nparams=7
whatwid=intarr(nparams)

param=strarr(nparams)
                param_text=STRARR(nparams)
		param_text(0) ='Theta (deg.)'
		param_text(1) ='Minimum wavelength (A)'
		param_text(2) ='Minimum x-pixel'
		param_text(3) ='Maximum x-pixel'
		param_text(4) ='Number of qx pixels'
		param_text(5) ='Number of qz pixels'
		
		param_text(6) ='Output filename'
		
		
;               defaults
                		
		param(0)='2.2'
		param(1)='0.7'
		param(2)='150'
		param(3)='250'
		param(4)='150'
		param(5)='300'
		param(6)='qspace.dat'
		
		
		
		
		
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
