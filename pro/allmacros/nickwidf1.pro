pro nickwidf1

;common vars1,q
;common vars2,th
common vars0,def
common vars,lab,txt,but,gap
common varsb,base
common vars4,fac1
common vars5,fac2
common vars6,fac3
common vars7,th1
common vars8,th2
common vars9,th3
;common vars10,y,x,e

lab=lonarr(50)
txt=lonarr(50)
but=lonarr(50)
gap=lonarr(50)

;defaults

def='def'

dtb1='4685'
;'4336'
;'4308'
;'3867'
dtb2='4682'
;'4344'
;'4313'
;'3866'
dtb3=''
;'3866'

refl1='4679'
;'4337,4338'
;'4307'
;'3858'
refl2='4680
;'4339,4343'
;'4310,4311'
;'3861'
refl3=''
;'3863'

water='4683'
;def
;'3668'

lamda='1.8,19'

th1=def
th2=def
th3=def

fac1=def
fac2=def
fac3=def

r='19'
bg='25'

norm=0
bgrd=2

fil='nickwitt.out'
path='/users/data/'

base=widget_base(title='TOF Data',uvalue='base',column=5) 

;column 1
gap(10)=widget_label(base,ysize=10,value='')
lab(0)=widget_label(base,value='Run')
lab(1)=widget_label(base,value='set')
gap(0)=widget_label(base,ysize=0,value='')
lab(2)=widget_label(base,value='(1)  ',/align_right)
gap(11)=widget_label(base,ysize=11,value='')
lab(3)=widget_label(base,value='(2)  ',/align_right)
gap(11)=widget_label(base,ysize=11,value='')
lab(4)=widget_label(base,value='(3)  ',/align_right)
gap(23)=widget_label(base,ysize=23,value='')
lab(5)=widget_label(base,value='Water',/align_right)
lab(6)=widget_label(base,value='runs',/align_right)
gap(10)=widget_label(base,ysize=10,value='')
lab(7)=widget_label(base,value='Lamda',/align_right)
lab(8)=widget_label(base,value='range',/align_right)
gap(10)=widget_label(base,ysize=10,value='')
gap(0)=widget_label(base,ysize=0,value='')
lab(9)=widget_label(base,value='Normalise',/align_right)
lab(10)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')

;column 2
gap(10)=widget_label(base,ysize=10,value='')
lab(11)=widget_label(base,value='Direct')
lab(12)=widget_label(base,value='runs')
txt(0)=widget_text(base,value=dtb1,xsize=9,/editable,uvalue='int')
txt(1)=widget_text(base,value=dtb2,xsize=9,/editable,uvalue='int')
txt(2)=widget_text(base,value=dtb3,xsize=9,/editable,uvalue='int')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(3)=widget_text(base,xsize=9,value=water,/editable,uvalue='int')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(4)=widget_text(base,xsize=9,value=lamda,/editable,uvalue='real')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
but(0)=cw_bgroup(base,['Runtime','Monitor'],set_value=norm,/column,/exclusive,uvalue='norm')
gap(0)=widget_label(base,ysize=0,value='')
lab(11)=widget_label(base,value='Path:',/align_right)
lab(11)=widget_label(base,value='Output file:',/align_right)

;column 3
gap(10)=widget_label(base,ysize=10,value='')
lab(13)=widget_label(base,value='Reflect')
lab(14)=widget_label(base,value='runs')
txt(5)=widget_text(base,value=refl1,xsize=9,/editable,uvalue='int')
txt(6)=widget_text(base,value=refl2,xsize=9,/editable,uvalue='int')
txt(7)=widget_text(base,value=refl3,xsize=9,/editable,uvalue='int')
gap(16)=widget_label(base,ysize=16,value='')
lab(15)=widget_label(base,value='Foregrd',/align_right)
lab(16)=widget_label(base,value='range',/align_right)
gap(0)=widget_label(base,ysize=10,value='')
lab(17)=widget_label(base,value='Backgrd',/align_right)
lab(18)=widget_label(base,value='range',/align_right)
gap(0)=widget_label(base,ysize=0,value='')
gap(20)=widget_label(base,ysize=20,value='')
lab(19)=widget_label(base,value='Use',/align_right)
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(8)=widget_text(base,value=path,xsize=9,/editable,uvalue='str')
txt(9)=widget_text(base,value=fil,xsize=9,/editable,uvalue='str')

;column 4
gap(10)=widget_label(base,ysize=10,value='')
lab(20)=widget_label(base,value='Theta')
gap(6)=widget_label(base,value='',ysize=6)
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(10)=widget_text(base,xsize=4,value=th1,/editable,uvalue='real')
txt(11)=widget_text(base,xsize=4,value=th2,/editable,uvalue='real')
txt(12)=widget_text(base,xsize=4,value=th3,/editable,uvalue='real')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(13)=widget_text(base,xsize=4,value=r,/editable,uvalue='int')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(14)=widget_text(base,xsize=4,value=bg,/editable,uvalue='int')
but(1)=cw_bgroup(base,['left','both','fit'],set_value=bgrd,/column,/exclusive,uvalue='bgrd')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')

;column 5
gap(10)=widget_label(base,ysize=10,value='')
lab(21)=widget_label(base,value='Factor')
gap(6)=widget_label(base,value='',ysize=6)
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(15)=widget_text(base,xsize=4,value=fac1,/editable,uvalue='real')
txt(16)=widget_text(base,xsize=4,value=fac2,/editable,uvalue='real')
txt(17)=widget_text(base,xsize=4,value=fac3,/editable,uvalue='real')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(10)=widget_label(base,ysize=10,value='')
gap(40)=widget_label(base,ysize=40,value='')
gap(40)=widget_label(base,ysize=40,value='')
gap(40)=widget_label(base,ysize=40,value='')
gap(20)=widget_label(base,ysize=20,value='')
gap(20)=widget_label(base,ysize=20,value='')
but(2)=widget_button(base,value='DONE',uvalue='done')

baseinfo=widget_info(base,/geometry)
widget_control,base,scr_xsize=baseinfo.scr_xsize+10
widget_control,base,scr_ysize=baseinfo.scr_ysize+10

widget_control,base,/realize

xmanager,'nickwidf',base

end



function convert,txtwid,defaults,intcheck,commacheck,strcheck,errcheck1,errcheck2

common flags1,flag2,flag3
common flags,flag4,flag5
common vars0,def

valnum=strarr(3)

widget_control,txtwid,get_value=val

valstr=val(0)

valstr=strcompress(valstr,/remove_all)

if (defaults eq 0) and (valstr eq def) then flag5=1
	
if valstr eq def then valnum(0)=1 else $

if (valstr ne def) and (strcheck eq 1) then valnum(1)=valstr else begin

	commapos=strpos(valstr,',')

	if commapos eq -1 then begin

		if commacheck eq 1 then flag4=1
	
		valflt=float(valstr)
		valint=fix(valstr)
	
		if strpos(string(valflt),valstr) eq -1 then flag2=1
		if (intcheck eq 1) and (valint ne valflt) then flag3=1
		if (intcheck eq 1) and (errcheck1 eq 1) then valnum=[0,valint,valint]
		if (intcheck eq 0) and (errcheck1 eq 1) then valnum=[0,valflt,valflt]
		if (intcheck eq 0) and (errcheck1 eq 0) then valnum(1)=valflt
		if (intcheck eq 1) and (errcheck1 eq 0) then valnum(1)=valint

	endif else begin
	
		if commacheck eq 0 then flag2=1

		valstr=['0',strmid(valstr,0,commapos),strmid(valstr,commapos+1)]

		valflt=float(valstr)
		valint=fix(valstr)
	
		if (strpos(string(valflt(1)),valstr(1)) eq -1) or (strpos(string(valflt(2)),valstr(2)) eq -1) then flag2=1
		if (intcheck eq 1) and ((valint(1) ne valflt(1)) or (valint(2) ne valflt(2))) then flag3=1
		if intcheck eq 1 then valnum=valint else valnum=valflt

	endelse

	if errcheck1 eq 1 then begin

		if valnum(1) gt valnum(2) then flag0=1

	endif

	if errcheck2 eq 1 then begin

		if valnum(1) eq 0 then flag1=1

	endif

endelse	

return,valnum

end



function factor,refa,refb,qa,qb,errora,errorb

;sa=size(ra)
;sb=size(rb)

xa=qa(sort(qa))
xb=qb(sort(qb))
ya=refa(sort(qa))
yb=refb(sort(qb))
errora=errora(sort(qa))
errorb=errorb(sort(qb))

;help,xa,ya,erra,xb,yb,errb

;fin=max(xa)
;sta=max(where(xa le min(xb)))

xaovsub=where(xa ge min(xb))

;if 

;help,xaovsub
;print,xaovsub
range=max(xaovsub)-min(xaovsub)

xaov=xa(min(xaovsub):max(xaovsub))

dataa=ya(min(xaovsub):max(xaovsub))
erra=errora(min(xaovsub):max(xaovsub))

datab=fltarr(range)
err2=fltarr(range)
errb=fltarr(range)

numersum=0
denomsum=0

for i=1,range-1 do begin
		
	pntabsub=min(where(xb ge xaov(i)))
	pntab=xb(pntabsub)
	databab=yb(pntabsub)
	errbab=errorb(pntabsub)
	pntbesub=max(where(xb lt xaov(i)))
	pntbe=xb(pntbesub)
	databbe=yb(pntbesub)
	errbbe=errorb(pntbesub)
	
	databdiff=databbe-databab

	datab(i)=(((pntab-xaov(i))/databdiff)*databbe)+(((xaov(i)-pntbe)/databdiff)*databab)
		errb(i)=sqrt((((pntab-xaov(i))/databdiff)*errbbe)^2+(((xaov(i)-pntbe)/databdiff)*errbab)^2)
	
	if pntab-xaov(i) le xaov(i)-pntbe then begin
		datab(i)=yb(pntabsub)
		errb(i)=errorb(pntabsub)
	endif else begin
		datab(i)=yb(pntbesub)
		errb(i)=errorb(pntbesub)
	endelse
	
	if (dataa(i) gt 1.e-11) and (datab(i) gt 1.e-11) and (erra(i) gt 0.) and (errb(i) gt 0.) then err2(i)=(erra(i)+errb(i))*(erra(i)+errb(i)) else err2(i)=1.e20

	numer=(1/err2(i))*dataa(i)*datab(i)

	denom=(1/err2(i))*datab(i)*datab(i)

	numersum=numersum+numer
	denomsum=denomsum+denom

endfor

fac=numersum/denomsum
help,fac
;print,fac
return,fac

end



pro nickwidf_event,event

common vars,lab,txt,but,gap
common varsb,base
;common vars1,q
;common vars2,th
common flags,flag4,flag5
common flags1,flag2,flag3
common vars0,def
;common vars5,th1
common vars4,fac1
common vars5,fac2
common vars6,fac3
common vars7,th1
common vars8,th2
common vars9,th3
common vars10,check0,check1
common varstuff,bgrd
common varbull,norm,bg,r,fil
common varpath,path
common varcrap1,bglim

widget_control,event.id,get_uvalue=ev

if ev eq 'done' then begin

	flag0=0	
	flag1=0	
	flag2=0
	flag3=0
	flag4=0
	flag5=0
	flag6=0

	check0=0
	check1=0
	check2=0

	dtb1=convert(txt(0),0,1,2,0,1,1) 
	
	dtb2=convert(txt(1),0,1,2,0,1,0) 

	dtb3=convert(txt(2),0,1,2,0,1,0) 

	water=convert(txt(3),1,1,2,0,1,0) 

	lamda=convert(txt(4),1,0,2,0,1,1)

	refl1=convert(txt(5),0,1,2,0,1,1) 

	refl2=convert(txt(6),0,1,2,0,1,0)

	refl3=convert(txt(7),0,1,2,0,1,0)

	path=convert(txt(8),0,0,0,1,0,0)

	fil=convert(txt(9),0,0,0,1,0,0)

	th1=convert(txt(10),1,0,0,0,0,0)

	th2=convert(txt(11),1,0,0,0,0,0)

	th3=convert(txt(12),1,0,0,0,0,0)

	r=convert(txt(13),0,1,0,0,0,0)

	bg=convert(txt(14),0,1,0,0,0,0)

	fac1=convert(txt(15),1,0,0,0,0,0)

	fac2=convert(txt(16),1,0,0,0,0,0)

	fac3=convert(txt(17),1,0,0,0,0,0)


	widget_control,but(0),get_value=norm
	widget_control,but(1),get_value=bgrd

	if (dtb2(1) ne 0) or (refl2(1) ne 0) then check0=1
	if (dtb3(1) ne 0) or (refl3(1) ne 0) then check1=1
;	if (check0 eq 1) and ((dtb2(1) eq 0) or (refl2(1) eq 0)) then flag6=1 
;	if (check1 eq 1) and ((dtb3(1) eq 0) or (refl3(1) eq 0) or (check0 eq 1)) then flag6=1 

	if flag0 eq 1 then errormessage,'Invalid ranges'
	if flag1 eq 1 then errormessage,'Incomplete fields'	
	if flag2 eq 1 then errormessage,'Field must be a number'	
	if flag3 eq 1 then errormessage,'Field must be integer'	
	if flag4 eq 1 then errormessage,'Comma expected'	
	if flag5 eq 1 then errormessage,'Default not available'	
	if flag6 eq 1 then errormessage,'Must complete all fields for given run set'	
	if (flag0 eq 0) and (flag1 eq 0) and (flag2 eq 0) and (flag3 eq 0) and (flag4 eq 0) and (flag5 eq 0) and (flag6 eq 0) then begin

		widget_control,base,sensitive=0

;		w3=make_array(3,286,type=4)
		if water(0) eq 1 then begin
			print,'Using default Water file ''water_LAMPascii''...'
			w3=make_array(3,286,type=4)

			close,4
			openr,4,"water_LAMPascii"
     			readf,4,w3
			w2=w3(1,*)
			close,4

		endif else begin
			watersum=0
			for i=water(1),water(2) do begin
				data_read,i,water
				watersum=water+watersum
			endfor
			awater,watersum,w2
;			w2=fltarr(3,286)
;			w2(1,*)=w3
		endelse
				
		db1sum=0
		db1monsum=0
		db1timesum=0

		for i=dtb1(1),dtb1(2) do begin
			data_read,i,db1,db1mon,db1time,dbdan1
                	db1sum=db1+db1sum
                	db1monsum=db1mon+db1monsum
                        db1timesum=db1time+db1timesum
                endfor
		erdb1=sqrt(db1sum)
		
		ref1sum=0
		ref1monsum=0
		ref1timesum=0
		for i=refl1(1),refl1(2) do begin
			data_read,i,ref1,ref1mon,ref1time,refdan1
			ref1sum=ref1+ref1sum
			ref1monsum=ref1mon+ref1monsum
			ref1timesum=ref1time+ref1timesum
		endfor
		erref1=sqrt(ref1sum)

		if norm eq 0 then begin
			normref1sum=ref1sum*(db1timesum/ref1timesum)
			normerref1sum=(db1timesum/ref1timesum)*erref1
		endif
;		if norm eq 1 then begin
;			normref1sum=ref1sum*(db1monsum/ref1monsum)
;			normerref1sum=(db1monsum/ref1monsum)*erref1
;		endif
		if norm eq 1 then begin 
			normref1sum=ref1sum/ref1monsum
			db1sum=db1sum/db1monsum
		endif

		if fac1(0) eq 1 then fac1(1)=1.
;print,'fac1',fac1(1)
help,w2	
	anal,db1sum,erdb1,dbdan1,normref1sum,normerref1sum,refdan1,w2,r(1),bg(1),w5,x5,e5,qe5,lamda,th1	
print,bg(1)	
;print,e5,w5

		w5=fac1(1)*w5
		e5=fac1(1)*e5

		z5=size(e5)

		xs=x5(sort(x5))
		ws=w5(sort(x5))
		es=e5(sort(x5))
		qes=qe5(sort(x5))

		if check0 eq 1 then begin
				
			db2sum=0
			db2monsum=0
			db2timesum=0
			for i=dtb2(1),dtb2(2) do begin
				data_read,i,db2,db2mon,db2time,dbdan2
     		           	db2sum=db2+db2sum
   		            	db2monsum=db2mon+db2monsum
      	                	db2timesum=db2time+db2timesum
                	endfor
			erdb2=sqrt(db2sum)

			ref2sum=0
			ref2monsum=0
			ref2timesum=0
			for i=refl2(1),refl2(2) do begin
				data_read,i,ref2,ref2mon,ref2time,refdan2
				ref2sum=ref2+ref2sum
				ref2monsum=ref2mon+ref2monsum
				ref2timesum=ref2time+ref2timesum
			endfor
			erref2=sqrt(ref2sum)

			if norm eq 0 then begin
				normref2sum=ref2sum*(db2timesum/ref2timesum)
				normerref2sum=(db2timesum/ref2timesum)*erref2
			endif
			if norm eq 1 then begin
				normref2sum=ref2sum*(db2monsum/ref2monsum)
				normerref2sum=(db2monsum/ref2monsum)*erref2
			endif	
;			if norm eq 1 then begin 
;				normref2sum=ref2sum/ref2monsum
;				db2sum=db2sum/db2monsum
;			endif

			anal,db2sum,erdb2,dbdan2,normref2sum,normerref2sum,refdan2,w2,r(1),bg(1),w6,x6,e6,qe6,lamda,th2
			if fac2(0) eq 1 then fac2(1)=factor(w5,w6,x5,x6,e5,e6)
			if fac2(1) eq 0. then print,'No Overlap'
				
;print,fac2(1)

			w6=fac2(1)*w6
			e6=fac2(1)*e6
			;print,e6*fac2(1),w6*fac2(1)			
			z6=size(e6)

			x=fltarr(z5(1)+z6(1))
			w=fltarr(z5(1)+z6(1))
			e=fltarr(z5(1)+z6(1))
			qe=fltarr(z5(1)+z6(1))
			for i=0,z5(1)-1 do begin
				x(i)=x5(i)
				w(i)=w5(i)
				e(i)=e5(i)
				qe(i)=qe5(i)
			endfor
			for i=z5(1),z5(1)+z6(1)-1 do begin
				x(i)=x6(i-z5(1))
				w(i)=w6(i-z5(1))
				e(i)=e6(i-z5(1))
				qe(i)=qe6(i-z5(1))
			endfor
			
			xs=x(sort(x))
			ws=w(sort(x))
			es=e(sort(x))
			qes=qe(sort(x))
			
		endif
			
		if (check0 eq 1) and (check1 eq 1) then begin

			db3sum=0
			db3monsum=0
			db3timesum=0
			for i=dtb3(1),dtb3(2) do begin
				data_read,i,db3,db3mon,db3time,dbdan3
     		           	db3sum=db3+db3sum
    		            	db3monsum=db3mon+db3monsum
      	                	db3timesum=db3time+db3timesum
                	endfor
			erdb3=sqrt(db3sum)

			ref3sum=0
			ref3monsum=0
			ref3timesum=0
			for i=refl3(1),refl3(2) do begin
				data_read,i,ref3,ref3mon,ref3time,refdan3
				ref3sum=ref3+ref3sum
				ref3monsum=ref3mon+ref3monsum
				ref3timesum=ref3time+ref3timesum
			endfor
			erref3=sqrt(ref3sum)

			if norm eq 0 then begin
				normref3sum=ref3sum*(db3timesum/ref3timesum)
				normerref3sum=(db3timesum/ref3timesum)*erref3
			endif
			if norm eq 1 then begin
				normref3sum=ref3sum*(db3monsum/ref3monsum)
				normerref3sum=(db3monsum/ref3monsum)*erref3
			endif
;			if norm eq 1 then begin 
;				normref3sum=ref3sum/ref3monsum
;				db3sum=db3sum/db3monsum
;			endif
			anal,db3sum,erdb3,dbdan3,normref3sum,normerref3sum,refdan3,w2,r(1),bg(1),w7,x7,e7,qe7,lamda,th3
			if fac3(0) eq 1 then fac3(1)=factor(w6,w7,x6,x7,e6,e7)
			if fac3(1) eq 0. then print,'No Overlap'
;print,fac3(1)

			w7=fac3(1)*w7
			e7=fac3(1)*e7
;print,e7*fac2(1),w7*fac2(1)

			z7=size(e7)

;print,fac2

			x=fltarr(z5(1)+z6(1)+z7(1))
			w=fltarr(z5(1)+z6(1)+z7(1))
			e=fltarr(z5(1)+z6(1)+z7(1))
			qe=fltarr(z5(1)+z6(1)+z7(1))
			for i=0,z5(1)-1 do begin
				x(i)=x5(i)
				w(i)=w5(i)
				e(i)=e5(i)
				qe(i)=qe5(i)
			endfor
			for i=z5(1),z5(1)+z6(1)-1 do begin
				x(i)=x6(i-z5(1))
				w(i)=w6(i-z5(1))
				e(i)=e6(i-z5(1))
				qe(i)=qe6(i-z5(1))
			endfor
			for i=(z5(1)+z6(1)),(z5(1)+z6(1)+z7(1)-1) do begin
				x(i)=x7(i-z5(1)-z6(1))
				w(i)=w7(i-z5(1)-z6(1))
				e(i)=e7(i-z5(1)-z6(1))
				qe(i)=qe7(i-z5(1)-z6(1))
			endfor
;e((z5(1)+z6(1)):(z5(1)+z6(1)+z7(1)-1))=0
;w((z5(1)+z6(1)):(z5(1)+z6(1)+z7(1)-1))=1e-11
;reverse(e((z5(1)+z6(1)):(z5(1)+z6(1)+z7(1)-1)))		

			xs=x(sort(x))
			ws=w(sort(x))
			es=e(sort(x))
			qes=qe(sort(x))

		endif
;for i=0,z5(1)-1 do print,x5(i),w5(i),e5(i),i
;for i=0,z6(1)-1 do print,x6(i),w6(i),e6(i),i
;for i=0,z7(1)-1 do print,x7(i),w7(i)*fac2(1)*fac3(1),e7(i)*fac2(1)*fac3(1),i

;		print,xs,ws,es		
		
;		print,x5,w5		

;print,w6
;print,w(z5(1):z5(1)+z6(1))

;print,'this is size',points(1)

;y=alog10(ws)
;e1=es/(ws*alog(10.))
;for i=0,points(1)-1 do begin
;	if e(i) gt 10 then e(i)=10
;endfor
;plot,alog10(x),y,yrange=[-12,1],psym=4

;ploterr,alog10(xs),y,e1
;yrange=[-12,1],psym=4
;print,y,e1

		ec=es(where(es))
		qec=qes(where(es))
		xc=xs(where(es))
		wc=ws(where(es))
		
if water(0) eq 1 then water(1)='water_LAMPascii'
if bglim eq 1 then bg(1)='limited'

		close,10
		openw,10,fil(1)
		printf,10,' db1:',dtb1(1),dtb1(2),' ref1:',refl1(1),refl1(2),' th1:',th1(1),' fac1:',fac1(1)
		printf,10,' db2:',dtb2(1),dtb2(2),' ref2:',refl2(1),refl2(2),' th2:',th2(1),' fac2:',fac2(1)
		printf,10,' db3:',dtb3(1),dtb3(2),' ref3:',refl3(1),refl3(2),' th3:',th3(1),' fac3:',fac3(1)
		printf,10,' water:',water(1),' lamda:',lamda(1),lamda(2),' fgrd:',r(1),' bgrd:',bg(1)
		close,10

		output,xc,wc,ec,qec,fil(1)
;print,'HERE IS bg',bg(1)


;print,alog10(xc),alog10(wc),e1





;		ploterr,xs,ws,es
;,xtitle='q',ytitle='log(Ref)'
;		oploterr,x6,w6,e6
;		oploterr,x7,w7,e7
;		plot,w2


		endmessage

	endif		

endif

return
end





pro data_read,num,w1,w2,runtime,dan

common varshit,tofd,opena,period,lamarr
common varshit2,nx,dett
common vars0,def
common varcrap,par2
common varpath,path

close,3

par1 = fltarr(128)
par2 = fltarr(256)
txt=sindgen(34)
txt(*)='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
txt1=txt(1)
name=path(1)+'00'+string(strtrim(num,2))

filcheck=findfile(name,count=checkfil)
if checkfil eq 0 then errormessage,'File not found: '+name

print,' '
print,' '
print,'opening... ',name
openr,3,name
print,'opened: ',name
readf,3,txt
print,txt(4)
print,txt(25)
readf,3,par1
readf,3,txt1
readf,3,txt1
readf,3,par2
dpr=180./!pi
print,'no of chans= ',par1(94),' chan width= ',par1(95),' tof delay= ',par1(96)
print,'x1= ',par1(97),' x2= ',par1(98),' y1= ',par1(99),' y2= ',par1(100)
nx=par1(101)
print,'nx= ',par1(101),' ny= ',par1(102)
print,'chop 1 speed req= ',par2(40),' chop 1 phase req= ',par2(41)
print,'chop 2 speed req= ',par2(42),' chop 2 phase req= ',par2(43)
print,'chop 1 speed act= ',par2(44),' chop 1 phase act= ',par2(45)
print,'chop 2 speed act= ',par2(46),' chop 2 phase act= ',par2(47)

; useful chopper variables
openr=45.-(par2(43)-par2(41))
opena=45.-(par2(47)-par2(45))

;! chopper opening offset =1.1!!!!

opena=opena-1.1

period=60./par2(44)
dela=(285.-3.3776-opena)/2.
delt=(dela/360.)*period
cht=85.e-3
chopsam=4.1138-(cht)/2.
chopmon=.455
tofd=chopsam+(par2(15)/1000.)
chanpa=(tofd/3956.)/(par1(95)*1.e-6)
chanpam=(chopmon/3956.)/(par1(95)*1.e-6)
r=chopsam/tofd
delchan=delt/(par1(95)*1e-6)
delechan=par1(96)/par1(95)
print,'TOF distance    = ',tofd, ' period chans    =',period/(par1(95)*1e-6)
print,'channels/Ang    = ',chanpa,' chop del chans  = ',delchan
print,'chopper period  = ',period*1000.,' elec del chans  = ',delechan
print,'opening      req= ',openr,' opening      act= ',opena
print,'chop delay angle= ',dela,' chop delay  time= ',delt*1000.,' ms'
;print,'     san= ',par2(2),' deg.'
print,' 27A TOF        = ',27.*chanpa,' 4.5A TOF        = ',4.5*chanpa
runtime=par1(2)/10.
print,'run time= ',runtime,' s',' det= ',par2(15),'dan = ',par2(16)
dan=par2(16)
dett=par2(15)
readf,3,txt1
readf,3,txt1
readf,3,txt1
readf,3,tot
tsize=long(par1(94))
xsize=long(par1(98)-par1(97)+1)
ysize=long(par1(100)-par1(99)+1)
dsize=xsize*ysize
if (tot ne (dsize*tsize+tsize)) then print,' Error in data array dimensions'
print,'tsize= ',tsize,' detector size= ',dsize
print,'xsize= ',xsize,' ysize= ',ysize,' tot1= ',dsize*tsize,' tot2= ',tot

det=lonarr(ysize,xsize,tsize)
if (ysize eq 1) then det=lonarr(xsize,tsize)

;det=lonarr(286,276)
mon=lonarr(tsize)
xy=lonarr(xsize,ysize)
xt=lonarr(xsize,tsize)
yt=lonarr(ysize,tsize)

c=1
readf,3,det,mon

;print,'mon= ',mon

nx=par1(101)

mmpp=1.04*nx

;useful area of det in mm
xminp=37
xmaxp=238

xminmm=xminp*mmpp
xmaxmm=xmaxp*mmpp


dpr=180./!pi

;help,th
;print,th
;if th(0) eq 1 then th(1)=par2(2)

;make lamda array
yy=findgen(tsize)
lamarr=(yy+delechan-delchan+0.5)/chanpa
	
;q=4*!pi*sin(th(1)/dpr)/lamarr

;m=(yy+delechan-delchan)/chanpam

w1=det
w2=mon

;print,w2
;print,y
;w4=total(w1,3)
;w3=total(w1,2)
;plot,mon

;make 2th array

;p0=135.79/nx

;mmpp=1.04*nx
;th=fltarr(xsize)


;for i=0,xsize-1 do th(i)=dan+dpr*atan(((p0-i)*mmpp)/dett)


;q=4*!pi*sin(th/dpr)/y


close,3
print,'total counts in detector = ',total(w1),' (',total(w1)/runtime,')'
;print,'total counts in monitor = ',total(w2),' (',total(w2)/runtime,')'
return

end


pro awater,w1,w2
w1=w1
xsize=286
;ysize=275
w2=fltarr(xsize)
;w3=fltarr(ysize) & w4=fltarr(xsize,ysize)

;useful area 37:238,30:248
x1=37 & x2= 238
;y1=30 & y2=248
help,w1
w2=total(w1,2)
;help,w2
;w3=total(w1,1)

w2=w2/(mean(w2(x1:x2)))
;w3=w3/(mean(w3(x1:x2)))
;w4=w1/mean(w1(x1:x2,y1:y2))
;w4(0:x1-1,*)=1 & w4(x2+1:xsize-1,*)=1 & w4(*,0:y1-1)=1 & w4(*,y2+1:ysize-1)=1 
w2(0:x1-1)=1 & w2(x2+1:xsize-1)=1
;w3(0:y1-1)=1 & w3(y2+1:ysize-1)=1

;print,!stime
return
end

function tth,d0,p0,dr,pr,nx,det
;TAKE_DATP,p
dpr=180./!pi

pcen=135.79/nx
mmpp=1.04*nx

print,'pcen= ',pcen,' mmpp= ',mmpp
th=(dr+dpr*atan((pcen-pr)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
print,'ref th= ',th
  
return,th
end



pro anal,db,dber,dbdan,ref,refer,refdan,water,r,bg,wdum,xdum,edum,qedum,lamda,th
help,water
help,db
wdum=0
xdum=0
edum=0
qedum=0

!except=0

common varshit,tofd,opena,period,lamarr
common varshit2,nx,dett
common vars0,def
common varcrap,par2
common varstuff,bgrd
common varcrap1,bglim

;pro anal,db,ref,fac,water,q,r,bg,w5,x5,e5,chop
;help,db,ref,water


; db is the direct beam run
; ref is the reflection run
; water is the x effiency of the detector
; q is the array of q(A^-1)
; r is the range over which to sum the intensity
; bg is the range to sum the background either side of r
; w5 is the output reflectivity with the q (x5) and error (e4)
; fac is the normalisation factor for w2 reflection data ie
; divide the ref data by this to make it normalised to the direct beam

chop=0
ref1=ref
ref2=ref
db1=db
db2=db
edb=db
ere=ref
detmax=238
detmin=37

z=size(db)
print,'db size: ',z(2)

;water correction
for i=0,z(2)-1 do begin
  db1(*,i)=db(*,i)/water
  dber(*,i)=dber(*,i)/water
  ref1(*,i)=ref(*,i)/water
  refer(*,i)=refer(*,i)/water
endfor

print,'done water correction'
;print,water
;print,db1(f1-bg:f1-1,i)

dbtot=total(db,2)
dbtotm=max(total(db,2),dbm)

f1=dbm-(r-1)/2
f2=dbm+(r-1)/2

;COM for db
numersum=0
denomsum=0
for i=f1,f2 do begin
  numer=dbtot(i)*i
  denom=dbtot(i)
  numersum=numer+numersum
  denomsum=denom+denomsum
endfor

dbcom=numersum/denomsum

;print,fix(dbcom),dbm
if fix(dbcom) ne dbm then dbm=fix(dbcom)

f1=dbm-(r-1)/2
f2=dbm+(r-1)/2

print,'db peak at: ',fix(dbcom)

if (f1-bg lt detmin) or (f2+bg gt detmax) then begin
  if f1-detmin le detmax-f2 then bg=f1-detmin else bg=detmax-f2
  print,'Outside useful area of detector, bg has been limited to ',bg
  bglim=1
endif else bglim=0

reftot=total(ref(*,40:z(2)-1),2)
reftotm=max(reftot,refm)

ff1=refm-(r-1)/2
ff2=refm+(r-1)/2

;COM for ref
numersum=0
denomsum=0
for i=ff1,ff2 do begin
  numer=reftot(i)*i
  denom=reftot(i)
  numersum=numer+numersum
  denomsum=denom+denomsum
endfor

refcom=numersum/denomsum

;if fix(refcom) ne refm then refm=fix(refcom)

ff1=refm-(r-1)/2
ff2=refm+(r-1)/2
  
;print,fix(refcom),refm
print,'ref peak at: ',fix(refcom)

if (ff1-bg lt detmin) or (ff2+bg gt detmax) then begin
  if ff1-detmin le detmax-ff2 then bg=ff1-detmin else bg=detmax-ff2
  print,'Outside useful area of detector, bg has been limited to ',bg
  bglim=1
endif else bglim=0



subarr=indgen(z(1))
dbb=fltarr(z(1),z(2))
rb=fltarr(z(1),z(2))
ed=fltarr(z(1),z(2))
eb=fltarr(z(1),z(2))
er=fltarr(z(1),z(2))

if bg gt 0 then begin


if bgrd eq 0 then begin

  for i=0,z(2)-1 do begin

    db2(*,i)=db1(*,i)-mean(db1(f1-bg:f1-1,i))
    ref2(*,i)=ref1(*,i)-mean(ref1(ff1-bg:ff1-1,i))

    dbb(*,i)=(sqrt(total(dber(f1-bg:f1-1,i)^2,1)))/bg
    rb(*,i)=(sqrt(total(refer(ff1-bg:ff1-1,i)^2,1)))/bg

    ed(*,i)=sqrt(dber(*,i)^2+dbb(*,i)^2)
    er(*,i)=sqrt(refer(*,i)^2+rb(*,i)^2)

   endfor
      
endif


if bgrd eq 1 then begin

  for i=0,z(2)-1 do begin

    db2(*,i)=db1(*,i)-(mean(db1(f1-bg:f1-1,i))+mean(db1(f2+1:f2+bg,i)))/2
    ref2(*,i)=ref1(*,i)-(mean(ref1(ff1-bg:ff1-1,i))+mean(ref1(ff2+1:ff2+bg,i)))/2

    dbb(*,i)=(sqrt(total(dber(f1-bg:f1-1,i)^2,1)+total(dber(f2+1:f2+bg,i)^2,1)))/(2.*bg)
    rb(*,i)=(sqrt(total(refer(ff1-bg:ff1-1,i)^2,1)+total(refer(ff2+1:ff2+bg,i)^2,1)))/(2.*bg)

    ed(*,i)=sqrt(dber(*,i)^2+dbb(*,i)^2)
    er(*,i)=sqrt(refer(*,i)^2+rb(*,i)^2)

  endfor

endif


if bgrd eq 2 then begin

;subtract backgrd from db
for i=0,z(2)-1 do begin

dbbgsubarr=intarr(2*bg)
dbbgarr=fltarr(2*bg,z(2))
dbbgfit=fltarr(z(1),z(2))

dbbgsubarr(0:bg-1)=subarr(f1-bg:f1-1) 
dbbgsubarr(bg:2*bg-1)=subarr(f2+1:f2+bg) 

dbbgarr(0:bg-1,i)=db1(f1-bg:f1-1,i)
dbbgarr(bg:2*bg-1,i)=db1(f2+1:f2+bg,i)

;print,dbbgsubarr
;print,dbbgarr
;print,db1(f1-bg:f1-1,i)

err=fltarr(2*bg)
for j=0,2*bg-1 do if sqrt(dbbgarr(j,i)) eq 0 then err(j)=1. else err(j)=sqrt(dbbgarr(j,i)+1)

dbbgcoeff=linfit(dbbgsubarr,dbbgarr(*,i),sdev=err,chisq=dbchisq,sigma=dbbgerr,/double)
;print,'dbchi',dbchisq
;print,dbbgsubarr,dbbgarr(*,i),err,dbchisq,dbchisq/(2*bg)

;print,'gothere'
;if dbchisq ne 1 then begin
;dbbgcoeff=linfit(dbbgsubarr(0:bg-1),dbbgarr(0:bg-1,i),sdev=err(0:bg-1),chisq=dbchisq,sigma=dbbgerr)
;endif
;print,'gothere'
;print,dbbgsubarr(0:bg-1),dbbgarr(0:bg-1,i),err(0:bg-1),dbchisq,dbchisq/(bg)

dbbgfit(*,i)=dbbgcoeff(0)+dbbgcoeff(1)*subarr

db2(*,i)=db1(*,i)-dbbgfit(*,i)

dbb(*,i)=sqrt((subarr*dbbgerr(1))^2+dbbgerr(0)^2)

ed(*,i)=sqrt(dber(*,i)^2+dbb(*,i)^2)

print,'dbfiterr',i,dbbgerr

endfor

;subtract backgrd from ref
for i=0,z(2)-1 do begin

refbgsubarr=intarr(2*bg)
refbgarr=fltarr(2*bg,z(2))
refbgfit=fltarr(z(1),z(2))

refbgsubarr(0:bg-1)=subarr(ff1-bg:ff1-1) 
refbgsubarr(bg:2*bg-1)=subarr(ff2+1:ff2+bg) 

refbgarr(0:bg-1,i)=ref1(ff1-bg:ff1-1,i)
refbgarr(bg:2*bg-1,i)=ref1(ff2+1:ff2+bg,i)

err=fltarr(2*bg)
for j=0,2*bg-1 do if sqrt(refbgarr(j,i)) eq 0 then err(j)=1. else err(j)=sqrt(refbgarr(j,i)+1)

refbgcoeff=linfit(refbgsubarr,refbgarr(*,i),sdev=err,chisq=refchisq,sigma=refbgerr,/double)
;print,'refchi',refchisq

;if refchisq ne 1 then begin
;refbgcoeff=linfit(refbgsubarr(0:bg-1),refbgarr(0:bg-1,i),sdev=err(0:bg-1),chisq=refchisq,sigma=refbgerr)
;endif

refbgfit(*,i)=refbgcoeff(0)+refbgcoeff(1)*subarr

ref2(*,i)=ref1(*,i)-refbgfit(*,i)

rb(*,i)=sqrt((subarr*refbgerr(1))^2+refbgerr(0)^2)

er(*,i)=sqrt(refer(*,i)^2+rb(*,i)^2)

print,'reffiterr',i,refbgerr

endfor

endif

endif else begin
 
  ref2=ref1
  db2=db1
  ed=dber
  er=refer

endelse

r=total(ref2(ff1:ff2,*),1)
d=total(db2(f1:f2,*),1)

print,'done background correction',f1-bg,f1-1,f1,f2,f2+1,f2+bg

;error calculation

ed=sqrt(total(ed(f1:f2,*)^2,1))

er=sqrt(total(er(ff1:ff2,*)^2,1))

;help,r & help,d & help,er & help,ed

reff=r
ereff=r

dpr=180./!pi

if th(0) eq 1 then th(1)=tth(dbdan,dbcom,refdan,refcom,nx,dett)
;if th(0) eq 1 then th(1)=par2(2)

q=4*!pi*sin(th(1)/dpr)/lamarr

for i=0,z(2)-1 do begin
  if ((r(i) gt 0.) and (d(i) gt 0.)) then begin
    reff(i)=r(i)/d(i)
    ereff(i)=reff(i)*sqrt((er(i)/r(i))^2+(ed(i)/d(i))^2) 
    print,' good point',i,ereff(i)/(reff(i)*alog(10.)),alog10(reff(i)),alog10(q(i))
  endif else begin
    ereff(i)=0
    reff(i)=1e-11
    print,'bum point',i,ereff(i),reff(i),q(i)  
  endelse
endfor

;resolution (error in q)
etf=((85e-3)/tofd)+(3956*opena*period)/(360*tofd*lamarr)

if par2(50) ge par2(54) then bigslit=par2(50) else bigslit=par2(54)
ethf=(bigslit*180)/(th(1)*3.4*!pi)

print,'slits',par2(54),par2(50)

qe=q*sqrt(etf^2+ethf^2)


; unlogged data for reflectivity is in reff, q is q and error is ereff
if lamda(0) eq 1 then begin

	qmin=(min(q))
	qmax=(max(q))

endif else begin

	dpr=180./!pi

	qmin=4*!pi*sin(th(1)/dpr)/lamda(2)
	qmax=4*!pi*sin(th(1)/dpr)/lamda(1)

endelse


b=where((q ge qmin) and (q le qmax))
;print,b,q(min(b)),q(max(b))

wdum=reverse(reff(min(b):max(b)))
xdum=reverse(q(min(b):max(b)))
edum=reverse(ereff(min(b):max(b)))
qedum=reverse(qe(min(b):max(b)))
;print,wdum,xdum,edum

print,bg
return
end











pro output,x,y,e,xe,fil

common varsb,base
;common vars10,y,x,e
points=size(x)

close,10
openw,10,fil,/append
for i=0,points(1)-1 do begin
	printf,10,x(i),y(i),e(i),xe(i)
endfor
close,10

plotbase=widget_base(group_leader=base,/floating,title='Reflectivity Plot',/column)
graph=widget_draw(plotbase,xsize=700,ysize=500,retain=2)
but=widget_button(plotbase,value='OK',uvalue='ok')

widget_control,plotbase,/realize

;wset,graph

ploterr,x,alog10(y),e/(y*alog(10.))

xmanager,'message',plotbase

return
end


pro endmessage

common varsb,base
common vars4,fac1
common vars5,fac2
common vars6,fac3
common vars7,th1
common vars8,th2
common vars9,th3
common vars10,check0,check1
common vars,lab,txt,but,gap
common varbull,norm,bg,r,fil
common varstuff,bgrd

res=intarr(20)

if norm eq 0 then nrm='runtime' else nrm='monitor'
if bgrd eq 0 then bgd='left'
if bgrd eq 1 then bgd='both'
if bgrd eq 2 then bgd='fit'

endbase=widget_base(group_leader=base,/floating,title='Finished',column=2)

;column1
res(0)=widget_label(endbase,value='fac1: '+strcompress(string(fac1(1)),/remove_all),/align_left)
res(2)=widget_label(endbase,value='fac2: '+strcompress(string(fac2(1)),/remove_all),/align_left)
res(2)=widget_label(endbase,value='fac3: '+strcompress(string(fac3(1)),/remove_all),/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Normalised: '+nrm,/align_left)
res(2)=widget_label(endbase,value='Backgrd used: '+bgd,/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Saved to: '''+strcompress(string(fil(1)),/remove_all)+'''',/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Go again?')
gap(0)=widget_label(endbase,value='',ysize=0)

;column2
res(1)=widget_label(endbase,value='th1: '+strcompress(string(th1(1)),/remove_all),/align_left)
res(1)=widget_label(endbase,value='th2: '+strcompress(string(th2(1)),/remove_all),/align_left)
res(1)=widget_label(endbase,value='th3: '+strcompress(string(th3(1)),/remove_all),/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Backgrd: '+strcompress(string(bg(1)),/remove_all),/align_left)
res(2)=widget_label(endbase,value='Foregrd: '+strcompress(string(r(1)),/remove_all),/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
gap(0)=widget_label(endbase,value='',ysize=0)
gap(0)=widget_label(endbase,value='',ysize=0)
endbut1=widget_button(endbase,value='Yes',uvalue='yes')
endbut2=widget_button(endbase,value='No',uvalue='no')

widget_control,endbase,/realize

xmanager,'message',endbase

return
end


pro errormessage,mes1,mes2,mes3

common varsb,base

errbase=widget_base(group_leader=base,/floating,title='Error Message',/column)
mess1=widget_label(errbase,value=mes1)
;mess2=widget_label(errbase,value=mes2)
;mess3=widget_label(errbase,value=mes3)
errbut=widget_button(errbase,value='OK',uvalue='ok')

widget_control,errbase,/realize

xmanager,'message',errbase

return
end

pro message_event,event

common varsb,base
common vars4,fac1
common vars5,fac2
common vars6,fac3
common vars7,th1
common vars8,th2
common vars9,th3
common vars10,check0,check1
common vars,lab,txt,but,gap

widget_control,event.id,get_uvalue=ev,get_value=val

widget_control,event.top,/destroy

if ev eq 'yes' then begin

	widget_control,base,sensitive=1
	;widget_control,txt(14),set_value=strcompress(string(fac1(1)),/remove_all)
;	widget_control,txt(9),set_value=strcompress(string(th1(1)),/remove_all)
;	if check0 eq 1 then begin
;		;widget_control,txt(15),set_value=strcompress(string(fac2(1)),/remove_all)
;		;widget_control,txt(10),set_value=strcompress(string(th2(1)),/remove_all)
;	endif
;	if (check0 eq 1) and (check1 eq 1) then begin
;		;widget_control,txt(16),set_value=strcompress(string(fac3(1)),/remove_all)
;		;widget_control,txt(11),set_value=strcompress(string(th3(1)),/remove_all)
;	endif

endif	

if ev eq 'no' then widget_control,base,/destroy

if ev eq 'ok' then widget_control,base,sensitive=1
	
return
end
