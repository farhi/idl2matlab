;	*********
;	*********
	pro alpha
;	*********
;	*********

;GIVE THE FILE NAMES
;*******************
 print ,''
 infile=''
 read,' Name of data  file: ',infile
 alphaf=''
 read,' Name of alpha file: ',alphaf
 print ,''

;OPEN THE FILES
;**************
 on_ioerror,mis1
 openr,u1,infile,/get_lun
 on_ioerror,mis2
 openr,u2,alphaf,/get_lun

;READ DATA FILE ----> .ima , .gel , LAMPbin
;**************
 print,' Reading data file '+infile+ '  ...'
 on_ioerror,mis3
 offset=bytarr(158)
 data  =intarr(4000,2000)
 idx   =strpos(infile,'LAMP')
 if idx lt 0 then readu,u1,offset,data
 if idx ge 0 then readu,u1,       data
 close,u1
 if !D.name eq 'X' then begin
		device,retain=2
 		w1=congrid(data,128,64) > 0
		window,1,xsize=512,ysize=512,title='RAW DATA'
 		shade_surf,w1
 		endif

;READ ALPHA FILE
;***************
 print,' Reading alpha file '+alphaf+ '  ...'
 on_ioerror,mis4
 alpha  =intarr(4000,2000)
 readu,u2,alpha
 close,u2

;CORRECTION
;**********
 print,' Start correction ...'
 s=size(data)
 if s(s(0)+1) eq 2 then begin
 
	index=where ( data lt 0 )
	
	data =long  ( data )
	
	if index(0) ge 0 then data(index)=65536+ data(index)
	index=0
 endif
 data=temporary(data)*alpha/100
 data=fix(data)
 if !D.name eq 'X' then begin
 		w1=congrid(data,128,64) > 0
		window,2,xsize=512,ysize=512,title='CORRECTED DATA'
 		shade_surf,w1
 		endif
 
;WRITE RESULT
;************
 pos=0 & idx=0
 while  pos ge 0 do begin
 	pos=strpos(infile,'/',idx)
 	if  pos ge 0 then idx=pos+1
 endwhile
 
 outfile=strmid(infile,idx,50)+'_cor'
 print,' Writing result to ',outfile+ '  integer*2(4000,2000)  ...'
 on_ioerror,mis5
 openw ,u,outfile,/get_lun
 writeu,u,data
 close ,u
 print,string(7b)
 if !D.name eq 'X' then read,' OK ?: ',infile
 
return

mis1:print,string(7b),'? Error opening data  file'
return
mis2:print,string(7b),'? Error opening alpha file'
return
mis3:print,string(7b),'? Error reading data  file'
return
mis4:print,string(7b),'? Error reading alpha file'
return
mis5:print,string(7b),'? Error writing result file'
return

end

;	****
;	****
;	MAIN
;	****
;	****

!quiet=1
 print ,''
 print ,''
 print ,' Data Correction Module for LADI  (C. Wilkinson & D. Richard)'
 print ,''
 print ,''

 alpha
 exit
 end
 
