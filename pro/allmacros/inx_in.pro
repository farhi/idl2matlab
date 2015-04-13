function inx_in, INST , PATH , FILENAME , STATUS , DATP
;
; Read INX files
stat=0 & catch,stat
if stat ne 0 then begin catch,/cancel & print,string(7b),!err_string & return,1 & endif
;	code 11 for open error
        if n_elements(datp) gt 0 then full_call=1 $
	else full_call=0
        if   full_call eq 0 then in_file=inst $
	else in_file=filename

	w_out  =11
	e_out  =0
	status =11
        on_ioerror, no_f
        openr,in,in_file,/get_lun
		w_out  =14
		nzone  =intarr(6)
        	nchans =0 & nlines=0
		tit_in =' '
		tmp_ang=0.0
        	p_out  =fltarr(31)
		einc   =0.0 & qinc=0.0 & temp=0.0 & amass=0.0 & isym=0
		deltaen=0.0 & deltatau=0.0 & deltak=0.0
	
       		on_ioerror, end_f
		   i=0
		   
		   while (1) do begin
			readf,in,nlines,nzone,nchans
			readf,in,tit_in
			readf,in,tmp_ang,einc,qinc,temp,amass,isym
			readf,in,deltaen,deltatau,deltak
			tmp=fltarr(3,nchans)
			readf,in,tmp,format='(5x,f10.5,e13.5,e12.4)'
			if i eq 0 then x_buf=tmp(0,*) $
			  	  else x_buf=[[x_buf],[tmp(0,*)]]
 			if i eq 0 then w_out=tmp(1,*) $
			  	  else w_out=[[w_out],[tmp(1,*)]]
 			if i eq 0 then e_out=tmp(2,*) $
			  	  else e_out=[[e_out],[tmp(2,*)]]
			if i eq 0 then y_buf=[tmp_ang] $
			  	  else y_buf=[[y_buf],[tmp_ang]]
			i=i+1
		   endwhile
 		end_f: free_lun,in

		if (i gt 0) and (nchans gt 1) then begin

		w_out=reform(reform(w_out,nchans,i))
		e_out=reform(reform(e_out,nchans,i))
		x_buf=reform(reform(x_buf,nchans,i))
		y_buf=reform(y_buf,i)
		
		if x_buf(0) gt x_buf(1)  then x_buf=-x_buf
		if (size(x_buf))(0) eq 2 then begin
					 y_buf=transpose([[y_buf],[y_buf]])
					 y_buf=congrid  (  y_buf ,nchans,i)
		endif

		status=9
         	datp={x:x_buf,y:y_buf,p:fltarr(31), $
		      e:e_out,w_tit:' ',x_tit:' ',y_tit:' ',$
     		      par_txt:strarr(31),other_tit:' '}

		datp.p(8)  =tmp_ang
		datp.p(11) =temp
		datp.p(15) =amass
		datp.p(16) =1				;data read by inx_in
		datp.p(17) =1				;scale=eV
		datp.p(18) =deltatau			;channel width
		datp.p(19) =nchans
		datp.p(30) =i
		datp.p(21) =sqrt(81.799/einc)
		datp.par_txt(8)= 'Sample Angle (deg.)                 '
		datp.par_txt(11)='Sample Temperature (K)              '
		datp.par_txt(15)='Sample Mass                         '
		datp.par_txt(16)='Energy Conversion                   '
		datp.par_txt(17)='Energy Scale                        '
		datp.par_txt(18)='Channel width                       '
 		datp.par_txt(19)='Number of channels used             '
		datp.par_txt(30)='Number of angles                    '
		datp.par_txt(21)='Wavelength (angstroms)              '
		datp.w_tit= tit_in
		datp.x_tit='Energy Transfer'
		datp.y_tit='Angle'
		datp.other_tit='INX file: '+in_file

		if (datp.p(21) gt 1.) then status=0

		endif
 no_f:
        
if full_call eq 0 then give_datp,datp 
return,w_out
end
