pro write_inx, o_file,w_buf ,XC=x_buf, YC=y_buf, PR=pmt_buf, E=e_buf ,$
                             ZC=z_buf,  N=n_buf, PV=pv,PAR_TXT=p_txt ,$
                             W_tit=wt  , X_tit=xt , Y_TIT=yt         ,$
                             Z_tit=zt  , OTHER_TIT=ot
;** *********
;**
; Dumps an INX file - parameters may be a bit odd

pi=3.1416

; Prepare zones
   nzone=intarr(7)
   nzone(1)=1
   nzone(2)=2
   nzone(3:6)=0

   my_check=size(w_buf)

;
; What dimensions for the data?
      case my_check(0) of
         0: return
         1: begin
               nchans=my_check(1)
               nspecs=1
            end
         2: begin
               nchans=my_check(1)
               nspecs=my_check(2)
            end
	 else: return
      endcase
   
   if n_elements(w_buf) ne n_elements(e_buf) then e_buf=w_buf*0.
   if (size(y_buf))(0) eq 2 then y_buf=reform(y_buf(0,*))
   sx=(size(x_buf))(0)
   
; Get the parameters
      temp	=pmt_buf(11) 
      einc	=81.799/pmt_buf(21)/pmt_buf(21) 
      qinc	=2*pi/pmt_buf(21) 
      amass	=1.0
      nlines	=nchans+3
      deltatau	=pmt_buf(18)
      deltaen	=0.0
      deltak	=0.0
      isym	=0
      dumme	=0.0

      on_ioerror,pb_wrt  
      get_lun,ifile
      openw,ifile,o_file
				frmv='(5x,f10.5,e13.5,e12.4)'
       if max(y_buf) ge 10 then frmt='(x,f6.2,f8.3,f8.4,f9.3,f6.1,i2)' $
			   else frmt='(x,f6.4,f8.3,f8.4,f9.3,f6.1,i2)'
       for i=0,nspecs-1 do begin
	 printf ,ifile,nlines,nzone(1:6),nchans          ,format='(8i5)'
	 printf ,ifile,wt
	 printf ,ifile,y_buf(i),einc,qinc,temp,amass,isym,format=frmt
	 printf ,ifile,deltaen,deltatau,deltak           ,format='(16x,3f8.4)'
	 if sx eq 1 then for j=0,nchans-1 do $
	  printf,ifile,x_buf(j)  ,w_buf(j,i),e_buf(j,i)  ,format=frmv

	 if sx eq 2 then for j=0,nchans-1 do $
	  printf,ifile,x_buf(j,i),w_buf(j,i),e_buf(j,i)  ,format=frmv

       endfor
       pb_wrt:free_lun,ifile
    return
    end	    
      
