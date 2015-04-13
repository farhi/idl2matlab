pro mod_datp,datp,tag,val
;** ********
sz=size(tag)
if (n_tags(datp) gt 0) and (sz(1) gt 0) and (sz(2) eq 1) and (n_elements(val) gt 0) then begin

	    		     x=0 & y=0 & z=0 & e=0 & n=0 & pv=0 & p=0      & par_txt=''
	    		     w_tit=''  & x_tit=''  & y_tit=''   & z_tit='' & other_tit='' & time=''
	    		     tlist=tag_names(datp)
	    		     for k=0,n_elements(tlist)-1 do begin
	    		         CASE tlist(k) of
	    		         
	    		         'X':	 x =datp.x
	    		         'Y':	 y =datp.y
	    		         'Z':	 z =datp.z
	    		         'E':	 e =datp.e
	    		         'N':	 n =datp.n
	    		         'PV':	 pv=datp.pv
	    		         
	    		         'W_TIT':    w_tit    =datp.w_tit
	    		         'X_TIT':    x_tit    =datp.x_tit
	    		         'Y_TIT':    y_tit    =datp.y_tit
	   			 'Z_TIT':    z_tit    =datp.z_tit
	   			 'OTHER_TIT':other_tit=datp.other_tit
	   			 'TIME' :    time     =datp.time
	   			 
	   			 'P'	  :  p        =datp.p
	   			 'PAR_TXT':  par_txt  =datp.par_txt
	   			  ELSE:
	   			  ENDCASE
	    		     endfor
	    		     
	    		     iii=execute(tag+'=val')
      			     DATP={X:x,Y:y,Z:z,E:e,N:n,PV:pv,W_TIT:w_tit,X_TIT:x_tit    ,$
      			     	   Y_TIT:y_tit,Z_TIT:z_tit,OTHER_TIT:other_tit,TIME:time,$
      			     	   P:p,PAR_TXT:par_txt}
endif
return
end
