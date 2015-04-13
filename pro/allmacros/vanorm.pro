function vanorm, w_in, w_van, ch1,ch2
;******* ******
;**
;** Normalize W_in with Vanadium elastic peak from channel ch1 to ch2.

;** Check for consistancies
    w_out=w_in
    s_in=size(w_in) & s_vn=size(w_van) & c1=n_elements(ch1) & c2=n_elements(ch2)
    if (c1 le 0) then ch1=1
    if (c2 le 0) then ch2=s_vn(1)

    if (s_in(0) eq 2) and (s_vn(0) eq 2)   then begin
	if (s_in(2) eq s_vn(2))		   then begin
	    if (ch1 le ch2) and (ch1 gt 0) and (ch2 le s_vn(1)) then begin

		TAKE_DATP,P
		w0=total(w_van(ch1-1:ch2-1,*),1) >1
		w0=max(w0)/w0

		for i=0,s_in(2)-1 do w_out(*,i)=w_in(*,i)*w0(i)
		
		IF n_elements(P.e) eq n_elements(w_in) then $
		for i=0,s_in(2)-1 do P.e(*,i)  =P.e(*,i) *w0(i)

		P.x_tit=P.x_tit+' vanorm used'
		GIVE_DATP,P

    	    endif else print, string(7b)+' Inconsistant channels ......' 
    	endif     else print, string(7b)+' Inconsistant angles ......'
     endif        else print, string(7b)+' Inconsistant Workspace dimensions ......'

return,w_out
end
