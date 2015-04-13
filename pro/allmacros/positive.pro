function positive, w_in
;******* ********
;**
;** Transform an integer*2 unsigned array in a long positive one.
;** Call: W1 = POSITIVE ( W1 )

    s=size(w_in)
    if s(s(0)+1) eq 2 then begin

	index=where ( w_in lt 0 )
	
	w_in       =long  (w_in)
	
	if index(0) ge 0 then w_in(index)=65536+ w_in(index)
	
    endif
    	
    return, w_in
    
end
