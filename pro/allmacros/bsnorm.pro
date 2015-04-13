function bsnorm, w_in
;******* *****
;**
;** Normalize W_in with monitor spectrum for in10, in16, in13

    w_out=float(w_in)
    s    =size (w_in)

;** Check for consistancies
    if (s(0) gt 0) and (s(0) lt 3) then begin 
	TAKE_DATP,P
	sp=size(P.n)
	P.n=P.n>1
;**	Normalize
	if s(0) eq 2 then begin
          if (s(1) eq sp(1)) or sp(0) eq 0 then $
			for i =0,s(2)-1 do w_out(0,i)=w_out(*,i)/P.n $
	  else		for i =0,s(2)-1 do w_out(0,i)=w_out(*,i)/P.n(i)
	endif
	if s(0) eq 1 then 		   w_out     =w_out     /P.n

;**	Errors
	IF n_elements(P.e) eq n_elements(w_in) then begin
	if s(0) eq 2 then begin
          if (s(1) eq sp(1)) or sp(0) eq 0 then $
			for i =0,s(2)-1 do P.e(0,i)  =P.e(*,i)/P.n $
	  else		for i =0,s(2)-1 do P.e(0,i)  =P.e(*,i)/P.n(i)
	endif
	if s(0) eq 1 then 		   P.e       =P.e     /P.n
	ENDIF
	
	P.n(*)=1
	P.x_tit=P.x_tit+' normalized'
	GIVE_DATP,P
    endif

return,w_out
end
