function mnorm13, w_in
;******* *******
;**
;** Normalize W_in with monitor spectrum for in13

;** Check for consistancies

    w_out=float(w_in)
    s    =size (w_in)

    if (s(0) gt 0) and (s(0) lt 3) then begin 
       TAKE_DATP,P
       if s(0) eq 2 then for i =0,s(2)-1 do w_out(0,i)=w_out(*,i)/(P.n(i)>1)
       if s(0) eq 1 then 		    w_out     =w_out     /(P.n>1)
       P.n(*)=1
       P.x_tit=P.x_tit+' normalized'
       GIVE_DATP,P
    endif

return,w_out
end
