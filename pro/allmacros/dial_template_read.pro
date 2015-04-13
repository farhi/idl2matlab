function DIAL_TEMPLATE_READ, name, sequence, ctxt
;******* ******************
;**
common template_read, seq, ran, val

if n_elements (seq) eq 0 then seq=-1.
if n_elements (sequence) ne 1 then sequence=seq+1.
if sequence ne seq then begin seq=sequence & val=randomu(ran,1) & endif

return, val(0)
end
