Function sdev,w,relation=relation
if n_elements(w(0,*)) ge 3 then begin
  take_datp,datp
  result=fltarr(N_elements(w(*,0)))
  IF KEYWORD_SET(relation) THEN relation=result
  for i=0,N_elements(w(*,0))-1 do begin
    if total(abs(w(i,*))) gt 0 then begin
      mean=moment(w(i,*),sdev=tmp) 
      result(i)=tmp
    endif else result(i)=0.
    IF KEYWORD_SET(relation) THEN if total(datp.e(i,*)) gt 0 then relation(i)=result(i)/(total(datp.e(i,*)))*n_elements(w(0,*))
  endfor
  datp.y_tit='sdev'
  give_datp,datp
  return,result/sqrt(N_ELEMENTS(w(0,*)))
endif else begin
  print,'Not enough data sets'
  relation=datp.e
  return,w
endelse
end
