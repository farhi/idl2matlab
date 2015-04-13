pro cell,s,cell,_extra=extra,index=index,counts=counts,$
             noplot=noplot,oplot=oplot,error=error

index=indgen(N_ELEMENTS(s(0,*)))+cell
counts=s(cell,*)
IF NOT KEYWORD_SET (noplot) THEN BEGIN
  IF KEYWORD_SET(oplot) THEN BEGIN
    oplot,index,counts,_extra=extra
  ENDIF ELSE plot,index,counts,_extra=extra
  IF KEYWORD_SET(error) THEN BEGIN
    oplot,index,counts+SQRT(counts),psym=3,_extra=extra
    oplot,index,counts-SQRT(counts),psym=3,_extra=extra
  ENDIF
ENDIF
end
