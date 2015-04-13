;Enter PRO or FUNCTION macro below. (the call is macro or a=macros() )

PRO igor,x,w,e
    print,!stime+' by macro'
FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
  openw,/get,fil,"fil"+strcompress(i,/re)
  FOR j=0,N_ELEMENTS(w(*,0))-1 DO BEGIN
    printf,fil,x(j),w(j,i),e(j,i)
  ENDFOR
  free_lun,fil
ENDFOR
return
end
