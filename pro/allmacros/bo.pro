pro macro_blap, stop_butt,mot,nm,min,max,nn
on_error, 2

on_error, 2
common dialshare2 & common madshare & common colours
common alphabet
;max=6.5
;min=1.0
;nn=100
numb=100
;nm=2
;mot='san'
s=fltarr(nn)
for ii=0,n_elements(s)-1 do begin
  r=randomu(seed,10)
  a=r(3)*(max-min)+min
  b=mot+' '+string(a)
  mad,b
  wait,.5
  while (mad_status(t_nother) eq 'POSITIONNING' ) do i=i
  wait,0.5       
if  stop_macro(stop_butt) then  return
  aa=get_mad_pos(act)
  print,a,act(nm), a-act(nm)
  s(ii)=a-act(nm)
  result=moment(s)
  tit=string(ii)+' mean= '+string(result(0))+'  rms= '
  tit2=string(sqrt(result(1)))+' skewness='+string(result(2))
  tit=tit+tit2
  b=(max(s)-min(s))/numb
  window,0
  x=fltarr(numb)
  x=findgen(numb)*b+min(s)+b/2
  plot,x,histogram(s,min=min(s),max=max(s),binsize=b),$
  color=green,title=tit,xtitle=mot+' error' 

;if  stop_macro(stop_butt) then  return
;	a=get_mad_nother(t_nother)
;	if t_nother.cstate_flags ne 0 then begin
;		print, t_nother.cstate_flags
;		c= mad_status(t_nother)
;		print, c
;	endif
endfor

end
