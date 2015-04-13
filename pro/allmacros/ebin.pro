;***************************************;

pro interpol,x_in,w_in,w_out,E,t,i            ,E_IN,E_OUT
;
; Do interpolation
;
  m=(w_in(t,*)-w_in(t-1,*))/(x_in(t)-x_in(t-1))
  b= w_in(t,*)-m*x_in(t)
  w_out  (i,*)=m*E+b

  if n_elements(e_out) gt 1 then begin
    r=(e_in(t,*)^2+e_in(t-1,*)^2)/(x_in(t)-x_in(t-1))
    s= e_in(t,*)^2  + r*x_in(t)
    e_out  (i,*)=SQRT(r*E+s)
  endif
  i=i+1
end

;***************************************;

pro calculate,Estep,x_in,w_in,w_out,E,t,i,nch ,E_IN,E_OUT
;
; Average over values within E..E+Estep
;
  counter=0
  e_out(i,*)=e_out(i,*)^2
  while (t lt nch-1) and (x_in(t) lt E+Estep)  do begin     
    w_out(i,*)=w_out(i,*)+w_in(t,*)
    if n_elements(e_out) gt 1 then e_out(i,*)=e_out(i,*)+e_in(t,*)^2
    t=t+1
    counter=counter+1
  endwhile
  w_out(i,*)=w_out(i,*)/counter
  if n_elements(e_out) gt 1 then e_out(i,*)=SQRT(e_out(i,*))/counter
  i=i+1
end
;***************************************;

pro starter,Estep,x_in,w_in,w_out,E,t,i       ,E_IN,E_OUT
;
; Find starting point
;
  while (x_in(t) lt E) do t=t+1
  while (x_in(t)-E) gt Estep do begin
    if t eq 0 then w_out(t,*)=0 $
    else interpol,x_in,w_in,w_out,E,t,i       ,E_IN,E_OUT
    E=E+Estep
  endwhile
end



function ebin,in_wk,Emin,Emax,Estep
;
; Bin with given Emin,Emx,Estep
;
@mac.in 
my_check=size(in_wk)

  if my_check(0) lt 1 then  begin
     P_MUS,'mus_cannon'
     mess='Workspace empty'
     widget_control,bad_id=iii,l_message,set_value=mess
     return,in_wk
   endif

  if strpos(his(two),'t2e') lt 0 then begin
     P_MUS,'mus_cannon'
     mess='Input workspace not in energy'
     widget_control,bad_id=iii,l_message,set_value=mess
     return,in_wk
  endif

  P_MUS,'mus_shot'
  nch=my_check(1)
  nang=my_check(2)
;
; Arguments reasonable ?
;
  if Emin gt Emax then begin
    E=Emax
    Emax=Emin
    Emin=E
  endif $
  else E=Emin
    if (Estep lt 0) then Estep=-Estep 
      if Estep gt (Emax-Emin) $
      or Estep eq 0 then Estep=max(x_in)-min(x_in)/nch
  length=round((Emax-Emin)/Estep)+1
  if length gt (3.*nch) then Estep=(Emax-Emin)/(3.*nch)
  w_out=fltarr(round((Emax-Emin)/Estep)+1,nang)
  x_out=fltarr(round((Emax-Emin)/Estep)+1)
  IF n_elements(e_in) eq n_elements(w_in) then E_OUT=w_out else E_OUT=0
  i=0 
  t=0
;
; Main program
;
  starter,Estep,x_in,w_in,w_out,E,t,i                ,E_IN,E_OUT
  repeat begin
    if (x_in(t)-E) gt Estep $
      then interpol,x_in,w_in,w_out,E,t,i            ,E_IN,E_OUT $
      else calculate,Estep,x_in,w_in,w_out,E,t,i,nch ,E_IN,E_OUT
    E=E+Estep
  endrep until E gt Emax
  c=findgen(round((Emax-Emin)/Estep)+1)
  x_out=Emin+Estep*c
  y_out=y_in
  p_out(30)=p_in(30)
  p_out(19)=round((Emax-Emin)/Estep)+1
@mac.out
  return,w_out
end
