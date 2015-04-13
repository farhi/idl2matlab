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

;***************************************;

pro eventhandler_bin_event, event
;
; Operations following widget events
;
common binwid, min_label,min_slider,max_label,max_slider,step_label, $
         step_slider,w_in_slider,w_out_slider,maxx,maxws,winsli, $
         Emin,Emax,Estep,sd   
common arns,av_epp,ep
@lamp.cbk
@dons.cbk
  widget_control, event.id, get_uvalue=p
;
  case p of 'done' :  begin 
;
;            Conversion to output format
;
      widget_control,min_label,get_value=min_b
      widget_control,max_label,get_value=max_b
      widget_control,step_label,get_value=step_b
      widget_control,w_in_slider,get_value=win_b
      widget_control,w_out_slider,get_value=wout_b
      min_bs =strtrim(strmid(min_b,6,7),2)
      max_bs =strtrim(strmid(max_b,6,7),2)
      step_bs=strtrim(strmid(step_b,7,7),2)
      win_bs =strtrim(string(win_b),2)
      wout_bs=strtrim(string(wout_b),2)
;
;            Execute BIN_W call
;
      rhs='=ebin(w'+win_bs +','+min_bs +','+max_bs + ',' +step_bs +')'
      lhs='w'+wout_bs
      comy=strarr(1)
      comy(0)=lhs+rhs
      xicuter,comy(0)
      widget_control,event.top,/destroy
      end
;
     'abort': widget_control,event.top,/destroy
;
     'w_in' : begin
; 
;            Check whether input workspace is full,
;            set output slider
;
      widget_control,w_in_slider, get_value=s
;      s=ws_full(s)
      widget_control, w_in_slider, set_value=s  
      if s eq maxws then $
        widget_control,w_out_slider,set_value=ws_full(s-1)+1 $
      else widget_control,w_out_slider,set_value=s+1
      winsli=s
      update1
      end

     'w_out': widget_control,event.id,get_value=t 

     'min'  : begin 
;
;           Read out Emin, Emax; is Emin < Emax ?
;           set label values
;
      widget_control,event.id,get_value=s 
      widget_control,max_slider,get_value=t
      if t le s then begin
        widget_control,max_slider,set_value=maxx
        widget_control,max_label,set_value='max = '+ $
          strtrim(string(Emax,format='(f10.2)'),2)
      endif
      widget_control,min_label,set_value='min = '+ $
        strtrim(string(Estep(s-1),format='(f10.2)'),2)
      end

     'max'  : begin
;
      widget_control,event.id,get_value=t
      widget_control,min_slider,get_value=s
      if t le s then begin $
        widget_control,max_slider,set_value=maxx
        widget_control,max_label,set_value='max = '+ $
          strtrim(string(Emax,format='(f10.2)'),2)
      endif $
      else widget_control,max_label,set_value='max = '+ $
        strtrim(string(Estep(t-1),format='(f10.2)'),2)                   
      end
;
   'step' : begin
;
;           Read out Estep
;
      widget_control,step_slider,get_value=s 
      dummy=(1./5.)*sd+s*((5.*sd-(1./5.)*sd)/maxx)
      widget_control,step_label,set_value='step = '+ $
        strtrim(string(dummy,format='(f10.4)'),2)
      end
  endcase
end

;-----------------------------------------------------------

;***************************************;


;***************************************;

pro update1
;
; Updates slider and label values
;
common binwid
common arns,av_epp,ep
@lamp.cbk
@dons.cbk
two=winsli
@mac.in

my_check=size(w_in)

  if my_check(0) lt 0 then   begin
     widget_control,min_label,set_value='No valid data'  
     widget_control,max_label,set_value='No valid data'  
     widget_control,step_label,set_value='No valid data'  
     mess='Input workspace empty'
     widget_control,bad_id=iii,l_message,set_value=mess
     return
  endif
  if strpos(his(two),'t2e') lt 0 then begin
     widget_control,min_label,set_value='No valid data'  
     widget_control,max_label,set_value='No valid data'  
     widget_control,step_label,set_value='No valid data'  
     mess='Input workspace not in energy'
     widget_control,bad_id=iii,l_message,set_value=mess
     return
  endif
     widget_control,bad_id=iii,l_message,set_value='ok'

  nch=my_check(1)
  nang=my_check(2)
; Calculate ranges of the sliders
;
  Estep  =fltarr(100)
  Emax   =x_in(nch-1)
  if (81.799/p_in(21)^2) gt 3*p_in(11)/11.6 then Emin=-81.799/p_in(21)^2 $
  else Emin=-3*p_in(11)/11.6
  if x_in(0) gt Emin then Emin=x_in(0)
  a=findgen(maxx)
  Estep=Emin+a*(Emax-Emin)/(maxx-1)
  if nch-1 ge p_in(9) then $
    sd=round((x_in(fix(p_in(9))+1)-x_in(fix(p_in(9))))*1000.)/1000. $
  else sd=0.01
;
; Set values of sliders and labels
;
  widget_control,min_slider,set_value=1
  widget_control,min_label,set_value='Emin = '+ $
    strtrim(string(Emin,format='(f10.2)'),2)
  widget_control,max_slider,set_value=maxx
  widget_control,max_label,set_value='Emax = '+ $
    strtrim(string(Emax,format='(f10.2)'),2)
  widget_control,step_slider, $
    set_value=round(maxx/6.)
  widget_control,step_slider,get_value=s
  dummy=(1./5.)*sd+s*((5.*sd-(1./5.)*sd)/maxx)
  widget_control,step_label,set_value='Estep = ' $ 
    +strtrim(string(dummy,format='(f10.4)'),2)
end

;--------------------------------------------------------------------------


pro rbin,ws=in_wk
;
; Widget window for the call of BIN_W
;
common binwid, min_label,min_slider,max_label,max_slider,step_label, $
         step_slider,w_in_slider,w_out_slider,maxx,maxws,winsli, $
         Emin,Emax,Estep,sd           
common arns,av_epp,ep
@lamp.cbk
@dons.cbk
  maxws=20
  maxx=100.
;
; Keyword check
;
;  if keyword_set(in_wk) then begin
;    two=in_wk
;    winsli=ws_full(in_wk)
;  endif $
;  else begin
;    two=maxws
;    winsli=ws_full(maxws)
;  endelse
;  two=winsli
@mac.in
emax=1.0
emin=-1.0
if n_elements(x_in) gt 1 then begin
emax=max(x_in)
emin=min(x_in)
endif

;
; Create and realize window
;
  winsli=1
  base     =widget_base (xsize=350, title='Rebin in Energy', $
     /column,space=20)
  head     =widget_label(base,value='REBINING OF SPECTRA',/frame)
  buttons  =widget_base(base,/row,/frame,space=245)
  wkspace  =widget_base(base,/row,/frame,space=105)
  Erange   =widget_base (base,/column,/frame)
  steprange=widget_base (base,/column,/frame)
;
  done_button =widget_button(buttons,value='done',uvalue='done')
  abort_button=widget_button(buttons,value='abort',uvalue='abort')
;
  w_in_slider =widget_slider(wkspace, title='workspace #(in)',$
    minimum=1,maximum=maxws,uvalue='w_in')
  w_out_slider=widget_slider(wkspace,title='workspace #(out)',$
    minimum=1,maximum=maxws,uvalue='w_out')
  widget_control,w_in_slider,set_value=winsli 
  if winsli eq maxws then $
    widget_control,w_out_slider,set_value=ws_full(winsli-1)+1 $
  else widget_control,w_out_slider,set_value=winsli+1
;
  range=widget_label(Erange, value='E-RANGE')
  min_label   =widget_label(Erange)
  min_slider  =widget_slider(Erange,uvalue='min',minimum=1,$
    maximum=maxx,/suppress_value)
  max_label   =widget_label(Erange) 
  max_slider  =widget_slider(Erange,uvalue='max',minimum=1,$
    maximum=maxx,/suppress_value)
;
  st_label   =widget_label(steprange,value='STEPWIDTH') 
  step_label =widget_label(steprange,value='step')
  step_slider=widget_slider(steprange,uvalue='step',/suppress_value)
;  update1
;
  bid=sys_dep      ('DYNLAB',base,1)
  widget_control,base,group_leader=lamp_b1,/realize
  xmanager,'eventhandler_bin',base
end

