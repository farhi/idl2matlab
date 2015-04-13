;
; Window with widgets for the call of SQW (gjk)
;
;This was originally written by Arne Dallmeyer Aug94
;
; Restructed (March95) for LAMP as installed (Feb95) gjk.
;
;Main difference is to get all common variables in lamp.cbk and
;make this a stand-alone interface. It eventually calls sqw.pro 
;must be in the standard-macro search-path for lamp. 
;
;Convention is all standard macros in ~kearley/lamp_macros

;**********************************************************;
pro window_creation
;
; Creates and realizes SQWWIN-Window
;
common sqwblock,maxws,maxE,maxQ,winsli,Esd,nang,nch,$
   lambd,pi,step,arb_factor,qmin,qmax,Q,fast,wsin_slider,$
   wsout_slider,Emin_slider,Estep_slider,Estep_label,$
   Qstep_slider,Qstep_label,Emin_label,Emax_slider,Emax_label,$
   Qmin_slider,Qmin_label,Qmax_slider,Qmax_label,message,base,$
   Emin,Emax,Esteps,p,error,wsin_sqw1,wsout_sqw1,Estep_sqw1,$
   Qstep_sqw1,Emin_sqw1,Emax_sqw1,Qmin_sqw1,Qmax_sqw1,Edim,Qdim,$
   dimtot,dim,Edimens,Qdimens,warn_label,i_do_it

@lamp.cbk

;
; Test if there is already a SQWWIN on the screen
i_test=xregistered('eventhandler_sqw')
if i_test ne 0 then widget_control,base,/destroy
  base=widget_base (space=5,/column,title='S(Qw) interpolation',resource_name='lampdon')
  buttons=widget_base (base, /row, space=350)
  warn_label=widget_label(FONT=ft_biggest,base, value=' ')

  si_tr=size(triang)
  if (si_tr(0) eq 2) and (si_tr(1) eq 3) then begin $
    farea=widget_base(base,/row,/frame,space=20)
;
    mess_left=widget_base(farea,/row,space=40)
    flabel1=widget_label(FONT=ft_biggest,mess_left,value='use previous triangulation ?');
    fab  =widget_base(mess_left,/column,/exclusive)
    fast_button  =widget_button(FONT=ft_biggest,fab,value='yes',uvalue='fast')
    nofast_button=widget_button(FONT=ft_biggest,fab,value='no',uvalue='slow')
    widget_control,nofast_button,/set_button
;
    mess_right=widget_base(farea,/row,space=40)
    tr_lab_1  =widget_label(FONT=ft_biggest,mess_right,value='destroy triangles ?')
    tria_but  =widget_base(mess_right,/column,/exclusive)
    tr_y_button=widget_button(FONT=ft_biggest,tria_but,value='yes',uvalue='destr')
    tr_n_button=widget_button(FONT=ft_biggest,tria_but,value='no',uvalue='keep')
    widget_control,tr_n_button,/set_button
  endif
  ;;;;;
  wspace =widget_base(base, /row, space=40, /frame)
  steps  =widget_base(base, /column, /frame)
  ostep  =widget_base(steps, /column)
  ustep  =widget_base(steps, /row)
  Estep  =widget_base(ustep, /column, /frame)
  Qstep  =widget_base(ustep, /column, /frame)
  ranges =widget_base(base, /row)
  Erange =widget_base(ranges, /column, /frame)
  Qrange =widget_base(ranges, /column, /frame)
  dimens =widget_base(base,/column)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  done_button =widget_button(FONT=ft_biggest,buttons,value='Do Interpolation',uvalue='done')
  abort_button=widget_button(FONT=ft_biggest,buttons,value='abort',uvalue='abort')
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  wsin_slider =widget_slider(FONT=ft_biggest,wspace,uvalue='wsin', $
    title='workspace #(in)',minimum=1,maximum=maxws)
  widget_control,wsin_slider,set_value=winsli
  wsout_slider=widget_slider(FONT=ft_biggest,wspace,uvalue='wsout', $
    title='workspace #(out)',minimum=1,maximum=maxws)
  if winsli eq maxws then $
    widget_control,wsout_slider,set_value=2 $
  else widget_control,wsout_slider,set_value=winsli+1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  head_label=widget_label(FONT=ft_biggest,ostep,value='STEPWIDTH',/frame) 
  ;
  Estep_label =widget_label(FONT=ft_biggest,Estep,value='E-step=',uvalue='Estep')
  Estep_slider=widget_slider(FONT=ft_biggest,Estep,xsize=300,value='1',uvalue='Estep',$
    /suppress_value)
  Qstep_label =widget_label (FONT=ft_biggest,Qstep,value='Q-step=',uvalue='Qstep')
  Qstep_slider=widget_slider(FONT=ft_biggest,Qstep,xsize=300,value='1',uvalue='Qstep',$
    minimum=1,maximum=maxQ,/suppress_value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  E_label=widget_label(FONT=ft_biggest,Erange, value='E-RANGE', /frame) 
  ;
  Emin_label =widget_label(FONT=ft_biggest,Erange,value='Emin=',uvalue='Eminl')
  Emin_slider=widget_slider(FONT=ft_biggest,Erange,xsize=305,uvalue='Emins', $
                            minimum=1,maximum=maxE,/suppress_value)
  Emax_label =widget_label(FONT=ft_biggest,Erange,value='Emax=',uvalue='Emaxl')
  Emax_slider=widget_slider(FONT=ft_biggest,Erange,xsize=305,uvalue='Emaxs', $
                            minimum=1,maximum=maxE,/suppress_value)
  Edim=widget_label(FONT=ft_biggest,Erange,/frame)
  ;;;;;;;;;;;;;;;;
  Q_label=widget_label(FONT=ft_biggest,Qrange, value='Q-RANGE', /frame)
  ;
  Qmin_label =widget_label (FONT=ft_biggest,Qrange,value='Qmin=',uvalue='Qminl')
  Qmin_slider=widget_slider(FONT=ft_biggest,Qrange,xsize=305,uvalue='Qmins', $
                            minimum=1,maximum=maxQ,/suppress_value)
  Qmax_label =widget_label (FONT=ft_biggest,Qrange,value='Qmax=',uvalue='Qmaxl')
  Qmax_slider=widget_slider(FONT=ft_biggest,Qrange,xsize=305,uvalue='Qmaxs', $
                            minimum=1,maximum=maxQ,/suppress_value)
  Qdim=widget_label(FONT=ft_biggest,Qrange,/frame)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  dimtot=widget_label(FONT=ft_biggest,dimens,/frame,value='   ',xsize=22)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  bid=sys_dep      ('DYNLAB',base,1)
  widget_control,base,group_leader=lamp_b1,/realize
  xmanager,'eventhandler_sqw',base,group_leader=group
end

;*********************************************************;
;
pro update2
;
; Updates slider and label values
;
common sqwblock
 
;##### mac.in should take care of all in lamp and dons.cbk ...gjk 2-3-95
@lamp.cbk
;@dons.cbk
  two=winsli
@mac.in
  i_check=size(p_in)
  if i_check(0) eq 0 then return
  nang   =fix(p_in(30))
  nch   =fix(p_in(19))
  lambd     =fix(p_in(21))
  Esteps=fltarr(maxE)
  Emax  =x_in(nch-1)
  pi        =3.1415927
  arb_factor=2.
;
;      Calculate ranges of the energy-sliders
;
  if (81.799/p_in(21)^2) gt (3*p_in(11)/11.6) then Emin=-81.799/p_in(21)^2 $
    else Emin=-3*p_in(11)/11.6
  if x_in(0) gt Emin then Emin=x_in(0)
  a=findgen(maxE)
  Esteps=Emin+a*(Emax-Emin)/(maxE-1)
  if nch-1 ge p_in(9) then $
    Esd=round((x_in(p_in(9)+1)-x_in(p_in(9)))*1000.)/1000. $
  else sd=0.01  
;
; Calculates ranges of the Q-sliders 
;
  numb_q=100
  qmin=4*pi*sin(min(y_in/2.)*pi/180.)/lambd
  qmax=arb_factor*4*pi*sin(max(y_in/2.)*pi/180.)/lambd
  a=findgen(numb_q)
  Q=qmin+a*(qmax-qmin)/(numb_q-1)
  stepmin=qmax/nang/arb_factor
  stepmax=qmax/arb_factor/2.
  a=findgen(numb_q)
  step=stepmin+a*(stepmax-stepmin)/(numb_q-1)
;
  if nang gt 50 then Qsd=qmax/50. $
    else Qsd=qmax/nang
  i=0
  repeat begin
    i=i+1
  endrep until (Qsd le step(i)) or (i eq (size(step))(1))
    Qsd=step(i)  
;
;      Set values of sliders and labels
;
  widget_control,Estep_slider, $
    set_value=round(maxE/6.)
  widget_control,Estep_slider,get_value=s
  Sdummy=(1./5.)*Esd+s*((5.*Esd-(1./5.)*Esd)/maxE)
  widget_control,Estep_label,set_value='Estep='+ $
    strtrim(string(Sdummy,format='(f10.4)'),2)
  widget_control,Qstep_slider,set_value=i
  widget_control,Qstep_label,set_value='Qstep='+ $
    strtrim(string(step(i),format='(f10.4)'),2)
  widget_control,Emin_slider,set_value=1
  widget_control,Emin_label,set_value='Emin='+ $
    strtrim(string(Emin,format='(f10.2)'),2)
  widget_control,Emax_slider,set_value=maxE
  widget_control,Emax_label,set_value='Emax='+ $
    strtrim(string(Emax,format='(f10.2)'),2)
  widget_control,Qmin_slider,set_value=1
  widget_control,Qmin_label,set_value='Qmin='+ $
    strtrim(string(qmin,format='(f10.2)'),2)
  widget_control,Qmax_slider,set_value=maxQ
  widget_control,Qmax_label,set_value='Qmax='+ $
    strtrim(string(qmax,format='(f10.2)'),2)
  dim_update
end


;*********************************************************;

pro dim_update
;
; Updates dimension-labels
;
common sqwblock
  joker=0
  read_out,joker
  if joker eq 0 then begin
    widget_control,Edim,set_value='E-dim=' +$
    strtrim(Edimens,2)
    widget_control,Qdim,set_value='Q-dim=' +$
    strtrim(Qdimens,2)
    widget_control,dimtot,set_value='number of points=' +$
    strtrim(Edimens*Qdimens,2)
  endif
end

;*************************************************************;

pro read_out,joker
;
; Reads out slider and label values; converts them into
; formatted strings; determines calculation steps
;
common sqwblock
@lamp.cbk
  joker=0
  widget_control,wsin_slider,get_value=wsin_sqw
	 iii=execute('w_test = w'+strtrim(string(wsin_sqw),2))
	 siz_test=size(w_test)
	 if(siz_test(0)) eq 0 then begin
	   i_do_it=-1
           P_MUS,'mus_cannon'
           joke_text='WORKSPACE EMPTY' 
           widget_control,warn_label,set_value=joke_text
           joker=1
           return
	 endif
  if (strpos(his(wsin_sqw),'t2e') lt 0) and $
     (strpos(his(wsin_sqw),'INX') lt 0) and $
     (strpos(his(wsin_sqw),'inx') lt 0) then begin
      P_MUS,'mus_cannon'
      joke_text='WORKSPACE NOT IN ENERGY'
      widget_control,warn_label,set_value=joke_text
      joker=1
      return
  endif

  if strpos(his(wsin_sqw),'sqw') gt 0 then begin
      P_MUS,'mus_cannon'
      joke_text='WORKSPACE ALREADY INTERPOLATED'
      widget_control,warn_label,set_value=joke_text
      joker=1
      return
  endif
  
  
  widget_control,wsin_slider,get_value=wsin_sqw
  widget_control,wsout_slider,get_value=wsout_sqw
 
  widget_control,Estep_label,get_value=Estep_sqw
  widget_control,Qstep_label,get_value=Qstep_sqw
  widget_control,Emin_label,get_value=Emin_sqw
  widget_control,Emax_label,get_value=Emax_sqw
  widget_control,Qmin_label,get_value=Qmin_sqw
  widget_control,Qmax_label,get_value=Qmax_sqw
;;;;;;;;;;;;;;;;;;;;;;;;
  wsin_sqw1 =strtrim(string(wsin_sqw),2)
  wsout_sqw1=strtrim(string(wsout_sqw),2)
  Estep_sqw1=strtrim(strmid(Estep_sqw,6,6),2)
  Qstep_sqw1=strtrim(strmid(Qstep_sqw,6,6),2)
  Emin_sqw1 =strtrim(strmid(Emin_sqw,5,6),2)
  Emax_sqw1 =strtrim(strmid(Emax_sqw,5,6),2)
  Qmin_sqw1 =strtrim(strmid(Qmin_sqw,5,6),2)
  Qmax_sqw1 =strtrim(strmid(Qmax_sqw,5,6),2)
  e1=float(Emax_sqw1)
  e2=float(Emin_sqw1)
  e3=float(Estep_sqw1)
  q1=float(Qmax_sqw1)
  q2=float(Qmin_sqw1)
  q3=float(Qstep_sqw1)
  Edimens=round((e1-e2)/e3)+1
  Qdimens=round((q1-q2)/q3)+1
  if wsin_sqw eq wsout_sqw then begin
     P_MUS,'mus_cannon'
     joke_text='INPUT AND OUTPUT WORKSPACES ARE THE SAME'
     widget_control,warn_label,set_value=joke_text
     joker=1
     return
  endif
 
end

;*******************************************************;

pro eventhandler_sqw_event, event
;
; Operations following widget events
;
common sqwblock
 
@lamp.cbk

  widget_control, event.id, get_uvalue=p
  joke_text=' '
  widget_control,warn_label,set_value=joke_text
  case p of 'done'  : begin
     joker=0
     read_out,joker
     if joker eq 0 then begin
     if Edimens*Qdimens ge 25000 then joke_text='TOO MANY POINTS GENERATED' $
     else begin
       sqw_output
       widget_control,base,/destroy
     endelse
     endif
     if joker eq 1 then begin 
        joke_text='SORRY - INVALID SELECTION OF WORKSPACES' 
        P_MUS,'mus_cannon'
        widget_control,warn_label,set_value=joke_text
     endif
   end
;
  'abort': widget_control,event.top,/destroy
;
  'fast' : fast=1
;
  'slow' : fast=-1
;
  'wsin' : begin
; 
;            Check whether input workspace has any data,
;            is in energy, or previously known to sqw
;
         widget_control,wsin_slider,get_value=winsli
	 iii=execute('w_test = w'+strtrim(string(winsli),2))
	 siz_test=size(w_test)
      if(siz_test(0)) eq 0 then begin
	  i_do_it=-1
          P_MUS,'mus_cannon'
          joke_text='WORKSPACE EMPTY' 
          widget_control,warn_label,set_value=joke_text
          joker=1
          return
      endif


      if (strpos(his(winsli),'t2e') lt 0) and $
         (strpos(his(winsli),'INX') lt 0) and $
         (strpos(his(winsli),'inx') lt 0) then begin
          P_MUS,'mus_cannon'
          joke_text='WORKSPACE NOT IN ENERGY'
          widget_control,warn_label,set_value=joke_text
          joker=1
          return
      endif
      
      if strpos(his(winsli),'sqw') gt 0 then begin
          P_MUS,'mus_cannon'
          joke_text='WORKSPACE ALREADY INTERPOLATED'
          widget_control,warn_label,set_value=joke_text
          joker=1
          return
      endif


           joke_text=' '
	   i_do_it=0 
           widget_control,warn_label,set_value=joke_text
            update2
         end
	 
  'wsout': widget_control,wsout_slider,get_value=o
;
  'Estep': begin 
;
;                Read out energy stepwidth
;                set label values
;
         widget_control,Estep_slider,get_value=Es
         dummy=(1./5.)*Esd+Es*((5.*Esd-(1./5.)*Esd)/maxE)
         widget_control,Estep_label,set_value='Estep='+ $
           strtrim(string(dummy,format='(f10.4)'),2)
         joker=0
         read_out,joker
         if joker eq 0 then dim_update
         end  
;
  'Qstep': begin 
;
;                Read out Q stepwidth
;                set label values
;   
         widget_control,Qstep_slider,get_value=Qs
	 q_test=size(step)
	 if q_test(0) gt 0 then begin
            widget_control,Qstep_label,set_value='Qstep='+ $
            strtrim(string(step(Qs-1),format='(f10.4)'),2)
            joker=0
            read_out,joker
            if joker eq 0 then dim_update
	 endif else begin
	      joke_text='NAUGHTY DATA (Q_STEPS)'
	      widget_control,warn_label,set_value=joke_text
         endelse
         end 
;
  'Emins': begin
;
;             Read out Emin, Emax; is Emin < Emax ?
;             set label values
;
         widget_control,Emin_slider,get_value=inE
         widget_control,Emax_slider,get_value=axE
	 e_test=size(Esteps)
	 if e_test(0) gt 0 then begin
            if inE ge axE then begin $
              widget_control,Emax_slider,set_value=maxE
              widget_control,Emax_label,set_value='Emax='+ $
              strtrim(string(Emax,format='(f10.2)'),2)
            endif
            widget_control,Emin_label,set_value='Emin='+ $
            strtrim(string(Esteps(inE-1),format='(f10.2)'),2)
            joker=0
            read_out,joker
            if joker eq 0 then dim_update
	 endif else begin
	      joke_text='NAUGHTY DATA (E_STEPS)'
	      widget_control,warn_label,set_value=joke_text
	 endelse
         end
;
  'Emaxs': begin
;
         widget_control,Emin_slider,get_value=inE
         widget_control,Emax_slider,get_value=axE
	 e_test=size(Esteps)
	 if e_test(0) gt 0 then begin
         if inE ge axE then begin $
           widget_control,Emax_slider,set_value=maxE
           widget_control,Emax_label,set_value='Emax='+ $
             strtrim(string(Emax,format='(f10.2)'),2)
         endif $
         else widget_control,Emax_label,set_value='Emax='+ $
           strtrim(string(Esteps(axE-1),format='(f10.2)'),2) 
         joker=0
         read_out,joker
         if joker eq 0 then dim_update
	 endif else begin
	      joke_text='NAUGHTY DATA (E_STEPS)'
	      widget_control,warn_label,set_value=joke_text
	 endelse
         end
;
  'Qmins': begin
;
;             Read out Qmin, Qmax; is Qmin < Qmax ?
;             set label values
;
         widget_control,Qmin_slider,get_value=inQ
         widget_control,Qmax_slider,get_value=axQ
	 q_test=size(Q)
	 if q_test(0) gt 0 then begin
         if inQ ge axQ then begin $
           widget_control,Qmax_slider,set_value=maxQ
           widget_control,Qmax_label, set_value='Qmax='+ $
             strtrim(string(qmax,format='(f10.2)'),2)
         endif
         widget_control,Qmin_label,set_value='Qmin='+ $
           strtrim(string(Q(inQ-1),format='(f10.2)'),1)
         joker=0
         read_out,joker
         if joker eq 0 then dim_update
	 endif else begin
	      joke_text='NAUGHTY DATA (Q_STEPS)'
	      widget_control,warn_label,set_value=joke_text
	 endelse
         end
;
  'Qmaxs': begin
         widget_control,Qmin_slider,get_value=inQ
         widget_control,Qmax_slider,get_value=axQ
	 q_test=size(Q)
	 if q_test(0) gt 0 then begin
         if inQ ge axQ then begin $
           widget_control,Qmax_slider,set_value=maxQ
           widget_control,Qmax_label,set_value='Qmax='+ $
             strtrim(string(qmax,format='(f10.2)'),2)
         endif $
         else widget_control,Qmax_label,set_value='Qmax='+ $
           strtrim(string(Q(axQ-1),format='(f10.2)'),2)
         dim_update
	 endif else begin
	      joke_text='NAUGHTY DATA (Q_STEPS)'
	      widget_control,warn_label,set_value=joke_text
	 endelse
         end
;
   'Yes' : begin
;
;               Keep many calculation steps
;
         widget_control,error,/destroy 
         widget_control,base,sensitive=1
         sqw_output
         widget_control,base,/destroy
         end
    'No' : begin
;
;               Change slider values
;
         widget_control,base,sensitive=1
         widget_control,error,/destroy
         end
;
  'destr': begin
;
;               Destroy triangles
;
         triang=0
         widget_control,base,/destroy
         window_creation
         end
;
  'keep' : dummy=1
  endcase
end

;**********************************************************;
;************************************************************;

pro sqw_output
;
 
@lamp.cbk
@dons.cbk
common sqwblock
;
; Execute SQW-call

  rhs2='sqw(w'+wsin_sqw1+','+Estep_sqw1+','+Qstep_sqw1+$
    ','+Emin_sqw1+','+Emax_sqw1+','+Qmin_sqw1+','+Qmax_sqw1
  if fast eq 1 then rhs2=rhs2+',/fast)'else rhs2=rhs2+')'
  lhs2='w'+wsout_sqw1+'='
  joke_text='THIS COULD TAKE A WHILE....'
  widget_control,warn_label,set_value=joke_text
  comy=strarr(1)
  comy(0)=lhs2+rhs2
  xicuter,comy(0)
  y_tit(fix(wsout_sqw1))='Q(A-1)'
end

pro sqwwin,ws=wsin
;
; Window with widgets for the call of SQW (gjk)
;
;This was originally written by Arne Dallmeyer Aug94
;
; Restructed (March95) for LAMP as installed (Feb95) gjk.
;
;Main difference is to get all common variables in lamp.cbk and
;make this a stand-alone interface. It eventually calls sqw.pro 
;must be in the standard-macro search-path for lamp. 
;
;Convention is all standard macros in ~kearley/lamp_macros
;
;
common sqwblock
 
@lamp.cbk
  maxws=20
  maxE=100.
  maxQ=100.
  fast=-1
  i_do_it=0
  esd=0 & step=0 & arb_factor=0 & qmin=0 & qmax=0 & Q=0
  emax=0 & emin=0 & esteps=0
  P_MUS,'mus_harp'
    
;
;        Keyword check
;
  if keyword_set(wsin) then begin
    two=wsin
    winsli=wsin 
  endif else begin
    two=maxws
    winsli=1
  endelse
      two=winsli
      window_creation
end
