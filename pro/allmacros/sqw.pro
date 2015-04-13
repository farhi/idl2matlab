function sqw,in_wk,xinc,yinc,xmin,xmax,ymin,ymax,fast=ky
;
;converts array w_in  to energy/Q in w_out.....GJK 1994
;Modified for 2-d x and y input - Nov 96
;Example call: w2=sqw(1,0.05,0.1,-2.0,2.0,0,2)
;         convert tof spectrum in w1 to energy and output in w2
;         with energy increment 0.05meV
;         with Q increment 0.1 A-1
;         energy range between -2.0 and 2.0 mev
;         q range between 0 and 2 A-1
;
; Repeated use with different scales:
;                      w2=sqw(1,0.05,0.1,-2.0,2.0,0,2,/fast)
;Available if no other macro used since last sqw function
; 
;
@mac.in
my_check=size(in_wk)
x_check=size(x_in)
  if my_check(0) lt 1 then  begin
     P_MUS,'mus_cannon'
     mess='Problem with input workspace'
     widget_control,bad_id=iii,l_message,set_value=mess
     return,in_wk
   endif
  P_MUS,'mus_shot'
  nch=my_check(1)
  nang=my_check(2)
        incs=[xinc,yinc]
	limits=[xmin,ymin,xmax,ymax]
    if not keyword_set(ky) then begin        
	coef=fltarr(nch)
        w_out=fltarr(nch,nang)   
        w_buf=fltarr(nch,nang)
	triang=lonarr(3,(nch*nang-2))
;
; Prepare y_array for Q values and x for energy
	y_buf=fltarr(nch,nang)
	x_buf=fltarr(nch,nang)

	wvl=p_in(21)
	pi=3.141592653 
	ei=81.799/p_in(21)^2 
	wvec_in=.695014*sqrt(ei)
	wv_in2=wvec_in^2 
	wvec_out=0.695014*sqrt(ei-x_in)
;
; Get 2-dimensional array y_out with q of each point
        if x_check(0) gt 1 then begin
	   x_buf=x_in
           y_buf=sqrt(wv_in2+wvec_out^2-2.0*wvec_in*wvec_out*cos(y_in*0.0174533))
	endif else begin
	   for i=0,nang-1 do begin
              x_buf(0,i)=x_in
	      y_buf(0,i)=sqrt(wv_in2+wvec_out^2-2.0*wvec_in*wvec_out*cos(y_in(i)*0.0174533))
	   endfor
	endelse
;
; Deluanay triangulate
        triangulate,x_buf,y_buf,triang
        endif
        w_out=trigrid(x_buf,y_buf,w_in,triang,incs,limits)
	if n_elements(e_in) eq n_elements(w_in) then $
	 e_out=SQRT(trigrid(x_buf,y_buf,e_in^2,triang,incs,limits))
	nx=1+fix((xmax-xmin)/xinc)
	ny=1+fix((ymax-ymin)/yinc)
       sighs=size(w_out)
	x_out=fltarr(sighs(1))
	y_out=fltarr(sighs(2))
       x_out(0)=xmin
        for i=1,sighs(1)-1 do x_out(i)=xmin+i*xinc
        y_out(0)=ymin
        for i=1,sighs(2)-1 do y_out(i)=ymin+i*yinc
        

	p_out=p_in
		x_tit(one)='Energy Transfer'
	y_tit(one)='Momentum Transfer (A-1)'
@mac.out
	return,w_out
	end

