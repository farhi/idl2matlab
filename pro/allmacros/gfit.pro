;			GKFIT
; A lamp interface to perform fits on plot.
;
; Fitit procedures were developped by Don Kearley
; Gfit was developped, improved and corrected by :
;					Don Kearley
;					Didier Richard
;					Stephane Thiers
;					Romuald Jouffrey
; During 1995-1996 
;
; This file contains both original fitit.pro and gfit.pro
; Version 1.14		Date: 6/10/96
; Modification History:
; gjk 22/9/96: Common gc_width_factor added with gw_fac and lw_fac. 
;	These take account width as sigma or real width. 
;	For sigma:      gw_fac = 0.5,    lw_fac=1.0
;	For real width: gw_fac = 2sqrt2, lw_fac=0.5
; gjk 22/9/96: If width or intensity set by cursor, g_resid=1.0e12
; gjk 23/9/96: Modified routine adapte to:
;		set symbol plotting if less than 200 points
;		set symbol size according to number of points
;		set error plotting if errors are present
;		to include most of the actual plotting:
;		plot_flag=0 no plot, 
;		         =1 plot all
;			 =2 no overplot
;			 =3 plot,/nodata
;gjk 1/10/96:  Change to middle mouse button for peak definition. Something
;              odd about event 2 2 with left button?
;gjk 5/10/96:  Derivatives for height corrected, and refienement altered,
;              now: ncycles of dumb routine with initial step decreasing
;                   with ncycles.
;              One cycle of derivative method, but 2 if ncycles > 1.
;gjk 6/10/96   Flag peaks with negative height or width with peak_neg equal
;              to offending peak number (0 if ok) and ht_or_wdth = 1 for height
;              and 2 for width. Set in sum_fit.
;gjk 10/10/96  Flat and sloping bg added when peak defined with mouse
;
;d.r 08/08/02  Detail balance added.
;d.r 09/08/02  scalling on Y is now possible.
;
; ***************************** Widgets Creating Procedure
; GFIT				Create main GKFIT interface
; CREER_EXCL, id		Create Excluding zone interface
; CREER_VIEW, id		Create viewing zone interface
; ***************************** Event processing procedures
; GFIT_EVENT		,Event
; TRAITVIEW_EVENT 	,Event
; TRAITEXCL_EVENT 	,Event
; ***************************** functions of this files are :
; JFIT		,x,y,w,a,stp,siga,g_ncycles,npk,poids
; GENFIT	,X,Y,W,A,stp,npk,resid,poids
; CALCULATE_ERROR,modele
; BELONG_TO	,elem,inter
; GK_FIT	,workspace
; ***************************** Procedures of this files are :
; CREATE_COMMONS
; GAUSS		,x,a,f
; LORENTZ	,x,a,f
; SUM_FIT_FCT	,x,a,f,g_npeaks,pder
; FITIT		,g_npeaks,w_in,w_out,...
; ADAPTE	,x_to_plot,w_to_plot
; AFF_EXCL	,excl_param
; DO_FIT	,option
; GET_PARAM	,g_parameters,idx,p1,p2,p3
; GFIT_HELP	, dummy, formu, formt
; GK_INITIALIZE
; GK_WARNING	,excl_param,viewarr
; PARAMETER_MENU,Ev
; PLOT_SUBF	,g_npeaks,g_parameters,x_ni,g_fct_type
; PUT_TIT	,x_dats,y_dats,f_tit,fx_tit,fy_tit
; PUT_VALS	,x_dats,y_dats,f_dats,plt_dev,poids,lamp_siz
; PRINT_VALS	,x_dats,y_dats,f_dats,plt_dev,poids
; READ_PAR_VALS
; SET_GFIT_PARAM,w,p,Fct_Type=ft
; WRITE_PAR_VALS
; WRITE_PAR_VALS_2
; GFIT		,GROUP=Group
; ERROR_MSG	,Error_Number
; ***************************** Little explanation on variables
; - Local variables are normaly named  			(ex. idx)
; - Each gkfit shared variable begins with g_ 		(ex. g_parameters)
; - gw_ stands for gkfit Widgets identifier variables	(ex. gw_base)
;
; Variables are ALWAYS lowercase, IDL Instructions are ALWAYS uppercase
; Keywords in IDL instructions are ALWAYS Capitalized.
; If you try this, you keep it ... very helpfull in reading ...

;===============================================================================
PRO create_commons				; ==============================
;===============================================================================
; Just to create correct commons while compiling. Explains also variables

COMMON gc_save,		keepw, 		$ ; keep w_ni for cutting
			keepe,		$ ; keep errors     ''
			w_ni,		$ ; input Workspace
			x_ni, 		$ ; input X
			e_ni,           $ ; input errors
			nw_ni,		$ ; in Workspace number
			nwt_in,		$ ; input Workspace number (str)
			nw_out,		$ ; out Workspace number
			sw_ni		  ; Size of input Workspace 

COMMON gc_data,		g_plot_wid, 	$ ; plot area id
			g_old_plot_wid, $ ; keep plot area id
			g_fct_name,	$ ; Name of the fitted function
			g_fct_type,	$ ; Array of function type for each peak
			g_npeaks,	$ ; Peak(s) number
			g_pk_nb,	$ ; Current peak number
			g_nb_pk_max,	$ ; Maximum number of peaks
			g_ncycles,	$ ; Cycle(s) number
			g_largeur,	$ ; Array of consecutive distances
			g_resid,	$ ; the residu after fitting
			g_print_nb,	$ ; Print filename incremental variable (Fix)
			g_step,		$ ; Array of extended parameters
			g_error, 	$ ; Errors on parameters
			g_tx_par, 	$ ; Tripx parameters string array
			g_parameters,	$ ; Array for Pos,Int&Width
			g_char		  ; for each defined peak plus at last
					  ; idx background and slope

					  ; ctrl_panel widget id
COMMON gc_cp,		cp_nb, 		$ ; Widget Id 
			cp_bck,	 	$ ; 	
			cp_pos, 	$ ;  
			cp_hgt,  	$ ; 
			cp_wid,  	$ ; 
			cp_pf,  	$ ; 
			cp_hf,  	$ ; 
			cp_wf,  	$ ; 
			cp_fb,  	$ ; 
			cp_sb,  	$ ; 
			cp_fbf,  	$ ; 
			cp_sbf  	  ; 

COMMON gc_flags,	g_bg_but,	$ ; State of background button
			g_show_subfct,	$ ; if set, subfunctions are plotted
			g_tripx, 	$ ; if set, tripx mode
			g_ctrl_panel, 	$ ; if set, ctrl_panel mode
			g_afitisdone	  ; set if a fit was done, for print fct

COMMON gc_wid,		gw_cycle_lab, 	$ ; Widget Id of cycle label
			gw_cut_slider,	$ ; Widget Id of cutting slider
			gw_cut_label,	$ ; Widget Id of cutting label
			gw_peak_but,	$ ; Array of widget id (peak buttons)
			gw_choice_but,	$ ; Widget Id of choice button
			gw_pk_slider,	$ ; Widget Id of peak slider
			gw_view_base,	$ ; Widget Id of view base
			gw_excl_base,	$ ; Widget Id of exclude base
			gw_pos_text,	$ ; Widget Id of position field text 
			gw_pos_pop,	$ ; Widget Id Array of menu
			gw_int_text,	$ ; Widget Id of integral text field 
			gw_int_pop,	$ ; Widget Id Array of menu
			gw_wdt_text,	$ ; Widget Id of width text field 
			gw_wdt_pop,	$ ; Widget Id Array of menu
			gw_fbg_text,	$ ; Widget Id of flat bg text field 
			gw_fbg_pop,	$ ; Widget Id Array of menu
			gw_sbg_text,	$ ; Widget Id of slope bg text field 
			gw_sbg_pop,	$ ; Widget Id Array of menu
			gw_resid_lab,	$ ; Widget Id of resid label
			gw_plot_area,	$ ; Widget Id of plot drawing area
			gw_err_lab,	$ ; Widget Id of error label
			gw_gkfit_base,	$ ; Widget Id of gkfit base
			gw_type_but,	$ ; Widget Id array of pop up menu button
			gw_bgfixed,	$ ; Widget Id of fixed background button
			gw_pk_no,	$ ; Widget Id of peak number label
			gw_get_but	  ; Widget Id of get button

COMMON gc_fit_excl,	gw_excl_but,	$ ; 
			bb1,		$ ; 
			bb2,		$ ; 
			intv_no,	$ ; 
			excl_widge,	$ ; 
			intv_nb,	$ ;
			ok_but,		$ ; 
			excl_param,	$ ; 
			max_nb_int	  ; 

COMMON gc_fit_view,	viewarr,	$ ; Array of view zone data
			no_zone,	$ ; 
			x_min,		$ ; 
			x_max,		$ ; 
			y_min,		$ ; 
			y_max,		$ ;
			sauv		  ;
			
COMMON gc_width_factor,  gw_fac, lw_fac

COMMON gc_negative, ht_or_wdth,peak_neg

COMMON gc_dids, ws_in_r, gw_temp, gw_bose, bose, curpic, gw_mimi,mimirange,mimion, ximion,gx_mim1,gx_mim2

END

;===============================================================================
pro Parameter_menu, Ev						; ==============
;===============================================================================
COMMON gc_wid
COMMON gc_data
COMMON gc_flags

fixed=0
IF Ev(1) EQ 0 then if g_bg_but EQ 1 then g_bg_but=0 else g_bg_but=1
IF Ev(1) GE 1 AND Ev(1) LE 5 THEN BEGIN
    IF Ev(1) eq 1 THEN gw_pop=gw_pos_pop
    IF Ev(1) eq 2 THEN gw_pop=gw_int_pop
    IF Ev(1) eq 3 THEN gw_pop=gw_wdt_pop
    IF Ev(1) eq 4 THEN gw_pop=gw_fbg_pop
    IF Ev(1) eq 5 THEN gw_pop=gw_sbg_pop
	    IF Ev(2) eq 0 THEN error_msg, 10		
	    IF Ev(2) eq 1 THEN BEGIN
		WIDGET_CONTROL, bad_id=i, gw_pop(Ev(2)), Get_Value=V
		IF STRPOS(V, 'Un') ge 0 THEN $
		    V=STRMID(V, 2, STRLEN(V)) $
		ELSE BEGIN
		    V='Un'+V & fixed=1
		ENDELSE
		WIDGET_CONTROL, bad_id=i, gw_pop(Ev(2)), Set_Value=V			    
	    ENDIF ELSE BEGIN
		IF Ev(1) GE 1 AND Ev(1) LE 3 THEN BEGIN
		    ; event generated by position, height width button
		    WIDGET_CONTROL, bad_id=i, gw_pop(2), Get_Value=V
		    IF STRLEN(V) GT 15 then V=STRMID(V, 0, 15)
		    IF Ev(2) NE 9 THEN BEGIN
			fixed=1
			WIDGET_CONTROL, bad_id=i, gw_pop(2), $
			    Set_Value=V+' '+STRTRIM(STRING(Ev(2)-2), 2)+' '
		    ENDIF ELSE WIDGET_CONTROL, bad_id=i, gw_pop(2), Set_Value=V
		ENDIF
	    ENDELSE
str=""
WIDGET_CONTROL, bad_id=i, gw_pop(0), Get_Value=str
IF fixed eq 1 THEN BEGIN
    if STRPOS(str, ' fx ') LT 0 THEN str=STRMID(str, 0,  STRPOS(str, '    '))+' fx '
ENDIF ELSE BEGIN
    p=STRPOS(str, ' fx ')
    if p GE 0 then str=STRMID(str,0, p)+'    ' 
ENDELSE
WIDGET_CONTROL, bad_id=i, gw_pop(0), Set_Value=str
ENDIF
read_par_vals
g_resid=1.0e12
END

;===============================================================================
FUNCTION jfit,x,y,w,a,stp,siga,ncycles,npk,poids		; ==============
;===============================================================================
;
; Free fall minimisation routine gjk March 1994
; Treats one parameter at a time: shifts value, if better then shifts
; twice as much, if worse shifts back half as much. Halts for each 
; parameter when change in residual is less than "tiny". 
; When all parameters are done this is one cycle. Repeated for ncycles.

 COMMON gc_negative

tiny=0.005
pc100=SQRT(TOTAL(y^2))
nvars=npk*3+2
nquit=2000
f=x

IF ncycles EQ 0 THEN BEGIN
    Sum_fit_fct,x,a,f,npk,dummy_par ;fifth parameter is not used here
    siga=100.0*SQRT(TOTAL(ABS(y-f)^2*poids))/pc100
    RETURN,f
ENDIF	

;.......Start loop round cycles 

FOR kk=1,ncycles DO BEGIN
    ;.......Get starting residual
	Sum_fit_fct,x,a,f,npk,dummy_par	;fifth parameter is not used here
    best=sqrt(total(abs(y-f)^2*poids))/pc100
    old_dify=0.0
    first_step=0.1/(kk^2)
    ;.......Start loop round each parameter
    FOR np=1,nvars DO BEGIN
	IF stp(np-1) gt 0 THEN BEGIN
	    ;.......Set up best value of parameter, default change in residual,
	    ;.......and initial shift (10% of parameter value)
	    bestp=a(np-1)
	    difydify=1.0
	    shifty=a(np-1)*first_step
	    a(np-1)=a(np-1)+shifty
	    ;.......Endless loop to minimise, quit after nquit tries and go home
	    iquit=0
	    WHILE abs(difydify) gt tiny DO BEGIN
		iquit=iquit+1
		if iquit gt nquit THEN return,f
		Sum_fit_fct,x,a,f,npk,dummy_par	;fifth parameter is not used here
		if peak_neg gt 0 then return,f
		chsq=sqrt(total(abs(y-f)^2*poids))/pc100
		dify=best-chsq
		difydify=old_dify-dify
		old_dify=dify
		;.......keep best value of parameter
		if chsq lt best THEN BEGIN
		   best=chsq
		   bestp=a(np-1)
		   shifty=shifty*2.
		endif
		if dify lt 0.0 THEN shifty=-shifty/2.0
		a(np-1)=bestp+shifty
		shifty=abs(shifty)
	    ENDWHILE
	    a(np-1)=bestp
	ENDIF ELSE BEGIN
	    ;.......Fixed or tied parameters
	    IF stp(np-1) lt 0 THEN BEGIN
		;Tie to which peak?
		n_piv=abs(stp(np-1))
		;What is current peak number?
		n_peak=fix((np-1)/3)
		ikind=np-(n_peak*3)
		a(np-1)=a((n_piv-1)*3+(ikind-1))
	    ENDIF
	ENDELSE
    ENDFOR

ENDFOR
Sum_fit_fct,x,a,f,npk,dummy_par	;fifth parameter is not used here
siga=100.0*SQRT(TOTAL(ABS(y-f)^2*poids))/pc100
return,f
END

;===============================================================================
FUNCTION genfit,X,Y,W,A,stp,npk,resid,poids 				; ======
;===============================================================================
;Doctored version of curvefit gjk March 95
;

;ON_ERROR,2		;return to caller if error ***removed gjk
yfit=fltarr(N_ELEMENTS(x))
nvars=npk*3+2
a=1.*a		;make params floating
nterms=N_ELEMENTS(a)	;# of params.
nfree=(N_ELEMENTS(y)<N_ELEMENTS(x))-nterms ;degs of freedom
if nfree le 0 THEN error_msg, 8
if nfree le 0 THEN return,yfit
flambda=0.001		;initial lambda
diag=indgen(nterms)*(nterms+1) ;subscripts of diagonal elements
total_y2=total(y^2)
pc100=sqrt(total_y2)
end_iter=total_y2/1.0e7/nfree
iter=0
while (iter le 20) DO BEGIN ;iteration loop, evaluate alpha and beta matricies.
    iter=iter+1
    Sum_fit_fct,x,a,yfit,npk,pder
    beta=(y-yfit)*w # pder
    alpha=transpose(pder) # (w # (fltarr(nterms)+1)*pder)
    chisq1=total(w*(y-yfit)^2*poids)/nfree ;present chi squared.

    ; if a good fit, no need to iterate
    IF chisq1 lt end_iter THEN BEGIN
	sigmaa=fltarr(nterms)	;return all 0's
	RETURN, yfit
    ENDIF

    ; invert modified curvature matrix to find new parameters.
    iescape=0
    REPEAT BEGIN
	c=SQRT(alpha(diag) # alpha(diag))
	iescape=iescape+1
	array=alpha/c
	array(diag)=array(diag)*(1.+flambda)		
	array=INVERT(array)
	b=a+(array/c # TRANSPOSE(beta)) ;new params
	    FOR np=1,nvars DO BEGIN
		IF stp(np-1) eq 0 THEN b(np-1)=a(np-1)
		;fixed or tied parameters
		if stp(np-1) lt 0 THEN BEGIN
		    ;tie to which peak?
		    n_piv=abs(stp(np-1))
		    ;what is current peak number?
		    n_peak=fix((np-1)/3)
		    ikind=np-(n_peak*3)
		    b(np-1)=b((n_piv-1)*3+(ikind-1))
		ENDIF
	    ENDFOR
	    Sum_fit_fct,x,b,yfit,npk,dummy_par	;fifth parameter is not used here
	    chisqr=TOTAL(w*(y-yfit)^2*poids)/nfree ;new chisqr
	    flambda=flambda*10.	;assume fit got worse
	    IF iescape gt 200 THEN RETURN,yfit
    ENDREP UNTIL chisqr le chisq1

    flambda=flambda/100.    	;decrease flambda by factor of 10
    a=b			  	;save new parameter estimate.	
    IF ((chisq1-chisqr)/chisq1) le .001 THEN iter=21  
endwhile

done:   pc100=SQRT(TOTAL(y^2*poids))
	resid=100.0*SQRT(TOTAL((y-yfit)^2*poids))/pc100
	RETURN,yfit		;return result
END

;===============================================================================
PRO Gauss,x_vector,p1,p2,p3,peak,partial_deriv			; ==============
;===============================================================================
; Define in peak a Gaussian on parameters p1 (pos),p2 (ht), p3 (wdth) into
; array peak. If partial_deriv parameters is done, partial_deriv contains
; partial_deriv for each parameters.
COMMON gc_width_factor
COMMON gc_dids

p2_tmp=p2*p3*2.50663		; compute integral for p2 height
z=(x_vector-p1)/p3
z2= (z^2) <15.
peak=p2_tmp*exp(-gw_fac*z2)/p3/2.50663
peakt=peak

temp=0 & det_bal=1. & peaks=0 & zs=0 & z2s=0 & expzs=0

if bose(curpic-1) then begin
   widget_control,gw_temp,get_value=tmpt & tmpt=tmpt(0)
   on_ioerror,mistmpt & tmpt=float(tmpt) & temp=tmpt & mistmpt:
endif
if temp gt 0 then begin                  ;STRUCTURE FACTOR ASKED (det_bal)
	w_t=x_vector/temp*11.6
	e_w_t=EXP(-w_t)
	det_bal=w_t*0+temp/11.6
	idx=where(abs(w_t) gt 0.0005)
	if idx(0) ge 0 then det_bal(idx)=w_t(idx)/(1.-e_w_t(idx))
	
	zs   =(x_vector+p1)/p3 & z2s= (zs^2) <15. ;symetric
	expzs=exp(-gw_fac*z2s)                    ;symetric
	peaks=p2_tmp*expzs/p3/2.50663             ;symetric
        peak =peak+peaks                          ;symetric
	peak =peak*det_bal
endif

IF N_PARAMS() GT 5 THEN BEGIN
    if gw_fac gt 0.5 then new_p3=p3*0.420448 else new_p3=p3
    partial_deriv=FLTARR(N_ELEMENTS(x_vector),3)           ; define partial_deriv array
    partial_deriv(*,0)=2.*gw_fac/new_p3*(peakt*z -peaks*zs) *det_bal ; partial deriv % p1
    partial_deriv(*,1)= (exp(-gw_fac*z2)+expzs)             *det_bal ; partial deriv % p2
    partial_deriv(*,2)=2.*gw_fac/new_p3*(peakt*z2+peaks*z2s)*det_bal ; partial deriv % p3
ENDIF
END

;===============================================================================
PRO Lorentz,x_vector,p1,p2,p3,peak,partial_deriv		; ==============
;===============================================================================
; Define in peak a Lorentzian on parameters p1 (pos),p2 (ht), p3 (wdth) into
; array peak. If partial_deriv parameters is done, partial_deriv contains
; partial_deriv for each parameters.
COMMON gc_width_factor
COMMON gc_dids

if lw_fac lt 1. then new_p3=p3*0.5 else new_p3=p3

p2_tmp=p2*new_p3*!PI			; compute integral for p2 height
xvt=(x_vector-p1)
peak=p2_tmp/(1+(xvt/new_p3)^2)/new_p3/!PI
peakt=peak

temp=0 & det_bal=1. & peaks=0 & xvs=0

if bose(curpic-1) then begin
   widget_control,gw_temp,get_value=tmpt & tmpt=tmpt(0)
   on_ioerror,mistmpt & tmpt=float(tmpt) & temp=tmpt & mistmpt:
endif
if temp gt 0 then begin                  ;STRUCTURE FACTOR ASKED (det_bal)
	w_t=x_vector/temp*11.6
	e_w_t=EXP(-w_t)
	det_bal=w_t*0+temp/11.6
	idx=where(abs(w_t) gt 0.0005)
	if idx(0) ge 0 then det_bal(idx)=w_t(idx)/(1.-e_w_t(idx))
	
	xvs =(x_vector+p1)                             ;symetric
        peak=peak+p2_tmp/(1+(xvs/new_p3)^2)/new_p3/!PI ;symetric
	peak=peak*det_bal
endif

IF N_PARAMS() GT 5 THEN BEGIN
    partial_deriv=FLTARR(N_ELEMENTS(x_vector),3); define partial_deriv array
    partial_deriv(*,0)=2./p2/new_p3^2 *( xvt*peakt ^2 - xvs*peaks ^2)*det_bal ;% p1
    partial_deriv(*,1)=   p2*(peakt+peaks)                           *det_bal ;% p2
    partial_deriv(*,2)=2./p2/new_p3^2 *((xvt*peakt)^2 +(xvs*peaks)^2)*det_bal ;% p3
ENDIF
END

;===============================================================================
PRO Sum_fit_fct,x,a,f,npk,pder					; ==============
;===============================================================================
; defines npk 'fct type' plus flat and sloping background
; parameters go pos,ht,wdth in array a

COMMON gc_data
COMMON gc_negative
COMMON gc_dids

nx =N_ELEMENTS(x)
pder=fltarr(nx,N_ELEMENTS(a))	; jacobienne
f(*)=0.0
peak=f
ht_or_wdth=0
peak_neg=0
FOR i=0,npk-1 DO BEGIN
    peak(*)=0.0 & ind=i*3
    p1=a(ind) & p2=a(ind+1) & p3=a(ind+2)
    curpic=i+1
    CALL_PROCEDURE, g_fct_type(i),x,p1,p2,p3,peak,partial_deriv
    if (p2 lt 0.0) or (p3 lt 0.0) then begin
       peak_neg=i+1
       if (p2 lt 0.0) then ht_or_wdth=1
       if (p3 lt 0.0) then ht_or_wdth=2
    endif
    f =f+peak
    pder(*,ind)  =partial_deriv(*,0)
    pder(*,ind+1)=partial_deriv(*,1)
    pder(*,ind+2)=partial_deriv(*,2)
ENDFOR

ilast=npk*3-1		; Add flat and sloping bg
get_bckg, npk, a, bbb, sss
f=f+bbb+sss*x

pder(*,ilast+1)=1	;.....Get partials for backgrounds
pder(0,ilast+2)=x
END

;===============================================================================
PRO fitit,	npeaks,	w_in, w_out, g_parameters, x_ci,$	; ==============
		ncycles,resid,g_step,gw_resid_lab,$		; ==============
		excl_param, poids				; ==============
;===============================================================================

;gjk April 1995

COMMON gc_save
COMMON gc_negative
peak_neg=0
npk	 =npeaks
tiny     =1.0e-4
g_step   =STRTRIM(g_step,2)
parstep  =fltarr((npk*3)+2)
wts	 =w_in
wts(*)   =1.0
size_of_x=N_ELEMENTS(x_ci)

;... calcul de la fonction poids associee a excl_param : (version zone de fit)
;on met 0 en dehors des intervalles entres, ie 1 dans les intervalles entres
poids=fltarr(size_of_x)
for i=1,N_ELEMENTS(excl_param)/2 DO BEGIN
    ;inclusion de l'intervalle [excl_param(2*i-2) , excl_param(2*i-1)] : 
    eps=abs((x_ci(size_of_x-1)-x_ci(0)))/(size_of_x-1) ; eps=distance moyenne 
    ; entre deux elements de x_ci
    FOR j=0,size_of_x-1 DO BEGIN
	IF ((x_ci(j) ge excl_param(2*i-2)) AND (x_ci(j) le excl_param(2*i-1)))$
	    THEN poids(j)=1
    ENDFOR  
ENDFOR
;... fin du calcul de la fonction poids associee a excl_param 

;........Copy across parameters and steps
FOR nvb=0,(npk*3)+1 DO BEGIN
    parstep(nvb)=1.
    ;do we know the step?
    IF STRPOS(g_step(nvb),'f') ge 0 THEN parstep(nvb)=0. ELSE BEGIN
	FOR jd=1,npk DO BEGIN
	    IF float(jd) eq float(g_step(nvb)) THEN BEGIN
 		  parstep(nvb)=-float(jd)
 	    ENDIF	
	ENDFOR
    ENDELSE 
ENDFOR

params=FLTARR((npk*3)+2)
params(0:(npk*3)+1)=g_parameters(0:(npk*3)+1)

if ncycles eq 0 then begin
    w_out=jfit(x_ci,w_in,wts,params,parstep,resid,ncycles,npk,poids)
endif else begin
    o_pars=params
    o_resid=resid
    t=systime(1)
    ; Dumb shifting routine to get it about right, ncycles times
    w_out=jfit(x_ci,w_in,wts,params,parstep,resid,ncycles,npk,poids)
   ; Hope that we are close enough for derivatives, do it twice if ncycles > 1
    if peak_neg eq 0 then begin
       w_out=genfit(x_ci,w_in,wts,params,parstep,npk,resid,poids)
       if ncycles gt 1 then $
       w_out=genfit(x_ci,w_in,wts,params,parstep,npk,resid,poids)
    endif
    IF (resid ge o_resid) or (peak_neg gt 0) THEN BEGIN
	resid=o_resid & params=o_pars
    ENDIF
    old_g=g_parameters
    g_parameters(0:(npk*3)+1)=params(0:(npk*3)+1)
endelse
END

; ************ END OF FITIT **********

;===============================================================================
FUNCTION calculate_error, modele					; ======
;===============================================================================
; retourne un vecteur contenant les erreurs faites sur les parametres
; de la fonction approximante.(cf. "variance-covariance matrix
; determination in non-linear least squares fitting" by R.F. Pettifer)

COMMON gc_data
COMMON gc_save
donnees=w_ni
points_mes=x_ni	

tiny=1e-10
m=N_ELEMENTS(points_mes)	; nombre de mesures
nvarf=0
FOR i=0,3*g_npeaks+1 DO BEGIN
    IF STRPOS(g_step(i),'f') ge 0 or (abs(g_parameters(i)) le tiny) THEN BEGIN
	nvarf=nvarf+1
	IF N_ELEMENTS(repf) eq 0 THEN repf=[i] ELSE repf=[repf,i]
    ENDIF
ENDFOR
IF N_ELEMENTS(repf) ne 0 THEN repfkept=repf
n=3*g_npeaks+2-nvarf		; nb de variables non fixees du modele
f=modele-donnees
s=total(f^2)
;... calcul de la jacobienne J: appel aux procedures de fitit
Sum_fit_fct,points_mes,g_parameters,f,g_npeaks,J
J=transpose(J)
J=J(0:3*g_npeaks+1,*)
nj=3*g_npeaks+2    ; nb de colonnes de J
FOR i=0,nvarf-1 DO BEGIN
    IF (repf(i) eq 0) THEN BEGIN	;cas ou on enleve la 1e colonne
	J=J(1:nj-1,*)
    END ELSE IF (repf(i) eq nj-1) THEN BEGIN	;cas ou on enleve la derniere
						;colonne
	J=J(0:nj-2,*)
    END ELSE BEGIN
	J=[J(0:repf(i)-1,*) , J(repf(i)+1:nj-1,*)]
    ENDELSE
    repf=repf-1
    nj  =nj-1
ENDFOR
; le J obtenu apres call_procedure etait la transposee de la jacobienne
G=2*j#transpose(J)	; produit matriciel !!
H=nr_invert(G)	; inversion matricielle !!
sigcarre=2*S/(m-n)*H
diag =fltarr(n)
FOR i=1,n DO BEGIN
    diag(i-1)=sigcarre(i-1,i-1)
ENDFOR
g_error(0)=sqrt(diag)
IF N_ELEMENTS(repf) ne 0 THEN BEGIN
    error2=fltarr(N_ELEMENTS(g_error))
    j=0 & k=0
    FOR i=0,N_ELEMENTS(error2)-1 DO BEGIN
	IF N_ELEMENTS(repfkept) gt j THEN IF repfkept(j) eq i THEN BEGIN
	    error2(i)=0.0
	    j=j+1
	ENDIF ELSE BEGIN
	    error2(i)=g_error(k)
	    k=k+1
	ENDELSE
    ENDFOR
g_error=error2
ENDIF
RETURN, g_error
END

;===============================================================================
PRO adapte 	,x_to_plot,w_to_plot,w_out,plot_flag				; ==============
;===============================================================================
; Adapt vector to current view zone

common c_trap, trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
COMMON gc_save
COMMON gc_data
COMMON gc_wid
COMMON gc_fit_view
COMMON gc_fit_excl
COMMON gc_dids

; Adapt vector to current plot zone
		noxrange=1
		if ximion then begin
		   widget_control,gx_mim1,get_value=tmpa  & tmpa=tmpa(0)
		   widget_control,gx_mim2,get_value=tmpb  & tmpb=tmpb(0)
		   if strpos(strlowcase(tmpa),'min') ge 0 then tmpa=min(x_to_plot)
		   if strpos(strlowcase(tmpb),'max') ge 0 then tmpb=max(x_to_plot)
		   on_ioerror,misximi & tmpa=float(tmpa) & tmpb=float(tmpb)
		   if tmpb gt tmpa then begin noxrange=0 & ximirange=[tmpa,tmpb] & viewarr(0)=tmpa & viewarr(1)=tmpb & endif
		   misximi:
		endif
		noyrange=1
		if mimion then begin
		   widget_control,gw_mimi,get_value=tmpt  & tmpt=tmpt(0)
		   if strpos(strlowcase(tmpt),'max') ge 0 then tmpt=max(w_to_plot)
		   on_ioerror,mismimi  & tmpt=float(tmpt)
		   if tmpt gt mimirange(0) then begin mimirange(1)=tmpt & noyrange=0 & viewarr(3)=tmpt & endif
		   mismimi:
		endif
ind =where((x_ni ge viewarr(0)) AND (x_ni le viewarr(1)) AND $
		(w_ni ge viewarr(2)) AND (w_ni le viewarr(3)), nb_pt1)

IF (nb_pt1 eq 0) THEN BEGIN
    error_msg, 1
    viewarr(0)=MIN(x_ni,MAX=ma) & viewarr(1)=ma
    viewarr(2)=MIN(w_ni,MAX=ma) & viewarr(3)=ma
    x_to_plot =x_ni
    w_to_plot =w_ni
    if n_elements(e_ni) gt 1 then e_to_plot=e_ni
ENDIF ELSE BEGIN
    x_to_plot =x_ni(ind) ; WE USE XRANGE !!!
    w_to_plot =w_ni(ind) ; WE USE YRANGE !!!
    if n_elements(e_ni) gt 1 then e_to_plot=e_ni(ind)
ENDELSE
    sym_test=size(x_to_plot)
    if sym_test(1) lt 200 then begin
    sym_to_plot=7
    size_to_plot=2.0-(sym_test(1)/200.)
    endif else begin 
    sym_to_plot=10
    size_to_plot=1.
    endelse
    if n_elements(e_ni) le 1 then e_to_plot=0
;
; Plot the data and errors if plot_flag set
    if plot_flag ge 1 then begin
        trap_current=!D.window
        if plot_flag eq 3 then plot,x_to_plot,w_to_plot,/noerase,/nodata $
	else begin
		if (noyrange and noxrange) then plot,x_to_plot,w_to_plot,psym=sym_to_plot,symsize=size_to_plot $
		else   begin if (noyrange) then plot,x_to_plot,w_to_plot,psym=sym_to_plot,symsize=size_to_plot,xrange=ximirange  $
		       else  if (noxrange) then plot,x_to_plot,w_to_plot,psym=sym_to_plot,symsize=size_to_plot,yrange=mimirange  $
		     			   else plot,x_to_plot,w_to_plot,psym=sym_to_plot,symsize=size_to_plot,xrange=ximirange,yrange=mimirange
		endelse
		if n_elements(e_to_plot) gt 1 and n_elements(e_to_plot) lt 100 then $
			errplot,x_to_plot,w_to_plot - e_to_plot,w_to_plot + e_to_plot
		aff_excl,excl_param
	endelse
	if plot_flag eq 1 then oplot,x_ni,w_out,linestyle=2,thick=2 
    endif
END

;===============================================================================
FUNCTION belong_to	,elem,inter				; ==============
;===============================================================================
; retourne un booleen indiquant si l'element elem est dans l'un des intervalles
; de inter ( sert a la procedure aff_excl)

nb  =N_ELEMENTS(inter)
i   =0
WHILE (i le nb/2-1) DO BEGIN
    IF ((elem ge inter(2*i)) AND (elem le inter(2*i+1))) THEN return ,1
    i=i+1
ENDWHILE

return ,0
END

;===============================================================================
PRO aff_excl	,excl_param					; ==============
;===============================================================================
; affichage des zones exclues du fit

COMMON gc_save

; calcul des zones exclues a partir des fit zones : l'ensemble complementaire
; de l'union des intervalles de excl_param dans le domaine de travail.
; on parcourt x_ni avec cour (element courant) et deux indicateurs :
; prec indique si l'element precedent cour etait dans une fit zone
; avprec indique si l'element precedent l'element precedent prec
; etait dans une fit zone.
; res est le tableau resultat contenant les bornes des intervalles
; complementaires de la fit zone

res=fltarr(1) & mini=MIN(x_ni)
IF (MIN(excl_param) le mini) THEN BEGIN
    prec=1 & res(0)=mini
ENDIF ELSE prec=0

FOR i=0,N_ELEMENTS(x_ni)-1 DO BEGIN
    cour=x_ni(i)
    IF belong_to(cour,excl_param) THEN BEGIN 	; We are in a fit zone  
	IF prec THEN avprec=1 ELSE BEGIN
	    avprec=0
	    prec=1
	ENDELSE
    ENDif ELSE BEGIN				; We are not in a fit zone    
	IF N_ELEMENTS(res) eq 1 THEN BEGIN
	    res(0)=cour
	    res=[res,cour]
	    prec  =0
	    avprec=0
	ENDIF ELSE BEGIN		; le nb d'elements de res est > 1   
	    IF prec THEN BEGIN		;prec etait dans une fit zone:
		res =[res,cour] ;on rajoute le min de l'intervalle exclu commence.
		prec  =0
		avprec=1
	    ENDIF ELSE BEGIN		; prec n'etait pas dans une fit zone ...
		prec=0
		IF avprec THEN res=[res,cour]$   ; ... mais avprec si :
		    ; il faut rajouter le max de l'intervalle courant.
		ELSE res(N_ELEMENTS(res)-1)=cour    ;prec n'etait pas dans une fit$
		    ; zone -> il suffit de deplacer la borne max de$
		    ;l'intervalle courant
		avprec=0
	    ENDELSE  
	ENDELSE
    ENDELSE
ENDFOR

; We can now plot fit excluded zone, only in view zone
w_out=1
adapte ,x_to_plot,w_to_plot,w_out,0
;x_to_plot=x_ni
;w_to_plot=w_ni

FOR i=0,N_ELEMENTS(res)/2-1 DO BEGIN
    ;affichage de l 'intervalle [res(i),res(i+1)]
    index=where((x_to_plot ge res(2*i)) AND (x_to_plot le res(2*i+1)),compt)
    IF compt ne 0 THEN BEGIN
	x	=fltarr(2*compt)
	x(0)	=x_to_plot(index)
	x(compt)=reverse(x(0:(compt-1)))
	w	=fltarr(2*compt)
	wmax	=MAX(w_to_plot,MIN=wmin)
	w(0)	=w_to_plot(index)
	w(compt)=reverse(w(0:(compt-1)))+(wmax-wmin)/10
	polyfill,x,w
    ENDIF
ENDFOR
END

FUNCTION gfit_event_func, event
gfit_event, event
RETURN, 0
END

;===============================================================================
PRO gfit_ctrl_panel, id						; ==============
;===============================================================================
; if id is defined,  then gfit_ctrl_panel has been destroyed, build it again

@lamp.cbk
COMMON gc_save
COMMON gc_wid
COMMON gc_data
COMMON gc_fit_excl
COMMON gc_cp
COMMON gc_flags
COMMON gc_fit_view

IF XREGISTERED('gfit_ctrl_panel') GT 0 AND N_PARAMS() EQ 0 THEN RETURN
IF XREGISTERED('gfit_ctrl_panel') GT 0 THEN WIDGET_CONTROL, bad_id=i, id, /Destroy
IF XREGISTERED('gfit') LE 0 then RETURN
if gw_gkfit_base       eq 0 then RETURN
BASE		=WIDGET_BASE  (Title='GFIT Control Panel', $
			       Group_Leader=gw_gkfit_base, $
			       Resource_Name='lampdon', /Column)
set_show_base	=WIDGET_BASE  (BASE, /Column, /frame)

peak_lab	=WIDGET_LABEL (set_show_base, Font= ft_b_bigger,$
				VALUE='Set fit parameters of function #')

peak_menu2	=WIDGET_BASE  (set_show_base , Column=g_nb_pk_max)
gw_type_but	=LONARR(g_nb_pk_max)  
gw_gau_but	=LONARR(g_nb_pk_max)
gw_lor_but	=LONARR(g_nb_pk_max)
gw_peak_but	=LONARR(g_nb_pk_max)

FOR i=1,g_nb_pk_max DO BEGIN
    gw_type_but(i-1)=WIDGET_BUTTON (peak_menu2, /Menu, Font=ft_propor, $
				UValue=[4,i,1], Value=STRMID(g_fct_type(i-1), 0, 3))
    gw_gau_but(i-1)=WIDGET_BUTTON(gw_type_but(i-1), UValue=[4,i,1],Value='Gauss', Font=ft_propor)
    gw_lor_but(i-1)=WIDGET_BUTTON(gw_type_but(i-1), UValue=[4,i,2],Value='Lorentz', Font=ft_propor)
    peak_base	   =WIDGET_BASE(peak_menu2, /Exclusive)
    gw_peak_but(i-1)=WIDGET_BUTTON(peak_base,/No_Release, Value=STRTRIM(STRING(i),2),$
				   UValue=[2,2,0], Font=ft_b_bigger) 
    IF i LE g_npeaks THEN sens=1 ELSE sens=0
    WIDGET_CONTROL, bad_id=iii, gw_peak_but(i-1) ,Sensitive=sens
ENDFOR
WIDGET_CONTROL, bad_id=i, gw_peak_but(g_pk_nb-1), Set_Button=1	; First button is set (Default)

gw_pk_slider=WIDGET_SLIDER(set_show_base, Maximum=g_nb_pk_max,$
				Minimum=1,/Suppress_Value, YSize=15,$
				UVALUE=[2,1,0],Value=g_npeaks)

button_base =WIDGET_BASE  (Title='GFIT Control Panel', BASE, /Column)
bloc_base   =WIDGET_BASE   (BASE , /row)		;.... gw_resid_lab .... 
resid_lab   =WIDGET_LABEL  (bloc_base,Value ='Residual:', Font=ft_normal)
gw_resid_lab=WIDGET_LABEL  (bloc_base,Value ='______________',Font=ft_b_normal)
bloc_baseb   =WIDGET_BASE   (BASE , /row)		;.... gw_resid_lab .... 
gw_cycle_lab=WIDGET_LABEL  (bloc_baseb, Font=ft_b_normal, Value='  '+$
				 STRTRIM(STRING(g_ncycles),2)+' Cycles')

cycles_sld  =WIDGET_SLIDER (bloc_baseb, Maximum=75 ,Minimum=0 , /Drag,$
				Value=g_ncycles,xsize=150, $
				YSize=16,/Suppress_Value, UValue=[2,3,0])

bloc_basec   =WIDGET_BASE   (BASE , /row)		;.... gw_resid_lab .... 
fit_but	    =WIDGET_BUTTON  (bloc_basec, Font=ft_biggest,$
				UValue=[2,4,0], Value ='FIT IT')
print_but   =WIDGET_BUTTON  (bloc_basec,Font=ft_biggest,$
				UValue=[3,1,0],Value=' Print ')
help_but    =WIDGET_BUTTON  (bloc_basec,Font=ft_biggest,$
				UValue=[3,2,0],Value=' Help ')
gw_exit_but =WIDGET_BUTTON  (bloc_basec,Font=ft_biggest,$
				UValue=[3,3,0], Value=' DONE ')


bloc_based  =WIDGET_BASE   (BASE , /Column, Frame=2)
title_lab   =WIDGET_LABEL  (bloc_based, Font=ft_propor, Value='Parameters')
bloc_based1 =WIDGET_BASE   (bloc_based, /Column, /Frame)
title_lab   =WIDGET_LABEL  (bloc_based1, Font=ft_smaller, Value='# bck.      Pos.     Fix.  Height   Fix.  HwHm  Fix.')
row	    =LONARR(g_nb_pk_max)
cp_nb	    =LONARR(g_nb_pk_max)
cp_bck	    =LONARR(g_nb_pk_max)
cp_pos	    =LONARR(g_nb_pk_max)
cp_hgt	    =LONARR(g_nb_pk_max)
cp_wid	    =LONARR(g_nb_pk_max)
cp_pf	    =LONARR(g_nb_pk_max)
cp_hf	    =LONARR(g_nb_pk_max)
cp_wf	    =LONARR(g_nb_pk_max)
FOR i=1, g_nb_pk_max DO BEGIN
    row(i-1)    =WIDGET_BASE   (bloc_based1, /Row, /Frame)
    cp_nb(i-1)  =WIDGET_LABEL  (row(i-1),Font=ft_smaller, Value=STRTRIM(STRING(i), 2))
    basecp	=WIDGET_BASE   (row(i-1), /NonExclusive)
    cp_bck(i-1) =WIDGET_BUTTON (basecp,Font=ft_propor, Value='')
    cp_pos(i-1) =WIDGET_TEXT   (row(i-1), Font=ft_smaller, Value='', XSize=6)
    basecp	=WIDGET_BASE   (row(i-1), /NonExclusive)
    cp_pf(i-1)  =WIDGET_BUTTON (basecp,Font=ft_smaller, Value='f')
    cp_hgt(i-1) =WIDGET_TEXT   (row(i-1), Font=ft_smaller, Value='', XSize=6)
    basecp	=WIDGET_BASE   (row(i-1), /NonExclusive)
    cp_hf(i-1)  =WIDGET_BUTTON (basecp,Font=ft_smaller, Value='f')
    cp_wid(i-1) =WIDGET_TEXT   (row(i-1), Font=ft_smaller, Value='', XSize=6)
    basecp	=WIDGET_BASE   (row(i-1), /NonExclusive)
    cp_wf(i-1)  =WIDGET_BUTTON (basecp,Font=ft_smaller, Value='f')
ENDFOR
bloc_based2 =WIDGET_BASE   (bloc_based, /Column, /Frame)
lab	    =WIDGET_LABEL  (bloc_based2, Font=ft_propor, Value='    Flat BKG Fix. Slope BKG Fix.')
row1	    =WIDGET_BASE   (bloc_based2, /Row)
bid	    =WIDGET_LABEL  (row1, Font=ft_smaller, Value='         ')
cp_fb	    =WIDGET_TEXT   (row1, Font=ft_smaller, Value='', XSize=7)
basecp	    =WIDGET_BASE   (row1, /NonExclusive)
cp_fbf	    =WIDGET_BUTTON (basecp,Font=ft_smaller, Value='f')
cp_sb	    =WIDGET_TEXT   (row1, Font=ft_smaller, Value='', XSize=7)
basecp	    =WIDGET_BASE   (row1, /NonExclusive)
cp_sbf	     =WIDGET_BUTTON(basecp,Font=ft_smaller, Value='f')
bid=sys_dep('DYNLAB', base, 1)
WIDGET_CONTROL, bad_id=i, BASE, /Realize
XMANAGER, 'gfit_ctrl_panel', BASE, /Just_reg, Cleanup='gfit_ctrl_panel'
WIDGET_CONTROL, bad_id=i, BASE, Event_Func='gfit_event_func'
RETURN
END

;===============================================================================
PRO do_fit, option						; ==============
;===============================================================================
; 
@lamp.cbk
COMMON gc_save
COMMON gc_wid
COMMON gc_data
COMMON gc_fit_excl
COMMON gc_flags
COMMON gc_fit_view
COMMON gc_negative

IF sw_ni(0) gt 0 THEN BEGIN
    ;calculation of the array used to calculate integrals for non-linear scales
    ;l(i)=(x(i+1)-x(i-1))/2 pour i variant de 1 a nombre mesures-1
    ;l(0)=(x(1)-x(0))/2  et  l(n)=(x(n)-x(n-1))/2
    larg =fltarr(N_ELEMENTS(x_ni))
    xprim=[x_ni(1:N_ELEMENTS(x_ni)-1),x_ni(N_ELEMENTS(x_ni)-1)]
    xsec =[x_ni(0),x_ni(0:N_ELEMENTS(x_ni)-2)]
    g_largeur=(xprim-xsec)/2
    read_par_vals
    gk_warning,excl_param,viewarr

    empty=0			; Check if each peaks is defined
    FOR i=0,g_npeaks-1 DO BEGIN
	if g_parameters(i*3+1) le 0 or g_parameters(i*3+2) le 0 THEN $
	    empty=empty+1
    ENDFOR
    IF empty gt 0 THEN BEGIN	; --- One of the peaks was undefined
	str=STRTRIM(STRING(empty),2)+' peak'
	if empty gt 1 THEN str=str +'s are ' ELSE str=str+' is '
	str=str+'not defined ! Define empty peak'
	if empty gt 1 THEN str=str+'s'
	error_msg, -1, str
    ENDIF ELSE BEGIN		; --- No peak is undefined
	old_resid=g_resid		; --- Push resid
	old_par=g_parameters	; --- Push parameters
	fitit,g_npeaks,w_ni,w_out,g_parameters,x_ni,g_ncycles,$
	      g_resid,g_step,gw_resid_lab ,excl_param,poids
	g_afitisdone=1		; a fit was done
	
	if peak_neg gt 0 then begin
	   if ht_or_wdth eq 1 then $
	   str='Height of peak '+ STRTRIM(STRING(peak_neg),2) + ' becomes negative'
           if ht_or_wdth eq 2 then $
	   str='Width of peak '+ STRTRIM(STRING(peak_neg),2) + ' becomes negative'
	   error_msg,-1,str 
	   g_resid=old_resid	    ; Pop resid
	   g_parameters=old_par    ; Pop parameters
	endif
	
	IF (g_resid ge old_resid) and (peak_neg eq 0) THEN BEGIN ; If worse, restore old values
	    g_resid=old_resid	    ; Pop resid
	    g_parameters=old_par    ; Pop parameters
	    error_msg, 4	    ; Alert user
	ENDIF ELSE $
;
; Plotting of results on screen here
	WIDGET_CONTROL, bad_id=i, gw_resid_lab, Set_Value=STRTRIM(STRING(g_resid),2)+'%'
	IF N_PARAMS() GT 0 THEN wset,did_win0 ELSE wset,g_plot_wid

	adapte ,x_to_plot,w_to_plot,w_out,1
	plt_dev=0
	;.....plot subfunctions if necessary
	IF g_show_subfct THEN plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type
	; --- Error calculation
	g_error=calculate_error(w_out)
	put_vals,x_ni,w_ni,w_out,plt_dev,poids,lamp_siz
	write_par_vals, /No_Update
    ENDELSE
ENDIF
IF N_PARAMS() GT 0 THEN BEGIN
    option=w_out
ENDIF
END

;===============================================================================
PRO get_param, g_parameters, idx, p1, p2, p3	    		; ==============
;===============================================================================

ind=(idx-1)*3
p1=g_parameters(ind)
p2=g_parameters(ind+1)
p3=g_parameters(ind+2)
RETURN
END

;===============================================================================
PRO get_bckg, g_npeaks, g_parameters, bbb, sss	    		; ==============
;===============================================================================

ilast=g_npeaks*3-1
bbb  =g_parameters(ilast+1)
sss  =g_parameters(ilast+2)
RETURN
END

;===============================================================================
PRO gfit_help	, dummy, formu, formt
;===============================================================================

formu=''
formt='Fitting Routines by G.Kearley. Interface by Romuald JOUFFREY.'
formu=[formu,'What is to be plotted :']
formt=[formt,'']
formu=[formu,'']
formt=[formt,'Adjust workspace number (upper left) and Click "GetWs #" to fetch data.']
formu=[formu,'']
formt=[formt,'Cutting value can be set with the tiny slider']
formu=[formu,'How to perform a fit :']
formt=[formt,'']
formu=[formu,'High level details']
formt=[formt,'']
formu=[formu,'']
formt=[formt,'Once a scan is plotted, choose the number of subfonctions, Then choose the type of fitting subfonction(s)']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Clicks on plot are significant :']
formt=[formt,'']
formu=[formu,'Left mouse button ']
formt=[formt,'- click current subfunction Height(position) then drag and release on half-Width']
formu=[formu,'Right mouse button']
formt=[formt,'- Drag to set the fitting area ("Fit-zone" button is used for multiple areas)']
formu=[formu,'']
formt=[formt,'']
RETURN
END

;===============================================================================
PRO gk_initialize						; ==============
;===============================================================================

COMMON gc_data
COMMON gc_flags
COMMON gc_fit_excl
COMMON gc_wid

;g_parameters(*)=0.0 & g_step(*)=''
;gc_fit_flags=0
V=''

WIDGET_CONTROL, bad_id=i, gw_pos_text,	Set_Value='0.0'	;Clear field
WIDGET_CONTROL, bad_id=i, gw_int_text,	Set_Value='0.0'	;Clear field
WIDGET_CONTROL, bad_id=i, gw_wdt_text,	Set_Value='0.0'	;Clear field
WIDGET_CONTROL, bad_id=i, gw_fbg_text,	Set_Value='0.0'	;Clear field
WIDGET_CONTROL, bad_id=i, gw_sbg_text,	Set_Value='0.0'	;Clear field
WIDGET_CONTROL, bad_id=i, gw_cut_label,	Set_Value=''
WIDGET_CONTROL, bad_id=i, gw_resid_lab,	Set_Value=''

g_resid=1.0e12
WSET, g_plot_wid
ERASE, 0
excl_param=fltarr(2*max_nb_int)	; Reset excl_param to [0,0]
END 

;===============================================================================
PRO gfit_Event, Event						; ==============
;===============================================================================
; Called when a gfit event is generated

@lamp.cbk
common c_trap, trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
COMMON gc_save
COMMON gc_wid
COMMON gc_data
COMMON gc_fit_excl
COMMON gc_flags
COMMON gc_fit_view
COMMON gc_width_factor
COMMON gc_negative
COMMON gc_dids

stat=0
catch,stat
if stat  ne 0  then BEGIN
		catch,/cancel
 		set_plot,my_path(3)
		WIDGET_CONTROL, bad_id=i, gw_err_lab, Set_Value=!err_STRING
		RETURN & ENDIF
error_msg, 0				; Clear error msg fields

if  tag_names(Event,/structure_name) ne 'WIDGET_DRAW' then $
	WIDGET_CONTROL, bad_id=i, Event.Id, /Hourglass
	WIDGET_CONTROL, bad_id=i, Event.Id, GET_UVALUE=Ev, Get_Value=value
no_plot=0
CASE Ev(0) OF
    1 	: BEGIN
    CASE Ev(1) OF
    1 : BEGIN			; W slider event
	    WIDGET_CONTROL, bad_id=i, event.id, Get_Value=nw_ni
	END

    2	: BEGIN			; Get Button event
	gk_initialize
	IF Ev(2) NE 0 THEN BEGIN
	    nw_ni=Ev(2)
	    tripx_mode=1
	ENDIF ELSE tripx_mode=0
	nwt_in=STRTRIM(STRING(nw_ni),2)	; Get W number id
	w_ni=0 & x_in=0 & y_in=0 & e_in=0
	iii=EXECUTE("w_ni=w"+nwt_in)
	iii=EXECUTE("x_in=x"+nwt_in) & x_ni=x_in(*,0)
	iii=EXECUTE("y_in=y"+nwt_in)
	iii=EXECUTE("e_ni=e"+nwt_in)
	if n_elements(e_ni) ne n_elements(w_ni) then e_ni=0
	w_ni=w_ni > 0
	sw_ni=SIZE(w_ni) & sx_ni=SIZE(x_ni)		    & sy_in=SIZE(y_in)
	if sy_in(1) gt 1  then begin y_in=reform(y_in(0,*)) & sy_in=SIZE(y_in) & endif
	keepw=w_ni
	keepe=e_ni
	IF sw_ni(0) gt 0 THEN BEGIN
	    IF sw_ni(0) gt 2 THEN BEGIN
		w_ni =TOTAL(w_ni,3) & e_ni=0
		sw_ni=SIZE(w_ni)
		error_msg, 2
	    ENDIF

	    IF (sw_ni(1) ne sx_ni(1)) or (sx_ni(0) ne 1) THEN BEGIN
		x_ni=indgen(sw_ni(1)) & x_in=x_ni      & ENDIF

	    IF sw_ni(0) gt 1 THEN BEGIN
		WIDGET_CONTROL, bad_id=i, ws_in_r      , Map=1
	    	WIDGET_CONTROL, bad_id=i, gw_cut_slider, /Sensitive
	    	; initializing regle
	    	WIDGET_CONTROL, bad_id=i, gw_cut_slider, Set_Slider_Max=sw_ni(2)
	    	; on cree un "y" par defaut :
		IF n_elements(y_in) ne sw_ni(2) THEN y_in=findgen(sw_ni(2))+1
	    	WIDGET_CONTROL, bad_id=i, gw_cut_slider, Get_Value=no_coupe
	    	w_ni =keepw(*,no_coupe-1)
	    	if n_elements(e_ni) gt 1 then $
		e_ni =keepe(*,no_coupe-1)
	    	sw_ni=size(w_ni)
		IF (size(x_in))(0) eq 2 then x_ni=x_in(*,no_coupe-1)

	    	; initialisation du label de la regle :
	    	WIDGET_CONTROL, bad_id=i, gw_cut_label, Set_Value='->'+$
	    	STRTRIM(STRING(y_in(no_coupe-1)),2)

	    ENDIF ELSE BEGIN
	        WIDGET_CONTROL, bad_id=i, gw_cut_slider, sensitive=0
		WIDGET_CONTROL, bad_id=i, ws_in_r      , Map=0 & ENDELSE

	    g_error=fltarr(3*g_nb_pk_max+2)

	    ;initialisation par defaut de excl_param :
	    excl_param(0)=MIN(x_in,MAX=ma)
	    excl_param(1)=ma
	    WIDGET_CONTROL, bad_id=i, excl_widge(0),Set_Value=$
				STRTRIM(STRING(excl_param(0)),2)
	    WIDGET_CONTROL, bad_id=i, excl_widge(1),Set_Value=$
				STRTRIM(STRING(excl_param(1)),2)
	    FOR i=1,max_nb_int-1 DO BEGIN
		excl_param(2*i)  =excl_param(0)-1
		excl_param(2*i+1)=excl_param(0)-1
		WIDGET_CONTROL, bad_id=iii, excl_widge(2*i)  ,Set_Value='***'
		WIDGET_CONTROL, bad_id=iii, excl_widge(2*i+1),Set_Value='***'
	    ENDFOR
	
	    ; initialisation de viewarr :
	    viewarr(0)=excl_param(0)	& viewarr(0+sauv)=excl_param(0)
	    viewarr(1)=excl_param(1)	& viewarr(1+sauv)=excl_param(1)
	    viewarr(2)=MIN(w_ni,MAX=ma)	& viewarr(2+sauv)=viewarr(2)
	    viewarr(3)=ma		& viewarr(3+sauv)=viewarr(3)
	    mimirange =[viewarr(2),viewarr(3)]

	    IF tripx_mode THEN WSET, did_win0 ELSE WSET,g_plot_wid
	   ;!p.title=w_tit(nw_ni)
	    !x.title=x_tit(nw_ni)
	    !y.title=y_tit(nw_ni)
	    
	    trap_current=!D.window
	    adapte ,x_ni,w_ni,w_ni,1 ;plot,x_ni,w_ni
    ENDIF ELSE error_msg, 3
    END
    3 : BEGIN			; Cut slider event
		    g_resid=1.0e12
		    ; --- Getting data to plot
		    WIDGET_CONTROL, bad_id=i, gw_cut_slider, Get_Value=no_coupe
		    w_ni=keepw(*,no_coupe-1)
		    if n_elements(e_ni) gt 1 then $
		    e_ni=keepe(*,no_coupe-1)
		    IF (size(x_in))(0) eq 2 then x_ni=x_in(*,no_coupe-1)

		    ; --- Initializing  viewarr(2) & viewarr(3)
		    viewarr(2)=MIN(w_ni,MAX=ma)	& viewarr(2+sauv)=viewarr(2)
		    viewarr(3)=ma		& viewarr(3+sauv)=viewarr(3)
		    mimirange =[viewarr(2),viewarr(3)]

		    ; update label of parameter
		    WIDGET_CONTROL, bad_id=i, gw_cut_label, Set_Value =$
			'->'+STRTRIM(STRING(y_in(no_coupe-1)),2)

		    ; do the plot
		    !x.title=x_tit(nw_ni)+' Spectrum '+STRTRIM(STRING(y_in(no_coupe-1)),2)
		    wset,g_plot_wid
		    adapte ,x_to_plot,w_to_plot,w_out,1
		  END
    4		: BEGIN			; --- W out Slider event
		    WIDGET_CONTROL, bad_id=i, event.id, Get_Value=nw_out
		  END
    5		: IF sw_ni(0) gt 0 THEN BEGIN	; --- W out Button event
			xicuter, 'w'+STRTRIM(STRING(nw_out),2) +$
			'=gk_fit(w' +STRTRIM(STRING(nw_ni),2) +')'
		  ENDIF
    ENDCASE
    END
    2	: BEGIN
    CASE Ev(1) OF
    1		: BEGIN			; --- Peak Slider 
		new_npeaks=0
		WIDGET_CONTROL, bad_id=i, event.id, Get_Value=new_npeaks
		IF new_npeaks LT g_pk_nb THEN BEGIN
		    ; peak_number 1 button Simulated event
		    gfit_Event,{WIDGET_BUTTON, ID:gw_peak_but(0), $
				TOP:gw_gkfit_base, HANDLER:0L, SELECT:1} 
		ENDIF ELSE BEGIN
		    g_step(g_npeaks*3:g_npeaks*3+1)=''
		ENDELSE
		g_npeaks=new_npeaks
		; Make Peak buttons number > g_npeaks NonSensible
		FOR i=1,g_nb_pk_max DO BEGIN
		    IF i GT g_npeaks THEN BEGIN
			WIDGET_CONTROL, bad_id=iii, gw_pos_pop (i+3), Sensitive=0
			WIDGET_CONTROL, bad_id=iii, gw_int_pop (i+3), Sensitive=0
			WIDGET_CONTROL, bad_id=iii, gw_wdt_pop (i+3), Sensitive=0
			WIDGET_CONTROL, bad_id=iii, gw_peak_but(i-1), Sensitive=0
			WIDGET_CONTROL, bad_id=iii, gw_type_but(i-1), Sensitive=0
			WIDGET_CONTROL, bad_id=iii, gw_peak_but(g_nb_pk_max+i-1), map=0
		    ENDIF ELSE BEGIN
			WIDGET_CONTROL, bad_id=iii, gw_pos_pop (i+3), /Sensitive
			WIDGET_CONTROL, bad_id=iii, gw_int_pop (i+3), /Sensitive
			WIDGET_CONTROL, bad_id=iii, gw_wdt_pop (i+3), /Sensitive
			WIDGET_CONTROL, bad_id=iii, gw_peak_but(i-1), /Sensitive
			WIDGET_CONTROL, bad_id=iii, gw_type_but(i-1), /Sensitive
			WIDGET_CONTROL, bad_id=iii, gw_peak_but(g_nb_pk_max+i-1), map=1
		    ENDELSE
		    g_resid=1.0e12
		ENDFOR
		g_resid=1.0e12
		 END
    2	       : BEGIN			; --- Peak number Button event
		    ; Get old selected peak parameters
		    read_par_vals
		    g_old_pk_nb=g_pk_nb
		    WIDGET_CONTROL, bad_id=i, event.id, Get_Value=g_pk_nb
		    IF g_pk_nb ne g_old_pk_nb THEN BEGIN
		        WIDGET_CONTROL, bad_id=i, gw_bose , set_button=bose(g_pk_nb-1)
			WIDGET_CONTROL, bad_id=i, gw_pk_no, Get_value=V
			V='Peak #'+STRTRIM(STRING(g_pk_nb), 2)+' param'
			WIDGET_CONTROL, bad_id=i, gw_pk_no, Set_Value=V
			FOR i=0,g_npeaks-1 DO $
			    WIDGET_CONTROL, bad_id=iii, gw_peak_but(i), $
					    Set_Button=(i EQ g_pk_nb-1)
			write_par_vals
			error_msg, -2, g_pk_nb
		    ENDIF
		   END
		




    3		: BEGIN		; --- Number of cycle(s) slider event
			WIDGET_CONTROL, bad_id=i, event.id, Get_Value=g_ncycles
			str='  '	; --- Display number of cycle(s)
			IF g_ncycles gt 1 THEN str='s'
			IF strlen(STRTRIM(STRING(g_ncycles),2)) eq 1 THEN $
			WIDGET_CONTROL, bad_id=i, gw_cycle_lab, Set_Value='  '+$
			STRTRIM(STRING(g_ncycles),2)+' Cycle'+str $
			ELSE $
			WIDGET_CONTROL, bad_id=i, gw_cycle_lab, Set_Value=$
			STRTRIM(STRING(g_ncycles),2)+' Cycle'+str
		    END
    4	: do_fit
    5	: BEGIN				; --- Fitting Zone Button event
		WIDGET_CONTROL, bad_id=i, gw_excl_base, /Map ,/Show
	  END
    6  	: BEGIN			; --- Exit Button event
		    WIDGET_CONTROL, bad_id=i, gw_view_base, /Map, /Show
		  END
    7		: IF sw_ni(0) gt 0 THEN $	; --- Plot area event
		    ; IF released button or right button is pressed
		    IF ((event.TYPE eq 1) or ((event.TYPE eq 0) $
		    	AND ((event.press eq 4) or (event.press eq 1)))) THEN BEGIN 
			WSET ,g_plot_wid
			if trap_current ne g_plot_wid then adapte ,x_to_plot,w_to_plot,w_out,3
			CURSOR,xcurs,ycurs,/nowait
			read_par_vals
			ind=(g_pk_nb-1)*3
			affiche=1
			IF g_parameters(ind+1) le 0 THEN g_parameters(ind+1)=10e-12  ;intensite par defaut 
			IF g_parameters(ind+2) le 0.0 THEN BEGIN ;largeur par defaut
			    sx=size(x_ni)
			    g_parameters(ind+2)=ABS(x_ni(sx(3)-1)-x_ni(0))/30.0
			    ; ???
			ENDIF
			; Set Position and Intensity
			g_resid=1.0e12
			IF event.press eq 1 THEN BEGIN
			    DEVICE ,cursor_standard=16
			    g_parameters(ind)=xcurs
			    ;g_parameters(ind+1)=ycurs
			    get_bckg, g_npeaks, g_parameters, bbb, sss
			    g_parameters(ind+1)=ycurs-(bbb+xcurs*sss)
			    no_plot=1  ; prevent plotting
			ENDIF
			; fin de determination de la position et de l'intensite 

			;..... width determination  
			IF event.release eq 1 THEN BEGIN
			   DEVICE ,cursor_standard=2
			   g_resid=1.0e12 
			   p=g_parameters(ind) & h=g_parameters(ind+1)
			   old_w=g_parameters(ind+2)
		           get_bckg, g_npeaks, g_parameters, bbb, sss
			   ht_bg=ycurs-(bbb+xcurs*sss)
			   IF g_fct_type(g_pk_nb-1) eq 'Gauss' THEN BEGIN
			    ;calcul de la largeur =|pos-xcurs|/rac(2|log(h)-log(ht_bg)|)
 			    denom=sqrt((abs(ALOG(h)-ALOG(ht_bg)))/gw_fac)
			    g_parameters(ind+2)=ABS(p-xcurs)/denom
			   ENDIF
			   IF g_fct_type(g_pk_nb-1) eq 'Lorentz' THEN BEGIN
			    ;calcul de la largeur =|pos-xcurs|/rac(h/y-1)
			    IF h le ht_bg THEN g_parameters(ind+2)=1.0e12 $
			    ELSE g_parameters(ind+2)=ABS(p-xcurs)/(SQRT(h/ht_bg-1)*lw_fac)
			   ENDIF
		        ENDIF
		        ;..... fin de determination de la largeur

		        ;.... determination de la fit zone :
		        IF event.press eq 4 THEN BEGIN
			   affiche=0
			   bb1=xcurs
			   bb2=x_ni(0)
			   DEVICE ,cursor_standard=32
		        ENDIF

		        IF event.release eq 4 THEN BEGIN
			   affiche=1
			   DEVICE ,cursor_standard=2
			   bb2=xcurs
			   ma =MAX(x_ni,MIN=mi)
			   IF abs(bb2-bb1) gt (ma-mi)/30 THEN BEGIN
			    ma=MAX([bb1,bb2],MIN=mi)
			    excl_param(2*intv_no-2)=mi
			    excl_param(2*intv_no-1)=ma
				WIDGET_CONTROL, bad_id=i, excl_widge(2*intv_no-2),$
				Set_Value=STRTRIM(STRING(mi),2)
				WIDGET_CONTROL, bad_id=i, excl_widge(2*intv_no-1),$
				Set_Value=STRTRIM(STRING(ma),2)
			   ENDIF
		        ENDIF
		        ;....  fin de determination des intervalles a exclure

		    	if no_plot eq 0 then $
		    	adapte ,x_to_plot,w_to_plot,w_out,2
		    	IF affiche THEN aff_excl,excl_param
		    	;..... plot peaky(s)
		    	if no_plot eq 0 then $
		    	plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type
		    	no_plot=0
		    	write_par_vals, /No_Update

;		    	IF event.release eq 2 THEN BEGIN
;		    	   read_par_vals
;		    	   new_pk_nb=g_pk_nb+1
;		    	   IF new_pk_nb GT g_npeaks THEN new_pk_nb=1
;		    	   gfit_Event,{WIDGET_BUTTON, ID:gw_peak_but(new_pk_nb-1), $
;				TOP:gw_gkfit_base, HANDLER:0L, SELECT:1} 
;		        ENDIF
		    ENDIF
    8		: IF sw_ni(0) gt 0 THEN BEGIN	; --- Show subfunctions
			g_show_subfct=event.select
		ENDIF
    9		: BEGIN	; --- Convolution Button event
			; To be continued ...
		END
    10:		bose(g_pk_nb-1)=event.select
    11:		begin mimion   =event.select & if not mimion then begin viewarr(2)=MIN(w_ni,MAX=ma) & viewarr(3)=ma & endif & end
    12:		begin ximion   =event.select & if not ximion then begin viewarr(0)=MIN(x_in,MAX=ma) & viewarr(1)=ma & endif & end

    ENDCASE
    END
    3	: BEGIN
    CASE Ev(1) OF
        1   : IF sw_ni(0) gt 0 AND g_afitisdone THEN BEGIN ; Print But event
		g_print_nb=g_print_nb+1
		psFile='fit'+STRTRIM(STRING(g_print_nb),2)+'.ps'
		wplot=!D.NAME
		err=1
		ON_IOERROR,IFerr
		set_plot,'PS'
		DEVICE, filename=psFile
		DEVICE, bits_per_pixel=8,color=0
		IF g_tripx THEN BEGIN		; Tripx printing mode
		    short_side	=17.0
		    small_offset=0.5
		    long_side	=14.0
		    big_offset	=14.0
		    w_out=0
		    DEVICE, /portrait, /inches
		    DEVICE, xsize=short_side, ysize=long_side
		    DEVICE, xoffset=small_offset, yoffset=big_offset
		    fitit,g_npeaks,w_ni,w_out,g_parameters,x_ni,0,g_resid,$ 
				g_step,gw_resid_lab,excl_param,poids

		    adapte ,x_to_plot,w_to_plot,w_out,1
		    IF g_show_subfct THEN plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type
		    plt_dev=1
				; Printing -> big character size
		    FOR i=0, N_ELEMENTS(g_tx_par)-1 DO BEGIN
			XYOUTS, 100, -360.-(i-1)*320., g_tx_par(i), $
				    CharSize=g_char, /Device
		    ENDFOR
		    print_vals,x_ni,w_ni,w_out,plt_dev,poids
		    DEVICE, /color
		    P_DID_PS_HEADER, 5.5, 0, psFile
		ENDIF ELSE BEGIN		; gfit printing mode
		    sx=7. & sy=11.5
		    DEVICE,yoffset=sy-.5,xoffset=.5,/inches,/landscape
		    read_par_vals
		    w_out=0
		    fitit,g_npeaks,w_ni,w_out,g_parameters,x_ni,0,g_resid,$ 
				g_step,gw_resid_lab,excl_param,poids

		    adapte ,x_to_plot,w_to_plot,w_out,1
		    IF g_show_subfct THEN plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type
		    plt_dev=1
				; Printing -> big character size
		    put_vals,x_ni,w_ni,w_out,plt_dev,poids,1000
		    print_vals,x_ni,w_ni,w_out,plt_dev,poids
		    DEVICE, bits_per_pixel=8, /color
		    P_DID_PS_HEADER, 7., nw_ni, psFile
		    err=0
		ENDELSE
		IFerr: DEVICE,/Close_File
		set_plot,wplot
		ENDIF
    2  	: BEGIN
		show_helps, [-88, 590]
	  END
    3  	: BEGIN			; --- Exit Button event
		gg=gw_gkfit_base & gw_gkfit_base=0
		WIDGET_CONTROL, bad_id=i, gg ,/Destroy & wait,.3
		IF g_old_plot_wid GT 0 then WSET,g_old_plot_wid
	  END
    ENDCASE
    END
    4	: BEGIN	; --- Peak pop up menu event
		if Ev(2) eq 1 then BEGIN
		g_fct_type(Ev(1)-1)='Gauss'
		v='Gau'
		ENDIF
		if Ev(2) eq 2 then BEGIN
		g_fct_type(Ev(1)-1)='Lorentz'
		v='Lor'
		ENDIF
		WIDGET_CONTROL, bad_id=i, gw_type_but(Ev(1)-1), Set_Value=v
		g_resid=1.0e12
	  END
    5   : Parameter_menu,Ev
    6	: BEGIN
		read_par_vals
		g_resid=1.0e12
		adapte ,x_to_plot,w_to_plot,w_out,1
		plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type
	  END

    -88 : if Ev(1) eq 391 then p_did_mvlog, Event,Ev

    ELSE: error_msg, 5
    ENDCASE
END

;===============================================================================
PRO gk_warning	,excl_param, viewarr				; ==============
;===============================================================================
; Warning if points of fitting zone are not in viewing zone

COMMON gc_wid

FOR i=1,N_ELEMENTS(excl_param)/2 DO BEGIN ; parcours sur chaque intervalle de fit
    IF ((viewarr(0) gt excl_param(2*i-2)) or $
	   (viewarr(1) lt excl_param(2*i-1)) ) $
	   AND (excl_param(2*i-2) ne excl_param(2*i-1)) $
	THEN error_msg, 6
    ENDFOR
END

;===============================================================================
PRO plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type		; ==============
;===============================================================================
;Puts titles on plot
;
COMMON gc_dids

FOR idx=1,g_npeaks DO BEGIN
    get_param, g_parameters, idx, p1, p2, p3
    IF p2 gt 0 AND p3 gt 0 THEN BEGIN
	IF p2 lt 0.0 THEN peaky=0.0
	IF p2 gt 0.0 THEN BEGIN
	    curpic=idx
	    CALL_PROCEDURE, g_fct_type(idx-1), $
			x_ni,p1,p2,p3,peaky
	    get_bckg, g_npeaks, g_parameters, bbb, sss
	    ma=MAX(peaky,MIN=mi)
	    pp=0.3*(ma-mi)
	    peaky=peaky + (bbb + x_ni*sss) 
	    OPLOT,x_ni,peaky,linestyle=1,thick=2
	    pp=pp+(bbb+p1*sss)
	    XYOUTS,p1,pp,STRTRIM(STRING(idx),2),CharSize=1.2, NoClip=0
	ENDIF
    ENDIF
ENDFOR
RETURN
END

;===============================================================================
PRO put_tit	,x_dats,y_dats,f_tit,fx_tit,fy_tit		; ==============
;===============================================================================
;Puts titles on plot
;

ma  =MAX(x_dats,MIN=mi)
xrng=ma-mi
ma  =MAX(y_dats,MIN=mi)
yrng=ma-mi
anx_mid=xrng/2
any_mid=yrng/2
dispx=-(xrng)*0.06
dispy=-(yrng)*0.08
dispt=ma+(yrng)*0.02
xyouts,anx_mid,dispy,fx_tit,alignment=0.5,size=1.5
xyouts,dispx,any_mid,fy_tit,alignment=0.5,orientation=90,size=1.5
xyouts,anx_mid,dispt,f_tit,alignment=0.5,size=1.5
return
END

;===============================================================================
PRO put_vals	,x_dats,y_dats,f_dats,plt_dev,poids,lamp_siz	; ==============
;===============================================================================
;Puts result on plot
;

COMMON gc_wid
COMMON gc_fit_excl
COMMON gc_data
COMMON gc_fit_view

downy=0.08
if lamp_siz gt 800 then downy=0.06
if lamp_siz gt 900 then downy=0.04

if plt_dev eq 0 THEN wset,g_plot_wid
size_of_x=N_ELEMENTS(x_dats)
eps=abs((x_dats(size_of_x-1)-x_dats(0)))/(size_of_x-1) 
device_fac=1.5
;	IF plt_dev eq 1 THEN device_fac=2.
boxx   =fltarr(5)
boxy   =fltarr(5)
min_x  =viewarr(0) 
min_y  =viewarr(2) 
xrng   =viewarr(1)-min_x 
w_out=1
adapte ,x_vu,y_vu,w_out,0
yrng   =MAX(y_vu)-min_y 
dispx  =min_x+(xrng*0.02)
dispy  =min_y+(yrng*0.96)
boxx(0)=min_x+(xrng*0.01)
boxy(0)=min_y+(yrng*0.98)
boxx(4)=boxx(0)
boxy(4)=boxy(0)
boxx(3)=boxx(0)
boxy(1)=boxy(0)
boxx(1)=min_x+(xrng*0.3)
boxx(2)=boxx(1)
boxy(2)=min_y+(yrng*0.85)
boxy(3)=boxy(2)

; Get total of fitted peak without that marked background "b"
sum_fit=0 
FOR j=1,g_npeaks DO BEGIN
    pk_int=0.0
    ind=(j-1)*3
    iback=0
    ; Look FOR "b" on a parameter
    FOR ipmt=0,2 DO BEGIN
	IF STRPOS(g_step(ind+ipmt),'b') ge 0 THEN iback=1
    ENDFOR

    IF iback eq 0 THEN BEGIN
	get_param, g_parameters, j, p1, p2, p3
;
; For Gaussian real width related to sigma by sqrt((1/2^3)*sqrt(2)) = 0.420448
; For Lorentz factor is 0.5
    if g_fct_type(j-1) eq 'Gauss'   then sum_fit=sum_fit+(p2*p3*0.420448*!pi)
    if g_fct_type(j-1) eq 'Lorentz' then sum_fit=sum_fit+(p2*p3*0.5*!pi)
    ENDIF
ENDFOR
get_bckg, g_npeaks, g_parameters, bbb, sss
dataIntegral=TOTAL(y_dats*poids*g_largeur)	; calculate Data integral

fittedbackground=TOTAL((bbb+x_dats*sss)*poids*g_largeur)
dataIntegral=dataIntegral-fittedbackground

partxt='Fit Integral ='+STRTRIM(STRING(sum_fit,format='(g10.2)'),2)
XYOUTS,dispx,dispy,partxt,charsize=g_char
partxt='Data Int.-bg.= '+STRTRIM(STRING(dataIntegral,format='(g10.2)'),2)
dispy=dispy-((yrng*downy)/device_fac)
XYOUTS,dispx,dispy,partxt,charsize=g_char
dispy=dispy-((yrng*downy)/device_fac)
partxt='Fitted Bg.   ='+STRTRIM(STRING(fittedbackground,format='(g10.2)'),2)
XYOUTS,dispx,dispy,partxt,charsize=g_char

IF (sum_fit gt 0.0) AND (dataIntegral GT 0) THEN BEGIN
    FOR j=1,g_npeaks DO BEGIN
	pk_int=0.0
	get_param, g_parameters, j, p1, p2, p3
;
; For Gaussian real width related to sigma by sqrt((1/2^3)*sqrt(2)) = 0.420448
; For Lorentz factor is 0.5
    if g_fct_type(j-1) eq 'Gauss'   then pk_int=p2*p3*0.420448*!pi
    if g_fct_type(j-1) eq 'Lorentz' then pk_int=p2*p3*0.5*!pi
	percy=pk_int*100.0/dataIntegral
;	percy_txt=' ('+STRTRIM(STRING(percy,format='(f5.1)'),2)+'%)'
;Something wrong with data integral here - I removed it gjk.
	percy_txt=' '
	dispy1=dispy-(j*(yrng*downy)/device_fac)
	partxt='Inty'+STRTRIM(STRING(j),2)+'='+STRTRIM(STRING(pk_int),2)
	partxt=partxt+percy_txt
	XYOUTS,dispx,dispy1,partxt,charsize=g_char
   ENDFOR
ENDIF
;boxy(2)=yrng*(0.85-(g_npeaks*0.05))	; ???
;boxy(2)=yrng*(0.84-(g_npeaks*0.06))	; ???
;boxy(3)=boxy(2)
;IF plt_dev eq 0 THEN oplot,boxx,boxy
return
END

;===============================================================================
PRO print_vals	,x_dats,y_dats,f_dats,plt_dev,poids		; ==============
;===============================================================================
;Put other result on print
;

COMMON gc_wid
COMMON gc_fit_excl
COMMON gc_data
COMMON gc_fit_view
COMMON gc_dids

downy=0.05	; value to go down on y axis for youts

device_fac=1.5
w_out=1
adapte ,x_vu,y_vu,w_out,0
min_x  =viewarr(0) 
min_y  =viewarr(2) 
xrng   =viewarr(1)-min_x 
yrng   =MAX(y_vu)-min_y 

; Get total of fitted peak without that marked background "b"
sum_fit=0 



FOR j=1,g_npeaks DO BEGIN
    pk_int=0.0
    ind=(j-1)*3
    iback=0
    ; Look FOR "b" on a parameter
    FOR ipmt=0,2 DO BEGIN
	IF STRPOS(g_step(ind+ipmt),'b') ge 0 THEN iback=1
    ENDFOR
    IF iback eq 0 THEN BEGIN
	get_param, g_parameters, j, p1, p2, p3
;
; For Gaussian real width related to sigma by sqrt((1/2^3)*sqrt(2)) = 0.420448
; For Lorentz factor is 0.5
    if g_fct_type(j-1) eq 'Gauss'   then sum_fit=sum_fit+(p2*p3*0.420448*!pi)
    if g_fct_type(j-1) eq 'Lorentz' then sum_fit=sum_fit+(p2*p3*0.5*!pi)
    ENDIF
ENDFOR



get_bckg, g_npeaks, g_parameters, bbb, sss
ilast=g_npeaks*3-1
dataIntegral=TOTAL(y_dats*poids*g_largeur)	; calculate Data integral
fittedbackground=TOTAL((bbb+x_dats*sss)*poids*g_largeur)
dataIntegral=dataIntegral-fittedbackground

dispx=min_x+xrng+(xrng*0.02)/device_fac
dispy=min_y+yrng+(yrng*0.02)/device_fac
dispy=dispy-((yrng*(downy+0.01))/device_fac)
partxt='Fit Int. = '+STRTRIM(STRING(sum_fit,format='(g10.2)'),2)
XYOUTS,dispx,dispy,partxt,charsize=g_char
partxt='Data Int.-bg.= '+STRTRIM(STRING(dataIntegral,format='(g10.2)'),2)
dispy=dispy-((yrng*downy)/device_fac)
XYOUTS,dispx,dispy,partxt,charsize=g_char
dispy=dispy-((yrng*downy)/device_fac)
partxt='Fitted Bg.    = '+STRTRIM(STRING(fittedbackground,format='(g10.2)'),2)
XYOUTS,dispx,dispy,partxt,charsize=g_char

FOR j=1,g_npeaks DO BEGIN
    pk_int=0.0
    get_param, g_parameters, j, p1, p2, p3
    IF p2 gt 0 AND p3 gt 0 THEN BEGIN
	curpic=j
	CALL_PROCEDURE, g_fct_type(j-1),x_dats,p1,p2,p3,peaky
    ENDIF
;
; For Gaussian real width related to sigma by sqrt((1/2^3)*sqrt(2)) = 0.420448
; For Lorentz factor is 0.5
    if g_fct_type(j-1) eq 'Gauss'   then pk_int=p2*p3*0.420448*!pi
    if g_fct_type(j-1) eq 'Lorentz' then pk_int=p2*p3*0.5*!pi
;
; Error on intensity as average of height and width
    ht_er=(g_error(ind+1)/p2)^2
    w_er=(g_error(ind+2)/p3)^2
    pk_error=pk_int*sqrt(ht_er+w_er)
    p_txt=STRING(p1,format='(g10.3)')
    i_txt=STRING(pk_int,format='(g10.3)')
    w_txt=STRING(p3,format='(g10.3)')
    e_p_txt=STRING(ABS(g_error(ind))  ,format='(g10.3)')
    e_i_txt=STRING(ABS(pk_error),format='(g10.3)')
    e_w_txt=STRING(ABS(g_error(ind+2)),format='(g10.3)')
    str=' --- Error'
    dispy=dispy-((yrng*downy)/device_fac)
    XYOUTS,dispx,dispy,'Peak '+STRTRIM(STRING(j),2)+str,charsize=1.0
    dispy=dispy-((yrng*downy)/device_fac)
    XYOUTS,dispx,dispy,'P'+p_txt+' '+e_p_txt,charsize=g_char
    dispy=dispy-((yrng*downy)/device_fac)
    XYOUTS,dispx,dispy,'I'+i_txt+' '+e_i_txt,charsize=g_char
    dispy=dispy-((yrng*downy)/device_fac)
    XYOUTS,dispx,dispy,'W'+w_txt+' '+e_w_txt,charsize=g_char
    dispy=dispy-((yrng*downy)/device_fac)
    percy=pk_int*100.0/dataIntegral
    percy_txt=STRTRIM(STRING(percy,format='(f5.1)'),2)+'%'
    delta_i=ABS(g_error(ind+1))
    delta_ie=2*SQRT(dataIntegral)
    ind=(j-1)*3
    ; Look FOR "b" on a parameter
    FOR ipmt=0,2 DO IF STRPOS(g_step(ind+ipmt),'b') ge 0 THEN $
	delta_ie=delta_ie+ABS(g_error(ilast+1))+ABS(g_error(ilast+2))
    delta=(delta_i*dataIntegral+p2*delta_ie)/(dataIntegral*dataIntegral)
    delta_txt=STRTRIM(STRING(ABS(delta),format='(g10.3)'),2)
    txt='Pct '+STRTRIM(STRING(j),2)+' '+percy_txt+'      '+delta_txt
    XYOUTS,dispx,dispy,txt,charsize=g_char
ENDFOR
    ; Add flat AND sloping bg
    bg_txt=STRTRIM(STRING(ABS(g_parameters(ilast+1))),2)
    sl_txt=STRTRIM(STRING(ABS(g_parameters(ilast+2))),2)
    e_bg_txt=STRTRIM(STRING(ABS(g_error(ilast+1)),format='(g10.3)'),2)
    e_sl_txt=STRTRIM(STRING(ABS(g_error(ilast+2)),format='(g10.3)'),2)
    dispy=dispy-((yrng*(downy+0.01))/device_fac)
    XYOUTS,dispx,dispy,'background',charsize=1.0
    dispy=dispy-((yrng*downy)/device_fac)
    XYOUTS,dispx,dispy,'flat  '+bg_txt+' '+e_bg_txt,charsize=g_char
    dispy=dispy-((yrng*downy)/device_fac)
    XYOUTS,dispx,dispy,'slope '+sl_txt+' '+e_sl_txt,charsize=g_char
    dispy=dispy-((yrng*(downy+0.01))/device_fac)
    XYOUTS,dispx,dispy,'P: Pos. I: Int. W: hwhm',charsize=g_char
return
END

;===============================================================================
FUNCTION gk_fit	,workspace					; ==============
;===============================================================================
;

@lamp.cbk
COMMON gc_save
COMMON gc_wid
COMMON gc_data

nwt_out=STRTRIM(STRING(nw_out),2)
par_txt(nw_out,*)=''

FOR ind_p=0,g_npeaks-1 DO BEGIN
    peak_num=STRTRIM(STRING(ind_p+1))
    i3=ind_p*3-1
    par_txt(nw_out,i3+1)="Gaussian "+peak_num+" position = "
    par_txt(nw_out,i3+2)="Gaussian "+peak_num+" Height   = "
    par_txt(nw_out,i3+3)="Gaussian "+peak_num+" Width    = "
ENDFOR

    jj=i3+6
    par_txt(nw_out,jj)  ="Spectrum number                = "

par_txt(nw_out,i3+4)="Flat background                = "
par_txt(nw_out,i3+5)="Sloping background             = "
read_par_vals
iii=execute("p"+nwt_out+"=g_parameters")
WIDGET_CONTROL, bad_id=i, gw_cut_slider, Get_Value=no_coupe
iii=execute("p"+nwt_out+"(jj)=no_coupe")

if strpos(x_tit(nw_out),'(Fitted)') lt 0 then x_tit(nw_out)=x_tit(nw_out)+' (Fitted)'

RETURN, w_out
END

;===============================================================================
PRO read_par_vals						; ==============
;===============================================================================
; Get parameters value from fields
;

COMMON gc_wid
COMMON gc_data
COMMON gc_cp
COMMON gc_flags

flag=" "
idx=(g_pk_nb-1)*3	;.......Read visible parameters for peak

FOR j=0,2 DO BEGIN
IF g_ctrl_panel THEN BEGIN
    CASE j OF
	0 : BEGIN & wid1=cp_pos(g_pk_nb-1) & END
	1 : BEGIN & wid1=cp_hgt(g_pk_nb-1) & END
	2 : BEGIN & wid1=cp_wid(g_pk_nb-1) & END
    ENDCASE
    ENDIF ELSE BEGIN
    CASE j OF
	0 : BEGIN & wid1=gw_pos_text & wid2=gw_pos_pop & END
	1 : BEGIN & wid1=gw_int_text & wid2=gw_int_pop & END
	2 : BEGIN & wid1=gw_wdt_text & wid2=gw_wdt_pop & END
    ENDCASE
    ENDELSE
    WIDGET_CONTROL, bad_id=i, wid1, Get_Value=parval
    g_parameters(idx+j)=float(parval(0))

 IF NOT(g_ctrl_panel) THEN BEGIN
    flag=''
    WIDGET_CONTROL, bad_id=i, wid2(1), Get_Value=str
    IF STRPOS(str, 'Un') GE 0 THEN flag=flag+'f'

    IF g_bg_but EQ 1 THEN flag=flag+'b'

    WIDGET_CONTROL, bad_id=i, wid2(3), Get_Value=str
    IF STRLEN(str) GT 15 THEN flag=flag+STRMID(str, 16, 1)
    g_step(idx+j)=flag
 ENDIF
ENDFOR

ilast=g_npeaks*3-1
WIDGET_CONTROL, bad_id=i, gw_fbg_text, Get_Value=parval
g_parameters(ilast+1)=float(parval(0))
flag=''
WIDGET_CONTROL, bad_id=i, gw_fbg_pop(0), Get_Value=str
IF STRPOS(str, 'fx') GE 0 THEN flag=flag+'f'
g_step(ilast+1)=flag

WIDGET_CONTROL, bad_id=i, gw_sbg_text, Get_Value=parval
g_parameters(ilast+2)= float(parval(0))

flag=''
WIDGET_CONTROL, bad_id=i, gw_sbg_pop(0), Get_Value=str
IF STRPOS(str, 'fx') GE 0 THEN flag=flag+'f'
g_step(ilast+2)=flag
END

;===============================================================================
PRO write_par_vals_2, wid, g_step, idx, j		; ======================
;===============================================================================
; Update interface parameters
; wid

str=''
fixed=0

WIDGET_CONTROL, bad_id=i, wid(1), Get_Value=str
IF STRPOS(g_step(idx+j), 'f') GE 0 THEN BEGIN
    fixed=1
    IF STRPOS(str, 'Un') LT 0 THEN str='Un'+str
ENDIF ELSE IF STRPOS(str, 'Un') GE 0 THEN str=STRMID(str, 2, STRLEN(str))
WIDGET_CONTROL, bad_id=i, wid(1), Set_Value=str

; ??? anciennement fix it as a parameter
;WIDGET_CONTROL, bad_id=i, wid(2), Get_Value=str
;IF STRPOS(g_step(idx+j), 'b') GE 0 THEN BEGIN
;    fixed=1
;    IF STRPOS(str, 'Un') LT 0 THEN str='Un'+str
;ENDIF ELSE IF STRPOS(str, 'Un') GE 0 THEN str=STRMID(str, 2, STRLEN(str))
;WIDGET_CONTROL, bad_id=i, wid(2), Set_Value=str

FOR k=0, strlen(g_step(idx+j)) do BEGIN
    IF (STRMID(g_step(idx+j),k, 1) NE 'f') AND (STRMID(g_step(idx+j),k, 1) NE 'b') THEN BEGIN
	IF  FIX(STRMID(g_step(idx+j),k, 1)) GE 48 AND $
	    FIX(STRMID(g_step(idx+j),k, 1)) LE 57 THEN BEGIN
	        fixed=1
		WIDGET_CONTROL, bad_id=i, wid(2), Set_Value=STRMID(g_step(idx+j),k, 1)
	ENDIF
    ENDIF
ENDFOR

WIDGET_CONTROL, bad_id=i, wid(0), Get_Value=str
IF fixed eq 1 THEN BEGIN
    if STRPOS(str, ' fx ') LT 0 then str=STRMID(str, 0,  STRPOS(str, '    '))+' fx '
ENDIF ELSE BEGIN
    p=STRPOS(str, ' fx ')
    if p GE 0 then str=STRMID(str, 0,  p)+'    '
ENDELSE
WIDGET_CONTROL, bad_id=i, wid(0), Set_Value=str
RETURN
END

;===============================================================================
PRO set_gfit_param, w, p, Fct_Type=ft				; ==============
;===============================================================================
;
COMMON gc_flags
COMMON gc_data
COMMON gc_wid
COMMON gc_save

g_resid		=   1e12	    ; residu is big
WIDGET_CONTROL, bad_id=i, gw_get_but, Set_UValue=[1, 2, w]
gfit_Event,{WIDGET_BUTTON, ID:gw_get_but, TOP:gw_gkfit_base, $
	    HANDLER:0L, SELECT:1}
WIDGET_CONTROL, bad_id=i, gw_get_but, Set_UValue=[1, 2, 0]

g_npeaks=N_ELEMENTS(p)/3
g_ncycles=1
FOR i=0, N_ELEMENTS(p)-1 DO g_parameters(i)=p(i)
IF KEYWORD_SET(ft)    THEN $
    FOR i=0, N_ELEMENTS(ft)-1 DO g_fct_type(i)=ft(i)

write_par_vals
read_par_vals
plot_subf, g_npeaks, g_parameters, x_ni, g_fct_type

RETURN
END

;===============================================================================
PRO write_par_vals, No_Update=no_update			; ======================
;===============================================================================
; Display errors values in widget labels

COMMON gc_wid
COMMON gc_data
COMMON gc_cp
COMMON gc_flags

;........Writes parameters to visible window
nvarf =0	; nb de variables fixees avant le pic en question
idx=(g_pk_nb-1)*3
;
fixed=0
FOR j=0,2 DO BEGIN
    parval=STRTRIM(STRING(g_parameters(idx+j)),2)
    IF g_ctrl_panel THEN BEGIN
    CASE j OF
	0 : BEGIN & wid1=cp_pos(g_pk_nb-1) & END
	1 : BEGIN & wid1=cp_hgt(g_pk_nb-1) & END
	2 : BEGIN & wid1=cp_wid(g_pk_nb-1) & END
    ENDCASE
    ENDIF ELSE BEGIN
    CASE j OF
	0 : BEGIN & wid1=gw_pos_text & wid2=gw_pos_pop & END
	1 : BEGIN & wid1=gw_int_text & wid2=gw_int_pop & END
	2 : BEGIN & wid1=gw_wdt_text & wid2=gw_wdt_pop & END
    ENDCASE
    ENDELSE
    WIDGET_CONTROL, bad_id=i, wid1, Set_Value=parval
    IF NOT(g_ctrl_panel) THEN if not(KEYWORD_SET(No_Update)) THEN write_par_vals_2,wid2, g_step, idx, j
IF (STRPOS(g_step(idx+j), 'b') GE 0) THEN fixed=1
ENDFOR
IF fixed EQ 1 THEN g_bg_but=1 else g_bg_but=0
WIDGET_CONTROL, bad_id=i, gw_bgfixed, Set_Button=g_bg_but
IF g_ctrl_panel THEN WIDGET_CONTROL, bad_id=i, cp_bck(g_pk_nb-1), Set_Button=g_bg_but
ilast=g_npeaks*3-1

parval=STRTRIM(STRING(g_parameters(ilast+1)),2)

WIDGET_CONTROL, bad_id=i, gw_fbg_text, Set_Value=parval
IF g_ctrl_panel THEN WIDGET_CONTROL, bad_id=i, cp_sb, Set_Value=parval
IF not(KEYWORD_SET(No_Update)) THEN write_par_vals_2,gw_fbg_pop, g_step, ilast, 1

parval=STRTRIM(STRING(g_parameters(ilast+2)),2)
WIDGET_CONTROL, bad_id=i, gw_sbg_text, Set_Value=parval
IF g_ctrl_panel THEN WIDGET_CONTROL, bad_id=i, cp_fb, Set_Value=parval

IF NOT(g_ctrl_panel) THEN if not(KEYWORD_SET(No_Update)) THEN write_par_vals_2,gw_sbg_pop, g_step, ilast, 1

; Errors Display:
;****************
; ???
;index=where(g_error ne 0,combien)
;IF combien ne 0 THEN g_error=g_error(where(g_error ne 0))
    FOR i=0,3*(g_pk_nb-1)-1 DO BEGIN 
	IF STRPOS(g_step(i),'f') ge 0 THEN nvarf=nvarf+1
    ENDFOR    
    inderr=3*(g_pk_nb-1)-nvarf

    IF STRPOS(g_step(idx),'f') ge 0 THEN err='* FIXED'$
    ELSE BEGIN
	err=STRTRIM(STRING(g_error(inderr)),2)
	inderr=inderr + 1
    ENDELSE  
    WIDGET_CONTROL, bad_id=i, gw_pos_pop(3), Set_Value='Err: '+err

    IF STRPOS(g_step(idx+1),'f') ge 0 THEN err='* FIXED'$
    ELSE BEGIN
	err=STRTRIM(STRING(g_error(inderr)),2)
	inderr=inderr + 1
    ENDELSE  
    WIDGET_CONTROL, bad_id=i, gw_int_pop(3), Set_Value='Err: '+err
    
    IF STRPOS(g_step(idx+2),'f') ge 0 THEN err='* FIXED'$
    ELSE BEGIN
	err=STRTRIM(STRING(g_error(inderr)),2)
	inderr=inderr + 1
    ENDELSE
    WIDGET_CONTROL, bad_id=i, gw_wdt_pop(3), Set_Value='Err: '+err
 
    IF STRPOS(g_step(g_nb_pk_max*3+1),'f') ge 0 THEN BEGIN
	err='* FIXED' 
	ajust=0
    ENDIF ELSE BEGIN
	err=STRTRIM(STRING(g_error(N_ELEMENTS(g_error)-1)),2)
	ajust=1
    ENDELSE
    WIDGET_CONTROL, bad_id=i, gw_sbg_pop(3), Set_Value='Err: '+err
  
    IF STRPOS(g_step(g_nb_pk_max*3),'f') ge 0 THEN err='* FIXED'$
    ELSE err=STRTRIM(STRING(g_error(N_ELEMENTS(g_error)-1-ajust)),2) 
    WIDGET_CONTROL, bad_id=i, gw_fbg_pop(3), Set_Value='Err: '+err
END

;===============================================================================
PRO error_msg, error_number, str1				; ==============
;===============================================================================
; Display error message identified by error_number
;

COMMON gc_wid

CASE error_number OF
    -2: str='Use left mouse-button to define peak #'+STRTRIM(STRING(str1), 2)
    -1: str=str1
    0 : str=''
    1 : str='Invalid view zone.     *** ignored ***'
    2 : str='3D Workspace. Reduced to 2D by summing !'
    3 : str='Woops ...no data !'
    4 : BEGIN
	    print,STRING(7b)
	    str='No better fit found ** Click on peak-slider (above) to reset **'
	END
    5 : str='You can'+STRING(39b)+'t do that. Please fill in a value'
    6 : str='WARNING : defined zone is smaller than fitting zone'
    7 : str='Bum fit : negative height or width !'
    8 : str='Not enough data points in genfit function !'
    9 : BEGIN
    	    print,STRING(7b)
    	    str='Sorry, Not yet implemented !'
	END
    10 : BEGIN
    	    print,STRING(7b)
    	    str='Just a display of position error'
	END
    ELSE:str='!error '+error_number
ENDCASE
WIDGET_CONTROL, bad_id=i, gw_err_lab, Set_Value=str
END

;===============================================================================
PRO traitview_event	,event					; ==============
;===============================================================================
;

COMMON gc_fit_view	& COMMON gc_data	& COMMON gc_fit_excl

WIDGET_CONTROL, bad_id=i, Event.Id, GET_UVALUE=Ev
CASE Ev(0) OF
    'load' : BEGIN ; charge les valeurs des zones texte dans viewarr et
		   ; initialise la zone courante avec ces valeurs
		WIDGET_CONTROL, bad_id=i, x_min, Get_Value=xmin
		WIDGET_CONTROL, bad_id=i, x_max, Get_Value=xmax
		WIDGET_CONTROL, bad_id=i, y_min, Get_Value=ymin
		WIDGET_CONTROL, bad_id=i, y_max, Get_Value=ymax
		viewarr(0+sauv+4*no_zone)=xmin & viewarr(0)=xmin
		viewarr(1+sauv+4*no_zone)=xmax & viewarr(1)=xmax
		viewarr(2+sauv+4*no_zone)=ymin & viewarr(2)=ymin
		viewarr(3+sauv+4*no_zone)=ymax & viewarr(3)=ymax
	     END

    'recall' :	BEGIN ; initialise la zone courante avec les valeurs
		;  de la zone specifiee
		viewarr(0)=viewarr(0+sauv+4*no_zone)
		viewarr(1)=viewarr(1+sauv+4*no_zone)
		viewarr(2)=viewarr(2+sauv+4*no_zone)
		viewarr(3)=viewarr(3+sauv+4*no_zone)
		WIDGET_CONTROL, bad_id=i, x_min, Set_Value=STRTRIM(STRING(viewarr(0)),2)
		WIDGET_CONTROL, bad_id=i, x_max, Set_Value=STRTRIM(STRING(viewarr(1)),2)
		WIDGET_CONTROL, bad_id=i, y_min, Set_Value=STRTRIM(STRING(viewarr(2)),2)
		WIDGET_CONTROL, bad_id=i, y_max, Set_Value=STRTRIM(STRING(viewarr(3)),2)
		END

    'memzone' : BEGIN
		no_zone=Ev(1)
		END

    'done' :	BEGIN  ; on avertit l'utilisateur s'il existe des points de
		 ; la zone a fitter qui ne sont pas dans la zone a tracer :
		gk_warning,excl_param,viewarr
		WIDGET_CONTROL, bad_id=i, event.top, Map=0
		END

    'reset' :	BEGIN
		viewarr(0)=viewarr(0+sauv)
		viewarr(1)=viewarr(1+sauv)
		viewarr(2)=viewarr(2+sauv)
		viewarr(3)=viewarr(3+sauv)
		WIDGET_CONTROL, bad_id=i, x_min, Set_Value=STRTRIM(STRING(viewarr(0)),2)
		WIDGET_CONTROL, bad_id=i, x_max, Set_Value=STRTRIM(STRING(viewarr(1)),2)
		WIDGET_CONTROL, bad_id=i, y_min, Set_Value=STRTRIM(STRING(viewarr(2)),2)
		WIDGET_CONTROL, bad_id=i, y_max, Set_Value=STRTRIM(STRING(viewarr(3)),2)
		END

    ELSE : BEGIN
	   ; Do nothing !
	   END
ENDCASE
END

;===============================================================================
PRO traitexcl_event	,event					; ==============
;===============================================================================
;

@lamp.cbk

COMMON gc_save
COMMON gc_fit_excl
COMMON gc_data
COMMON gc_flags
COMMON gc_wid

g_resid=1.0e12
WIDGET_CONTROL, bad_id=i, Event.Id, GET_UVALUE=Ev
CASE Ev(0) OF 
    'coucou':	BEGIN
		    intv_no=float(Ev(1))
		END

    'done' :  	BEGIN
		    WIDGET_CONTROL, bad_id=i,event.top,Map=0
		END

    'reset':	BEGIN
		    ;initialisation par defaut de excl_param :
		    excl_param(0)=MIN(x_ni,MAX=ma)
		    excl_param(1)=ma
		    WIDGET_CONTROL, bad_id=i,excl_widge(0),$
			Set_Value=STRTRIM(STRING(excl_param(0)),2)
		    WIDGET_CONTROL, bad_id=i,excl_widge(1),$
			Set_Value=STRTRIM(STRING(excl_param(1)),2)
		    FOR i=1,max_nb_int-1 do BEGIN
			excl_param(2*i)  =excl_param(0)-1
			excl_param(2*i+1)=excl_param(0)-1
			WIDGET_CONTROL, bad_id=iii,excl_widge(2*i)  ,Set_Value='***'
			WIDGET_CONTROL, bad_id=iii,excl_widge(2*i+1),Set_Value='***'
		    ENDFOR
		    ; selection du bouton no 1 :
		    WIDGET_CONTROL, bad_id=i,gw_excl_but,set_button=1
		    intv_no=1
		    adapte ,x_to_plot,w_to_plot,w_out,1
		END
ENDCASE
END

;===============================================================================
pro creer_excl,		id					; ==============
;===============================================================================
; Create widget window to define determinant fitting zone

@lamp.cbk
COMMON gc_fit_excl
COMMON gc_wid

if XREGISTERED('gfit') le 0 then RETURN
if gw_gkfit_base       eq 0 then RETURN
;--------------------------------- gw_excl_base --------------------------------
gw_excl_base= WIDGET_BASE (Group_Leader=gw_gkfit_base ,Resource_Name='lamp', $
				/Column, Title= 'Fit zone', Map=0) 

conseil_lab=WIDGET_LABEL( gw_excl_base ,Font=ft_b_bigger,$
				Value='To choose your fit zone:')
conseil_lab=WIDGET_LABEL( gw_excl_base, Font=ft_b_bigger,$
				Value='Drag the right mouse-button')

; ..... all_but_base :
all_but_bse=WIDGET_BASE  (gw_excl_base, /Row)
reset_but  =WIDGET_BUTTON(all_but_bse, Font=ft_b_bigger ,$
				UValue=['reset'], Value='Reset')
empty_lab  =WIDGET_LABEL (all_but_bse, Font=ft_propor, Value='     ')
int_but_bse=WIDGET_BASE  (all_but_bse ,/Row ,/Exclusive)

FOR i=1,max_nb_int DO BEGIN
    coucou =WIDGET_BUTTON(int_but_bse ,font=ft_b_bigger, $
			    Value=STRTRIM(STRING(i),2)+'     ',$
			    UValue=['coucou',STRTRIM(STRING(i),2)])
    IF i EQ 1 THEN gw_excl_but=coucou
ENDFOR
intv_no=1
WIDGET_CONTROL, bad_id=i, coucou, /Set_Button 


ok_but	      =WIDGET_BUTTON(all_but_bse,Font=ft_b_bigger,$
				UValue=['done'],Value='Done')
; ..... END of all_but_base

int_defin  =WIDGET_BASE (gw_excl_base ,/Column)
lab_min_max=WIDGET_BASE (int_defin ,/Row)
intv_nb	   =WIDGET_LABEL(lab_min_max, Font=ft_propor, Value='          ')

FOR i=1,max_nb_int DO BEGIN
    lab_min_max=WIDGET_BASE(int_defin,/Row)
    intv_nb=WIDGET_LABEL(lab_min_max, Font=ft_propor, Value='interval #'+$
			   STRTRIM(STRING(i),2)+':   xmin=')
    xmin =WIDGET_LABEL(lab_min_max,Font=ft_propor,UValue=['xmin',STRTRIM(STRING(i),2)])
    excl_widge(2*(i-1))=xmin	; to be able to write in this label
    WIDGET_CONTROL, bad_id=iii,xmin,Set_Value=STRTRIM(STRING(excl_param(2*i-2)),2)
    xmax_lab=WIDGET_LABEL(lab_min_max ,Font=ft_propor,Value='     xmax=')
    xmax  =WIDGET_LABEL ( lab_min_max ,Font=ft_propor,UValue =['xmax',STRTRIM(STRING(i),2)])
    excl_widge(2*i-1)=xmax
    WIDGET_CONTROL, bad_id=iii, xmax, Set_Value=STRTRIM(STRING(excl_param(2*i-1)),2)
ENDFOR

bid=sys_dep('DYNLAB', gw_excl_base, 1)
WIDGET_CONTROL, bad_id=i, gw_excl_base, /Realize
XMANAGER, 'traitexcl', gw_excl_base, /Just_Reg, Cleanup='creer_excl'
END

;-------------------------------- END of gw_excl_base --------------------------

;===============================================================================
PRO creer_view,		id					; ============== 
;===============================================================================
;

@lamp.cbk
COMMON gc_fit_view	& COMMON gc_data	& COMMON gc_wid

if XREGISTERED('gfit') le 0 then RETURN
if gw_gkfit_base       eq 0 then RETURN
;----------------------------- gw_view_base --------------------------------------
gw_view_base=WIDGET_BASE(Group_Leader=gw_gkfit_base ,Resource_Name='lampdon' ,$
			 /Column, Title='View', Map=0)
invit_lab=WIDGET_LABEL	(gw_view_base , Font=ft_b_bigger ,$
			 Value='Select plot range :')
qwer_BASE=WIDGET_BASE 	( gw_view_base ,/row ) 
load_but =WIDGET_BUTTON	(qwer_base ,UValue=['load'] ,$
				Font=ft_b_bigger, Value=' Load ')
zone_base=WIDGET_BASE	(qwer_BASE ,/Row ,/Exclusive )

nbzone=3			    ; as defined in gfit
FOR i=1,nbzone DO BEGIN
    m=WIDGET_BUTTON(zone_base ,UValue=['memzone',STRTRIM(STRING(i),2)] ,$
				Font=ft_b_bigger, Value=STRTRIM(STRING(i),2))
    IF (i eq 1) THEN WIDGET_CONTROL, bad_id=iii, m, /Set_Button
ENDFOR

recall_but =WIDGET_BUTTON(qwer_base, UValue=['recall'] ,$
				Font=ft_b_bigger, value='Recall')
x_base	   =WIDGET_BASE  ( gw_view_base, /Row )
x_lab	   =WIDGET_LABEL ( x_base, Font=ft_b_bigger, Value='Xrange  :')
x_min	   =WIDGET_TEXT  ( x_base, /Editable, Font=ft_propor, $
				UValue=['x_min'] , Xsize=9 , Value='') 
x_max	   =WIDGET_TEXT  ( x_base, /Editable, Font=ft_propor, $
				UValue=['x_max'] , XSize=9, Value='') 
y_base	   =WIDGET_BASE  ( gw_view_base ,/Row )
y_lab	   =WIDGET_LABEL ( y_base, Font=ft_propor, Value='Yrange  :')
y_min	   =WIDGET_TEXT  ( y_base, /Editable ,Font=ft_propor, $
				UValue=['y_min'], Xsize=9, Value='')
y_max	   =WIDGET_TEXT  ( y_base, /Editable,Font=ft_propor, $
				UValue=['y_max'], Xsize=9, Value='')
  
  
small_base =WIDGET_BASE  ( gw_view_base ,/Row )

reset_but  =WIDGET_BUTTON(small_base, UValue=['reset'],$
				Font=ft_b_bigger, Value=' Reset ')
vide	   =WIDGET_LABEL (small_base , Font=ft_propor, Value='                           ')
done_but   =WIDGET_BUTTON(small_base , UValue=['done'] ,$
				Font=ft_b_bigger, Value=' Done ')
bid=sys_dep('DYNLAB', gw_view_base, 1)
WIDGET_CONTROL, bad_id=i, gw_view_base, /Realize
XMANAGER , 'traitview' , gw_view_base ,/Just_Reg, Cleanup='creer_view'
;------------------------------ END of gw_view_base ----------------------------
END

;===============================================================================
PRO gfit, Just=just, Group=group, Tripx=tripx, tx_param		; ==============
;===============================================================================
;

@lamp.cbk
COMMON gc_save
COMMON gc_fit_view
COMMON gc_wid
COMMON gc_data
COMMON gc_flags
COMMON gc_fit_excl
COMMON gc_cp
COMMON gc_width_factor
COMMON gc_dids

;Factors for using measured widths for peaks
;
gw_fac=2.0*sqrt(2.)
lw_fac=0.5

IF N_ELEMENTS(tx_param) EQ 0 THEN g_tx_par=[''] ELSE g_tx_par=tx_param
IF KEYWORD_SET(tripx) THEN g_tripx=1 ELSE g_tripx=0
IF KEYWORD_SET(just) THEN g_ctrl_panel=1 ELSE g_ctrl_panel=0
IF (!D.flags and 65536) eq 0  THEN print,'set_plot,"X" before using rdfilter' else  $
IF (XREGISTERED('gfit') le 0) THEN BEGIN
    cp_nb	=0
    cp_bck	=0 	
    cp_pos	=0  
    cp_hgt	=0
    cp_wid	=0 
    cp_pf	=0
    cp_hf	=0 
    cp_wf	=0
    cp_fb	=0
    cp_sb	=0
    cp_fbf	=0 
    cp_sbf	=0 
    
  g_old_plot_wid=!D.window
  P_MUS,'mus_harp'				; PLAY a tune
  g_fct_name	='Gauss' ; Default is Gauss
  g_old_pk_nb	=1	 ; old peak number
  g_afitisdone	=0	 ; No fit was done
  g_pk_nb	=1	 ; number of peak processed
  g_char	=0.8	 ;
  g_print_nb	=0	 ;
  g_show_subfct	=1	 ; if this flag is set, subfunctions are plotted
  bb1		=0	 ;l'une des bornes d'un intervalle a exclure (sert a la saisie)
  bb2		=0	 ;idem bb1 
  g_nb_pk_max	=6	 ;<----change peaks maximum number 
  max_nb_int	=3	 ;<----change excluded zone maximum number
  sauv		=4	 ;deplacement (dans viewarr) donnant la zone a afficher initiale
  nbzone	=3	 ;stored zones number
  no_zone	=1	 ;initializing zone number
  g_bg_but	=0	 ;fixed as background button is unset by default
  viewarr	=FLTARR(8+nbzone*4)	 ;dim zone to view array
  excl_param	=FLTARR(2*max_nb_int)	 ;bornes des intervalles exclus
  excl_widge	=LONARR(2*max_nb_int)	 ;widget IDs des fenetres d'affichage des bornes 
  g_parameters	=FLTARR(3*g_nb_pk_max+3) ;parametres position,height,width + flat,slope
  g_step	=STRARR(3*g_nb_pk_max+2) ;extensions des champs pos,height,width,flat,slope
  g_error	=FLTARR(3*g_nb_pk_max+2) ;erreurs sur les parametres correspondants de g_parameters
  g_fct_type	=STRARR(g_nb_pk_max)
  gw_pos_pop	=LONARR(10)		 ;widget ID of pos parameter menu
  gw_int_pop	=LONARR(10)		 ;widget ID of int parameter menu
  gw_wdt_pop	=LONARR(10)		 ;widget ID of wdt parameter menu
  gw_fbg_pop	=LONARR(10)		 ;widget ID of fbg parameter menu
  gw_sbg_pop	=LONARR(10)		 ;widget ID of sbg parameter menu
  sw_ni		=[0]
  g_npeaks	=1
  g_ncycles	=2
  peaky		=0.0
  g_resid	=1.0e12
  bose		=LONARR(g_nb_pk_max)
  mimirange	=[0,0]
  mimion	=0
  ximion	=0

  g_fct_type(*)	=g_fct_name
  nw_ni=0 & nw_out=1
  IF g_tripx THEN nw_ni=23 ELSE BEGIN
      if lamp_b1 ne 0 then p_did_getw_cur, nw_ni, wstr	; get current w number
      if nw_ni EQ 0 THEN nw_ni=1	; if not defined THEN w=1
      if nw_ni GT 20 THEN nw_ni=1
      nw_out	=nw_ni+1
  ENDELSE
  IF nw_out GT 20 THEN nw_out=20

IF N_ELEMENTS(Group) EQ 0 THEN GROUP=lamp_b1
junk		= { CW_PDMENU_S, flags:0, name:'' }

; the whole lamp size depending modifications is here
sl_size =16
if lamp_siz gt 900 then BEGIN
    xsiz=800 & ysiz=440
ENDIF ELSE BEGIN
    xsiz=700 & ysiz=280
ENDELSE
if lamp_siz lt 800 then BEGIN	; adapt to lamp size
    xsiz=550 & ysiz=250		; adapt plotting window size
    sl_size=15
ENDIF
txt_hlp=  '(LEFT press=position  release=width)....(RIGHT= Define Fitting Area)'

gw_gkfit_base=WIDGET_BASE(GROUP_LEADER=Group, /Row, $
				Title='Lamp GKfit Version 8th Aug 02 (gjk)',resource_name='lamp')
MAIN_BIS     =WIDGET_BASE  (gw_gkfit_base, /Column, Title='main_col_base')
; -------------------------  GET_WRITE_BASE  ------------------------------------
GET_WRITE_BASE=WIDGET_BASE  (MAIN_BIS, /Row, Frame=2,resource_name='don')
gw_get_but  =WIDGET_BUTTON(GET_WRITE_BASE, Font=ft_b_bigger,$
				UValue=[1,2,0], Value='Get Ws #')
ws_in_b	    =WIDGET_BASE  (GET_WRITE_BASE ,/column)
IF g_tripx THEN i=23 ELSE i=20
ws_in_sld   =WIDGET_SLIDER(ws_in_b, XSIZE=xsiz*.36,$
				Maximum=i,Minimum=1,Font=ft_b_bigger,$
				UValue=[1,1,0], Value=nw_ni)
ws_in_r	    =WIDGET_BASE  (ws_in_b,/row)
gw_cut_slider= WIDGET_SLIDER(ws_in_r, /Drag, Minimum=1, XSize=100,$
				UValue=[1,3,0], /Suppress)
WIDGET_CONTROL, bad_id=i, gw_cut_slider, Sensitive=0
WIDGET_CONTROL, bad_id=i, ws_in_r      , Map=0
gw_cut_label=WIDGET_LABEL (ws_in_r,Font=ft_b_normal,Value=' ',xsize=100)
ws_in_b	    =WIDGET_BASE  (GET_WRITE_BASE ,/column)
ws_out_sld  =WIDGET_SLIDER(ws_in_b, XSize=xsiz*.36, Maximum=20, Minimum=1,$
				Title='Out Workspace #', Font=ft_b_bigger,$
				UValue=[1,4,0], Value=nw_out)
ws_write_but=WIDGET_BUTTON(GET_WRITE_BASE ,Font=ft_b_bigger ,$
				UVALUE=[1,5,0] ,VALUE='Write to Ws #')
;---------------------- END of GET_WRITE_BASE  -------------------------
; Threerows : base of the 3 rows under the get and write buttons.
THREEROWS=WIDGET_BASE     (MAIN_BIS, /Column, Title='row_base')
; ----------- END of ROW1_BASE ------ BEGIN of ROW2_BASE ---------------
IF g_ctrl_panel THEN gfit_ctrl_panel ELSE BEGIN
    ROW2_BASE=WIDGET_BASE     (THREEROWS ,/Row)
    set_show_base= WIDGET_BASE  (ROW2_BASE, /Column, /frame,resource_name='don')
    
;   peak_menu2=WIDGET_BASE     (set_show_base , Column=g_nb_pk_max)
    peak_men2 =WIDGET_BASE     (set_show_base ,/row)
    gw_type_but=LONARR(g_nb_pk_max)  
    gw_gau_but=LONARR(g_nb_pk_max)
    gw_lor_but=LONARR(g_nb_pk_max)
    gw_peak_but=LONARR(g_nb_pk_max*2)
    
    FOR i=1,g_nb_pk_max DO BEGIN
	peak_menu2      =WIDGET_BASE   (peak_men2 , /Column) & gw_peak_but(g_nb_pk_max+i-1)=peak_menu2
	gw_type_but(i-1)=WIDGET_BUTTON (peak_menu2, /Menu, Font=ft_propor,$
				    UValue=[4,i,1], Value='Gau')
	gw_gau_but(i-1)	=WIDGET_BUTTON(gw_type_but(i-1), UValue=[4,i,1],Value='Gauss'  , Font=ft_propor)
	gw_lor_but(i-1)	=WIDGET_BUTTON(gw_type_but(i-1), UValue=[4,i,2],Value='Lorentz', Font=ft_propor)
	peak_base	=WIDGET_BASE(peak_menu2, /Exclusive)
	gw_peak_but(i-1)=WIDGET_BUTTON(peak_base,/No_Release , Value=STRTRIM(STRING(i),2),$
				       UValue=[2,2,0], Font=ft_b_bigger) 
	; only first button remains sensitive as g_npeaks=1
	floo=floor(1/i)
	WIDGET_CONTROL, bad_id=iii, gw_peak_but(i-1), Sensitive=floo
	WIDGET_CONTROL, bad_id=iii, gw_type_but(i-1), Sensitive=floo
	WIDGET_CONTROL, bad_id=iii, gw_peak_but(g_nb_pk_max+i-1), map=floo
    ENDFOR
    
    WIDGET_CONTROL, bad_id=iii, gw_peak_but(0), Set_Button=1    ; First button is set (Default)
    
    ;.... END of set/show peak
    szsl=0 & str=''
    if sys_dep('VERSION') ge 4.0 then ii=execute('str=WIDGET_INFO(peak_menu2,/geometry) & szsl=str.xsize')
    if szsl le 0 then $
    gw_pk_slider=WIDGET_SLIDER(set_show_base, Maximum=g_nb_pk_max,      $
				    Minimum=1,/Suppress_Value, YSize=15,$
				    UVALUE=[2,1,0],Value=g_npeaks) else $
    gw_pk_slider=WIDGET_SLIDER(set_show_base, Maximum=g_nb_pk_max,      $
				    Minimum=1,/Suppress_Value, YSize=15,$
				    UVALUE=[2,1,0],Value=g_npeaks,XSize=szsl>150)
    
    peak_lab    =WIDGET_LABEL (set_show_base, Font= ft_b_bigger,$
				    VALUE='The slider sets the # of functions')
    
    ;.... fit , cycle # , peak # , spectrum #.....
    
    base	   =WIDGET_BASE   (ROW2_BASE ,/Column, Map=1,/Frame,resource_name='don')
    bloc_base  =WIDGET_BASE   (base , /row)		;.... gw_resid_lab .... 
    resid_lab  =WIDGET_LABEL  (bloc_base,Value ='Residual:', Font=ft_normal)
    gw_resid_lab= WIDGET_LABEL  (bloc_base,Value ='______________',UValue='resid_field',Font=ft_b_normal)
    gw_cycle_lab= WIDGET_LABEL  (base, Font=ft_b_normal, Value='  '+$
				     STRTRIM(STRING(g_ncycles),2)+' Cycles')
    
    cycles_sld =WIDGET_SLIDER (base, Maximum=75, Minimum=0, /Drag,$
				    Value=g_ncycles, xsize=xsiz/4.8, $
				    YSize=sl_size, /Suppress_Value, UValue=[2,3,0])
    
    fit_but	   =WIDGET_BUTTON (base ,Font=ft_biggest, $
				    UValue=[2,4,0], Value ='FIT IT')
    
    base3	   =WIDGET_BASE   (ROW2_BASE, /Column, /Frame,resource_name='don')
    base3_row1 =WIDGET_BASE	  (base3, /Row)
;   view_but= WIDGET_BUTTON   (base3_row1, Font=ft_biggest,$
;				    UValue=[2,6,0], Value=' View  ')
;   WIDGET_CONTROL, bad_id=iii, view_but, Sensitive=0
    excl_but =WIDGET_BUTTON   (base3_row1, Font=ft_biggest ,$
				    UValue=[2,5,0], Value='Fit zone')
    
    base3r   =WIDGET_BASE     (base3 ,/Row )
    show_bse =WIDGET_BASE     (base3r,/NonExclusive, /Row )
    show_but =WIDGET_BUTTON   (show_bse, Font=ft_b_normal, UValue=[2, 8,0],$
				    Value='Show subfuncts')
    WIDGET_CONTROL, bad_id=iii,show_but, /Set_Button 
    gw_bose  =WIDGET_BUTTON   (show_bse, Font=ft_b_normal, UValue=[2,10,0],$
				    Value='T(K):')
    gw_temp  =WIDGET_TEXT     (base3r, Font=ft_propor,xsize=4,ysize=1,/editable)
    
    base3_row2 =WIDGET_BASE	  (base3, /Row)
    print_but=WIDGET_BUTTON   (base3_row2,Font=ft_biggest,$
				    UValue=[3,1,0],Value=' Print ')
    help_but =WIDGET_BUTTON   (base3_row2,Font=ft_biggest,$
				    UValue=[3,2,0],Value=' Help ')
    gw_exit_but=WIDGET_BUTTON (base3_row2,Font=ft_biggest,$
				    UValue=[3,3,0], Value=' DONE ')
ENDELSE    
;------------------------------- BEGIN of ROW3_BASE -------------------
err_base     =WIDGET_BASE   (MAIN_BIS, /Frame,resource_name='don')
blog	     =WIDGET_BASE   (err_base,/row)
	      put_logo	    ,blog
gw_err_lab   =WIDGET_LABEL  (blog, Value=' ', Font=ft_b_normal,xsize=xsiz-100)

membis       =WIDGET_BASE   (MAIN_bis ,/row)
gwmimi       =WIDGET_BUTTON (widget_base(membis,/NonExclusive),Font=ft_smaller,UValue=[2,11,0],Value='Yr:')
gw_mimi      =WIDGET_TEXT   (membis, Font=ft_smaller,xsize=6,ysize=1,/editable,Value=' Max')
gxmimi       =WIDGET_BUTTON (widget_base(membis,/NonExclusive),Font=ft_smaller,UValue=[2,12,0],Value='Xr:')
gx_mim1      =WIDGET_TEXT   (membis, Font=ft_smaller,xsize=6,ysize=1,/editable,Value=' Min')
gx_mim2      =WIDGET_TEXT   (membis, Font=ft_smaller,xsize=6,ysize=1,/editable,Value=' Max')
bid          =WIDGET_LABEL  (membis, Value=' ',Font=ft_propor)
help_label   =WIDGET_LABEL  (membis ,Value=txt_hlp,Font=ft_b_normal)
;------------------------------- BEGIN of gw_plot_area --------------------
gw_plot_area =WIDGET_DRAW    (MAIN_BIS, /Button_Events, Frame=5,Retain=2,$
			     UValue=[2,7,0],Colors=-8,XSize=xsiz,YSize=ysiz)
;------------------------------- BOTTOM BASE ---------------------------
bottom_base =WIDGET_BASE  (MAIN_BIS , /Row,resource_name='don')
IF lamp_siz GE 900 THEN $
    label_blanc =WIDGET_LABEL (bottom_base ,Value=' ',Font=ft_smaller)

gw_base      =WIDGET_BASE  (bottom_base, /Column)
gw_pk_no     =WIDGET_LABEL (gw_base, Font=ft_b_normal, Frame=2, Value='Peak #1 param')
;gw_peak_no  =WIDGET_BUTTON(gw_base, Font=ft_b_normal, Value='Peak # ')
gw_base	     =WIDGET_BASE  (gw_base, /NonExclusive)
gw_bgfixed   =WIDGET_BUTTON(gw_base, UValue=[5, 0], Value='is background',Font=ft_b_normal)
;.... position ....
gw_base      =WIDGET_BASE  (bottom_base, /Column)
gw_pos_text  =WIDGET_TEXT  (gw_base, Value='0.0',/Editable, $
			    Font=ft_propor,XSize=8, UValue=[6, 0, 0])
gw_pos_pop(0)=WIDGET_BUTTON(gw_base, /Menu, Font=ft_propor,$
				UValue=[5,1,0], Value='Position    ')
gw_pos_pop(3)=WIDGET_BUTTON(gw_pos_pop(0), UValue=[5,1,0], $
			    Value='Err :',  Font=ft_propor)
gw_pos_pop(1)=WIDGET_BUTTON(gw_pos_pop(0) , UValue=[5,1,1], $
			    Value='Fix this parameter', Font=ft_propor)
gw_pos_pop(2)=WIDGET_BUTTON(gw_pos_pop(0), UValue=[5,1,2], $
			    Value='Fixed as peak #',/Menu, Font=ft_propor)
FOR j=1,6 do $
gw_pos_pop(3+j)=WIDGET_BUTTON(gw_pos_pop(2), UValue=[5,1,2+j], $
			      Value=' '+STRTRIM(STRING(j), 2)+' ', Font=ft_propor)
gw	     =WIDGET_BUTTON(gw_pos_pop(2),  UValue=[5,1,9], Value='None', Font=ft_propor)

;.... integral ....
gw_base      =WIDGET_BASE  (bottom_base, /Column)
gw_int_text  =WIDGET_TEXT  (gw_base, Value='0.0',/Editable, $
			    Font=ft_propor,XSize=8, UValue=[6, 0, 0])
gw_int_pop(0)=WIDGET_BUTTON(gw_base, /Menu, Font=ft_propor, $
			    Value='Height    ')
gw_int_pop(3)=WIDGET_BUTTON(gw_int_pop(0), UValue=[5,2,0], $
			    Value='Err :', Font=ft_propor)
gw_int_pop(1)=WIDGET_BUTTON(gw_int_pop(0) , UValue=[5,2,1], $
			    Value='Fix this parameter', Font=ft_propor)
gw_int_pop(2)=WIDGET_BUTTON(gw_int_pop(0), UValue=[5,2,2], $
			    Value='Fixed as peak #', /Menu, Font=ft_propor)
FOR j=1,6 do $
gw_int_pop(3+j)=WIDGET_BUTTON(gw_int_pop(2), UValue=[5,2,2+j], $
			    Value=' '+STRTRIM(STRING(j), 2)+' ', Font=ft_propor)
gw	     =WIDGET_BUTTON(gw_int_pop(2),  UValue=[5,2,9], $
			    Value='None', Font=ft_propor)

;.... width ....  
gw_base      =WIDGET_BASE  (bottom_base, /Column)
gw_wdt_text  =WIDGET_TEXT  (gw_base, Value='0.0',/Editable, $
			    Font=ft_propor,XSize=8, UValue=[6, 0, 0])
gw_wdt_pop(0)=WIDGET_BUTTON(gw_base, /Menu, Font=ft_propor, $
			    Value='Width    ')
gw_wdt_pop(3)=WIDGET_BUTTON(gw_wdt_pop(0), UValue=[5,3,0], $
			    Value='Err :', Font=ft_propor)
gw_wdt_pop(1)=WIDGET_BUTTON(gw_wdt_pop(0) ,UValue=[5,3,1], $
			    Value='Fix this parameter', Font=ft_propor)
gw_wdt_pop(2)=WIDGET_BUTTON(gw_wdt_pop(0), UValue=[5,3,2], $
			    Value='Fixed as peak #', /Menu, Font=ft_propor)
FOR j=1,6 do $
gw_wdt_pop(3+j)=WIDGET_BUTTON(gw_wdt_pop(2), UValue=[5,3,2+j], $
			    Value=' '+STRTRIM(STRING(j), 2)+' ', Font=ft_propor)
gw	     = WIDGET_BUTTON(gw_wdt_pop(2),  UValue=[5,3,9], $
			    Value='None', Font=ft_propor)

; .... flat_bg ....
gw_base      =WIDGET_BASE  (bottom_base, /Column)
gw_fbg_text  =WIDGET_TEXT  (gw_base, Value='0.0',/Editable, $
			    Font=ft_propor,XSize=8, UValue=[6, 0, 0])
gw_fbg_pop(0)=WIDGET_BUTTON (gw_base, /Menu, Font=ft_propor,Value='Flat Bg.    ')
gw_fbg_pop(3)=WIDGET_BUTTON(gw_fbg_pop(0), UValue=[5,4,0], $
			    Value='Err :', Font=ft_propor)
gw_fbg_pop(1)=WIDGET_BUTTON(gw_fbg_pop(0) ,UValue=[5,4,1], $
			    Value='Fix this parameter', Font=ft_propor)

;.... slope_bg ....
gw_base      =WIDGET_BASE  (bottom_base, /Column)
gw_sbg_text  =WIDGET_TEXT  (gw_base, Value='0.0',/Editable, $
			    Font=ft_propor,XSize=8, UValue=[6, 0, 0])
gw_sbg_pop(0)=WIDGET_BUTTON (gw_base, /Menu, Font=ft_propor, $
			    Value='Slope Bg.    ')
gw_sbg_pop(3)=WIDGET_BUTTON(gw_sbg_pop(0), UValue=[5,5,0], $
			    Value='Err :',  Font=ft_propor)
gw_sbg_pop(1)=WIDGET_BUTTON(gw_sbg_pop(0), UValue=[5,5,1], $
			    Value='Fix this parameter', Font=ft_b_normal)

;--------------------------- END of BOTTOM BASE --------------------------
FOR j=1,g_nb_pk_max DO BEGIN	    ; Make unsensitive peak popup menu
    ; only first button remains sensitive as g_npeaks=1
    WIDGET_CONTROL, bad_id=iii, gw_pos_pop(j+3) ,Sensitive=floor(1/j) 
    WIDGET_CONTROL, bad_id=iii, gw_int_pop(j+3), Sensitive=floor(1/j) 
    WIDGET_CONTROL, bad_id=iii, gw_wdt_pop(j+3), Sensitive=floor(1/j) 
ENDFOR

; If active, those lines disable the fix as line ...
WIDGET_CONTROL, bad_id=i, gw_pos_pop(2), Sensitive=0
WIDGET_CONTROL, bad_id=i, gw_int_pop(2), Sensitive=0
WIDGET_CONTROL, bad_id=i, gw_wdt_pop(2), Sensitive=0

bid=sys_dep('DYNLAB', gw_gkfit_base, 1)
WIDGET_CONTROL, bad_id=i, gw_gkfit_base, /Realize & put_logo

; Get drawable window index
WIDGET_CONTROL, bad_id=i, gw_plot_area, GET_VALUE=g_plot_wid
wset,g_plot_wid

if lamp_b1 gt 0 then XMANAGER, 'gfit', gw_gkfit_base, /just_reg $
else g_char=0.4

creer_excl		; creation de gw_excl_base
creer_view		; creation de gw_view_base
; Simulate a get workspace button event
gfit_event,{WIDGET_BUTTON,ID:gw_get_but,TOP:gw_gkfit_base,HANDLER:0L,SELECT:1}
error_msg, -2, 1

if lamp_b1 le 0 then XMANAGER, 'gfit', gw_gkfit_base

ENDIF ELSE IF g_tripx THEN $
    gfit_event,{WIDGET_BUTTON,ID:gw_get_but,TOP:gw_gkfit_base,HANDLER:0L,SELECT:1}
IF g_ctrl_panel THEN gfit_ctrl_panel
;mmmap=0 ELSE mmmap=1
;WIDGET_CONTROL, bad_id=i, gw_gkfit_base, Map=mmmap
END
