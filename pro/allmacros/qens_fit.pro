; ** ***************************************************************************************
; ** Program qens_fit
; ** This module permits to fit Quasi-elastic data to a set of
; ** different models (translations, rotations, jumps ...)
; ** S. Rols 09/01 srols@anl.gov
; ** Last modification: 02/04/02
; ** Last modification: 15/05/02 - correction of several buggs when fitting all spectra 
; **                             - modification of the output files format
; ** Last modification: 21/05/02 - modification of the convolution routine ... use of convol
; ** Last modification: 28/05/02 - modification of the convolution routine ... use of convol
; ** Last modification: 07/07/02 - symetrization of the slopping backg or constant backg
; ** ****************************************************************************************

; ** **************************************************************
pro create_commons
; ** **************************************************************
; ** Just to create correct commons while compiling.
; ** Explains also variables



common sc_wid, $
			sw_ba_leader,								$;Widget Id of leader base
			sw_bu_filem, sw_bu_exitm,					$
			sw_bu_fitm,sw_bu_lgm,sw_bu_isorotm,			$
			sw_bu_axrotm,sw_bu_rwm,						$
			sw_ba_up,sw_ba_up1,sw_ba_up2,sw_ba_up3,		$
			sw_ba_up11,sw_ba_up12,						$
			sw_ba_up21,sw_ba_up22,						$
			sw_ba_up31,sw_ba_up32,						$
			sw_bu_gwin,sw_bu_gres,sw_bu_wwou,			$
			sw_bu_winf,sw_bu_resf,sw_bu_wouf,			$
			sw_bu_winb,sw_bu_resb,sw_bu_woub,			$
			sw_la_win,sw_la_res,sw_la_wou,				$
			sw_ba_mid,sw_ba_mid2,sw_ba_mid3,			$
			sw_ba_mid11,sw_ba_mid12,sw_ba_mid13,		$
			sw_ba_mid14,sw_ba_mid15,					$
			sw_la_xmin,sw_la_xmax,sw_la_ymin,			$
			sw_la_ymax,sw_la_wsub,sw_tx_xmin,			$
			sw_tx_xmax,sw_tx_ymin,sw_tx_ymax,			$
			sw_tx_wsub,sw_bu_plot,sw_bu_plre,			$
			sw_pl_area,sw_ba_plre,sw_bu_norm,			$
			sw_ba_bot,sw_bu_start,sw_bu_cancel,			$
			sw_la_nlor,sw_la_ngau,sw_tx_resmin,			$
			sw_tx_nlor,sw_tx_ngau,sw_tx_resmax,			$
			sw_tx_balg2,sw_bu_balg2,sw_tx_fwsub,		$
			sw_tx_fxmin,sw_tx_fxmax,sw_tx_cycl,			$
			sw_pl_areabot,sw_bu_gdos,sw_bu_fit,     $
      sw_bu_done,sw_bu_try,sw_bu_fitall,      $
      sw_bu_acquire,sw_tx_acquire,sw_bu_reverse,$
      sw_bu_fromlast,sw_bu_symsqw,sw_tx_temp

common sc_loc, $
			sw_in,sw_res,sw_ou,sw_result,				$ ; input Workspaces (input,res fun,output)
			str_win,str_wres,str_wou,					$ ; input Worksp strings (input,res fun,output)
			sx_in,sx_res,sx_ou,sx_result,				$ ; input X
			sy_in,sy_res,sy_ou,sy_result,				$ ; input Y
			se_in,se_res,se_ou,se_result,				$ ; input errors
			s_pl_wid,num_spectrum,nb_spe_tot,			$
			nb_channels,x_to_plot,w_wk,w_wkres,e_wk,	$ ; variables de plot (x,S(q,w),Resf(Q,w) normee, error)
			w_resc,e_resc,ce,							$ ; res. func treated and its error for the convolution
			xmin,xmax,ymin,ymax,s_pl_widbot


common sc_flag, $
			flag_pl_res,flag_norm,						$ ;
			flag_lfit,flag_gfit,flag_lcfit,flag_gcfit,	$ ;
			flag_lwfit,flag_gwfit,flag_lifit,flag_gifit,$ ;
			flag_dfit,flag_dcfit,flag_difit,			$ ;
			flag_bfit,flag_bcfit,flag_bifit,			$ ;
			flag_lgifit,flag_pl_fit,flag_lect_in, $
      flag_pl_fit2,flag_all,flag_reverse,flag_fromlast, $
      flag_sym

common sc_fit,	$
			num_lor,num_gau,rfmin,rfmax,				$ ;
			sf_index,sf_values,ibmin,ibmintx,idmin,		$ ;
			idmintx,igmin,igmintx,igminind,idminind,	$ ;
			ibminind,sf_param_to_fit,sf_new_values,		$ ;
			sf_last_values,sf_xmin,sf_xmax,				$ ;
			sf_sp_num_min,sf_sp_num_max,sf_iter_num,	$ ;
			sf_w_fitted,sf_e_values,sf_w_exp_fit,		$ ;
			sf_sub_function,name_func,sf_new,sf_e,  $
      sf_functions_labels,sf_errors_labels,chisqr,betatemp

common sc_convs,$
			aalpha,abeta,lmin,lmax,cel,fwhm_rf,center_rf

common sc_lorgau,$
			n_l,n_g,ispecc,falpha,fbeta,w_resc2,w_resc2_interp

end

; ** ********************************************************************* ** ;
; ** ********************************************************************* ** ;
; ** This is the complete contain of the MPFIT.pro program developed       ** ;
; ** by C. B. Markwardt, NASA/GSFC, craigm@lheamail.gsfc.nasa.edu          ** ;
; ** The complete notice on how to use this routine is developed in the    ** ;
; ** MPFIT.pro file as well as on the web page :                           ** ;
; ** http://cow.physics.wisc.edu/~craigm/idl/fitting.html                  ** ;
; ** ********************************************************************* ** ;
; ** ********************************************************************* ** ;

pro mpfit_dummy
  ;; Enclose in a procedure so these are not defined in the main level
  forWARD_function mpfit_fdjac2, mpfit_enorm, mpfit_lmpar, mpfit_covar, $
    mpfit, mpfit_call

  common mpfit_error, error_code  ;; For error passing to user function
  common mpfit_config, mpconfig   ;; For internal error configrations
end

;; Reset profiling registers for another run.  By default, and when
;; uncommented, the profiling registers simply accumulate.

pro mpfit_resetprof
  common mpfit_profile, mpfit_profile_vals

  mpfit_profile_vals = { status: 1L, fdjac2: 0D, lmpar: 0D, mpfit: 0D, $
                         qrfac: 0D,  qrsolv: 0D, enorm: 0D}
  return
end

;; Following are machine constants that can be loaded once.  I have
;; found that bizarre underflow messages can be produced in each call
;; to MACHAR(), so this structure minimizes the number of calls to
;; one.

pro mpfit_setmachar, double=isdouble
  common mpfit_profile, profvals
  if n_elements(profvals) eq 0 then mpfit_resetprof

  common mpfit_machar, mpfit_machar_vals

  ;; In earlier versions of IDL, MACHAR itself could produce a load of
  ;; error messages.  We try to mask some of that out here.
  if (!version.release) lt 5 then dummy = check_math(1, 1)

  mch = 0.
  mch = machar(double=keyword_set(isdouble))
  dmachep = mch.eps
  dmaxnum = mch.xmax
  dminnum = mch.xmin
  dmaxlog = alog(mch.xmax)
  dminlog = alog(mch.xmin)
  if keyword_set(isdouble) then $
    dmaxgam = 171.624376956302725D $
  else $
    dmaxgam = 171.624376956302725
  drdwarf = sqrt(dminnum*1.5) * 10
  drgiant = sqrt(dmaxnum) * 0.1

  mpfit_machar_vals = {machep: dmachep, maxnum: dmaxnum, minnum: dminnum, $
                       maxlog: dmaxlog, minlog: dminlog, maxgam: dmaxgam, $
                       rdwarf: drdwarf, rgiant: drgiant}

  if (!version.release) lt 5 then dummy = check_math(0, 0)

  return
end



;; Call user function or procedure, with _EXTRA or not, with
;; derivatives or not.
function mpfit_call, fcn, x, fjac, _EXTRA=extra

  on_error, 2
  common mpfit_config, mpconfig

  if keyword_set(mpconfig.qanytied) then mpfit_tie, x, mpconfig.ptied

  ;; Decide whether we are calling a procedure or function
  if mpconfig.proc then proc = 1 else proc = 0
  mpconfig.nfev = mpconfig.nfev + 1

  if proc then begin
      if n_params() eq 3 then begin
          if n_elements(extra) gt 0 then $
            call_procedure, fcn, x, f, fjac, _EXTRA=extra $
          else $
            call_procedure, fcn, x, f, fjac
      endif else begin
          if n_elements(extra) gt 0 then $
            call_procedure, fcn, x, f, _EXTRA=extra $
          else $
            call_procedure, fcn, x, f
      endelse
  endif else begin
      if n_params() eq 3 then begin
          if n_elements(extra) gt 0 then $
            f = call_function(fcn, x, fjac, _EXTRA=extra) $
          else $
            f = call_function(fcn, x, fjac)
      endif else begin
          if n_elements(extra) gt 0 then $
            f = call_function(fcn, x, _EXTRA=extra) $
          else $
            f = call_function(fcn, x)
      endelse
  endelse

  if n_params() eq 2 and mpconfig.damp gt 0 then begin
      damp = mpconfig.damp(0)

      ;; Apply the damping if requested.  This replaces the residuals
      ;; with their hyperbolic tangent.  Thus residuals larger than
      ;; DAMP are essentially clipped.
      f = tanh(f/damp)
  endif

  return, f
end

function mpfit_fdjac2, fcn, x, fvec, step, ulimited, ulimit, dside, $
                 iflag=iflag, epsfcn=epsfcn, autoderiv=autoderiv, $
                 FUNCTARGS=fcnargs, xall=xall, ifree=ifree, dstep=dstep

  common mpfit_machar, machvals
  common mpfit_profile, profvals
  common mpfit_error, mperr

;  prof_start = systime(1)
  MACHEP0 = machvals.machep
  DWARF   = machvals.minnum

  if n_elements(epsfcn) eq 0 then epsfcn = MACHEP0
  if n_elements(xall)   eq 0 then xall = x
  if n_elements(ifree)  eq 0 then ifree = lindgen(n_elements(xall))
  if n_elements(step)   eq 0 then step = x * 0.
  nall = n_elements(xall)

  eps = sqrt(max([epsfcn, MACHEP0]));
  m = n_elements(fvec)
  n = n_elements(x)

  ;; Compute analytical derivative if requested
  if not keyword_set(autoderiv) then begin
      mperr = 0
      fjac = intarr(nall)
      fjac(ifree) = 1      ;; Specify which parameters need derivatives
      fp = mpfit_call(fcn, xall, fjac, _EXTRA=fcnargs)
      iflag = mperr

      if n_elements(fjac) ne m*nall then begin
          message, 'ERRor: Derivative matrix was not computed properly.', /info
          iflag = 1
;          profvals.fdjac2 = profvals.fdjac2 + (systime(1) - prof_start)
          return, 0
      endif

      ;; This definition is consistent with CURVEFIT
      ;; Sign error found (thanks Jesus Fernandez <fernande@irm.chu-caen.fr>)
      fjac = reform(-temporary(fjac), m, nall, /overwrite)

      ;; Select only the free parameters
      if n_elements(ifree) lt nall then $
        fjac = reform(fjac(*,ifree), m, n, /overwrite)
;      profvals.fdjac2 = profvals.fdjac2 + (systime(1) - prof_start)
      return, fjac
  endif

  fjac = make_array(m, n, value=fvec(0)*0.)
  fjac = reform(fjac, m, n, /overwrite)

  h = eps * abs(x)

  ;; if STEP is given, use that
  if n_elements(step) gt 0 then begin
      wh = where(step gt 0, ct)
      if ct gt 0 then h(wh) = step(wh)
  endif

  ;; if relative step is given, use that
  if n_elements(dstep) gt 0 then begin
      wh = where(dstep gt 0, ct)
      if ct gt 0 then h(wh) = abs(dstep(wh)*x(wh))
  endif

  ;; In case any of the step values are zero
  wh = where(h eq 0, ct)
  if ct gt 0 then h(wh) = eps

  ;; Reverse the sign of the step if we are up against the parameter
  ;; limit, or if the user requested it.
  mask = dside eq -1
  if n_elements(ulimited) gt 0 and n_elements(ulimit) gt 0 then $
    mask = mask or (ulimited and (x gt ulimit-h))
  wh = where(mask, ct)
  if ct gt 0 then h(wh) = -h(wh)

  ;; Loop through parameters, computing the derivative for each
  for j=0L, n-1 do begin
      xp = xall
      xp(ifree(j)) = xp(ifree(j)) + h(j)

      mperr = 0
      fp = mpfit_call(fcn, xp, _EXTRA=fcnargs)

      iflag = mperr
      if iflag lt 0 then return, !values.d_nan

      if abs(dside(j)) le 1 then begin
          ;; COMPUTE THE One-SIDED DERIVATIVE
          ;; Note optimization fjac(0:*,j)
          fjac(0,j) = (fp-fvec)/h(j)

      endif else begin
          ;; COMPUTE THE TWO-SIDED DERIVATIVE
          xp(ifree(j)) = xall(ifree(j)) - h(j)

          mperr = 0
          fm = mpfit_call(fcn, xp, _EXTRA=fcnargs)

          iflag = mperr
          if iflag lt 0 then return, !values.d_nan

          ;; Note optimization fjac(0:*,j)
          fjac(0,j) = (fp-fm)/(2*h(j))
      endelse

  endfor

;  profvals.fdjac2 = profvals.fdjac2 + (systime(1) - prof_start)
  return, fjac
end

function mpfit_enorm, vec

  ;; notE: it turns out that, for systems that have a lot of data
  ;; points, this routine is a big computing bottleneck.  The extended
  ;; computations that need to be done cannot be effectively
  ;; vectorized.  The introduction of the FASTNorM configuration
  ;; parameter allows the user to select a faster routine, which is
  ;; based on total() alone.
  common mpfit_profile, profvals
;  prof_start = systime(1)

  common mpfit_config, mpconfig
; Very simple-minded sum-of-squares
  if n_elements(mpconfig) gt 0 then if mpconfig.fastnorm then begin
      ans = sqrt(total(vec^2))
      goto, TERminATE
  endif

  common mpfit_machar, machvals

  agiant = machvals.rgiant / n_elements(vec)
  adwarf = machvals.rdwarf * n_elements(vec)

  ;; This is hopefully a compromise between speed and robustness.
  ;; Need to do this because of the possibility of over- or underflow.
  mx = max(vec, min=mn)
  mx = max(abs([mx,mn]))
  if mx eq 0 then return, vec(0)*0.

  if mx gt agiant or mx lt adwarf then ans = mx * sqrt(total((vec/mx)^2))$
  else                                 ans = sqrt( total(vec^2) )

  TERminATE:
;  profvals.enorm = profvals.enorm + (systime(1) - prof_start)
  return, ans
end

pro mpfit_qrfac, a, ipvt, rdiag, acnorm, pivot=pivot

  sz = size(a)
  m = sz(1)
  n = sz(2)

  common mpfit_machar, machvals
  common mpfit_profile, profvals
;  prof_start = systime(1)

  MACHEP0 = machvals.machep
  DWARF   = machvals.minnum

  ;; Compute the initial column norms and initialize arrays
  acnorm = make_array(n, value=a(0)*0.)
  for j = 0L, n-1 do $
    acnorm(j) = mpfit_enorm(a(*,j))
  rdiag = acnorm
  wa = rdiag
  ipvt = lindgen(n)

  ;; Reduce a to r with householder transformations
  minmn = min([m,n])
  for j = 0L, minmn-1 do begin
      if not keyword_set(pivot) then goto, HOUSE1

      ;; Bring the column of largest norm into the pivot position
      rmax = max(rdiag(j:*))
      kmax = where(rdiag(j:*) eq rmax, ct) + j
      if ct le 0 then goto, HOUSE1
      kmax = kmax(0)

      ;; Exchange rows via the pivot only.  Avoid actually exchanging
      ;; the rows, in case there is lots of memory transfer.  The
      ;; exchange occurs later, within the body of MPFIT, after the
      ;; extraneous columns of the matrix have been shed.
      if kmax ne j then begin
          temp     = ipvt(j)   & ipvt(j)    = ipvt(kmax) & ipvt(kmax)  = temp
          rdiag(kmax) = rdiag(j)
          wa(kmax)    = wa(j)
      endif

      HOUSE1:

      ;; Compute the householder transformation to reduce the jth
      ;; column of A to a multiple of the jth unit vector
      lj     = ipvt(j)
      ajj    = a(j:*,lj)
      ajnorm = mpfit_enorm(ajj)
      if ajnorm eq 0 then goto, neXT_ROW
      if a(j,j) lt 0 then ajnorm = -ajnorm

      ajj     = ajj / ajnorm
      ajj(0)  = ajj(0) + 1
      ;; *** Note optimization a(j:*,j)
      a(j,lj) = ajj

      ;; Apply the transformation to the remaining columns
      ;; and update the norms

      ;; notE to SELF: tried to optimize this by removing the loop,
      ;; but it actually got slower.  Reverted to "for" loop to keep
      ;; it simple.
      if j+1 lt n then begin
          for k=j+1, n-1 do begin
              lk = ipvt(k)
              ajk = a(j:*,lk)
              ;; *** Note optimization a(j:*,lk)
              ;; (corrected 20 Jul 2000)
              if a(j,lj) ne 0 then $
                a(j,lk) = ajk - ajj * total(ajk*ajj)/a(j,lj)

              if keyword_set(pivot) and rdiag(k) ne 0 then begin
                  temp = a(j,lk)/rdiag(k)
                  rdiag(k) = rdiag(k) * sqrt((1.-temp^2) > 0)
                  temp = rdiag(k)/wa(k)
                  if 0.05D*temp*temp le MACHEP0 then begin
                      rdiag(k) = mpfit_enorm(a(j+1:*,lk))
                      wa(k) = rdiag(k)
                  endif
              endif
          endfor
      endif

      neXT_ROW:
      rdiag(j) = -ajnorm
  endfor

;  profvals.qrfac = profvals.qrfac + (systime(1) - prof_start)
  return
end

pro mpfit_qrsolv, r, ipvt, diag, qtb, x, sdiag

  sz = size(r)
  m = sz(1)
  n = sz(2)
  delm = lindgen(n) * (m+1) ;; Diagonal elements of r

  common mpfit_profile, profvals
;  prof_start = systime(1)

  ;; copy r and (q transpose)*b to preserve input and initialize s.
  ;; in particular, save the diagonal elements of r in x.

  for j = 0L, n-1 do $
    r(j:n-1,j) = r(j,j:n-1)
  x = r(delm)
  wa = qtb
  ;; Below may look strange, but it's so we can keep the right precision
  zero = qtb(0)*0.
  half = zero + 0.5
  quart = zero + 0.25

  ;; Eliminate the diagonal matrix d using a givens rotation
  for j = 0L, n-1 do begin
      l = ipvt(j)
      if diag(l) eq 0 then goto, STorE_RESTorE
      sdiag(j:*) = 0
      sdiag(j) = diag(l)

      ;; The transformations to eliminate the row of d modify only a
      ;; single element of (q transpose)*b beyond the first n, which
      ;; is initially zero.

      qtbpj = zero
      for k = j, n-1 do begin
          if sdiag(k) eq 0 then goto, ELIM_neXT_LOOP
          if abs(r(k,k)) lt abs(sdiag(k)) then begin
              cotan  = r(k,k)/sdiag(k)
              sine   = half/sqrt(quart + quart*cotan*cotan)
              cosine = sine*cotan
          endif else begin
              tang   = sdiag(k)/r(k,k)
              cosine = half/sqrt(quart + quart*tang*tang)
              sine   = cosine*tang
          endelse

          ;; Compute the modified diagonal element of r and the
          ;; modified element of ((q transpose)*b,0).
          r(k,k) = cosine*r(k,k) + sine*sdiag(k)
          temp = cosine*wa(k) + sine*qtbpj
          qtbpj = -sine*wa(k) + cosine*qtbpj
          wa(k) = temp

          ;; Accumulate the transformation in the row of s
          if n gt k+1 then begin
              temp = cosine*r(k+1:n-1,k) + sine*sdiag(k+1:n-1)
              sdiag(k+1:n-1) = -sine*r(k+1:n-1,k) + cosine*sdiag(k+1:n-1)
              r(k+1:n-1,k) = temp
          endif
ELIM_neXT_LOOP:
      endfor

STorE_RESTorE:
      sdiag(j) = r(j,j)
      r(j,j) = x(j)
  endfor

  ;; Solve the triangular system for z.  If the system is singular
  ;; then obtain a least squares solution
  nsing = n
  wh = where(sdiag eq 0, ct)
  if ct gt 0 then begin
      nsing = wh(0)
      wa(nsing:*) = 0
  endif

  if nsing ge 1 then begin
      wa(nsing-1) = wa(nsing-1)/sdiag(nsing-1) ;; Degenerate case
      ;; *** Reverse loop ***
      for j=nsing-2,0,-1 do begin
          sum = total(r(j+1:nsing-1,j)*wa(j+1:nsing-1))
          wa(j) = (wa(j)-sum)/sdiag(j)
      endfor
  endif

  ;; Permute the components of z back to components of x
  x(ipvt) = wa

;  profvals.qrsolv = profvals.qrsolv + (systime(1) - prof_start)
  return
end
;
function mpfit_lmpar, r, ipvt, diag, qtb, delta, x, sdiag, par=par

  common mpfit_machar, machvals
  common mpfit_profile, profvals
;  prof_start = systime(1)

  MACHEP0 = machvals.machep
  DWARF   = machvals.minnum

  sz = size(r)
  m = sz(1)
  n = sz(2)
  delm = lindgen(n) * (m+1) ;; Diagonal elements of r

  ;; Compute and store in x the gauss-newton direction.  If the
  ;; jacobian is rank-deficient, obtain a least-squares solution
  nsing = n
  wa1 = qtb
  wh = where(r(delm) eq 0, ct)
  if ct gt 0 then begin
      nsing = wh(0)
      wa1(wh(0):*) = 0
  endif

  if nsing gt 1 then begin
      ;; *** Reverse loop ***
      for j=nsing-1,0,-1 do begin
          wa1(j) = wa1(j)/r(j,j)
          if (j-1 ge 0) then $
            wa1(0:(j-1)) = wa1(0:(j-1)) - r(0:(j-1),j)*wa1(j)
      endfor
  endif

  ;; Note: ipvt here is a permutation array
  x(ipvt) = wa1

  ;; Initialize the iteration counter.  Evaluate the function at the
  ;; origin, and test for acceptance of the gauss-newton direction
  iter = 0L
  wa2 = diag * x
  dxnorm = mpfit_enorm(wa2)
  fp = dxnorm - delta
  if fp le 0.1*delta then goto, TERminATE

  ;; If the jacobian is not rank deficient, the newton step provides a
  ;; lower bound, parl, for the zero of the function.  Otherwise set
  ;; this bound to zero.

  zero = wa2(0)*0.
  parl = zero
  if nsing ge n then begin
      wa1 = diag(ipvt)*wa2(ipvt)/dxnorm

      wa1(0) = wa1(0) / r(0,0) ;; Degenerate case
      for j=1L, n-1 do begin   ;; Note "1" here, not zero
          sum = total(r(0:(j-1),j)*wa1(0:(j-1)))
          wa1(j) = (wa1(j) - sum)/r(j,j)
      endfor

      temp = mpfit_enorm(wa1)
      parl = ((fp/delta)/temp)/temp
  endif

  ;; Calculate an upper bound, paru, for the zero of the function
  for j=0, n-1 do begin
      sum = total(r(0:j,j)*qtb(0:j))
      wa1(j) = sum/diag(ipvt(j))
  endfor
  gnorm = mpfit_enorm(wa1)
  paru  = gnorm/delta
  if paru eq 0 then paru = DWARF/min([delta,0.1])

  ;; If the input par lies outside of the interval (parl,paru), set
  ;; par to the closer endpoint

  par = max([par,parl])
  par = min([par,paru])
  if par eq 0 then par = gnorm/dxnorm

  ;; Beginning of an interation
  ITERATION:
  iter = iter + 1

  ;; Evaluate the function at the current value of par
  if par eq 0 then par = max([DWARF, paru*0.001])
  temp = sqrt(par)
  wa1 = temp * diag
  mpfit_qrsolv, r, ipvt, wa1, qtb, x, sdiag
  wa2 = diag*x
  dxnorm = mpfit_enorm(wa2)
  temp = fp
  fp = dxnorm - delta

  if (abs(fp) le 0.1D*delta) $
    or ((parl eq 0) and (fp le temp) and (temp lt 0)) $
    or (iter eq 10) then goto, TERminATE

  ;; Compute the newton correction
  wa1 = diag(ipvt)*wa2(ipvt)/dxnorm

  for j=0,n-2 do begin
      wa1(j) = wa1(j)/sdiag(j)
      wa1(j+1:n-1) = wa1(j+1:n-1) - r(j+1:n-1,j)*wa1(j)
  endfor
  wa1(n-1) = wa1(n-1)/sdiag(n-1) ;; Degenerate case

  temp = mpfit_enorm(wa1)
  parc = ((fp/delta)/temp)/temp

  ;; Depending on the sign of the function, update parl or paru
  if fp gt 0 then parl = max([parl,par])
  if fp lt 0 then paru = min([paru,par])

  ;; Compute an improved estimate for par
  par = max([parl, par+parc])

  ;; End of an iteration
  goto, ITERATION

TERminATE:
  ;; Termination
;  profvals.lmpar = profvals.lmpar + (systime(1) - prof_start)
  if iter eq 0 then return, par(0)*0.
  return, par
end

;; Procedure to tie one parameter to another.
pro mpfit_tie, p, _ptied
  if n_elements(_ptied) eq 0 then return
  if n_elements(_ptied) eq 1 then if _ptied(0) eq '' then return
  for _i = 0L, n_elements(_ptied)-1 do begin
      if _ptied(_i) eq '' then goto, neXT_TIE
      _cmd = 'p('+strtrim(_i,2)+') = '+_ptied(_i)
      _err = execute(_cmd)
      if _err eq 0 then begin
          message, 'ERRor: Tied expression "'+_cmd+'" failed.'
          return
      endif
      neXT_TIE:
  endfor
end

;; Default procedure to be called every iteration.  It simply prints
;; the parameter values.
pro mpfit_defiter, fcn, x, iter, fnorm, FUNCTARGS=fcnargs, $
                   quiet=quiet, iterstop=iterstop, parinfo=parinfo, $
                   format=fmt, pformat=pformat, _EXTRA=iterargs

  common mpfit_error, mperr
  mperr = 0
  if keyword_set(quiet) then return
  if n_params() eq 3 then begin
      fvec = mpfit_call(fcn, x, _EXTRA=fcnargs)
      fnorm = mpfit_enorm(fvec)^2
  endif
; *** Stef ajout
;if abs(fnorm) le 1e-20 then begin
;print, 'Probleme fnorm trop petit'
;stop
;endif
;if abs(fnorm) ge 1e+20 then begin
;print, 'Probleme fnorm trop grand'
;stop
;endif
; *** fin Stef ajout
  print, iter, fnorm, $
    format='("Iter ",I6,"   CHI-SQUARE = ",G20.8)'
  if n_elements(fmt) gt 0 then begin
      print, x, format=fmt
  endif else begin
      if n_elements(parinfo) gt 0 then begin
          parinfo_tags = tag_names(parinfo)
          wh = where(parinfo_tags eq 'PARNAME', ct)
          if ct eq 1 then begin
              plen = max(strlen(parinfo.parname)) < 25
              plen = strtrim(plen,2)
              p = string(parinfo.parname, format='("    ",A'+plen+'," = ")')
          endif
      endif
      if n_elements(p) eq 0 then $
        p = '    P('+strtrim(lindgen(n_elements(x)),2)+') = '
      if n_elements(pformat) eq 0 then pformat = '(G20.6)'
      p = p + string(x,format=string(pformat(0))) + '  '
      print, p, format='(A)'
  endelse

  if keyword_set(iterstop) then begin
      k = get_kbrd(0)
      if k eq string(byte(7)) then begin
          message, 'WARNING: minimization not complete', /info
          print, 'Do you want to terminate this procedure? (y/n)', $
            format='(A,$)'
          k = ''
          read, k
          if strupcase(strmid(k,0,1)) eq 'Y' then begin
              message, 'WARNING: Procedure is terminating.', /info
              mperr = -1
          endif
      endif
  endif

  return
end

;; Procedure to parse the parameter values in PARINFO
pro mpfit_parinfo, parinfo, tnames, tag, values, default=def, status=status, $
                   n_param=n

  status = 0
  if n_elements(n) eq 0 then n = n_elements(parinfo)

  if n eq 0 then begin
      if n_elements(def) eq 0 then return
      values = def
      status = 1
      return
  endif

  if n_elements(parinfo) eq 0 then goto, do_DEFAUlt
  if n_elements(tnames) eq 0 then tnames = tag_names(parinfo)
  wh = where(tnames eq tag, ct)

  if ct eq 0 then begin
      do_DEFAUlt:
      if n_elements(def) eq 0 then return
      values = make_array(n, value=def(0))
      values(0) = def
  endif else begin
      values = parinfo.(wh(0))
  endelse

  status = 1
  return
end

function mpfit_covar, rr, ipvt, tol=tol

  sz = size(rr)
  if sz(0) ne 2 then begin
      message, 'ERRor: r must be a two-dimensional matrix'
      return, -1L
  endif
  n = sz(1)
  if n ne sz(2) then begin
      message, 'ERRor: r must be a square matrix'
      return, -1L
  endif

  zero = rr(0) * 0.
  one  = zero  + 1.
  if n_elements(ipvt) eq 0 then ipvt = lindgen(n)
  r = rr
  r = reform(rr, n, n, /overwrite)

  ;; For the inverse of r in the full upper triangle of r
  l = -1L
  if n_elements(tol) eq 0 then tol = one*1.E-14
  tolr = tol * abs(r(0,0))
  for k = 0L, n-1 do begin
      if abs(r(k,k)) le tolr then goto, INV_end_LOOP
      r(k,k) = one/r(k,k)
      for j = 0L, k-1 do begin
          temp = r(k,k) * r(j,k)
          r(j,k) = zero
          r(0,k) = r(0:j,k) - temp*r(0:j,j)
      endfor
      l = k
  endfor
  INV_end_LOOP:

  ;; Form the full upper triangle of the inverse of (r transpose)*r
  ;; in the full upper triangle of r
  if l ge 0 then $
    for k = 0L, l do begin
      for j = 0L, k-1 do begin
          temp = r(j,k)
          r(0,j) = r(0:j,j) + temp*r(0:j,k)
      endfor
      temp = r(k,k)
      r(0,k) = temp * r(0:k,k)
  endfor

  ;; For the full lower triangle of the covariance matrix
  ;; in the strict lower triangle or and in wa
  wa = replicate(r(0,0), n)
  for j = 0L, n-1 do begin
      jj = ipvt(j)
      sing = j gt l
      for i = 0L, j do begin
          if sing then r(i,j) = zero
          ii = ipvt(i)
          if ii gt jj then r(ii,jj) = r(i,j)
          if ii lt jj then r(jj,ii) = r(i,j)
      endfor
      wa(jj) = r(j,j)
  endfor

  ;; Symmetrize the covariance matrix in r
  for j = 0L, n-1 do begin
      r(0:j,j) = r(j,0:j)
      r(j,j) = wa(j)
  endfor

  return, r
end

function mpfit, fcn, xall, FUNCTARGS=fcnargs, SCAle_FCN=scalfcn, $
                ftol=ftol, xtol=xtol, gtol=gtol, epsfcn=epsfcn, resdamp=damp, $
                nfev=nfev, maxiter=maxiter, errmsg=errmsg, $
                factor=factor, nprint=nprint, STATUS=info, $
                iterproc=iterproc, iterargs=iterargs, niter=iter, iterstop=ss,$
                diag=diag, rescale=rescale, autoderivative=autoderiv, $
                perror=perror, covar=covar, nocovar=nocovar, bestnorm=fnorm, $
                parinfo=parinfo, quiet=quiet, nocatch=nocatch, $
                fastnorm=fastnorm, proc=proc, query=query

;stop
  if keyword_set(query) then return, 1

  if n_params() eq 0 then begin
      message, "USAge: PARMS = MPFIT('MYFUNCT', START_PARAMS, ... )", /info
      return, !values.d_nan
  endif

  ;; Use of double here not a problem since f/x/gtol are all only used
  ;; in comparisons
  if n_elements(ftol) eq 0 then ftol = 1.D-10
  if n_elements(xtol) eq 0 then xtol = 1.D-10
  if n_elements(gtol) eq 0 then gtol = 1.D-10
  if n_elements(factor) eq 0 then factor = 100.
  if n_elements(nprint) eq 0 then nprint = 1
  if n_elements(iterproc) eq 0 then iterproc = 'MPFIT_DEFITER'
  if n_elements(autoderiv) eq 0 then autoderiv = 1
  if strupcase(iterproc) eq 'MPFIT_DEFITER' and n_elements(iterargs) eq 0 $
    and keyword_set(ss) then iterargs = {iterstop:1}
  if n_elements(fastnorm) eq 0 then fastnorm = 0
  if n_elements(damp) eq 0 then damp = 0 else damp = damp(0)

  common mpfit_config, mpconfig
  mpconfig = {fastnorm: keyword_set(fastnorm), proc: 0, nfev: 0L, damp: damp}

  info = 0L
  iflag = 0L
  errmsg = ''
  catch_msg = 'in MPFIT'

  ;; Parameter damping doesn't work when user is providing their own
  ;; gradients.
  if damp ne 0 and not keyword_set(autoderiv) then begin
      errmsg = 'ERRor: keywords DAMP and AUTODERIV are mutually exclusive'
      goto, TERminATE
  endif


  ;; Handle error conditions gracefully
  if not keyword_set(nocatch) then begin
      catch, catcherror
      if catcherror ne 0 then begin
          catch, /cancel
          message, 'Error detected while '+catch_msg+':', /info
          message, !err_string, /info
          message, 'Error condition detected. Returning to MAIN level.', /info
          return, !values.d_nan
      endif
  endif

  ;; Parinfo:
  ;; --------------- STARTING/CONFIG INFO (passed in to routine, not changed)
  ;; .value   - starting value for parameter
  ;; .fixed   - parameter is fixed
  ;; .limited - a two-element array, if parameter is bounded on
  ;;            lower/upper side
  ;; .limits - a two-element array, lower/upper parameter bounds, if
  ;;           limited vale is set
  ;; .step   - step size in Jacobian calc, if greater than zero

  catch_msg = 'parsing input parameters'
  ;; Parameters can either be stored in parinfo, or x.  Parinfo takes
  ;; precedence if it exists.
  if n_elements(xall) eq 0 and n_elements(parinfo) eq 0 then begin
      errmsg = 'ERRor: must pass parameters in P or PARINFO'
      goto, TERminATE
  endif

  ;; Be sure that PARINFO is of the right type
  if n_elements(parinfo) gt 0 then begin
      parinfo_size = size(parinfo)
      if parinfo_size(parinfo_size(0)+1) ne 8 then begin
          errmsg = 'ERRor: PARINFO must be a structure.'
          goto, TERminATE
      endif
      if n_elements(xall) gt 0 and n_elements(xall) ne n_elements(parinfo) $
        then begin
          errmsg = 'ERRor: number of elements in PARINFO and P must agree'
          goto, TERminATE
      endif
  endif

  ;; If the parameters were not specified at the command line, then
  ;; extract them from PARINFO
  if n_elements(xall) eq 0 then begin
      mpfit_parinfo, parinfo, tagnames, 'value', xall, status=status
      if status eq 0 then begin
          errmsg = 'ERRor: either P or PARINFO(*).value must be supplied.'
          goto, TERminATE
      endif

      sz = size(xall)
      ;; Convert to double if parameters are not float or double
      if sz(sz(0)+1) ne 4 and sz(sz(0)+1) ne 5 then $
        xall = double(xall)
  endif
  npar = n_elements(xall)
  zero = xall(0) * 0.
  one  = zero    + 1.
  fnorm  = -one
  fnorm1 = -one

  ;; TIED parameters?
  mpfit_parinfo, parinfo, tagnames, 'TIED', ptied, default='', n=npar
  ptied = strtrim(ptied, 2)
  wh = where(ptied ne '', qanytied)
  qanytied = qanytied gt 0
  mpconfig = create_struct(mpconfig, 'QANYTIED', qanytied, 'PTIED', ptied)

  ;; FIXED parameters ?
  mpfit_parinfo, parinfo, tagnames, 'FIXED', pfixed, default=0, n=npar
  pfixed = pfixed eq 1
  pfixed = pfixed or (ptied ne '')   ;; Tied parameters are also effectively fixed

  ;; Finite differencing step, absolute and relative, and sidedness of derivative
  mpfit_parinfo, parinfo, tagnames, 'STEP',     step, default=zero, n=npar
  mpfit_parinfo, parinfo, tagnames, 'RELSTEP', dstep, default=zero, n=npar
  mpfit_parinfo, parinfo, tagnames, 'MPSIDE',  dside, default=0,    n=npar

  ;; Maximum and minimum steps allowed to be taken in one iteration
  mpfit_parinfo, parinfo, tagnames, 'MPmaxSTEP', maxstep, default=zero, n=npar
  mpfit_parinfo, parinfo, tagnames, 'MPminSTEP', minstep, default=zero, n=npar
  qmin = minstep *  0  ;; Remove minstep for now!!
  qmax = maxstep ne 0
  wh = where(qmin and qmax and maxstep lt minstep, ct)
  if ct gt 0 then begin
      errmsg = 'ERRor: MPminSTEP is greater than MPmaxSTEP'
      goto, TERminATE
  endif
  wh = where(qmin and qmax, ct)
  qminmax = ct gt 0

  ;; Finish up the free parameters
  ifree = where(pfixed ne 1, ct)
  if ct eq 0 then begin
      errmsg = 'ERRor: no free parameters'
      goto, TERminATE
  endif

  ;; Compose only VARYING parameters
  xnew = xall      ;; xnew is the set of parameters to be returned
  x = xnew(ifree)  ;; x is the set of free parameters

  ;; LIMITED parameters ?
  mpfit_parinfo, parinfo, tagnames, 'LIMITED', limited, status=st1
  mpfit_parinfo, parinfo, tagnames, 'LIMITS',  limits,  status=st2
  if st1 eq 1 and st2 eq 1 then begin

      ;; Error checking on limits in parinfo
      wh = where((limited(0,*) and xall lt limits(0,*)) or $
                 (limited(1,*) and xall gt limits(1,*)), ct)
      if ct gt 0 then begin
          errmsg = 'ERRor: parameters are not within PARINFO limits'
          goto, TERminATE
      endif
      wh = where(limited(0,*) and limited(1,*) and limits(0,*) ge limits(1,*) and $
                 pfixed eq 0, ct)
      if ct gt 0 then begin
          errmsg = 'ERRor: PARINFO parameter limits are not consistent'
          goto, TERminATE
      endif


      ;; Transfer structure values to local variables
      qulim = limited(1, ifree)
      ulim  = limits (1, ifree)
      qllim = limited(0, ifree)
      llim  = limits (0, ifree)

      wh = where(qulim or qllim, ct)
      if ct gt 0 then qanylim = 1 else qanylim = 0

  endif else begin

      ;; Fill in local variables with dummy values
      qulim = lonarr(n_elements(ifree))
      ulim  = x * 0.
      qllim = qulim
      llim  = x * 0.
      qanylim = 0

  endelse

  n = n_elements(x)
  if n_elements(maxiter) eq 0 then maxiter = 200L

  ;; Check input parameters for errors
  if (n le 0) or (ftol le 0) or (xtol le 0) or (gtol le 0) $
    or (maxiter le 0) or (factor le 0) then begin
      errmsg = 'ERRor: input keywords are inconsistent'
      goto, TERminATE
  endif

  if keyword_set(rescale) then begin
      errmsg = 'ERRor: DIAG parameter scales are inconsistent'
      if n_elements(diag) lt n then goto, TERminATE
      wh = where(diag le 0, ct)
      if ct gt 0 then goto, TERminATE
      errmsg = ''
  endif

  common mpfit_error, mperr

  mperr = 0
  catch_msg = 'calling '+fcn
  fvec = mpfit_call(fcn, xnew, _EXTRA=fcnargs)
  iflag = mperr
  if iflag lt 0 then begin
      errmsg = 'ERRor: first call to "'+fcn+'" failed'
      goto, TERminATE
  endif

  catch_msg = 'calling MPFIT_setMACHAR'
  sz = size(fvec(0))
  isdouble = (sz(sz(0)+1) eq 5)

  common mpfit_machar, machvals
  mpfit_setmachar, double=isdouble

  common mpfit_profile, profvals
;  prof_start = systime(1)

  MACHEP0 = machvals.machep
  DWARF   = machvals.minnum

  szx = size(x)
  ;; The parameters and the squared deviations should have the same
  ;; type.  Otherwise the MACHAR-based evaluation will fail.
  catch_msg = 'checking parameter data'
  tp = szx(szx(0)+1)
  if tp ne 4 and tp ne 5 then begin
      if not keyword_set(quiet) then begin
          message, 'WARNING: input parameters must be at least float', /info
          message, '         (converting parameters to float)', /info
      endif
      x = float(x)
      xnew = float(x)
      szx = size(x)
  endif
  if isdouble and tp ne 5 then begin
      if not keyword_set(quiet) then begin
          message, 'WARNING: data is doUBle but parameters are float', /info
          message, '         (converting parameters to doUBle)', /info
      endif
      x = double(x)
      xnew = double(xnew)
  endif

  m = n_elements(fvec)
  if (m lt n) then begin
      errmsg = 'ERRor: number of parameters must not exceed data'
      goto, TERminATE
  endif

  fnorm = mpfit_enorm(fvec)

  ;; Initialize Levelberg-Marquardt parameter and iteration counter

  par = zero
  iter = 1L
  qtf = x * 0.

  ;; Beginning of the outer loop

  OUTER_LOOP:

  ;; If requested, call fcn to enable printing of iterates
  xnew(ifree) = x
  if qanytied then mpfit_tie, xnew, ptied

  if nprint gt 0 and iterproc ne '' then begin
      catch_msg = 'calling '+iterproc
      iflag = 0L
      if (iter-1) MOD nprint eq 0 then begin
          mperr = 0
          xnew0 = xnew

          call_procedure, iterproc, fcn, xnew, iter, fnorm^2, $
            FUNCTARGS=fcnargs, parinfo=parinfo, quiet=quiet, _EXTRA=iterargs
          iflag = mperr

          ;; Check for user termination
          if iflag lt 0 then begin
              errmsg = 'WARNING: premature termination by "'+iterproc+'"'
              goto, TERminATE
          endif

          ;; If parameters were changed (grrr..) then re-tie
          if max(abs(xnew0-xnew)) gt 0 then begin
              if qanytied then mpfit_tie, xnew, ptied
              x = xnew(ifree)
          endif

      endif
  endif

  ;; Calculate the jacobian matrix
  iflag = 2
  catch_msg = 'calling MPFIT_FDJAC2'
  fjac = mpfit_fdjac2(fcn, x, fvec, step, qulim, ulim, dside, $
                      iflag=iflag, epsfcn=epsfcn, $
                      autoderiv=autoderiv, dstep=dstep, $
                      FUNCTARGS=fcnargs, ifree=ifree, xall=xnew)
  if iflag lt 0 then begin
      errmsg = 'WARNING: premature termination by FDJAC2'
      goto, TERminATE
  endif

  ;; Rescale the residuals and gradient, for use with "alternative"
  ;; statistics such as the Cash statistic.
  catch_msg = 'prescaling residuals and gradient'
  if n_elements(scalfcn) gt 0 then begin
      call_procedure, strtrim(scalfcn(0),2), fvec, fjac
  endif

  ;; Determine if any of the parameters are pegged at the limits
  if qanylim then begin
      catch_msg = 'zeroing derivatives of pegged parameters'
      whlpeg = where(qllim and (x eq llim), nlpeg)
      whupeg = where(qulim and (x eq ulim), nupeg)

      ;; See if any "pegged" values should keep their derivatives
      if (nlpeg gt 0) then begin
          ;; Total derivative of sum wrt lower pegged parameters
          for i = 0L, nlpeg-1 do begin
              sum = total(fvec * fjac(*,whlpeg(i)))
              if sum gt 0 then fjac(*,whlpeg(i)) = 0
          endfor
      endif
      if (nupeg gt 0) then begin
          ;; Total derivative of sum wrt upper pegged parameters
          for i = 0L, nupeg-1 do begin
              sum = total(fvec * fjac(*,whupeg(i)))
              if sum lt 0 then fjac(*,whupeg(i)) = 0
          endfor
      endif
  endif

  ;; Compute the QR factorization of the jacobian
  catch_msg = 'calling MPFIT_QRFAC'
  mpfit_qrfac, fjac, ipvt, wa1, wa2, /pivot

  ;; On the first iteration if "diag" is unspecified, scale
  ;; according to the norms of the columns of the initial jacobian
  catch_msg = 'rescaling diagonal elements'
  if (iter eq 1) then begin

      if not keyword_set(rescale) or (n_elements(diag) lt n) then begin
          diag = wa2
          wh = where (diag eq 0, ct)
          if ct gt 0 then diag(wh) = one
      endif

      ;; On the first iteration, calculate the norm of the scaled x
      ;; and initialize the step bound delta
      wa3 = diag * x
      xnorm = mpfit_enorm(wa3)
      delta = factor*xnorm
      if delta eq zero then delta = zero + factor
  endif

  ;; Form (q transpose)*fvec and store the first n components in qtf
  catch_msg = 'forming (q transpose)*fvec'
  wa4 = fvec
  for j=0L, n-1 do begin
      lj = ipvt(j)
      temp3 = fjac(j,lj)
      if temp3 ne 0 then begin
          fj = fjac(j:*,lj)
          wj = wa4(j:*)
          ;; *** optimization wa4(j:*)
          wa4(j) = wj - fj * total(fj*wj) / temp3
      endif
      fjac(j,lj) = wa1(j)
      qtf(j) = wa4(j)
  endfor
  ;; From this point on, only the square matrix, consisting of the
  ;; triangle of R, is needed.
  fjac = fjac(0:n-1, 0:n-1)
  fjac = reform(fjac, n, n, /overwrite)
  fjac = fjac(*, ipvt)
  fjac = reform(fjac, n, n, /overwrite)

  ;; Check for overflow.  This should be a cheap test here since FJAC
  ;; has been reduced to a (small) square matrix, and the test is
  ;; O(N^2).
  wh = where(finite(fjac) eq 0, ct)
  if ct gt 0 then goto, FAIL_OVERFLOW

  ;; Compute the norm of the scaled gradient
  catch_msg = 'computing the scaled gradient'
  gnorm = zero
  if fnorm ne 0 then begin
      for j=0L, n-1 do begin
          l = ipvt(j)
          if wa2(l) ne 0 then begin
              sum = total(fjac(0:j,j)*qtf(0:j))/fnorm
              gnorm = max([gnorm,abs(sum/wa2(l))])
          endif
      endfor
  endif

  ;; Test for convergence of the gradient norm
  if gnorm le gtol then info = 4
  if info ne 0 then goto, TERminATE

  ;; Rescale if necessary
  if not keyword_set(rescale) then $
    diag = diag > wa2

  ;; Beginning of the inner loop
  INneR_LOOP:

  ;; Determine the levenberg-marquardt parameter
  catch_msg = 'calculating LM parameter (MPFIT_LMPAR)'
  par = mpfit_lmpar(fjac, ipvt, diag, qtf, delta, wa1, wa2, par=par)

  ;; Store the direction p and x+p. Calculate the norm of p
  wa1 = -wa1

  if qanylim eq 0 and qminmax eq 0 then begin
      ;; No parameter limits, so just move to new position WA2
      alpha = one
      wa2 = x + wa1

  endif else begin

      ;; Respect the limits.  If a step were to go out of bounds, then
      ;; we should take a step in the same direction but shorter distance.
      ;; The step should take us right to the limit in that case.
      alpha = one

      if qanylim eq 1 then begin
          ;; Do not allow any steps out of bounds
          catch_msg = 'checking for a step out of bounds'
          if nlpeg gt 0 then wa1(whlpeg) = wa1(whlpeg) > 0
          if nupeg gt 0 then wa1(whupeg) = wa1(whupeg) < 0

          dwa1 = abs(wa1) gt MACHEP0
          whl = where(dwa1 and qllim and (x + wa1 lt llim), lct)
          if lct gt 0 then $
            alpha = min([alpha, (llim(whl)-x(whl))/wa1(whl)])
          whu = where(dwa1 and qulim and (x + wa1 gt ulim), uct)
          if uct gt 0 then $
            alpha = min([alpha, (ulim(whu)-x(whu))/wa1(whu)])
      endif

      ;; Obey any max step values.

      if qminmax eq 1 then begin
          nwa1 = wa1 * alpha
          whmax = where(qmax and maxstep gt 0, ct)
          if ct gt 0 then begin
              mrat = max(nwa1(whmax)/maxstep(whmax))
              if mrat gt 1 then alpha = alpha / mrat
          endif
      endif

      ;; Scale the resulting vector
      wa1 = wa1 * alpha
      wa2 = x + wa1

      ;; Adjust the final output values.  If the step put us exactly
      ;; on a boundary, make sure it is exact.
      wh = where(qulim and wa2 ge ulim*(1-MACHEP0), ct)
      if ct gt 0 then wa2(wh) = ulim(wh)

      wh = where(qllim and wa2 le llim*(1+MACHEP0), ct)
      if ct gt 0 then wa2(wh) = llim(wh)
  endelse

  wa3 = diag * wa1
  pnorm = mpfit_enorm(wa3)

  ;; On the first iteration, adjust the initial step bound
  if iter eq 1 then delta = min([delta,pnorm])

  ;; Evaluate the function at x+p and calculate its norm
  mperr = 0
  xnew(ifree) = wa2
  catch_msg = 'calling '+fcn
  wa4 = mpfit_call(fcn, xnew, _EXTRA=fcnargs)
  iflag = mperr
  if iflag lt 0 then begin
      errmsg = 'WARNING: premature termination by "'+fcn+'"'
      goto, TERminATE
  endif
  fnorm1 = mpfit_enorm(wa4)

  ;; Compute the scaled actual reduction
  catch_msg = 'computing convergence criteria'
  actred = -one
  if 0.1D * fnorm1 lt fnorm then actred = - (fnorm1/fnorm)^2 + 1.

  ;; Compute the scaled predicted reduction and the scaled directional
  ;; derivative
  for j = 0L, n-1 do begin
      wa3(j) = 0
      wa3(0:j) = wa3(0:j) + fjac(0:j,j)*wa1(ipvt(j))
  endfor

  ;; Remember, alpha is the fraction of the full LM step actually
  ;; taken
  temp1 = mpfit_enorm(alpha*wa3)/fnorm
  temp2 = (sqrt(alpha*par)*pnorm)/fnorm
  half  = zero + 0.5
  prered = temp1*temp1 + (temp2*temp2)/half
  dirder = -(temp1*temp1 + temp2*temp2)

  ;; Compute the ratio of the actual to the predicted reduction.
  ratio = zero
  tenth = zero + 0.1
  if prered ne 0 then ratio = actred/prered

  ;; Update the step bound
  if ratio le 0.25D then begin
      if actred ge 0 then temp = half $
      else temp = half*dirder/(dirder + half*actred)
      if ((0.1D*fnorm1) ge fnorm) or (temp lt 0.1D) then temp = tenth
      delta = temp*min([delta,pnorm/tenth])
      par = par/temp
  endif else begin
      if (par eq 0) or (ratio ge 0.75) then begin
          delta = pnorm/half
          par = half*par
      endif
  endelse

  ;; Test for successful iteration
  if ratio ge 0.0001 then begin
      ;; Successful iteration.  Update x, fvec, and their norms
      x = wa2
      wa2 = diag * x

      fvec = wa4
      xnorm = mpfit_enorm(wa2)
      fnorm = fnorm1
      iter = iter + 1
  endif

  ;; Tests for convergence
  if (abs(actred) le ftol) and (prered le ftol) $
    and  (0.5D * ratio le 1) then info = 1
  if delta le xtol*xnorm then info = 2
  if (abs(actred) le ftol) and (prered le ftol) $
    and (0.5D * ratio le 1) and (info eq 2) then info = 3
  if info ne 0 then goto, TERminATE

  ;; Tests for termination and stringent tolerances
  if iter ge maxiter then info = 5
  if (abs(actred) le MACHEP0) and (prered le MACHEP0) $
    and (0.5*ratio le 1) then info = 6
  if delta le MACHEP0*xnorm then info = 7
  if gnorm le MACHEP0 then info = 8
  if info ne 0 then goto, TERminATE

  ;; End of inner loop. Repeat if iteration unsuccessful
  if ratio lt 0.0001 then begin
      goto, INneR_LOOP
  endif

  ;; Check for over/underflow
  wh = where(finite(wa1) eq 0 or finite(wa2) eq 0 or finite(x) eq 0, ct)
  if ct gt 0 or finite(ratio) eq 0 then begin
      FAIL_OVERFLOW:
      errmsg = ('ERRor: parameter or function value(s) have become '+$
                'infinite; check model function for over- '+$
                'and underflow')
      info = -16
      goto, TERminATE
  endif

  ;; End of outer loop.
  goto, OUTER_LOOP

TERminATE:
  catch_msg = 'in the termination phase'
  ;; Termination, either normal or user imposed.
  if iflag lt 0 then info = iflag
  iflag = 0
  if n_elements(ifree) eq 0 then xnew = xall else xnew(ifree) = x
  if nprint gt 0 and info gt 0 then begin
      catch_msg = 'calling '+fcn
      fvec = mpfit_call(fcn, xnew, _EXTRA=fcnargs)
      catch_msg = 'in the termination phase'
      fnorm = mpfit_enorm(fvec)
  endif

  if n_elements(fnorm) gt 0 and n_elements(fnorm1) gt 0 then begin
      fnorm = max([fnorm, fnorm1])
      fnorm = fnorm^2.
  endif

  covar = !values.d_nan
  ;; (very carefully) set the covariance matrix COVAR
  if info gt 0 and not keyword_set(nocovar) $
    and n_elements(n) gt 0 and n_elements(fvec) gt 0 $
    and n_elements(fjac) gt 0 and n_elements(ipvt) gt 0 then begin
      sz = size(fjac)
      if n gt 0 and sz(0) gt 1 and sz(1) ge n and sz(2) ge n $
        and n_elements(ipvt) ge n then begin
          catch_msg = 'computing the covariance matrix'
          if n eq 1 then $
            cv = mpfit_covar(reform([fjac(0,0)],1,1), ipvt(0)) $
          else $
            cv = mpfit_covar(fjac(0:n-1,0:n-1), ipvt(0:n-1))
          cv = reform(cv, n, n, /overwrite)
          nn = n_elements(xall)

          ;; Fill in actual covariance matrix, accounting for fixed
          ;; parameters.
          covar = replicate(zero, nn, nn)
          for i = 0L, n-1 do begin
              covar(ifree, ifree(i)) = cv(*,i)
          end

          ;; Compute errors in parameters
          catch_msg = 'computing parameter errors'
          i = lindgen(nn)
          perror = replicate(covar(0)*0., nn)
          wh = where(covar(i,i) ge 0, ct)
          if ct gt 0 then $
            perror(wh) = sqrt(covar(wh, wh))
      endif
  endif
;  catch_msg = 'returning the result'
;  profvals.mpfit = profvals.mpfit + (systime(1) - prof_start)

  nfev = mpconfig.nfev
  return, xnew
end

; ** ********************************************************************* ** ;
; ** ********************************************************************* ** ;
; ** 						end of MPFIT								   ** ;
; ** ********************************************************************* ** ;
; ** ********************************************************************* ** ;

; ** **************************************************************
function bose,xinput
; ** **************************************************************
; ** Calculate the bose factor
bosex=xinput/(1-exp(-1.*xinput*11.6045/300))
bosex(where(abs(xinput) le 1e-5))=1.
return, bosex
end
; ** **************************************************************
function fungdos_to_fit,xinput,sf_new_values
; ** **************************************************************
; On construit la fonction mais on ne convolue pas ...
common sc_lorgau
;sf_sp_num_min=workspace number min to fit
;sf_sp_num_max=workspace number max to fit ... not for the moment

; ** Expression of the S(Q,w) with a gdos=soundv*w^dimsys
w_2_fit=xinput*0.
soundv=sf_new_values(0)
dimsys=sf_new_values(1)
;w_2_fit=(bose(xinput)/xinput^2)*soundv*(abs(xinput))^dimsys
w_2_fit=(bose(xinput)/xinput^2)*soundv*(abs(xinput))^dimsys
w_2_fit(where(abs(w_2_fit) ge 1e10))=0.
; ** Now deal with the elastic line
dcenter=sf_new_values(2)
x_interp=xinput+dcenter
w_resc2_interp=spline(x_interp,w_resc2,xinput)
dint=sf_new_values(3)
w_2_fit=w_2_fit+dint*w_resc2_interp
; ** Now deal with the background
slope=sf_new_values(4)
const=sf_new_values(5)
w_2_fit=w_2_fit+slope*xinput+const

return,w_2_fit

end
; ** **************************************************************
; ** **************************************************************
function qlorentz,xinput,p1,p2,p3
; ** **************************************************************
; ** p1=position p2=FWHM p3=intensity
; ** lorentz(x)=(2*P2*P3*/PI)*(4*(x-P1)^2+P2^2)
;
; ** Compute the function first
qlor=(2*p2*p3/!PI)/(4*(xinput-p1)^2+p2^2)
if abs(p2) le 1e-20 then print,'Probleme p2'
; ** Compute the derivatives after
;dlor_dp1=(-16*p3*p2/!PI)/(4*(xinput-p1)^2+p2^2)^2
;dlor_dp2=(2*p3/!PI)*(1/(4*(xinput-p1)^2+p2^2)-p2/(4*(xinput-p1)^2+2*p2))
;dlor_dp3=(2*p2/!PI)/(4*(xinput-p1)^2+p2^2)
;return,[lor,dlor_dp1,dlor_dp2,dlor_dp3]

return,[qlor]
end

; ** **************************************************************
function qgauss,xinput,p1,p2,p3
; ** **************************************************************
; ** p1=position p2=FWHM p3=intensity
; ** gauss(x)=(P3/(p2'*sqrt(PI))*EXP(-(x-P1)^2/(P2')^2)
; ** in that case, P2'=P2/(2sqrt(Ln(2))) is not the FWHM
;
; ** Compute the function first
p2p=p2/(2.*alog(2.))
if abs(p2p) le 1e-20 then print,'Probleme p2p'
qgau=(p3/p2p/sqrt(!PI))*EXP(-((xinput-p1)^2)/(p2p^2))
; ** Compute the derivatives
;dgau_dp1=2.*p3*(xinput-p1)*EXP(-(xinput-p1)^2/(p2p^2))/(p2p*sqrt(!PI))
;dgau_dp2=p3*EXP(-((xinput-p1)^2)/(p2p^2))/(2.*p2p*sqrt(!PI*alog(2.)))
;dgau_dp2=dgau_dp2*(-1.+2*(xinput-p1)^2/p2p^2)
;dgau_dp3=EXP(-((xinput-p1)^2)/(p2p^2))/(p2p*sqrt(!PI))
;return,[gau,dgau_dp1,dgau_dp2,dgau_dp3]

return,[qgau]
end

; ** **************************************************************************
function convs,w_to_conv,ispe,falpha,fbeta,w_resc2
; ** **************************************************************************
; ** This program performs the numerical convolution of an input theoretical
; ** vector w_to conv with the resolution function as treated by the preceding
; ** program
; ** [sx_in(nx1),sx_in(nx2)]=fitting range
; ** treatment for spectrum ispmin to ispmax but for the moment, full treatment e.g
; ** ispmin and ispmax=(size(w_res))(2) (nspectra)

common sc_loc
common sc_fit
common sc_convs

on_error,0
sz=size(w_resc) & ns_x=sz(1)-1
nx2=n_elements(w_to_conv)
w_to_fit=w_to_conv*0.0
deltaw=sx_in(1)-sx_in(0) & test=sx_res(1)-sx_res(0)
if deltaw ne test then treat_resf
test=sx_res(1)-sx_res(0)
if deltaw ne test then message,'PROBLEM ! Resf and input not the same binning in energy'
for i=0,nx2-1 do begin
	val_conv=0.
	for j=i-lmax(ispe-1),i+lmin(ispe-1) do begin
	if j lt 0 or j gt nx2-1 then continue
	k=i-j+cel(ispe-1)-falpha & if k lt 0 or k gt nx2-1 then message,'PROBLEM in convolution ! '
	val_conv=val_conv+w_resc2(i-j+cel(ispe-1)-falpha,ispe-1)*w_to_conv(j)
	endfor
	w_to_fit(i)=val_conv*deltaw
endfor
return, w_to_fit
end

; ** **************************************************************
function funlg_to_fit,xinput,sf_new,betatemp
; ** **************************************************************
; On construit la fonction mais on ne convolue pas ...
common sc_loc
common sc_lorgau
common sc_convs
;sf_sp_num_min=workspace number min to fit
;sf_sp_num_max=workspace number max to fit ... not for the moment

;stop

num_lor=n_l & num_gau=n_g
w_resc2_ispe=w_resc2(*,num_spectrum-1)
aamin=aalpha(num_spectrum-1) & aamax=abeta(num_spectrum-1) & ess_conv=w_resc(aamin:aamax,num_spectrum-1)
;ess_conv=w_resc2_ispe(aalpha(num_spectrum-1):abeta(num_spectrum-1))

w_to_conv=xinput*0. & w_to_convl=w_to_conv & w_to_convg=w_to_conv
arr_index=fltarr(1)
if num_lor gt 0 then begin
for il=0,num_lor-1 do begin
;lor_to_add=lorentz(xinput,sf_new_values(ispecc-1,il*3),sf_new_values(ispecc-1,il*3+1),sf_new_values(ispecc-1,il*3+2))
lor_to_add=qlorentz(xinput,sf_new(il*3),sf_new(il*3+1),sf_new(il*3+2))
ind_lor_0=where(lor_to_add ge 1e-10,nn)
if nn le 7 then begin
lor_to_add=0.
arr_index=[arr_index,il*3]
endif
w_to_convl=w_to_convl+lor_to_add;lorentz(xinput,sf_new_values(il*3),sf_new_values(il*3+1),sf_new_values(il*3+2))
endfor
endif
igminind=num_lor*3
if num_gau gt 0 then begin
for ig=0,num_gau-1 do begin
;gau_to_add=gauss(xinput,sf_new_values(ispecc-1,igminind+ig*3),sf_new_values(ispecc-1,igminind+ig*3+1),sf_new_values(ispecc-1,igminind+ig*3+2))
gau_to_add=qgauss(xinput,sf_new(igminind+ig*3),sf_new(igminind+ig*3+1),sf_new(igminind+ig*3+2))
ind_gau_0=where(gau_to_add ge 1e-10,nn)
if nn le 7 then begin
gau_to_add=0.
arr_index=[arr_index,igminind+ig*3]
endif
w_to_convg=w_to_convg+gau_to_add;gauss(xinput,sf_new_values(igminind+ig*3),sf_new_values(igminind+ig*3+1),sf_new_values(igminind+ig*3+2))
endfor
endif
; ** Now sum the Lorentzian and Gaussian parts
w_to_conv=w_to_convl+w_to_convg

; ** Now do the convolution with the Resolution function (to be done)
;w_2_fit=convs(w_to_conv,num_spectrum,falpha,fbeta,w_resc2)
;stop
w_2_fit=convol(w_to_conv,ess_conv,total(ess_conv),/edge_truncate);,center=0)
; ** Now deal with the background
;slope=sf_new_values(ispecc-1,3*num_lor+3*num_gau+2)
slope=sf_new(3*num_lor+3*num_gau+2)
;const=sf_new_values(ispecc-1,3*num_lor+3*num_gau+3)
const=sf_new(3*num_lor+3*num_gau+3)
w_2_fit=w_2_fit+slope*xinput+const
; ** ... and multiply by the temperature factor in case of symetric data (detailed balance factor)
w_2_fit=w_2_fit*exp(betatemp*xinput)

if n_elements(arr_index) gt 1 then begin
; ** Now deal with the line too sharp that we change in res. function
for ic=1,n_elements(arr_index)-1 do begin
;dcenter=sf_new_values(ispecc-1,arr_index(ic)) & dint=sf_new_values(ispecc-1,arr_index(ic)+2) & x_interp=xinput+dcenter
dcenter=sf_new(arr_index(ic)) & dint=sf_new(arr_index(ic)+2) & x_interp=xinput+dcenter
w_2_fit=w_2_fit+dint*spline(x_interp,w_resc2_ispe,xinput)
endfor
endif

; ** Now deal with the elastic line
;dcenter=sf_new_values(ispecc-1,3*num_lor+3*num_gau)
dcenter=sf_new(3*num_lor+3*num_gau)
x_interp=xinput+dcenter
w_resc2_interp=spline(x_interp,w_resc2_ispe,xinput)

;dint=sf_new_values(ispecc-1,3*num_lor+3*num_gau+1)
dint=sf_new(3*num_lor+3*num_gau+1)
w_2_fit=w_2_fit+dint*w_resc2_interp
;
return,w_2_fit

end

; ** **************************************************************
function myfunc,P,XVAL=X,YVAL=Y,ERRVAL=E
; ** **************************************************************
;P=sf_new_values : values of the fit parameters
;X=x values constrainted by the fit range ... Y=corresponding Y values
;and E=corresponding errors
;model=returned array containing the model values
common sc_fit
case name_func of
	'LOR_GAU':model=funlg_to_fit(X,P,betatemp)
	'GDOS':model=fungdos_to_fit(X,P,betatemp)
else:stop
endcase
return,(Y-model)/E

end
; ** **************************************************************
pro prepare_2_fit,X,Y,E,parinfo
; ** **************************************************************
; ** Called when the FIT button is pressed
; ** Call back the new values of the fit parameters as well as
; ** other necessary features

common sc_wid
common sc_loc
common sc_flag
common sc_fit
common sc_lorgau

; ** parinfo is the structure containing the indication on how the fit has to be performed
case name_func of
	'LOR_GAU':begin
	 parinfo=replicate({value:0.,fixed:0},3*num_lor+3*num_gau+4)
	 fact=3
	 end
	'GDOS':begin
	 parinfo=replicate({value:0.,fixed:0},2*num_lor+4)
	 fact=2
	 end
else:stop
endcase
;
iprint=0
sf_last_values(num_spectrum-1,*)=sf_new_values(num_spectrum-1,*)	; new values become last values

if not flag_all then begin
for i=0,fact*num_lor+fact*num_gau+4-1 do begin 	; sf_new_values must read the new value of the parameters
widget_control,sw_tx_balg2(3*i),get_value=str_balg2 & flt_balg2=float(strcompress(str_balg2,/remove_all)) & flt_balg2=flt_balg2(0)
sf_new_values(num_spectrum-1,i)=flt_balg2
endfor
endif else begin
if flag_reverse then iadprev=1 else iadprev=-1
num_prev=num_spectrum+1*iadprev
if not((num_prev lt 1 or num_prev gt nb_spe_tot) or (not flag_fromlast)) then begin
sf_new_values(num_spectrum-1,*)=sf_new_values(num_prev-1,*)
sf_index(num_spectrum-1,*)=sf_index(num_prev-1,*)
endif
endelse

;
sf_param_to_fit=intarr(3*num_lor+3*num_gau+4)
sf_param_to_fit(*)=abs(not sf_index(num_spectrum-1,*)) 	; contains the indices of the parameters to be fitted
;
; ** Now deal with the string to float or integer transformation (see "plot it")
widget_control,sw_tx_fxmin,get_value=s & sf_xmin=float(strcompress(s,/remove_all)) & sf_xmin=sf_xmin(0) ;x min fit range
widget_control,sw_tx_fxmax,get_value=s & sf_xmax=float(strcompress(s,/remove_all)) & sf_xmax=sf_xmax(0) ;x max fit range

;if ((sf_sp_num_min le 0) or (sf_sp_num_min gt nb_spe_tot)) then sf_sp_num_min=1
widget_control,sw_tx_cycl,get_value=s & sf_iter_num=fix(strcompress(s,/remove_all)) & sf_iter_num=sf_iter_num(0) ;number of iteration maximum to perform

rin=where((sx_in le sf_xmax) and (sx_in ge sf_xmin),npt,complement=out)
falpha=rin(0) & fbeta=rin(npt-1)
if sf_xmax le sf_xmin then begin $
print, "PROBLEM: Fitting range, sf_xmin not lt sf_xmax !!"
stop
endif
if iprint ne 0 then begin
print,"falpha=",falpha,"fbeta=",fbeta
print,"rin=",rin
print,"out=",out
endif
w_resc2=w_resc(falpha:fbeta,*)
X=sx_in(rin) & Y=sw_in(rin,num_spectrum-1)
E=se_in(rin,num_spectrum-1) & rrr=where(E le 0.) & if rrr(0) ne -1 then E(rrr)=100000
sf_w_fitted(*,num_spectrum-1)=0
;ispecc=sf_sp_num_min;-1
sf_new=fltarr(3*num_lor+3*num_gau+4) & sf_new(*)=sf_new_values(num_spectrum-1,*)
sf_e=fltarr(3*num_lor+3*num_gau+4) & sf_e(*)=sf_e_values(num_spectrum-1,*)
parinfo.value=sf_new
;parinfo.value=sf_new_values(num_spectrum,*)
parinfo.fixed=sf_param_to_fit
end


; ** **************************************************************
pro qens_fit_event, Event
; ** **************************************************************
; ** Called when a qens_fit event is generated

@lamp.cbk
common c_trap, trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current
common sc_wid
common sc_loc
common sc_flag
common sc_fit
common sc_lorgau
common sc_convs
stat=0
;catch,stat

;if stat  ne 0  then begin
;		catch,/cancel
; 		set_plot,my_path(3);
;		widget_control, bad_id=i, gw_err_lab, Set_Value=!err_string
;		return & endif
;error_msg, 0				; Clear error msg fields
;print,'On est dans qens_fit'
if  TAG_NAMES(Event,/structure_name) ne 'widget_draw' then $
	widget_control, bad_id=i, Event.Id, /Hourglass
	widget_control, bad_id=i, Event.Id, geT_uvalue=Ev, Get_Value=value
no_plot=0

case Ev(0) of
    0:case Ev(1) of									;Menu Event
          0:case Ev(2) of
                0:widget_control,event.top,/DESTROY ;Exit Event
                1:begin                                                     ;Save Curves Event
                save_file=dialog_pickfile(file='fit_curves.sav',filter='*.sav',title='Save in File:')
                openw,unit,save_file,/get_lun
                ts=transpose(sf_sub_function)
                f2print=reform(sf_w_fitted,1,nb_channels,nb_spe_tot)
                dat2print=reform(sw_in,1,nb_channels,nb_spe_tot)
                err2print=reform(se_in,1,nb_channels,nb_spe_tot)
                x2print=sx_in & for i=0,nb_spe_tot-2 do x2print=[x2print,sx_in] & x2print=reform(x2print,1,nb_channels,nb_spe_tot)
                w2print=[x2print,dat2print,err2print,f2print,ts]
                nsub=size(w2print) & nsub2=nsub(1) & iformat=nsub2
                sformat=strcompress(string(iformat),/remove_all)
                sformat='('+sformat+'(G15.8,1X))'
                strdate=systime(0)
                  case name_func of
                    'LOR_GAU':str2add='Data were fitted using a set of independent Lorentzian and Gaussian functions'
                    else: str2add='Model non existent'
                  endcase  
                printf,unit,'File printed on '+strdate   
                printf,unit,str2add
                printf,unit,'--------------------------------------------------',format='(a50)'                
                for isp=0,nb_spe_tot-1 do begin
                printf,unit,'# Spectrum# ',isp+1,'Angle=',sy_in(isp)
                printf,unit,w2print(*,*,isp),format=sformat
                printf,unit,'##################################################',format='(a50)'
                endfor
                free_lun,unit        
                end
                2:begin
                save_file=dialog_pickfile(file='fit_param.sav',filter='*.sav',title='Save in File:')
                openw,unit,save_file,/get_lun
                openw,unit2,'logfile.sav',/get_lun
                strdate=systime(0)
                  case name_func of
                    'LOR_GAU':str2add='Data were fitted using a set of independent Lorentzian and Gaussian functions'
                    else: str2add='Model non existent'
                  endcase  
                printf,unit,'File printed on '+strdate &  printf,unit2,'# '+'File printed on '+strdate 
                printf,unit,str2add       & printf,unit2,'# '+name_func
                printf,unit,'--------------------------------------------------',format='(a50)'
                printf,unit2,'# '+'--------------------------------------------------'
                for isp=0,nb_spe_tot-1 do begin
                printf,unit,'# Spectrum#',isp+1,'Angle=',sy_in(isp),'value of CHISQR=',chisqr(isp),format='(a,1x,i4,1x,a,1x,f7.3,3x,a,1x,f9.3)'
                for isub=0,n_elements(sf_functions_labels)-1 do begin
                printf,unit,sf_functions_labels(isub),sf_new_values(isp,isub),sf_errors_labels(isub),sf_e_values(isp,isub)$
                ,format='(a26,3x,E15.8,3x,a,3x,E15.8)'
                endfor
                printf,unit,'##################################################',format='(a50)'                
                endfor
                sformat=strcompress(string(2*n_elements(sf_functions_labels)+1),/remove_all) & sformat='('+sformat+'(G15.8,1X))'
                printf,unit2,[transpose(sy_in),transpose(sf_new_values),transpose(sf_e_values)],format=sformat               
                free_lun,unit
                free_lun,unit2                                      
                end
            endcase    
          1:case Ev(2) of
                0:fit_basegl							; Lor-Gauss fit
                1:							; Isotropic rotation
                2:							; Uniaxial rotation
                3:							; RW translation
                4:print,'Do nothing at the moment'
                ;fit_basegdos
            endcase
      endcase
    1:begin								; First Line Event
      case Ev(1) of
    	  1:begin ; Get data event
    		widget_control,sw_la_win,get_value=str_win
    		str_test=STRMID(str_win,1,STRleN(str_win)-1)
			sw_in=0 & sx_in=0 & sy_in=0 & se_in=0
			iii=EXECUTE("w_in=w"+str_test)
			iii=EXECUTE("x_in=x"+str_test)	;& sx_in=sx_in(*,0)
			iii=EXECUTE("y_in=y"+str_test)
			iii=EXECUTE("e_in=e"+str_test)
			sw_in=w_in & sx_in=x_in & sy_in=y_in & se_in=e_in
			sw_result=sw_in & sx_result=sx_in & sy_result=sy_in & se_result=se_in
			sz_win=size(w_in)
			case n_elements(sz_win) of
			4:begin
			nb_spe_tot=1 & nb_channels=sz_win(1)
			end
			5:begin
			nb_spe_tot=sz_win(2) & nb_channels=sz_win(1)
			end
			else:begin
			Print,"Problem in array dimension ... quiting"
			Return
			end
			endcase
			flag_lect_in=1
      widget_control,sw_tx_fxmin,set_value=strcompress(string(min(sx_in)),/remove_all)
      widget_control,sw_tx_fxmax,set_value=strcompress(string(max(sx_in)),/remove_all)
			sf_w_fitted=fltarr(nb_channels,nb_spe_tot) 	; the final fitted array has the same dimensions as the input array
			sf_w_exp_fit=sf_w_fitted					; the difference in between the experimental and the fitted curves
      widget_control,sw_bu_gres,sensitive=1
    		end
    	  2:begin
    		widget_control,sw_la_res,get_value=str_wres
    		str_test=strmid(str_wres,1,strlen(str_wres)-1)
			sw_res=0 & sx_res=0 & sy_res=0 & se_res=0
			iii=execute("w_in=w"+str_test)
			iii=execute("x_in=x"+str_test) ;& sx_res=sx_res(*,0)
			iii=execute("y_in=y"+str_test)
			iii=execute("e_in=e"+str_test)
			sw_res=w_in & sx_res=x_in & sy_res=y_in & se_res=e_in
			sz_wres=size(w_in)
			case n_elements(sz_wres) of
			4:begin
			test_spec=1 & test_ch=sz_wres(1)
			end
			5:begin
			test_spec=sz_wres(2) & test_ch=sz_wres(1)
			end
			else: begin
			Print,"Problem in array dimension ... quiting"
			Return
			end
			endcase
			if flag_lect_in and ((test_spec ne nb_spe_tot) or (test_ch ne nb_channels)) then begin
			print,'W_in and W_Res_function do not have the same dimensions' & stop
			endif
      widget_control,sw_bu_plot,sensitive=1
    		end
    	  3:begin							; Write Result event
    		widget_control,sw_la_wou,get_value=str_wou
    		str_test=strmid(str_wou,1,strlen(str_wou)-1)
			w_out=sw_result & x_out=sx_in & y_out=sy_in & e_out=0.;se_result
			iii=execute("w"+str_test+"=w_out")
			iii=execute("x"+str_test+"=x_out") ;& sx_res=sx_res(*,0)
			iii=execute("y"+str_test+"=y_out")
			iii=execute("e"+str_test+"=e_out")
    		end
       endcase
       end
    2:begin									; Second Line Event
      case Ev(1) of
    	  1:ids=sw_la_win				    ; concerns w_in
    	  2:ids=sw_la_res					; concerns w_res
    	  3:ids=sw_la_wou					; concerns w_ou
      endcase
      widget_control,ids,get_value=str_w_test
      ilen=strlen(str_w_test)
      str_w_test=strmid(str_w_test,1,ilen-1) & w_test=fix(str_w_test)
      case Ev(2) of
          1:begin							; w_test=w_test-1
          w_test=w_test-1
          if w_test lt 1 then w_test=20
          end
          2: ;Label ... no event should occur
          3:	begin							; w_test=w_test+1
          w_test=w_test+1
          if w_test gt 20 then w_test=1
          end
      endcase
      str_w_test='W'+string(w_test)
      str_w_test=strcompress(str_w_test,/remove_all)
      widget_control,ids,set_value=str_w_test
      end
    3:begin									; Mid Line Event
      case Ev(1) of
          1:								; Mid column1 Event
          2:begin
            case Ev(2) of
                1:begin           ; "Accept !" Event
                widget_control,sw_tx_wsub,get_value=str_wsub & str_wsub=strlowcase(strcompress(str_wsub,/remove_all))
                num_spectrum=1 & str_wsub=str_wsub(0)
                num_spectrum=fix(str_wsub) & num_spectrum=num_spectrum(0)
                if ((num_spectrum le 0) or (num_spectrum gt nb_spe_tot)) then begin
                print,'ERROR num_spectrum out of range ... put it to 1'
                num_spectrum=1
                widget_control,sw_tx_wsub,set_value=' 1'
                endif
          		  plot_it
               if flag_pl_fit2 then begin
                fact=1
                ; Change the parameters text box values
                for i=0,fact*num_lor*3+fact*num_gau*3+4-1 do begin ; sf_new_values must print the new value of the parameters
                str_balg2=string(sf_new_values(num_spectrum-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
                widget_control,sw_tx_balg2(3*i),set_value=str_balg2,sensitive=sf_index(num_spectrum-1,i)
                str_balg2=string(sf_last_values(num_spectrum-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
                widget_control,sw_tx_balg2(3*i+1),set_value=str_balg2
                str_balg2=string(sf_e_values(num_spectrum-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
                widget_control,sw_tx_balg2(3*i+2),set_value=str_balg2
                widget_control,sw_bu_balg2(i),set_button=abs(not sf_index(num_spectrum-1,i))
                endfor                             
              endif
          		end
          		2:begin
          		flag_pl_res= not flag_pl_res
          		wset,s_pl_wid
          		plot_it
          		end
          		3:begin
          		flag_norm= not flag_norm
          		wset,s_pl_wid
          		plot_it
          		end
          	endcase
          	end
          3:	no_plot=1							; Mid column3 Event ... plot should not occur
      endcase
      end
    4:case Ev(2) of						; Bottom Event
          1:begin
          num_lor=0 & num_gau=0
          widget_control,sw_tx_nlor,get_value=str_numlor
          widget_control,sw_tx_ngau,get_value=str_numgau
          str_numlor=strcompress(str_numlor,/remove_all) & num_lor=fix(str_numlor) & num_lor=num_lor(0)
          str_numgau=strcompress(str_numgau,/remove_all) & num_gau=fix(str_numgau) & num_gau=num_gau(0)
          print,'num_lor=',num_lor
          print,'num_gau=',num_gau
          n_l=num_lor & n_g=num_gau
          if ((num_lor lt 0) or (num_lor gt 5)) then begin
          str_numlor='1'
          widget_control,sw_tx_nlor,set_value=str_numlor
          endif $
          else if ((num_gau lt 0) or (num_gau gt 5)) then begin
          str_numgau='1'
          widget_control,sw_tx_ngau,set_value=str_numgau
          endif else begin
          widget_control,bad_id=i,sw_ba_bot,/DESTROY
          Print,'Traitement Res function'
          widget_control,sw_tx_resmin,get_value=str_resmin
          str_test=strcompress(str_resmin,/remove_all) & rfmin=float(str_resmin) & rfmin=rfmin(0)
          widget_control,sw_tx_resmax,get_value=str_resmax
          str_test=strcompress(str_resmax,/remove_all) & rfmax=float(str_resmax) & rfmax=rfmax(0)
          treat_resf
          fit_basegl2
          endelse
          end							; Start Event
          2:widget_control,bad_id=i,sw_ba_bot,/DESTROY			; Done Event
      endcase
    6:case Ev(1) of
          0:begin
                widget_control,sw_tx_wsub,get_value=str_wsub & str_wsub=strlowcase(strcompress(str_wsub,/remove_all))
                num_spectrum=1 & str_wsub=str_wsub(0)
                num_spectrum=fix(str_wsub) & num_spectrum=num_spectrum(0)
                if ((num_spectrum le 0) or (num_spectrum gt nb_spe_tot)) then begin
                num_spectrum=1
                widget_control,sw_tx_wsub,set_value=' 1'
                endif
;                ispecc=num_spectrum
                betatemp=0
                if flag_sym then begin                
                widget_control,sw_tx_temp,get_value=str_temp & str_temp=strlowcase(strcompress(str_temp,/remove_all))
                str_temp=str_temp(0) & temp=float(str_temp) & temp=temp(0)
                if temp le 1.5 then begin
                widget_control,sw_tx_temp,set_value='0.',sensitive=0
                flag_sym=not flag_sym
                widget_control,sw_bu_symsqw,set_button=abs(flag_sym)
                endif else $
                betatemp=11.6045/temp
                endif
;                print,'betatemp=',betatemp,'temp-meV=',1./betatemp
          case Ev(2) of
                0:begin
                on_error,0
                ; ** THE TRY BUTTON HAS BEEN PRESSED !!!
                prepare_2_fit, X,Y,E,parinfo
                end               
                1:begin
                on_error,0
                ispecc_min=num_spectrum & ispecc_max=num_spectrum
                if flag_all then begin ispecc_min=1 & ispecc_max=nb_spe_tot & endif
                for ispecc=ispecc_min,ispecc_max do begin
                if not flag_reverse then num_spectrum=ispecc else num_spectrum=nb_spe_tot-ispecc+1
                print,"Treating spectrum# ", num_spectrum
                ; ** THE FIT BUTTON HAS BEEN PRESSED !!!
;                stop
                prepare_2_fit, X,Y,E,parinfo
                ; ** Now I can launch the fitting procedure
                sf_new=MPFIT('myfunc',sf_new,FUNCTARGS={XVAL:X,YVAL:Y,ERRVAL:E},$
                PERROR=sf_e,BESTNorM=chisq,PARINFO=parinfo)
                sf_new_values(num_spectrum-1,*)=sf_new(*)
                sf_e_values(num_spectrum-1,*)=sf_e(*)
                chisqr(num_spectrum-1)=chisq
                print,'For spectrum# ',num_spectrum,' CHISQ=',chisq
                endfor
                ispecc=ispecc-1
                end
           endcase
           
           ispecc_min=num_spectrum & ispecc_max=num_spectrum
           if flag_all then begin ispecc_min=1 & ispecc_max=nb_spe_tot & endif
           for ispect=ispecc_min-1,ispecc_max-1 do begin
;                if not flag_reverse then num_spectrum=ispect+1 else num_spectrum=nb_spe_tot-ispecc+1
                num_spectrum=ispect+1
                print,"calculating spectrum# ", num_spectrum
;                prepare_2_fit, X,Y,E,parinfo			  
           case name_func of
					  'LOR_GAU':begin
					  fact=1
        str_wsub=string(num_spectrum) & str_wsub=strcompress(str_wsub(0),/remove_all) & widget_control,sw_tx_wsub,set_value=str_wsub
        for i=0,fact*num_lor*3+fact*num_gau*3+4-1 do begin ; sf_new_values must print the new value of the parameters
        str_balg2=string(sf_new_values(ispect,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
        widget_control,sw_tx_balg2(3*i),set_value=str_balg2
        str_balg2=string(sf_last_values(ispect,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
        widget_control,sw_tx_balg2(3*i+1),set_value=str_balg2
        str_balg2=string(sf_e_values(ispect,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
        widget_control,sw_tx_balg2(3*i+2),set_value=str_balg2
        endfor
;        if flag_all then flag_all=not flag_all
;        prepare_2_fit,X,Y,E,parinfo
					  falpha=0 & fbeta=nb_channels-1
					  w_resc2=w_resc
					  w_resc2_ispe=w_resc2(*,ispect)
					  ;f_fitted=funlg_to_fit(sx_in,sf_new_values)
            f_fitted=funlg_to_fit(sx_in,sf_new_values(ispect,*),betatemp)
					  ; ** Calculate the fitted curve
            ;stop
					  sf_w_fitted(*,ispect)=f_fitted(*)
            sw_result=sf_w_fitted & widget_control,sw_bu_wwou,sensitive=1
					  ; ** Do the difference exp-th
					  sf_w_exp_fit(*,ispect)=sw_in(*,ispect)-sf_w_fitted(*,ispect)
            ;stop
					  ; ** Express the sub-functions
            
            w_resc2_ispe=w_resc2(*,num_spectrum-1)
            ess_conv=w_resc2_ispe(aalpha(num_spectrum-1):abeta(num_spectrum-1)+1)
					  
            dcenter=sf_new_values(ispect,3*num_lor+3*num_gau)
					  dint=sf_new_values(ispect,3*num_lor+3*num_gau+1)
					  sf_sub_function(ispect,*,num_lor+num_gau)=dint*w_resc2_interp(*)
					  slope=sf_new_values(ispect,3*num_lor+3*num_gau+2)
					  const=sf_new_values(ispect,3*num_lor+3*num_gau+3)
					  sf_sub_function(ispect,*,num_lor+num_gau+1)=slope*sx_in(*)+const
					  if num_lor gt 0 then begin
					  for i=0,num_lor-1 do begin
					  ll=qlorentz(sx_in,sf_new_values(ispect,i*3),sf_new_values(ispect,i*3+1),sf_new_values(ispect,i*3+2))
					  ind_lor_0=where(ll ge 1e-10,nn)
					  if nn le 7 then begin
					  dcenter=sf_new_values(ispect,i*3)
					  dint=sf_new_values(ispect,i*3+2) & x_interp=sx_in+dcenter
					  lll=dint*spline(x_interp,w_resc2_ispe,sx_in)
					  endif else lll=convol(ll,ess_conv,total(ess_conv),/edge_truncate);,center=0);lll=convs(ll,ispect+1,falpha,fbeta,w_resc2)
					  sf_sub_function(ispect,*,i)=lll(*)
					  endfor
					  endif
					  if num_gau gt 0 then begin
					  for i=0,num_gau-1 do begin
				    gg=qgauss(sx_in,sf_new_values(ispect,igminind+i*3),$
            sf_new_values(ispect,igminind+i*3+1),sf_new_values(ispect,igminind+i*3+2))
            ind_gau_0=where(gg ge 1e-10,nn)
					  if nn le 7 then begin
					  dcenter=sf_new_values(ispect,igminind+i*3)
					  dint=sf_new_values(ispect,igminind+i*3+2) & x_interp=sx_in+dcenter
					  ggg=dint*spline(x_interp,w_resc2_ispe,sx_in)
					  endif else ggg=convol(gg,ess_conv,total(ess_conv),/edge_truncate);,center=0);ggg=convs(gg,ispect+1,falpha,fbeta,w_resc2)
					  sf_sub_function(ispect,*,num_lor+i)=ggg(*)
					  endfor
					  endif
	;				convs(w_to_conv,ispecc,falpha,fbeta,w_resc2)
                 ; ** And I can write the results
;                 if ispecc_min ne ispecc_max then flag_all=not flag_all
        if not flag_pl_fit then flag_pl_fit=not flag_pl_fit
  				plot_it
					  end
					  else:print,'There is only one function at the moment'
				endcase
        endfor
;        stop
;				str_wsub=string(num_spectrum) & str_wsub=strcompress(str_wsub(0),/remove_all) & widget_control,sw_tx_wsub,set_value=str_wsub
;				if not flag_pl_fit then flag_pl_fit=not flag_pl_fit
;				plot_it
        
                ; ** And I can write the results
;        for i=0,fact*num_lor*3+fact*num_gau*3+4-1 do begin ; sf_new_values must print the new value of the parameters
;        str_balg2=string(sf_new_values(ispecc-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
;        widget_control,sw_tx_balg2(3*i),set_value=str_balg2
;        str_balg2=string(sf_last_values(ispecc-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
;        widget_control,sw_tx_balg2(3*i+1),set_value=str_balg2
;        str_balg2=string(sf_e_values(ispecc-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
;        widget_control,sw_tx_balg2(3*i+2),set_value=str_balg2
;        endfor
                end
          1:begin
                widget_control,sw_tx_wsub,get_value=str_wsub & str_wsub=strlowcase(strcompress(str_wsub,/remove_all))
                num_spectrum=1 & str_wsub=str_wsub(0)
                num_spectrum=fix(str_wsub) & num_spectrum=num_spectrum(0)
                if ((num_spectrum le 0) or (num_spectrum gt nb_spe_tot)) then begin
                num_spectrum=1
                widget_control,sw_tx_wsub,set_value=' 1'
                endif
            i2fix=Ev(2)
            sf_index(num_spectrum-1,i2fix)=not sf_index(num_spectrum-1,i2fix)
				    widget_control,sw_tx_balg2(i2fix*3),sensitive=sf_index(num_spectrum-1,i2fix)
            widget_control,sw_bu_balg2(i2fix),set_button=abs(not sf_index(num_spectrum-1,i2fix))
            end
          2:case Ev(2) of
                0:begin
                widget_control,bad_id=i,sw_ba_bot,/DESTROY			; Done Event
                widget_control,bad_id=i,sw_bu_fit,sensitive=0   ;Set the FIT button non-sensitive
                widget_control,bad_id=i,sw_bu_done,sensitive=0          ;Set the DONE button non-sensitive
                widget_control,sw_tx_resmin,sensitive=1         ;set the x res ranges actives
                widget_control,sw_tx_resmax,sensitive=1
                if flag_pl_fit then flag_pl_fit=not flag_pl_fit
                if flag_pl_fit2 then flag_pl_fit2=not flag_pl_fit2
                if flag_all then flag_all=not flag_all
                end
                1:begin                                         ;"Fit all Sp" check box 
                flag_all=not flag_all
                widget_control,sw_bu_try,sensitive=abs(not flag_all)
                widget_control,sw_bu_fitall,set_button=abs(flag_all)
                widget_control,sw_bu_acquire,sensitive=abs(not flag_all)
                widget_control,sw_tx_acquire,sensitive=abs(not flag_all)
                if not flag_all then begin
                flag_fromlast=0 & flag_reverse=0
                widget_control,sw_bu_reverse,set_button=0
                widget_control,sw_bu_fromlast,set_button=0
                endif
                widget_control,sw_bu_reverse,sensitive=abs(flag_all)
                widget_control,sw_bu_fromlast,sensitive=abs(flag_all)
                end
                2:begin                                              ;"Acquire from Sp#" button
                widget_control,sw_tx_acquire,get_value=str_acq & str_acq=strlowcase(strcompress(str_acq,/remove_all))
                i_acq=1 & str_acq=str_acq(0)
                i_acq=fix(str_acq) & i_acq=i_acq(0)
                if ((i_acq le 0) or (i_acq gt nb_spe_tot)) then begin
                print,'ERROR num_spectrum out of range ... put it to 1'
                i_acq=1
                widget_control,sw_tx_acquire,set_value=' 1'
                endif
                fact=1
                ; Change the parameters text box values
                for i=0,fact*num_lor*3+fact*num_gau*3+4-1 do begin ; sf_new_values must print the new value of the parameters
                str_balg2=string(sf_new_values(i_acq-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
                widget_control,sw_tx_balg2(3*i),set_value=str_balg2,sensitive=sf_index(i_acq-1,i)
                str_balg2=string(sf_last_values(i_acq-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
                widget_control,sw_tx_balg2(3*i+1),set_value=str_balg2
                str_balg2=string(sf_e_values(i_acq-1,i)) & str_balg2=strcompress(str_balg2(0),/remove_all)
                widget_control,sw_tx_balg2(3*i+2),set_value=str_balg2
                widget_control,sw_bu_balg2(i),set_button=abs(not sf_index(i_acq-1,i))
                sf_index(num_spectrum-1,i)=sf_index(i_acq-1,i)
                endfor           
                end
                3:begin
                flag_reverse=not flag_reverse
                widget_control,sw_bu_reverse,set_button=abs(flag_reverse)
                end
                4:begin
                flag_fromlast=not flag_fromlast
                widget_control,sw_bu_fromlast,set_button=abs(flag_fromlast)
                end
                5:begin
                flag_sym=not flag_sym
                widget_control,sw_bu_symsqw,set_button=abs(flag_sym)
                widget_control,sw_tx_temp,sensitive=abs(flag_sym)
                ;sw_bu_symsqw,sw_tx_temp
                end
                6:
         endcase        
      endcase
    else: error_msg, 5
endcase
end

; ** **************************************************************
pro plot_it
; ** **************************************************************
; ** This routine defines the plot-limits required
;
@lamp.cbk
common sc_wid
common sc_loc
common sc_flag
common sc_fit

; ** Getting the information about the desired plot (xrange,yrange and subplot)
widget_control,sw_tx_xmin,get_value=str_xmin & str_xmin=strlowcase(strcompress(str_xmin,/remove_all))
widget_control,sw_tx_xmax,get_value=str_xmax & str_xmax=strlowcase(strcompress(str_xmax,/remove_all))
widget_control,sw_tx_ymin,get_value=str_ymin & str_ymin=strlowcase(strcompress(str_ymin,/remove_all))
widget_control,sw_tx_ymax,get_value=str_ymax & str_ymax=strlowcase(strcompress(str_ymax,/remove_all))

; ** Some necessary transformation from string to float or integer
str_xmin=str_xmin(0) & str_ymin=str_ymin(0)
str_xmax=str_xmax(0) & str_ymax=str_ymax(0)

; ** Putting the arrays into one and two dimensional array
w_wk=sw_in(*,num_spectrum-1)
w_wkres=sw_res(*,num_spectrum-1)
w_wkfit=sf_w_fitted(*,num_spectrum-1)
d_wk=sf_w_exp_fit(*,num_spectrum-1)
; ** Normalize to elastic peak intensity when normalize button is pushed
if flag_norm then w_wkres=w_wkres/(max(w_wkres)/max(w_wk))
e_wk=se_in(*,num_spectrum-1)
x_to_plot=sx_in & sss=size(x_to_plot)
if sss(0) gt 1 then x_to_plot=x_to_plot(0)
; ** Checking the input value for errors
if str_xmin eq 'min' then xmin=min(sx_in) $
else begin
xmin=float(str_xmin) & xmin=xmin(0)
endelse
if str_xmax eq 'max' then xmax=max(sx_in) $
else begin
xmax=float(str_xmax) & xmax=xmax(0)
endelse
if str_ymin eq 'min' then ymin=min(w_wk)  $
else begin
ymin=float(str_ymin) & ymin=ymin(0)
endelse
if str_ymax eq 'max' then ymax=max(w_wk)  $
else begin
ymax=float(str_ymax) & ymax=ymax(0)
endelse
if ((xmin ge xmax) or (ymin ge ymax)) then begin
xmin=min(sx_in) & xmax=max(sx_in) & ymin=min(sy_in) & ymax=max(sy_in)
endif
;
; Adapt vector to current plot zone
;ind=where((sx_in ge xmin) and (sx_in le xmax) and (w_wk le ymax) and (w_wk ge ymin), nb_pt1) & ind=ind(0) ;last one to have a 1d vector
; do the plot
!x.title='Energy transfert (meV)'
!y.title='Intensity (a.u)'
widget_control, bad_id=i, sw_pl_area, get_value=s_pl_wid 	; Store the number assigned to this window in s_pl_wid
wset,s_pl_wid											                        ; Set the s_pl_wid active
plot,x_to_plot,w_wk,psym=7,xrange=[xmin,xmax],yrange=[ymin,ymax]
errlow=w_wk-e_wk & errhigh=w_wk+e_wk
indzero=where(errlow lt 0.) & if (indzero(0) ne -1) then errlow(indzero)=abs(10.*w_wk(indzero))
;errplot,x_to_plot,w_wk-e_wk,w_wk+e_wk
errplot,x_to_plot,errlow,errhigh;,color=1
if flag_pl_res then oplot,x_to_plot,w_wkres,color=1
if flag_pl_fit then begin
oplot,x_to_plot,w_wkfit,color=65,linestyle=0,thick=2
for i=0,num_lor+num_gau+1 do begin
oplot,x_to_plot,sf_sub_function(num_spectrum-1,*,i),color=11+i*10,linestyle=2,thick=2
endfor
widget_control, bad_id=i, sw_pl_areabot, get_value=s_pl_widbot 	; Store the number assigned to this window in s_pl_wid
wset,s_pl_widbot
!x.title=''
!y.title='e-t';!y.title='exp-th '
plot,x_to_plot,d_wk,xrange=[xmin,xmax],pos=[0.12,0.,0.97,1.]
wset,s_pl_wid
endif
;plot_flag=1
;trap_current=!D.window
return
end

; ** **************************************************************
pro fit_basegl
; ** **************************************************************
; ** create the base of the fitting routine
;
@lamp.cbk
common sc_wid
common sc_loc
common sc_flag
common sc_fit

;print,"ON Y EST ! C'est parti"

if (xregistered('fit_base') le 0) then begin

; ** Start definition of the widget object
; ** *************************************

; ** Base for the fit
sw_ba_bot=widget_base(group_leader=sw_ba_leader, /row, $
				Title='Choose the number of functions to use',$
				resource_name='lamp')
;sw_ba_bot=widget_base(sw_ba_leader,Title='Base row bottom',/row)

; ** First up : button, labels and text boxes
sw_bu_start=widget_button(sw_ba_bot,Font=ft_b_smaller,Frame=2,value='Start',uvalue=[4,0,1])
sw_bu_cancel=widget_button(sw_ba_bot,Font=ft_b_smaller,Frame=2,value='Cancel',uvalue=[4,0,2])
sw_la_nlor=widget_label(sw_ba_bot,Font=ft_b_smaller,value='Number of Lorentzian ?')
sw_tx_nlor=widget_text(sw_ba_bot,Font=ft_b_smaller,/editable,value='1',uvalue=[4,0,3],xsize=2)
sw_la_ngau=widget_label(sw_ba_bot,Font=ft_b_smaller,value='Number of Gaussian ?')
sw_tx_ngau=widget_text(sw_ba_bot,Font=ft_b_smaller,/editable,value='1',uvalue=[4,0,4],xsize=2)

; ** Realization of the Base
widget_control, sw_ba_bot, /Realize; & put_logo
xmanager,'qens_fit',sw_ba_bot,/just_reg

endif

end

; ** **************************************************************
pro fit_basegl2
; ** **************************************************************
; ** create the base of the fitting routine for a Lor-Gau fit
;
@lamp.cbk
common sc_wid
common sc_loc
common sc_flag
common sc_fit
common sc_convs

name_func='LOR_GAU'

; ** Defining the flag for the L and G click box
; ** *******************************************
flag_lfit=0 & flag_gfit=0 & flag_lcfit=0 & flag_gcfit=0 & flag_lwfit=0
flag_gwfit=0 & flag_lifit=0 & flag_gifit=0
flag_dfit=0 & flag_bfit=0 & flag_dcfit=0 & flag_bcfit=0
flag_difit=0 & flag_bifit=0
flag_lgifit=intarr(num_lor+num_gau+2)
flag_pl_fit2=not flag_pl_fit2
flag_reverse=0
flag_fromlast=0
flag_sym=0

; ** Defining the widgetID vectors
; ** *****************************
dimlongtx=9*(num_lor+num_gau)+12
ilor=0 & igau=0
if num_lor gt 0 then ilor=1
if num_gau gt 0 then igau=1
;dimlongbu=4*(num_lor+num_gau+ilor+igau)+6
dimlongbu=3*(num_lor+num_gau)+4
sw_tx_balg2=lonarr(dimlongtx)
sw_bu_balg2=lonarr(dimlongbu)

; ** Defining the fit variable vectors
; ** *********************************
sf_index=intarr(nb_spe_tot,3*num_lor+3*num_gau+4) & sf_index=not sf_index
sf_new_values=fltarr(nb_spe_tot,3*num_lor+3*num_gau+4) & for ii=0,nb_spe_tot-1 do sf_new_values(ii,3*indgen(num_lor+num_gau))=center_rf(ii)
sf_new_values(0:nb_spe_tot-1,3*indgen(num_lor+num_gau)+1)=0.1 & sf_new_values(0:nb_spe_tot-1,3*indgen(num_lor+num_gau)+2)=1.
sf_new_values(0:nb_spe_tot-1,3*num_lor+3*num_gau:3*num_lor+3*num_gau+3)=0. & sf_new_values(0:nb_spe_tot-1,3*num_lor+3*num_gau+1)=10.
sf_last_values=fltarr(nb_spe_tot,3*num_lor+3*num_gau+4)
sf_e_values=fltarr(nb_spe_tot,3*num_lor+3*num_gau+4)
sf_sub_function=fltarr(nb_spe_tot,nb_channels,num_lor+num_gau+2)
sf_functions_labels=strarr(num_lor*3+num_gau*3+2*2)
sf_errors_labels=strarr(num_lor*3+num_gau*3+2*2)
chisqr=fltarr(nb_spe_tot)+1.e30

if (xregistered('fit_base') le 0) then begin

; ** Start definition of the widget object
; ** *************************************

; ** Base for the fit
; ** First Base
sw_ba_bot=widget_base(group_leader=sw_ba_leader, /column, $
				Title='Fitted parameters and errors ',$
				resource_name='lamp')

; ** Now do the Lorentzian functions if any

sw_ba_botup1=widget_base(sw_ba_bot,Frame=2,Title='Base botlow left',/column)


if num_lor gt 0 then begin
for il=1,num_lor do begin
sw_ba_botup=widget_base(sw_ba_botup1,/row)
str_il='L'+string(il) & str_il=strcompress(str_il,/remove_all)
sw_la_lor=widget_label(sw_ba_botup,value=str_il,font=font_smaller,xsize=50,/align_center)
;
str_il='C'+string(il) & str_il=strcompress(str_il,/remove_all)
sw_la_clor=widget_label(sw_ba_botup,value=str_il,font=font_smaller,xsize=30,/align_center);'
sw_tx_balg2((il-1)*9)=widget_text(sw_ba_botup,font=font_smaller,value=strcompress(string(center_rf(num_spectrum-1)),/remove_all),/editable,xsize=5,/align_center)
sw_tx_balg2((il-1)*9+1)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2((il-1)*9+2)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_botup,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2((il-1)*3)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,(il-1)*3],/align_center)
;
str_il='W'+string(il) & str_il=strcompress(str_il,/remove_all)
sw_la_wlor=widget_label(sw_ba_botup,value=str_il,font=font_smaller,xsize=30,/align_center);'
sw_tx_balg2((il-1)*9+3)=widget_text(sw_ba_botup,font=font_smaller,value='0.10',/editable,xsize=5,/align_center)
sw_tx_balg2((il-1)*9+4)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2((il-1)*9+5)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_botup,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2((il-1)*3+1)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,(il-1)*3+1],/align_center)
;
str_il='I'+string(il) & str_il=strcompress(str_il,/remove_all)
sw_la_ilor=widget_label(sw_ba_botup,value=str_il,font=font_smaller,xsize=30,/align_center);'
sw_tx_balg2((il-1)*9+6)=widget_text(sw_ba_botup,font=font_smaller,value='1.00',/editable,xsize=5,/align_center)
sw_tx_balg2((il-1)*9+7)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2((il-1)*9+8)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_botup,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2((il-1)*3+2)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,(il-1)*3+2],/align_center)
sf_functions_labels((il-1)*3)='Center value for Lor#'+strcompress(string(il),/remove_all)
sf_functions_labels((il-1)*3+1)='Width value for Lor#'+strcompress(string(il),/remove_all)
sf_functions_labels((il-1)*3+2)='Intensity value for Lor#'+strcompress(string(il),/remove_all)
sf_errors_labels((il-1)*3)='with error'
sf_errors_labels((il-1)*3+1)='with error'
sf_errors_labels((il-1)*3+2)='with error'

endfor
endif
igmin=num_lor*3
igmintx=num_lor*9
igminind=num_lor*3
; ** Now do the Gaussian function if any
if num_gau gt 0 then begin

for ig=1,num_gau do begin
;
sw_ba_botup=widget_base(sw_ba_botup1,/row)
;
str_ig='G'+string(ig) & str_ig=strcompress(str_ig,/remove_all)
sw_la_gau=widget_label(sw_ba_botup,value=str_ig,font=font_smaller,xsize=50,/align_center)
;
str_ig='C'+string(ig) & str_ig=strcompress(str_ig,/remove_all)
sw_la_cgau=widget_label(sw_ba_botup,value=str_ig,font=font_smaller,xsize=30,/align_center);'
sw_tx_balg2(igmintx+(ig-1)*9)=widget_text(sw_ba_botup,font=font_smaller,value=strcompress(string(center_rf(num_spectrum-1)),/remove_all),/editable,xsize=5,/align_center)
sw_tx_balg2(igmintx+(ig-1)*9+1)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(igmintx+(ig-1)*9+2)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_botup,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(igmin+(ig-1)*3)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,igmin+(ig-1)*3],/align_center)
;
str_ig='W'+string(ig) & str_ig=strcompress(str_ig,/remove_all)
sw_la_wlor=widget_label(sw_ba_botup,value=str_ig,font=font_smaller,xsize=30,/align_center);'
sw_tx_balg2(igmintx+(ig-1)*9+3)=widget_text(sw_ba_botup,font=font_smaller,value='0.10',/editable,xsize=5,/align_center)
sw_tx_balg2(igmintx+(ig-1)*9+4)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(igmintx+(ig-1)*9+5)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_botup,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(igmin+(ig-1)*3+1)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,igmin+(ig-1)*3+1],/align_center)
;
str_ig='I'+string(ig) & str_ig=strcompress(str_ig,/remove_all)
sw_la_ilor=widget_label(sw_ba_botup,value=str_ig,font=font_smaller,xsize=30,/align_center);'
sw_tx_balg2(igmintx+(ig-1)*9+6)=widget_text(sw_ba_botup,font=font_smaller,value='1.00',/editable,xsize=5,/align_center)
sw_tx_balg2(igmintx+(ig-1)*9+7)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(igmintx+(ig-1)*9+8)=widget_text(sw_ba_botup,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_botup,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(igmin+(ig-1)*3+2)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,igmin+(ig-1)*3+2],/align_center)

sf_functions_labels(igmin+(ig-1)*3)='Center value for Gau#'+strcompress(string(ig),/remove_all)
sf_functions_labels(igmin+(ig-1)*3+1)='Width value for Gau#'+strcompress(string(ig),/remove_all)
sf_functions_labels(igmin+(ig-1)*3+2)='Intensity value for Gau#'+strcompress(string(ig),/remove_all)
sf_errors_labels(igmin+(ig-1)*3)='with error'
sf_errors_labels(igmin+(ig-1)*3+1)='with error'
sf_errors_labels(igmin+(ig-1)*3+2)='with error'
endfor
endif

; ** The Delta function
; ** ********************************
sw_ba_botlow=widget_base(sw_ba_bot,Frame=2,Title='Base bot right',/column)
idmin=igmin+num_gau*3
idmintx=igmintx+num_gau*9
idminind=igminind+num_gau*3

sw_ba_c1=widget_base(sw_ba_botlow,/row)
;
sw_la_del=widget_label(sw_ba_c1,value='D ',font=font_smaller,/align_center,xsize=50)
;
sw_la_cdel=widget_label(sw_ba_c1,value='Cd',font=font_smaller,xsize=30,/align_center)
sw_tx_balg2(idmintx)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',/editable,xsize=5,/align_center)
sw_tx_balg2(idmintx+1)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(idmintx+2)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(idmin)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,idmin],/align_center)

sw_la_idel=widget_label(sw_ba_c1,value='Id',font=font_smaller,xsize=30,/align_center)
sw_tx_balg2(idmintx+3)=widget_text(sw_ba_c1,font=font_smaller,value='10.0',/editable,xsize=5,/align_center)
sw_tx_balg2(idmintx+4)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(idmintx+5)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(idmin+1)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,idmin+1],/align_center)

sf_functions_labels(idmin)='Center value for Delta'
sf_functions_labels(idmin+1)='Intensity value for Delta'
sf_errors_labels(idmin)='with error'
sf_errors_labels(idmin+1)='with error'


;; ** Insert the "Try without fitting" button and "fitALL" chekbox
;; ** *************************************************************
;sw_bu_try=widget_button(sw_ba_c1,Font=ft_b_smaller,Frame=1,value='Try and enter',uvalue=[6,0,0])
;sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box2',/nonexclusive,/align_center,/row)
;sw_bu_fitall=widget_button(sw_ba_cb1,value=' fit all Sp?',uvalue=[6,2,1],/align_center)
;sw_bu_reverse=widget_button(sw_ba_cb1,value=' reverse?',uvalue=[6,2,3],/align_center,sensitive=0)
;sw_bu_fromlast=widget_button(sw_ba_cb1,value=' from last?',uvalue=[6,2,4],/align_center,sensitive=0)
;
;;sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box4',/nonexclusive,/align_center)
;;sw_bu_balg2(idmin+2)=widget_button(sw_ba_cb1,value='',uvalue=[6,1,80],/align_center)

; ** Now the slopping background
; ** *************************************************
ibmin=idmin+2
ibmintx=idmintx+6
ibminind=idminind+2

sw_ba_c1=widget_base(sw_ba_botlow,/row)
;
sw_la_bac=widget_label(sw_ba_c1,value='B ',font=font_smaller,/align_center,xsize=50)
;
sw_la_sbac=widget_label(sw_ba_c1,value='Sb',font=font_smaller,xsize=30,/align_center)
sw_tx_balg2(ibmintx)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',/editable,xsize=5,/align_center)
sw_tx_balg2(ibmintx+1)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(ibmintx+2)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(ibmin)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,ibmin],/align_center)

sw_la_idel=widget_label(sw_ba_c1,value='Cb',font=font_smaller,xsize=30,/align_center)
sw_tx_balg2(ibmintx+3)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',/editable,xsize=5,/align_center)
sw_tx_balg2(ibmintx+4)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_tx_balg2(ibmintx+5)=widget_text(sw_ba_c1,font=font_smaller,value='0.00',xsize=5,/align_center)
sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box2',/nonexclusive,/align_center)
sw_bu_balg2(ibmin+1)=widget_button(sw_ba_cb1,value='fix?',uvalue=[6,1,ibmin+1],/align_center)

sf_functions_labels(ibmin)='Slope value for Backg'
sf_functions_labels(ibmin+1)='Constant value for Backg'
sf_errors_labels(ibmin)='with error'
sf_errors_labels(ibmin+1)='with error'

; ** Insert the "Try without fitting" button and "fitALL" chekbox
; ** *************************************************************
sw_ba_botlow=widget_base(sw_ba_bot,Frame=2,Title='Base bot right',/column)
sw_ba_c1=widget_base(sw_ba_botlow,/row)
sw_bu_try=widget_button(sw_ba_c1,Font=ft_b_smaller,Frame=1,value='Try and enter',uvalue=[6,0,0])
sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box2',/nonexclusive,/align_center,/row)
sw_bu_fitall=widget_button(sw_ba_cb1,value=' fit all Sp?',uvalue=[6,2,1],/align_center)
sw_bu_reverse=widget_button(sw_ba_cb1,value=' reverse?',uvalue=[6,2,3],/align_center,sensitive=0)
sw_bu_fromlast=widget_button(sw_ba_cb1,value=' from last?',uvalue=[6,2,4],/align_center,sensitive=0)

;sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box4',/nonexclusive,/align_center)
;sw_bu_balg2(idmin+2)=widget_button(sw_ba_cb1,value='',uvalue=[6,1,80],/align_center)

; ** Insert the "Acquire from Sp#" button and "Sp#" textbox and symetrization possibility
; ** *************************************************************************************
sw_ba_c1=widget_base(sw_ba_botlow,/row)
sw_bu_acquire=widget_button(sw_ba_c1,Font=ft_b_smaller,Frame=1,value='Acquire from Sp#',uvalue=[6,2,2])
sw_tx_acquire=widget_text(sw_ba_c1,font=font_smaller,value=' 1',/editable,xsize=3,/align_center)
sw_ba_cb1=widget_base(sw_ba_c1,Title='Fit box1',/nonexclusive,/align_center)
sw_bu_symsqw=widget_button(sw_ba_cb1,value='Sym.S(Q,w)?',/align_center,uvalue=[6,2,5])
sw_tx_temp=widget_text(sw_ba_c1,font=font_smaller,value=' 0',/editable,xsize=3,/align_center,sensitive=0)
sw_la_temp=widget_label(sw_ba_c1,value='K',font=font_smaller,xsize=10,/align_center)
;

; ** We initialise the sf_new_values, sf_last_values, sf_e_values arrays !
; ** *********************************************************************
;for i=0,num_lor*3+num_gau*3+4-1 do begin ; sf_new_values must print the new value of the parameters
;widget_control,sw_tx_balg2(3*i),get_value=str_balg2
;flt_balg2=float(str_balg2) & flt_balg2=flt_balg2(0) & sf_new_values(*,i)=flt_balg2
;widget_control,sw_tx_balg2(3*i+1),get_value=str_balg2
;flt_balg2=float(str_balg2) & flt_balg2=flt_balg2(0) & sf_last_values(*,i)=flt_balg2
;widget_control,sw_tx_balg2(3*i+2),get_value=str_balg2
;flt_balg2=float(str_balg2) & flt_balg2=flt_balg2(0) & sf_e_values(*,i)=flt_balg2
;endfor


; ** Realization of the Base
widget_control, sw_ba_bot, /Realize; & put_logo
xmanager,'qens_fit',sw_ba_bot,/just_reg

; ** Set the FIT and DONE button sensitive:
widget_control,sw_bu_fit, sensitive=1
widget_control,sw_bu_done, sensitive=1

endif

end

; ** **************************************************************************
pro treat_resf
; ** **************************************************************************
; ** This program gives several treatment to the resolution function spectrum
; ** Basically, il substracts a flat background, performs the normalization to 1
; ** and deal with the range under consideration (e.g w where res(w) ne 0)
; ** sw_res=vector containing the exp. res. function
; ** w_resc=vector containing the treated res function
; ** rfmin and rfmax define the range of res. fun. to consider [rfmin,rfmax]
; ** treatment for spectrum ispmin to ispmax but for the moment, full treatment e.g
; ** ispmin and ispmax=(size(w_res))(2) (nspectra)

common sc_loc
common sc_fit
common sc_convs
common sc_wid
; ** recovering the data parameter
; ** *****************************
x_rf=sx_res ; energy array
y_rf=sy_res ; Q or theta array
e_rf=se_res ; error array
w_rf=sw_res   ; raw RF array
iprint=0 & ispmax=1
aalpha=y_rf*0. & abeta=aalpha & cel=aalpha
s_w_rf=size(sw_res) & nrfx=s_w_rf(1)
s_x_rf=size(sx_in) & nrx=s_x_rf(1)
ispmin=1 & ispmax=nb_spe_tot
fwhm_rf=fltarr(nb_spe_tot) & center_rf=fwhm_rf
;ispmin=1 & if s_w_rf(0) eq 2 then ispmax=s_w_rf(2)
if nrfx ne nrx then print,"PROBLEM: Dimension of X and W don't match !!"

; ** Start the loop on the spectra
; ** ******************************
for ispec=ispmin-1,ispmax-1 do begin
if iprint ne 0 then print,"Spectrum #",ispec
;
; ** obtaining alpha, beta and ce
; ** cad x_rf(alpha)=rfmin & x_rf(beta)=rfmax (limits)
; ** and x_rf(ce)=0. meV (elastic channel)
; ** *************************************************
rfin=where((x_rf lt rfmax) and (x_rf gt rfmin),nrfpts, complement=rfout)
aalpha(ispec)=rfin(0) & abeta(ispec)=rfin(nrfpts-1)
if (floor(nrfpts/2.)*2) eq nrfpts then begin
print,'PROBLEM: number of points in the Res. Function range should be odd and symetric % 0meV'
stop
endif
ce=where(w_rf(*,ispec) eq max(w_rf(*,ispec))) & e_el=x_rf(ce)
cel(ispec)=ce(0) & ce=ce(0) & test=where(ce eq rfin) & test=test(0)
if rfmax le rfmin then begin $
print, "PROBLEM: range of Res. function, rfmin not lt rfmax !!"
stop
endif
if test eq -1 then begin $
print, "PROBLEM: range of Res. function doesn't have the elastic peak !!"
stop
endif
if nrfpts le 5 then begin $
print, "PROBLEM: not enough points in the resolution function range !!"
stop
endif
if iprint ne 0 then begin
print,"aalpha=",aalpha(ispec),"abeta=",abeta(ispec),"ce=",ce,"e_el=",e_el
print,"rfin=",rfin
print,"rfout=",rfout
print,"test=",test
endif

; ** Fitting the resolution function with a Gaussian shape to have
; ** the width at half maximum
; ** **************************************************************

x_in_fit=x_rf(rfin)
y_in_fit=w_rf(rfin,ispec)
reso_fitted= gaussfit(x_in_fit,y_in_fit,A_param,NTERMS=4)
;print,A_param
plot,x_in_fit,y_in_fit
oplot,x_in_fit,reso_fitted,color=80000
fwhm_rf(ispec)=2*A_param(2)*sqrt(2*alog(2.)) & print,'FWHM for the RF at group#',ispec,fwhm_rf(ispec)
center_rf(ispec)=A_param(1) & print,'Center for the RF at group#',ispec,A_param(1)
;wait,0.2
; ** Substraction of a flat background
; ** *************************************
if iprint ne 0 then print, w_rf(*,ispec)
rfalpha=total(w_rf(aalpha(ispec):aalpha(ispec)+9,ispec),1)/10. & rfbeta=total(w_rf(abeta(ispec)-9:abeta(ispec),ispec),1)/10.
backg=rfalpha < rfbeta
if iprint ne 0 then print,backg,rfalpha,rfbeta
if backg le 0 then backg=0.
w_rf(*,ispec)=w_rf(*,ispec)-backg
w_rf(0:aalpha(ispec)-1,ispec)=0. & w_rf(abeta(ispec)+1:nrfx-1,ispec)=0.
e_rf(0:aalpha(ispec)-1,ispec)=0. & e_rf(abeta(ispec)+1:nrfx-1,ispec)=0.
infzero=where(w_rf(*,ispec) lt 0.)
if infzero(0) ne -1 then begin
w_rf(infzero,ispec)=0. & e_rf(infzero,ispec)=0.
endif
if iprint ne 0 then print, w_rf(*,ispec)

; ** Normalization to unity
; ** *************************************
deltaw1=x_rf(1)-x_rf(0) & deltaw2=x_rf(nrx-1)-x_rf(nrx-2)
deltaw=x_rf(ce+1)-x_rf(ce)
diff1=abs(deltaw1-deltaw) & diff2=abs(deltaw2-deltaw)
if (diff1 gt 0.00001) or (diff2 gt 0.00001) then begin
print,"PROBLEM: non constant energy step"
print,"Deltaw1=",deltaw1," Deltaw2=",deltaw2," Deltaw=",deltaw
print,"Diff1=",diff1," Diff2=",diff2
endif
factor=total(w_rf(*,ispec))*deltaw & sfac=size(factor)
if iprint ne 0 then print,"factor=",factor," deltaw=",deltaw
w_rf(*,ispec)=w_rf(*,ispec)/factor
e_rf(*,ispec)=e_rf(*,ispec)/factor
if iprint ne 0 then print,"NEWSUM=",total(w_rf(*,ispec))*deltaw


; ** End loop on ispec
; *********************
endfor

; ** Return modified values in w_out
; ** *******************************
w_resc=w_rf
sw_result=w_rf
lmin=cel-aalpha & lmax=abeta-cel

; ** Set the text boxes relative to the x range to consider inactives
; ** ****************************************************************
widget_control,sw_tx_resmin,sensitive=0
widget_control,sw_tx_resmax,sensitive=0

; ** End of Subroutine convs
; ** ***********************
end

; ** **************************************************************
pro qens_fit, Just=just, Group=group, Tripx=tripx, tx_param
; ** **************************************************************
; ** Main body of this complete routine ... the first to be called
;
@lamp.cbk
common sc_wid
common sc_loc
common sc_flag
common sc_fit

;Factors for using measured widths for peaks
;
;resolve_routine,'treat_resf'
;resolve_routine,'convs',/IS_function
gw_fac=2.0*sqrt(2.)
lw_fac=0.5
nb_channels=0
;print,"ON Y EST ! C'est parti"
flag_pl_res=0
flag_norm=0
flag_pl_fit=0 & flag_pl_fit2=0 & flag_all=0

if keyword_set(just) then g_ctrl_panel=1 else g_ctrl_panel=0
if (!D.flags and 65536) eq 0  then print,'set_plot,"X" before using rdfilter' else  $
if (xregistered('qens_fit') le 0) then begin
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
  viewarr	=fltarr(8+nbzone*4)	 ;dim zone to view array
  excl_param	=fltarr(2*max_nb_int)	 ;bornes des intervalles exclus
  excl_widge	=lonarr(2*max_nb_int)	 ;widget IDs des fenetres d'affichage des bornes
  g_parameters	=fltarr(3*g_nb_pk_max+3) ;parametres position,height,width + flat,slope
  g_step	=strarr(3*g_nb_pk_max+2) ;extensions des champs pos,height,width,flat,slope
  g_error	=fltarr(3*g_nb_pk_max+2) ;erreurs sur les parametres correspondants de g_parameters
  g_fct_type	=strarr(g_nb_pk_max)
  gw_pos_pop	=lonarr(10)		 ;widget ID of pos parameter menu
  gw_int_pop	=lonarr(10)		 ;widget ID of int parameter menu
  gw_wdt_pop	=lonarr(10)		 ;widget ID of wdt parameter menu
  gw_fbg_pop	=lonarr(10)		 ;widget ID of fbg parameter menu
  gw_sbg_pop	=lonarr(10)		 ;widget ID of sbg parameter menu
  sw_ni		=[0]
  g_npeaks	=1
  g_ncycles	=2
  peaky		=0.0
  g_resid	=1.0e12

  g_fct_type(*)	=g_fct_name
  nw_ni=0 & nw_out=1
  g_tripx=1 &wdtr='0'
  str_win='W1 ' & str_wres='W2 ' & str_wou='W3 '

  ;if g_tripx then nw_ni=23 else begin
      if lamp_b1 ne 0 then p_did_getw_cur, nw_ni, wstr	; get current w number
      if nw_ni eq 0 then nw_ni=1	; if not defined then w=1
      if nw_ni gt 20 then nw_ni=1
      nw_out	=nw_ni+1
  ;endelse
  if nw_out gt 20 then nw_out=20

if n_elements(Group) eq 0 then GROUP=lamp_b1
junk		= { CW_PDMENU_S, flags:0, name:'' }

; the whole lamp size depending modifications is here
sl_size =16
if lamp_siz gt 900 then begin
    xsiz=800 & ysiz=440
endif else begin
    xsiz=700 & ysiz=280
endelse
if lamp_siz lt 800 then begin	; adapt to lamp size
    xsiz=550 & ysiz=250		; adapt plotting window size
    sl_size=15
endif
txt_hlp=  '(LEFT press=position  release=width)....(RIGHT= Define Fitting Area)'

; ** Start definition of the widget object
; ** *************************************

; ** First Base
sw_ba_leader=widget_base(group_leader=Group, /column, $
				Title='Lamp SRfit Version 1st Oct 01 (sr)',$
				resource_name='lamp' $;)
				,MBAR=m_bar)
; ** Menu Bar
sw_bu_filem=widget_button(m_bar,/menu,value='File')
sw_bu_fitm=widget_button(m_bar,/menu,value='Fitting Model')
sw_bu_savec=widget_button(sw_bu_filem,value='Save curves',uvalue=[0,0,1])
sw_bu_savep=widget_button(sw_bu_filem,value='Save parameters',uvalue=[0,0,2])
sw_bu_exitm=widget_button(sw_bu_filem,value='Exit',uvalue=[0,0,0])
sw_bu_lgm=widget_button(sw_bu_fitm,value='Lorentzian and Gaussian',uvalue=[0,1,0])
sw_bu_isorotm=widget_button(sw_bu_fitm,value='Isotropic rotation',uvalue=[0,1,1],sensitive=0)
sw_bu_axrotm=widget_button(sw_bu_fitm,value='Axial rotation',uvalue=[0,1,2],sensitive=0)
sw_bu_rwm=widget_button(sw_bu_fitm,value='Random Walk translations',uvalue=[0,1,3],sensitive=0)
sw_bu_gdos=widget_button(sw_bu_fitm,value='GDOS treatment',uvalue=[0,1,4],sensitive=0)
sw_bu_plm=widget_button(m_bar,/menu,value='Analysis')
sw_bu_plwq=widget_button(sw_bu_plm,value='Plot width',uvalue=[0,2,0])
sw_bu_plwi=widget_button(sw_bu_plm,value='Plot intewnsity',uvalue=[0,2,1])
sw_pl_eisf=widget_button(sw_bu_plm,value='Plot EISF',uvalue=[0,2,2])

; ** First up : bases
sw_ba_up=widget_base(sw_ba_leader,Title='Base row up',frame=1,/row)
sw_ba_up1=widget_base(sw_ba_up,Title='Base row up 1',/column,/align_center)
sw_ba_up2=widget_base(sw_ba_up,Title='Base row up 2',/column,/align_center)
sw_ba_up3=widget_base(sw_ba_up,Title='Base row up 3',/column,/align_center)
sw_ba_up40=widget_base(sw_ba_up,Title='Base row up 40',/row,/align_center)
sw_ba_up4=widget_base(sw_ba_up,Title='Base row up 4',/column,/align_center)
sw_ba_up11=widget_base(sw_ba_up1,Title='Base row up 11',/row,/align_center)
sw_ba_up12=widget_base(sw_ba_up1,Title='Base row up 12',/row,/align_center)
sw_ba_up21=widget_base(sw_ba_up2,Title='Base row up 21',/row,/align_center)
sw_ba_up22=widget_base(sw_ba_up2,Title='Base row up 22',/row,/align_center)
sw_ba_up31=widget_base(sw_ba_up3,Title='Base row up 31',/row,/align_center)
sw_ba_up32=widget_base(sw_ba_up3,Title='Base row up 32',/row,/align_center)
sw_ba_up41=widget_base(sw_ba_up4,Title='Base row up 41',/row,/align_center)
sw_ba_up42=widget_base(sw_ba_up4,Title='Base row up 42',/row,/align_center)
; ** First up : button, labels and texts
sw_bu_gwin=widget_button(sw_ba_up11,Font=ft_b_smaller,Frame=2,value='Get W_in',uvalue=[1,1,0],/align_center)
sw_bu_gres=widget_button(sw_ba_up21,Font=ft_b_smaller,Frame=2,value='Get Res_fun',uvalue=[1,2,0],/align_center,sensitive=0)
sw_bu_wwou=widget_button(sw_ba_up31,Font=ft_b_smaller,Frame=2,value='Write W_out',uvalue=[1,3,0],/align_center,sensitive=0)
sw_bu_winb=widget_button(sw_ba_up12,Font=ft_smaller,Frame=2,value='<<',uvalue=[2,1,1],/align_center)
sw_la_win=widget_label(sw_ba_up12,Font=ft_b_smaller,Frame=2,value=str_win,uvalue=[2,1,2],/align_center)
sw_bu_winf=widget_button(sw_ba_up12,Font=ft_smaller,Frame=2,value='>>',uvalue=[2,1,3],/align_center)
sw_bu_resb=widget_button(sw_ba_up22,Font=ft_smaller,Frame=2,value='<<',uvalue=[2,2,1],/align_center)
sw_la_res=widget_label(sw_ba_up22,Font=ft_b_smaller,Frame=2,value=str_wres,uvalue=[2,2,2],/align_center)
sw_bu_resf=widget_button(sw_ba_up22,Font=ft_smaller,Frame=2,value='>>',uvalue=[2,2,3],/align_center)
sw_bu_woub=widget_button(sw_ba_up32,Font=ft_smaller,Frame=2,value='<<',uvalue=[2,3,1],/align_center)
sw_la_wou=widget_label(sw_ba_up32,Font=ft_b_smaller,Frame=2,value=str_wou,uvalue=[2,3,2],/align_center)
sw_bu_rouf=widget_button(sw_ba_up32,Font=ft_smaller,Frame=2,value='>>',uvalue=[2,3,3],/align_center)
sw_la_reslim=widget_label(sw_ba_up40,Font=ft_smaller,value='Give limits for Res. fun.:',/align_center)
sw_la_resmin=widget_label(sw_ba_up41,Font=ft_smaller,value='xmin=  ',/align_center)
sw_la_resmax=widget_label(sw_ba_up42,Font=ft_smaller,value='xmax=  ',/align_center)
sw_tx_resmin=widget_text(sw_ba_up41,Font=ft_smaller,/editable,xsize=3,value='-1',/align_center)
sw_tx_resmax=widget_text(sw_ba_up42,Font=ft_smaller,/editable,xsize=3,value=' 1',/align_center)
sw_la_mev=widget_label(sw_ba_up41,Font=ft_smaller,value='  meV/mmeV',/align_center)
sw_la_mev=widget_label(sw_ba_up42,Font=ft_smaller,value='  meV/mmeV',/align_center)

; ** Second mid : bases
sw_ba_mid=widget_base(sw_ba_leader,Title='Base row mid',frame=1,/row)
sw_ba_mid1=widget_base(sw_ba_mid,Title='Base row mid 1',/column)
sw_ba_mid2=widget_base(sw_ba_mid,Title='Base row mid 2',/column)

;sw_ba_mid3=widget_base(sw_ba_mid,Title='Base row mid 3',/column)

sw_ba_mid11=widget_base(sw_ba_mid1,Title='Base row mid 11',/column,frame=1)
sw_ba_mid12=widget_base(sw_ba_mid1,Title='Base row mid 12',/column,frame=1)
sw_ba_mid13=widget_base(sw_ba_mid1,Title='Base row mid 13',/column,frame=1)

sw_ba_mid11up=widget_base(sw_ba_mid11,Title='Base row mid 11up',/row)
sw_ba_mid11bot=widget_base(sw_ba_mid11,Title='Base row mid 11bot',/row)
sw_ba_mid12up=widget_base(sw_ba_mid12,Title='Base row mid 12up',/row)
sw_ba_mid12bot=widget_base(sw_ba_mid12,Title='Base row mid 12bot',/row,/nonexclusive)


;sw_ba_mid14=widget_base(sw_ba_mid1,Title='Base row mid 14',/row)
;sw_ba_mid15=widget_base(sw_ba_mid1,Title='Base row mid 15',/row)

; ** Second mid : button, labels and txtboxes
sw_la_xrange=widget_label(sw_ba_mid11up,Font=ft_b_smaller,value='X range:')
sw_tx_xmin=widget_text(sw_ba_mid11up,Font=ft_b_smaller,/editable,value='min',xsize=5,uvalue=[3,1,1])
sw_tx_xmax=widget_text(sw_ba_mid11up,Font=ft_b_smaller,/editable,value='max',xsize=5,uvalue=[3,1,2])
sw_la_yrange=widget_label(sw_ba_mid11bot,Font=ft_b_smaller,value='Y range:')
sw_tx_ymin=widget_text(sw_ba_mid11bot,Font=ft_b_smaller,/editable,value='min',xsize=5,uvalue=[3,1,3])
sw_tx_ymax=widget_text(sw_ba_mid11bot,Font=ft_b_smaller,/editable,value='max',xsize=5,uvalue=[3,1,4])

sw_la_wsub=widget_label(sw_ba_mid12up,Font=ft_b_smaller,value='Sp_#') & blabla=widget_label(sw_ba_mid12up,Font=ft_b_smaller,value='',xsize=15)
sw_tx_wsub=widget_text(sw_ba_mid12up,Font=ft_b_smaller,/editable,value=' 1',xsize=3,uvalue=[3,1,5]) & sw_tx_fwsub=sw_tx_wsub
blabla=widget_label(sw_ba_mid12up,Font=ft_b_smaller,value='',xsize=15)
sw_bu_plot=widget_button(sw_ba_mid12up,Font=ft_b_smaller,frame=4,value="Accept!",uvalue=[3,2,1],sensitive=0)

sw_bu_plre=widget_button(sw_ba_mid12bot,value="Res. Func.",uvalue=[3,2,2])
sw_bu_norm=widget_button(sw_ba_mid12bot,value="Norm.?",uvalue=[3,2,3])

; ** **********************************************************************************************
; ** Second up : button, labels and text boxes
;
sw_la_sdl=widget_label(sw_ba_mid13,Font=ft_b_smaller,value='** Fitting Range: **')
sw_ba_mid13up=widget_base(sw_ba_mid13,Title='Base row mid 13up',/row)
sw_ba_mid13bot=widget_base(sw_ba_mid13,Title='Base row mid 13bot',/row)
sw_bu_done=widget_button(sw_ba_mid13,Font=ft_b_smaller,Frame=1,value='Done',uvalue=[6,2,0],sensitive=0)

sw_la_xrange=widget_label(sw_ba_mid13up,Font=ft_b_smaller,value='X range:')
sw_tx_fxmin=widget_text(sw_ba_mid13up,/editable,value='min',xsize=5,uvalue=[6,0,2])
sw_tx_fxmax=widget_text(sw_ba_mid13up,/editable,value='max',xsize=5,uvalue=[6,0,3])
sw_bu_fit=widget_button(sw_ba_mid13bot,Font=ft_b_smaller,Frame=2,value='   FIT !   ',uvalue=[6,0,1],sensitive=0)
sw_tx_cycl=widget_text(sw_ba_mid13bot,Font=ft_b_smaller,/editable,value=' 5',xsize=3,uvalue=[6,0,5])
sw_la_cycl=widget_label(sw_ba_mid13bot,Font=ft_b_smaller,value='Cycles')

; ** *********************************************************************************************
; ** Second mid : graph
sw_pl_area=widget_draw(sw_ba_mid2,/Button_Events,Frame=5,Retain=2,uvalue=[3,3,1],Colors=-8, $
					   XSize=3*xsiz/4,YSize=9*ysiz/16)
sw_pl_areabot=widget_draw(sw_ba_mid2,/Button_Events,Frame=5,Retain=2,uvalue=[3,3,2],Colors=-8, $
					   XSize=3*xsiz/4,YSize=3*ysiz/16)
szsl=0 & str=''
if sys_dep('VERSION') ge 4.0 then ii=execute('str=widget_info(peak_menu2,/geometry) & szsl=str.xsize')
bid=sys_dep('DYNLAB', sw_ba_leader, 1)

;widget_control, bad_id=i, sw_ba_leader, /Realize & put_logo

; ** Realization of the Base
widget_control, sw_ba_leader, /Realize; & put_logo
xmanager,'qens_fit',sw_ba_leader,/just_reg

; ** Get the window Id for the plotting area and store it in s_pl_wid
widget_control, bad_id=i, sw_pl_area, get_value=s_pl_wid 	; Store the number assigned to this window in s_pl_wid
wset,s_pl_wid											; Set the s_pl_wid active

; ** Get the window Id for the plotting area #2 and store it in s_pl_widbot
widget_control, bad_id=i, sw_pl_areabot, get_value=s_pl_widbot 	; Store the number assigned to this window in s_pl_wid
wset,s_pl_widbot											; Set the s_pl_wid active


endif
; Get drawable window index
;widget_control, bad_id=i, gw_plot_area, get_value=g_plot_wid
;wset,g_plot_wid

;if lamp_b1 gt 0 then xmanager, 'qens_fit', sw_ba_leader, /just_reg $
;else g_char=0.4

;if lamp_b1 le 0 then xmanager, 'qens_fit', sw_ba_leader

end
