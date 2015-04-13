; $Id: curvefit.pro,v 1.7 1995/06/15 16:24:13 dave Exp $
function lsfit, x, y, w, a, sigmaa, Function_Name = Function_Name, $
                        itmax=itmax, iter=iter, tol=tol, chi2=chi2, $
                        noderivative=noderivative,damping=damping,plot=plot,print=print
; Copyright (c) 1988-1995, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       CURVEFIT
;
; PURPOSE:
;       Non-linear least squares fit to a function of an arbitrary 
;       number of parameters.  The function may be any non-linear 
;       function.  If available, partial derivatives can be calculated by 
;       the user function, else this routine will estimate partial derivatives
;       with a forward difference approximation.
;
; CATEGORY:
;       E2 - Curve and Surface Fitting.
;
; CALLING SEQUENCE:
;       Result = CURVEFIT(X, Y, W, A, SIGMAA, FUNCTION_NAME = name, $
;                         ITMAX=ITMAX, ITER=ITER, TOL=TOL, /NODERIVATIVE)
;
; INPUTS:
;       X:  A row vector of independent variables.  This routine does
;		not manipulate or use values in X, it simply passes X
;		to the user-written function.
;
;       Y:  A row vector containing the dependent variable.
;
;       W:  A row vector of weights, the same length as Y.
;               For no weighting,
;               w(i) = 1.0.
;               For instrumental weighting,
;               w(i) = 1.0/y(i), etc.
;
;       A:  A vector, with as many elements as the number of terms, that 
;           contains the initial estimate for each parameter.  If A is double-
;           precision, calculations are performed in double precision, 
;           otherwise they are performed in single precision.
;
; KEYWORDS:
;       FUNCTION_NAME:  The name of the function (actually, a procedure) to 
;       fit.  If omitted, "FUNCT" is used. The procedure must be written as
;       described under RESTRICTIONS, below.
;
;       ITMAX:  Maximum number of iterations. Default = 20.
;       ITER:   The actual number of iterations which were performed
;       TOL:    The convergence tolerance. The routine returns when the
;               relative decrease in chi-squared is less than TOL in an 
;               interation. Default = 1.e-3.
;       CHI2:   The value of chi-squared on exit
;       NODERIVATIVE:   If this keyword is set then the user procedure will not
;               be requested to provide partial derivatives. The partial
;               derivatives will be estimated in CURVEFIT using forward
;               differences. If analytical derivatives are available they
;               should always be used.
;
; OUTPUTS:
;       Returns a vector of calculated values.
;       A:  A vector of parameters containing fit.
;
; OPTIONAL OUTPUT PARAMETERS:
;       Sigmaa:  A vector of standard deviations for the parameters in A.
;
; COMMON BLOCKS:
;       NONE.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       The function to be fit must be defined and called FUNCT,
;       unless the FUNCTION_NAME keyword is supplied.  This function,
;       (actually written as a procedure) must accept values of
;       X (the independent variable), and A (the fitted functions
;       parameter values), and return F (the function`s value at
;       X), and PDER (a 2D array of partial derivatives).
;       For an example, see FUNCT in the IDL User`s Libaray.
;       A call to FUNCT is entered as:
;       FUNCT, X, A, F, PDER
; where:
;       X = Variable passed into CURVEFIT.  It is the job of the user-written
;		function to interpret this variable.
;       A = Vector of NTERMS function parameters, input.
;       F = Vector of NPOINT values of function, y(i) = funct(x), output.
;       PDER = Array, (NPOINT, NTERMS), of partial derivatives of funct.
;               PDER(I,J) = DErivative of function at ith point with
;               respect to jth parameter.  Optional output parameter.
;               PDER should not be calculated if the parameter is not
;               supplied in call. If the /NODERIVATIVE keyword is set in the
;               call to CURVEFIT then the user routine will never need to
;               calculate PDER.
;
; PROCEDURE:
;       Copied from "CURFIT", least squares fit to a non-linear
;       function, pages 237-239, Bevington, Data Reduction and Error
;       Analysis for the Physical Sciences.
;
;       "This method is the Gradient-expansion algorithm which
;       combines the best features of the gradient search with
;       the method of linearizing the fitting function."
;
;       Iterations are performed until the chi square changes by
;       only TOL or until ITMAX iterations have been performed.
;
;       The initial guess of the parameter values should be
;       as close to the actual values as possible or the solution
;       may not converge.
;
; EXAMPLE:  Fit a function of the form f(x) = a * exp(b*x) + c to
;	sample pairs contained in x and y.
;	In this example, a=a(0), b=a(1) and c=a(2).
;	The partials are easily computed symbolicaly:
;		df/da = exp(b*x), df/db = a * x * exp(b*x), and df/dc = 1.0
;
;		Here is the user-written procedure to return F(x) and
;		the partials, given x:
;       pro gfunct, x, a, f, pder	; Function + partials
;	  bx = exp(a(1) * x)
;         f= a(0) * bx + a(2)		;Evaluate the function
;         if N_PARAMS() ge 4 then $	;Return partials?
;		pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(y))]]
;       end
;
;         x=findgen(10)			;Define indep & dep variables.
;         y=[12.0, 11.0,10.2,9.4,8.7,8.1,7.5,6.9,6.5,6.1]
;         w=1.0/y			;Weights
;         a=[10.0,-0.1,2.0]		;Initial guess
;         yfit=curvefit(x,y,w,a,sigmaa,function_name='gfunct')
;	  print, 'Function parameters: ', a
;         print, yfit
;       end
;
; MODIFICATION HISTORY:
;       Written, DMS, RSI, September, 1982.
;       Does not iterate if the first guess is good.  DMS, Oct, 1990.
;       Added CALL_PROCEDURE to make the function`s name a parameter.
;              (Nov 1990)
;       12/14/92 - modified to reflect the changes in the 1991
;            edition of Bevington (eq. II-27) (jiy-suggested by CreaSo)
;       Mark Rivers, U of Chicago, Feb. 12, 1995
;           - Added following keywords: ITMAX, ITER, TOL, CHI2, NODERIVATIVE
;             These make the routine much more generally useful.
;           - Removed Oct. 1990 modification so the routine does one iteration
;             even if first guess is good. Required to get meaningful output
;             for errors. 
;           - Added forward difference derivative calculations required for 
;             NODERIVATIVE keyword.
;           - Fixed a bug: PDER was passed to user`s procedure on first call, 
;             but was not defined. Thus, user`s procedure might not calculate
;             it, but the result was then used.
;           
;-
;       on_error,2              ;Return to caller if error
COMMON fit,voigt,nterms,fitflag,key,undo,npeaks,rectangle,bragg,peakpars
IF KEYWORD_SET(damping) THEN damp=damping ELSE damp=FLTARR(N_ELEMENTS(a))+1.
IF N_ELEMENTS(damp) NE N_ELEMENTS(a) THEN damp=a*0.+1.
       ;Name of function to fit
       if n_elements(function_name) le 0 then function_name = "FUNCT"
       if n_elements(tol) eq 0 then tol = 1.e-3		;Convergence tolerance
       if n_elements(itmax) eq 0 then itmax = 20	;Maximum # iterations
	type = size(a)
	type = type(type(0)+1)
	double = type eq 5
	if (type ne 4) and (type ne 5) then a = float(a)  ;Make params floating

       ; If we will be estimating partial derivatives then compute machine
       ; precision
       if keyword_set(NODERIVATIVE) then begin
          res = nr_machar(DOUBLE=double)
          eps = sqrt(res.eps)
       endif

       terms2fit = n_elements(a)   ; # of parameters
       nfree = n_elements(y) - terms2fit ; Degrees of freedom
       if nfree le 0 then message, 'Curvefit - not enough data points.'
       flambda = 0.001          ;Initial lambda
       diag = lindgen(terms2fit)*(terms2fit+1) ; Subscripts of diagonal elements

;      Define the partial derivative array
       if double then pder = dblarr(n_elements(y), terms2fit) $
	else pder = fltarr(n_elements(y), terms2fit)
;
       for iter = 1, itmax do begin   ; Iteration loop

;PRINT,'Least-squares iteration no.',iter
if ITER EQ 0 then begin
  if NOT keyword_set(NODERIVATIVE) then begin
          res = nr_machar(DOUBLE=double)
          eps = sqrt(res.eps)
  endif
  call_procedure, Function_name, x, a, yfit
  for term=0, terms2fit-1 do begin
                p = a       ; Copy current parameters
                inc = eps * abs(p(term))    
                if (inc eq 0.) then inc = eps
                p(term) = p(term) + inc
                call_procedure, function_name, x, p, yfit1
                pder(0,term) = (yfit1-yfit)/inc
  endfor
  PRINT,'Evaluated  derivatives:',pder
  call_procedure, function_name, x, a, yfit, pder 
  PRINT,'Analytical derivatives:',pder
endif

          if keyword_set(NODERIVATIVE) then begin
;            Evaluate function and estimate partial derivatives
             call_procedure, Function_name, x, a, yfit
             for term=0, terms2fit-1 do begin
                p = a       ; Copy current parameters
                ; Increment size for forward difference derivative
                inc = eps * abs(p(term))    
                if (inc eq 0.) then inc = eps
                p(term) = p(term) + inc
                call_procedure, function_name, x, p, yfit1
                pder(0,term) = (yfit1-yfit)/inc
             endfor
          endif else begin
             ; The user`s procedure will return partial derivatives
             call_procedure, function_name, x, a, yfit, pder 
          endelse

          beta = (y-yfit)*w # pder
          alpha = transpose(pder) # (w # (fltarr(terms2fit)+1)*pder)
          chisq1 = total(w*(y-yfit)^2)/nfree ; Present chi squared.

				; If a good fit, no need to iterate
	  all_done = chisq1 lt total(abs(y))/1e7/NFREE
;
;         Invert modified curvature matrix to find new parameters.

          repeat begin
             c = sqrt(alpha(diag) # alpha(diag))
             array = alpha/(c>1e-45)
             array(diag) = array(diag)*(1.+flambda)              
             array = invert(array)
             tmp=damp*(array/(c>1e-45) # transpose(beta))
             ;b = a+ damp*(array/(c>1e-45) # transpose(beta)); New params
             
             b=a+tmp
             IF npeaks GE 1 THEN BEGIN
               b(INDGEN(npeaks)*peakpars)=b(INDGEN(npeaks)*peakpars)>0
               smallpeak=WHERE(b(INDGEN(npeaks)*peakpars) LE (MIN(y)*1e-3),smallpeaks)
               IF smallpeaks GT 0 THEN BEGIN
                 b(smallpeak*peakpars)=0
                 FOR peak=0,smallpeaks-1 DO BEGIN
                   b(smallpeak(peak)*peakpars+1+INDGEN(peakpars-1))=a(smallpeak(peak)*peakpars+1+INDGEN(peakpars-1))
                 ENDFOR
               ENDIF
               b(INDGEN(npeaks)*peakpars+2)=ABS(b(INDGEN(npeaks)*peakpars+2))
               IF rectangle THEN b(INDGEN(npeaks)*peakpars+3+voigt)=ABS(b(INDGEN(npeaks)*peakpars+3+voigt))
               ;IF voigt THEN b(INDGEN(npeaks)*peakpars+3)=ABS(FLOOR(ABS(b(INDGEN(npeaks)*peakpars+3))/2.)*2.-1.)
               IF voigt THEN b(INDGEN(npeaks)*peakpars+3)=ABS(b(INDGEN(npeaks)*peakpars+3))
             ENDIF
             call_procedure, function_name, x, b, yfit  ; Evaluate function
             chisqr = total(w*(y-yfit)^2)/nfree         ; New chisqr
           
;IF KEYWORD_SET(print) THEN PRINT,'chi2=',chisqr,', flambda=',flambda
	     if all_done then goto, done
             flambda = flambda*10.                      ; Assume fit got worse
          endrep until (chisqr le chisq1) OR NOT FINITE(chisqr)
;
          flambda = flambda/100.  ; Decrease flambda by factor of 10
          IF FINITE(chisqr) THEN BEGIN
            a=b                     ; Save new parameter estimate.
            IF KEYWORD_SET(print) THEN PRINT,iter,chisqr,a
            IF KEYWORD_SET(plot) THEN BEGIN
              PLOT,x,y,TITLE='Iteration'+STRCOMPRESS(iter)+', chi2='+STRCOMPRESS(chisqr,/RE),YRANGE=[0,MAX(y)],background=255,color=0
              OPLOTERR,x,y,w,0
              OPLOT,x,yfit,color=1
              OPLOT,x,y-yfit+MIN(y)/2.,color=3
              OPLOT,x,yfit*0.0+MIN(y)/2.,color=0
              OPLOT,x,y-yfit+w+MIN(y)/2.,color=2,LINE=0
              OPLOT,x,y-yfit-w+MIN(y)/2.,color=2,LINE=0
            ENDIF
            if ((chisq1-chisqr)/chisq1) le tol then goto,done  ; Finished?
          ENDIF ELSE BEGIN
            PRINT,'infinite chi2 -> stopped iteration loop'
            fitflag=1
            sigmaa = sqrt(array(diag)/alpha(diag))
            call_procedure, function_name, x, a, yfit, pder 
            chi2 = total(w*(y-yfit)^2)/nfree ; Present chi squared.
            return,yfit
          ENDELSE
          
       endfor                        ;iteration loop
;
       message, 'Failed to converge', /INFORMATIONAL
;
done:  sigmaa = sqrt(array(diag)/alpha(diag)) ; Return sigma`s
       ;nansigma=WHERE(NOT FINITE(sigmaa),nansigmas)
       ;IF nansigmas GT 0 THEN sigmaa(nansigma) =0.0
       chi2 = chisqr                          ; Return chi-squared
       return,yfit              ;return result
END
