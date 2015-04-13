pro pretreat,w,parasite,W=workspace
;
; Macro (started 18/10/96 by Th.Hansen) in order to interpolate or eliminate zero-counting or
; parasite-affected detector cells and/or non-executed or wrong steps/slices of scans/stroboscopic
; acquisitions.
; DATP is given by take_datp, so a call in another macro isn't possible yet!
; To change this, a keyword parameter with DATP has to be introduced.
; By default, all detector cells and all steps/slices with counting sums of zero are eliminated.
; Later on for calibration of angle/efficiency the corresponding cell-number for each counting rate
; has to be calculated by the formula CELL(i) = ROUND ((XX(i) - XX(0)) * 10). That works only if
; any angle calibration doesn't shift the 2*Theta value for any cell of 0.05 deg or more.
; Otherwise PRETREAT has either to be executed before any angle calibration or to be execute 
; after this calibration (angle calibration flag set) but in that case the cell number is simply
; the first index of the workspace w and it must not be calculated as above!
;
; Last modification 20-Jun-97 by Th.Hansen
;
take_datp,datp			; be cautious with this LAMP procedure !!!
pv = datp.pv 
x  = datp.x
e  = datp.e
y  = datp.y 
n  = datp.n  
nd = N_ELEMENTS (w(*,0))	; Number of Detector cells
np = N_ELEMENTS (pv(*,0))	; Number of variable Parameters
ns = N_ELEMENTS (w(0,*))	; Number of Scans/slices per numor
nx = N_ELEMENTS (x(0,*))	; Number of Scans/Slices per numor in X
ny = N_ELEMENTS (y(0,*))	; Number of Scans/Slices per numor in y if 2-dim, else 1/0
IF ns le 1 THEN w=reform(w,n_elements(w),1)
IF nx ne ns THEN nx=0
IF n_params() gt 1 THEN parasite = parasite(*,0,0,0)
cell = indgen (nd) 

 
;
; Elimination of zero-counting steps/slices:
;
IF nx gt 1 THEN x =x (*,where(total(w,1)))
IF ny gt 1 THEN y =y (*,where(total(w,1))) ELSE y =y (where(total(w,1)))
e =e (*,where(total(w,1)))
n =n (  where(total(w,1)))
pv=pv(*,where(total(w,1)))
w =w (*,where(total(w,1)))

IF n_params() gt 1 THEN BEGIN
  a=indgen(nd)
  b=parasite
  a(parasite)=-1
  a=a(where(a ge 0))
  wold=w  
  IF N_ELEMENTS(w(0,*)) GE N_ELEMENTS(x(0,*)) THEN BEGIN
    FOR i=0,N_ELEMENTS(w(0,*))-1 DO w(b,i)=interpol(w(a,i),x(a),x(b))
  ENDIF ELSE w(b)=interpol(w(a),x(a),x(b))
  e(b,*)=w(b,*)*e(b,*)/wold(b,*)
  wold=0
ENDIF

mod_datp, datp, 'y' , y
mod_datp, datp, 'x',  x
mod_datp, datp, 'n' , n 
mod_datp, datp, 'pv', pv
mod_datp, datp, 'e' , e
datp.p(38)=datp.p(38)+4.	; PREATREAT-flag
give_datp, datp
END
