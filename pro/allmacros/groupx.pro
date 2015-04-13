function groupx, win ,by
;******* ******
;**
;** Group canals by by ... (J. COOK)
;** CALL: Wi=groupx(wj [,2])

dim =SIZE(win)

IF dim(0) GE 1 THEN IF dim(1) GE 4 THEN BEGIN

	IF n_elements(by) ne 1 THEN by=2
	by=by>2<(dim(1)/4)

	IF dim(0) EQ 1 THEN wout =FLTARR( dim(1)/by )
	IF dim(0) EQ 2 THEN wout =FLTARR( dim(1)/by ,dim(2) )
			    xout =wout(*,0)
;	Take all parameters
;	**** *** **********
	TAKE_DATP,P

	FOR i=0,dim(1)/by-1 DO BEGIN j=i*by
	    IF dim(0) EQ 1 THEN wout(i)  = total(win(j:j+by-1  ))
	    IF dim(0) EQ 2 THEN wout(i,*)= total(win(j:j+by-1,*),1)
				xout(i)  = total(P.x(j:j+by-1  ))/by
	ENDFOR

;	Errors
;	******
	IF n_elements(P.e) eq n_elements(win) then begin
	FOR i=0,dim(1)/by-1 DO BEGIN j=i*by
	    IF dim(0) EQ 1 THEN P.e(i)   = total(P.e(j:j+by-1  )^2)
	    IF dim(0) EQ 2 THEN P.e(i,*) = total(P.e(j:j+by-1,*)^2,1)
	ENDFOR
	P.e=SQRT(P.e)
	ENDIF

;	Give back parameters
;	**** **** **********
	P.X_TIT  =P.X_TIT+' (group*'+strtrim(string(by),2)+')'
	MOD_DATP ,P,'X'    ,xout	;P.X=xout is the normal formulation but it
					;d'nt work (because the size changed)
	GIVE_DATP,P			;Give back

	RETURN,wout
ENDIF

P_MUS ,'mus_cannon'
RETURN, win
END

