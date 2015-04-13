function biny, win ,by
;******* ****
;**
;** Group ordinates by by (DR 2001)
;** CALL: Wi=biny(wj [,2])

dim =SIZE(win)

IF dim(0) eq 2 THEN IF dim(2) GE 4 THEN BEGIN

	IF n_elements(by) ne 1 THEN by=2
	by=by>2<(dim(2)/2)

	wout =FLTARR( dim(1) ,dim(2)/by )
	yout =FLTARR( dim(2)/by)

;	Take all parameters
;	**** *** **********
	TAKE_DATP,P
	r=0
	IF n_elements(P.e) eq n_elements(win) THEN BEGIN
		r=1 & P.e=P.e^2 & eout=wout & ENDIF

	FOR i=0,dim(2)/by-1 DO BEGIN
	    j=i*by
	    wout(*,i)= total(win(*,j:j+by-1),2)/by
	    yout(i)  = total(P.Y(  j:j+by-1)  )/by
	    IF r  THEN eout (*,i)= total(P.e(*,j:j+by-1),2)
	ENDFOR
	
;	Give back parameters
;	**** **** **********
	P.Y_TIT  =P.Y_TIT+' (binY*'+strtrim(string(by),2)+')'
	MOD_DATP ,P,'Y',yout	;P.Y=yout is the normal formulation but it
				;d'nt work (because the size changed)

	IF r THEN MOD_DATP ,P,'E',SQRT(eout)/by

	GIVE_DATP,P		;Give back

	RETURN,wout
ENDIF

P_MUS ,'mus_cannon'
RETURN, win
END

