PRO	pure_GAUSS,X,A,F,PDER
	ON_ERROR,2                        ;Return to caller if an error occurs
	if a(2) ne 0.0 then Z = (X-A(1))/A(2) $	;GET Z
	else z= 10.
	EZ = EXP(-Z^2/2.)*(ABS(Z) LE 7.) ;GAUSSIAN PART IGNORE SMALL TERMS
	F = A(0)*EZ ;FUNCTIONS.
	IF N_PARAMS(0) LE 3 THEN RETURN ;NEED PARTIAL?
	PDER = FLTARR(N_ELEMENTS(X),3) ;YES, MAKE ARRAY.
	PDER(0,0) = EZ		;COMPUTE PARTIALS
	if a(2) ne 0. then PDER(0,1) = A(0) * EZ * Z/A(2)
	PDER(0,2) = PDER(*,1) * Z
	RETURN
END



