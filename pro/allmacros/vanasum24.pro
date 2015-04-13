FUNCTION vanasum, w,a
;********
;**
;** The call is w6=vanasum(...)
	take_datp,d
	x=d.x
	s=FLTARR(N_ELEMENTS(w(*,0)))
	a=s
	e=s
	print,median(w)
	FOR i=32,N_ELEMENTS(w(*,0))-1-32 DO BEGIN
		ind=WHERE(	(x(i,*) LE -9.1 OR x(i,*) GE  5.1) $
			AND 	(x(i,*) LE  62 OR x(i,*) GE  65) $
			AND 	(x(i,*) LE 129 OR x(i,*) GE 131), cnt)
		;tmp=WHERE(w(i,ind) LT median(w)/2,tmpcnt)
		;FOR j=0,tmpcnt-1 DO BEGIN
		;	PRINT,i,ind(tmp(j)),x(i,ind(tmp(j))),w(i,ind(tmp(j)))
		;ENDFOR
		IF cnt GT 1 THEN s(i)=MEAN(w(i,ind)) ELSE s(i)=0
		IF cnt GT 1 THEN e(i)=STDDEV(w(i,ind)) ELSE e(i)=0
		a(i)=cnt
	ENDFOR
	mod_datp,d,'x',indgen(N_ELEMENTS(w(*,0)))
	mod_datp,d,'z',a
	mod_datp,d,'e',e
	give_datp,d
	RETURN,s
END
