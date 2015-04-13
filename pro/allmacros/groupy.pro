pro groupy, w,y,e ,AVERAGE=average
;** ******
;**
;** To group spectra (w) having the same ordinate (y)
;** Spectra are added.

d=100 ;means check only 2 decimals

;** Check concistencies
    sw=SIZE(w)
    sy=SIZE(y)
    IF sw(0) eq 2 THEN IF sw(2) eq sy(1) THEN BEGIN

	IF n_elements(e) eq n_elements(w) THEN r=1 else r=0
	IF r THEN E=E^2
	flg =lonarr(sy(1))+1
	wout=w*0 & wout(*,0)=w(*,0)
	yout=y*0 & yout  (0)=y  (0)
	j   =  0

	FOR i=1,sy(1)-1 DO BEGIN

	    IF  round(y(i)*d) eq round(yout(j)*d) THEN BEGIN
		wout(*,j)= wout(*,j)+w(*,i)
		IF r THEN  E   (*,j)=E(*,j)+w(*,i)
		flg(j)=flg(j)+1

	    ENDIF ELSE BEGIN
		j=j+1
		wout(*,j)=w(*,i)
		yout(  j)=y(  i)
		IF r THEN E(*,j)=E(*,i)
	    ENDELSE
	ENDFOR

	IF r THEN E=SQRT(E)

	IF keyword_set(average) THEN FOR i=0,j DO BEGIN
					wout(*,i)=wout(*,i)/flg(i)
					IF r THEN E   (*,i)=E(*,i)/flg(i)
				     ENDFOR
	w=wout(*,0:j)
	y=yout(  0:j)
	IF r THEN E=E(*,0:j)

    ENDIF
RETURN
END
