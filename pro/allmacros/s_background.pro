	FUNCTION s_background, w_in1, emp, cad, T

; Performs background subtraction of normalised SANS data.

; w_in1=sample run
; emp is number of workspace containing the background run
; cad is number of workspace containing the cadmium run (optional)
; For attenuation corrections without a cadmium run set cad=0

;						JRS, 2/3/00
 	
	iprint=0

	IF (iprint GT 0) THEN PRINT,'Start s_background:'

	take_datp, dat1
	take_w, w_in2, W=emp
	take_datp, dat2, W=emp
	IF(N_ELEMENTS(cad) EQ 0) THEN cad=0
	IF (cad GT 0) THEN BEGIN
		take_w, w_in3, W=cad
		take_datp, dat3, W=cad
	ENDIF ELSE BEGIN
		w_in3=0.*w_in2
	ENDELSE

;-------------------------------------------------------------------------------
;Check dimensions of input workspaces and setup output workspace

	sw1=SIZE(w_in1)
	sw2=SIZE(w_in2)
	sw3=SIZE(w_in3)
	par1=dat1.p
	par2=dat2.p
	IF (cad GT 0) THEN par3=dat3.p
	parv2=dat2.pv
	IF (cad GT 0) THEN parv3=dat3.pv	
	e_in1=dat1.e
	e_in2=dat2.e
	IF(cad GT 0) THEN e_in3=dat3.e ELSE e_in3=0.*e_in2
		
	s1=SIZE(w_in1)
	s2=SIZE(w_in2)
	s3=SIZE(w_in3)
	IF (s1(0) EQ 3) THEN nruns=s1(3) ELSE nruns=1
	IF (s2(0) NE 2 OR s3(0) NE 2) THEN BEGIN
		PRINT, 's_background:ERROR - background runs should not be concatenated'
	ENDIF
	
	IF (iprint GT 0) THEN PRINT,'End of "w_in dimensions check etc." section'

;-------------------------------------------------------------------------------
; Perform Backgound Subtraction

	IF(N_ELEMENTS(T) EQ 0) THEN T=1.		;default T=1

	PRINT,'Background Subtraction using:'
	PRINT,'Background runs in workspace',emp
	IF (cad GT 0) THEN PRINT,'Cadmium runs in workspace',cad ELSE $
			   PRINT,'No cadmium runs'
	PRINT,'T= ',T

        tarr=0.*w_in1+T
	w_out=w_in1
	e_out=e_in1
	FOR i=1,nruns DO BEGIN
		w_out(*,*,i-1)=(1./tarr)*(w_in1(*,*,i-1)-w_in3)-(w_in2-w_in3)
		e_out(*,*,i-1)=sqrt((e_in1(*,*,i-1)/tarr)^2+e_in2^2+((1.-1./tarr)*e_in3)^2)	
	ENDFOR

        IF(iprint GT 0) THEN PRINT,'End of background subtraction section'

;-------------------------------------------------------------------------------
;Return parameters and exit
	
	dat1.e=e_out

        bgnumor=STRTRIM(STRING(LONG(par2(26))),2)
	
	IF (cad GT 0) THEN BEGIN
        	cdnumor=STRTRIM(STRING(LONG(par3(26))),2)
	ENDIF ELSE BEGIN
		cdnumor='no cd'
	ENDELSE

	muRs=STRTRIM(STRING(T),2)
muR0:	n=STRLEN(muRs)	& i=RSTRPOS(muRs,'0')
	IF (i EQ n-1) THEN BEGIN
		muRs=STRMID(muRs,0,n-1)
		GOTO, muR0
	ENDIF

	params=muRs

	dat1.other_tit=dat1.other_tit+' -bg(#'+bgnumor+',#'+cdnumor+','+params+')'

finished:

	IF (iprint GT 0) THEN PRINT,'End background:'

	give_datp, dat1

	RETURN, w_out

	END
