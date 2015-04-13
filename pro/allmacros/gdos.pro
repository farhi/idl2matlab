FUNCTION gdos,w_in,Temp=temp
;** *********************************************************
;** S. Rols 11/01 srols@anl.gov
;** The call is w6=gdos(w_in,Temp=T)
;** Last modification 04.02.02 to include QENS_raw treatment
;**
;** *********************************************************

COMMON c_lamp_access, inst
COMMON printing, iprint, outstring

take_datp, datp
par=datp.p
x_in=datp.x
y_in=datp.y
e_in=datp.e
;--------------------------------------------
; Donnees experimentales

Print,'GDOS ## S.ROLS 2002 ##'
Print,inst
lambda=0.

CASE inst OF
;		'MiBeMol':BEGIN
;		lambda=FLOAT(par(10))
;		temp=FLOAT(par(5))
;		ei=81.799/(lambda^2)+x_in*0.
;		END
		'DCSasc':BEGIN
		lambda=FLOAT(par(8))
		ei=81.799/(lambda^2)+x_in*0.
		END
		'QENS_raw':BEGIN
		ef=3.167 ;average final neutron energy in meV
		lambda=SQRT(81.799/ef)
		ei=x_in+ef
		END
		ELSE:BEGIN
		lambda=par(21)
		temp=par(11)
		ei=81.799/(lambda^2)+x_in*0.
		END
ENDCASE
print,inst
print, 'temp=',temp
print, 'lambda=',lambda
IF temp EQ 0.0 THEN temp=300.0

;ei=81.799/(lambda^2)
;if n_elements(y_in) GT 0 then tetam=total(y_in)/n_elements(y_in) else tetam=90
;print,'tetam=',tetam
;w_buf ... = Tableaux de travail
w_buf=w_in & e_buf=e_in & y_out=y_in
;
;Calcul de Q^2
q2=w_in*0.0
x_in=FLOAT(x_in)
FOR itet=0,n_elements(y_in)-1 DO q2(*,itet)=2*ei(*)-x_in(*)-2*sqrt(ei(*)*ABS(ei(*)-x_in(*)))*cos(y_in(itet)*!pi/180)
q2=q2/2.072
;print,'q2=',q2
;
;Calcul de w/n(w) ou n(w) est le facteur de temperature dans le cas Stokes et anti Stokes
bosex=FLOAT(x_in)
bosex=bosex/(1-exp(-1.*bosex*11.6045/temp))
;
;Corrections
indnul=WHERE(ABS(bosex) LE 1.e-12)
IF n_elements(indnul) GT 1 THEN BEGIN
w_buf(indnul,0:n_elements(y_in)-1)=0.
bosex(indnul)=1.
END
w_buf=w_in/q2
e_buf=e_buf/q2
FOR itet=0,n_elements(y_in)-1 DO BEGIN
w_buf(*,itet)=w_buf(*,itet)/bosex(*)
w_buf(*,itet)=w_buf(*,itet)*(x_in(*))^2
e_buf(*,itet)=e_buf(*,itet)/bosex(*)
e_buf(*,itet)=e_buf(*,itet)*(x_in(*))^2
ENDFOR

;
x_out=x_in
e_out=e_buf
w_out=w_buf
mod_datp, datp, "e", e_out
give_datp, datp
return, w_out
end
