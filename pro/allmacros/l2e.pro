	FUNCTION l2e, w_in

;converts from cts/lambda to cts/energy
;
; input=output format: w(128,nspectra)
;							KHA, 30/10/96


	take_datp, datp

	sw=SIZE(w_in)
	PRINT,'SIZE(w_in)=',sw
	IF (sw(0) EQ 1) THEN nspectra=1 $
			ELSE nspectra=sw(2)
	nchannels=sw(1)

	PRINT,'nchannels=',nchannels,' nspectra=',nspectra
	x_in=datp.x
	e_in=datp.e

	const1=5.22697		; E(meV)=const1*V(m/ms)^2 for neutron
	const2=2.07193571	; E(meV)=const2*Q(A^-1)^2 for neutron
	const3=3.956076		; V(m/ms)=const3/lambda(A) for neutron
	const4=81.8066		; E(meV)=const4/lambda(A)^2 for neutron

	x_out=const4/x_in^2

	x_out=REVERSE(x_out)
	w_out=REVERSE(w_in,1)
	e_out=REVERSE(e_in,1)

	datp.x=x_out
	datp.x_tit='Neutron Energy (meV)'
	datp.e=e_out

	give_datp, datp

	RETURN, w_out
	END
