;-------------------------------------------------------------------------------
;*******************************************************************************
;
	FUNCTION read_pol, w_in

;reads in the calculated polariser transmissions from polariser.dat
;w_in is a dummy workspace - not used for anything
;							KHA, 30/10/96
;-------------------------------------------------------------------------------
;*******************************************************************************

	take_datp, datp

	in_file='/hosts/d7sgi/usr1/people/lambda/polariser.dat'

	line=''
	A=FLTARR(6,32)
	OPENR, 1, in_file, ERROR=err
	IF (err NE 0) THEN PRINT, !ERR_STRING
	READF, 1, A
	CLOSE, 1
	x_out=A(0,*)
	w_out=A(3,*)/10000.
	e_out=FLTARR(32)	& e_out(*)=0.

	mod_datp, datp, "x", x_out
	mod_datp, datp, "e", e_out

	datp.x_tit='Neutron Wavelength'
	datp.y_tit='Transmission'

	give_datp, datp

	RETURN, w_out
	END
