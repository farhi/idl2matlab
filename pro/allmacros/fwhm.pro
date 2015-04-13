FUNCTION FWHM,U,tt
tt=float(tt)
RETURN,SQRT(U(0)*tan(tt/360*!pi)^2+U(1)*tan(tt/360*!pi)+U(2))
END
