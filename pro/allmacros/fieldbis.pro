FUNCTION field,probe,charge,mindist

;+
;FUNCTION field
;PARAMETERS
;	probe:		1D array (4)   of one position  in 3D and its charge
;	charge:		2D array (4,n) of n   positions in 3D (0:2,*) and n charges (3,*)
;	mindist:	minimum distance between probe and charge
;-

x=probe(0)-charge(0,*)
y=probe(1)-charge(1,*)
z=probe(2)-charge(2,*)

div = SQRT(x^2+y^2+z^2)^3 > mindist
f = [TOTAL(probe(3)*charge(3,*)*x), $
     TOTAL(probe(3)*charge(3,*)*y), $
     TOTAL(probe(3)*charge(3,*)*z)]

RETURN,f/div

END
