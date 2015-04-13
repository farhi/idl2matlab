FUNCTION fieldvec,probe,charge,mindist

;+
;FUNCTION field
;PARAMETERS
;	probe:		1D array (3)   of one position  in 3D
;	charge:		2D array (4,n) of n   positions in 3D (0:2,*) and n charges (3,*)
;	mindist:	minimum distance between probe and charge
;-

div=SQRT((probe[0]-charge[0,*])^2+(probe[1]-charge[1,*])^2+(probe[2]-charge[2,*])^2)^3>mindist
f=[TOTAL(charge[3,*]*(probe[0]-charge[0,*])/div),TOTAL(charge[3,*]*(probe[1]-charge[1,*])/div),TOTAL(charge[3,*]*(probe[2]-charge[2,*])/div)]

RETURN,f

END
