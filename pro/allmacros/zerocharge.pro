PRO zerocharge,charge
sum=TOTAL(charge(3,*))/FLOAT(N_ELEMENTS(charge(3,*)))
minus=WHERE(charge(3,*) LT sum,neg)
plus= WHERE(charge(3,*) GT sum,pos)
charge=FLOAT(charge)
charge(3,minus)=-1.
charge(3,plus) =FLOAT(neg)/FLOAT(pos)
PRINT,TOTAL(charge(3,*))
END