PRO makemicrostripplate,plate,charge

plate = INTARR(82176L)

FOR cell=0L,31L DO BEGIN

  cellpos = cell * 2568L

  FOR strip=0L,3L DO BEGIN
    
    strippos = cellpos + strip * 642L
    plate (strippos + 315L        : strippos + 315L +  12L        - 1L) =  1
    plate (strippos               : strippos + 145L               - 1L) = -1
    plate (strippos + 145L + 352L : strippos + 145L + 352L + 145L - 1L) = -1
    
  ENDFOR

  plate (cellpos               : cellpos        + 60L - 1L) = 0
  plate (cellpos + 2568L - 60L : cellpos + 2568L      - 1L) = 0

ENDFOR

index       = WHERE (plate NE 0, count)
charge      = LONARR (4, count)
charge(0,*) = FLOAT (index) 
charge(3,*) = plate (index) 
zerocharge, charge

END