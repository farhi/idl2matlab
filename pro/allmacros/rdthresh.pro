function rdthresh,fil

OPENR,in,fil,/get_lun
w=fltarr(1600)
readf,in,w
free_lun,in
RETURN,w

END
