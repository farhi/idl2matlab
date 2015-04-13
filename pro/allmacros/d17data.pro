pro d17data,data
  DIMS=lonarr(3)
; get the data structure and read the data array
  d=DIAL_MAD_READ('t_res')
  DIMS(0)=d.par_tof(0)
  DIMS(1)=d.par_tof(4) - d.par_tof(3) +1
  DIMS(2)=d.par_tof(2) - d.par_tof(1) +1
;  if prin eq 1 then print,"Dimensions:",DIMS
  t=DIMS(0) & y=DIMS(1) & x=DIMS(2)
;  print,x,y,t
  data=DIAL_MAD_READ('data')
;  help,data
  data=data(0:x*y*t-1)

	if y eq 1 then begin 
          data=reform(data,x,t)
        endif
        if t eq 1 then begin
          data=reform(data,y,x)
          data=transpose(data)
        endif
;   help,data
;  data is in array data(x,y) for mono and data(x,t) for tof
wait,.2
end
