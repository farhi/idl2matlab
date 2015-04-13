function makemask,w,cenx,ceny,radius


arx = findgen(64)#(fltarr(64)+1)
ary = transpose(arx)
w_out = w
;TAKE_DATP,P
list = where(((arx-cenx)^2+(ary-ceny)^2) lt radius^2,count)
if (count ne 0) then w_out(list) = 0 else print, 'fuckit'
;GIVE_DATP,P
return, w_out
end
