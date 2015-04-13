PRO xbu_scan_om_xy ,ty_1,ty_n,ty_step,tx_1,tx_n,tx_step,om_1,om_o,om_step,sec
;***
;**
;** The call is mymacro,...

print,!stime
openw,u,"courtois01.xbu",/get_lun
for ty=ty_1,ty_n,ty_step do begin
	printf,u,"pty",ty
	for tx=tx_1,tx_n,tx_step do begin
		printf,u,"ptx",tx
  		printf,u,"som",om_1,om_o,om_step,sec," 1 1 1"
	endfor
endfor
free_lun,u
end
