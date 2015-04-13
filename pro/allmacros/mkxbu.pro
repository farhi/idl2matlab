PRO mkxbu, xbu,motor,start,stop,step,time,flag,save,rep
;***
;**
;** The call is mkxbu,'sth.xbu','2theta',-5,-3,.1,10,1,1,1

OPENW,file,xbu,/get_lun
start=float(start)
stop=float(stop)
FOR value=start,stop,step DO BEGIN
	PRINTF,file,"pos ",motor,value
	PRINTF,file,"acq",time,flag,save,rep
ENDFOR
FREE_LUN,file
end
