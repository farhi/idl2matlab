FUNCTION rdxrin, filename

take_datp,datp
Openr,in,strmid(filename,0,3)+'in.obc',/get_lun
readf,in,first,step,last,steps,bid
steps=round((last-first)/step)+1
x=findgen(steps)*step+first
w1=fltarr(steps)
w2=fltarr(steps)
readf,in,w1
readf,in,w2
close,in
free_lun,in

plot,x,w2,yrange=[0,max(w1)]
plot,x,w1,yrange=[0,max(w1)],/noerase

set_plot,'ps'
device,filename='xrin.ps'
plot,x,w2,yrange=[0,max(w1)],TITLE=filename
plot,x,w1,yrange=[0,max(w1)],/noerase
device,/close
XICUTE,'$lp -dlj1_d20 xrin.ps'

set_plot,'X'
mod_datp,datp,'x',x
mod_datp,datp,'e',SQRT(w1)
give_datp,datp
return,w2
END
