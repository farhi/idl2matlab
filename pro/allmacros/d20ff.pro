pro d20ff,w,filename
@lamp.cbk
take_datp,datp
datname=filename+'.dat'
PRINT,'Please use d20reg for output of three-columns xy-data!'
wmax=0.
OPENW,dat,datname,/get_lun
IF N_ELEMENTS(w(0,*,*)) GT 1 THEN PRINT,'Only the first pattern will be exported'
ww=w(*,0,0)
nd=ROUND(10.*(max(datp.x(*,0,0))-min(datp.x(*,0,0)))+1.)
x=FINDGEN(nd)+min(datp.x(*,0,0))
ww=INTERPOL(w(*,0,0),datp.x(*,0,0),x)
ee=INTERPOL(datp.e(*,0,0),datp.x(*,0,0),x)
wmax=max(ww)
wmax_old=wmax
WHILE wmax gt 9999999.9 DO wmax=wmax/10.
norm=wmax_old/wmax
if datp.p(12) gt 9990. then datp.p(12)=0.
if datp.p(11) gt 9990. then datp.p(11)=0.
PRINTF,dat,format='(A5,A75)','D1A5 ',datp.w_tit    
PRINTF,dat,format='(A80)',datp.other_tit    
PRINTF,dat,format='(A80)',' '
PRINTF,dat,format='(i6,2f8.2,i6,2f12.0)',10*FLOOR(.1+N_ELEMENTS(w(*,0,0))/10),datp.p(12),datp.p(11),1,datp.n(0,0,0)/norm,datp.p(30)
NNN=N_ELEMENTS(w(*,0,0))-1
PRINTF,dat,FORMAT='(3f10.3)',datp.x(0,0,0),(datp.x(1,0,0)-datp.x(0,0,0)),datp.x(nnn,0,0)
NN=(N_ELEMENTS(w(*,0,0)))/10.
FOR ii=0,NN-1 DO BEGIN
i=10*ii
PRINTF,dat,FORMAT='(10F8.0)',ww(i)/norm,ww(i+1)/norm,ww(i+2)/norm,ww(i+3)/norm,ww(i+4)/norm,ww(i+5)/norm,ww(i+6)/norm,ww(i+7)/norm,ww(i+8)/norm,ww(i+9)/norm
ENDFOR
FOR ii=0,NN-1 DO BEGIN
i=10*ii
PRINTF,dat,FORMAT='(10F8.2)',ee(i)/norm,ee(i+1)/norm,ee(i+2)/norm,ee(i+3)/norm,ee(i+4)/norm,ee(i+5)/norm,ee(i+6)/norm,ee(i+7)/norm,ee(i+8)/norm,ee(i+9)/norm
ENDFOR
CLOSE,dat
FREE_LUN,dat
END
