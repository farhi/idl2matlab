pro pscell_32,w,x=x,mincell=mincell,maxcell=maxcell,cell=cell,wcell=wcell,$
             wnorm=wnorm,wrevers=wrevers,wrevnorm=wrevnorm,enorm=enorm,$
             erevers=erevers,erevnorm=erevnorm,wmean=wmean,emean=emean,$
             mean=mean,sdev=sdev,nops=nops,noplot=noplot
IF NOT KEYWORD_SET(x) THEN x=indgen(N_ELEMENTS(w(0,*)))
e    =w/SQRT(w+1.)
;e    =SQRT(w)
wmean=TOTAL(w,2)/N_ELEMENTS(w(0,*))
emean=TOTAL(e,2)/N_ELEMENTS(e(0,*))
wnorm=float(w)
FOR i=0,N_ELEMENTS(w(0,*))-1 DO wnorm(*,i)=wnorm(*,i)/float(wmean)
enorm=float(e)
FOR i=0,N_ELEMENTS(e(0,*))-1 DO enorm(*,i)=enorm(*,i)/float(wmean)
set_plot,'ps'
mean=fltarr(1600)
sdev=fltarr(1600)
IF NOT KEYWORD_SET(mincell) THEN mincell=0
IF NOT KEYWORD_SET(maxcell) THEN maxcell=1599
IF KEYWORD_SET(cell) THEN BEGIN
  mincell=cell
  maxcell=cell
ENDIF
FOR i=0,1599 DO IF total(w(i,*)) NE 0 THEN BEGIN
  res=MOMENT(w(i,*),SDEV=dev)
  mean(i)=res(0)
  sdev(i)=dev
ENDIF ELSE wnorm(i,*)=1.
FOR i=mincell,maxcell do IF total(w(i,*)) NE 0 THEN BEGIN
  IF NOT KEYWORD_SET(nops) THEN set_plot,'ps'
  filename='000'+strcompress(string(i),/REMOVE_ALL)
  filename=strmid(filename,strlen(filename)-4,4)
  IF NOT KEYWORD_SET(nops) THEN DEVICE,filename='cell'+filename+'.ps'
  wcell=wnorm(i,*)
  IF NOT KEYWORD_SET(nops) THEN plot,wnorm(i,*),$
       yrange=[0,2],$
       title='Cell no.'+strcompress(string(i))+' '+SYSTIME()+', counts:'+strcompress(string(wmean(i))),$       
       subtitle='Error:'+strcompress(string(emean(i)))+' ='+strcompress(string(emean(i)/wmean(i)*100.))+'%, sdev:'+strcompress(string(sdev(i)))
  IF NOT KEYWORD_SET(nops) THEN DEVICE,/CLOSE
  IF NOT KEYWORD_SET(nops) THEN set_plot,'X'
  IF NOT KEYWORD_SET(noplot) THEN plot,wnorm(i,*),yrange=[0,2],title='Cell no.'+strcompress(string(i))+', counts:'+strcompress(string(wmean(i))),$
       subtitle='Error:'+strcompress(string(emean(i)))+' ='+strcompress(string(emean(i)/wmean(i)*100.))+'%, sdev:'+strcompress(string(sdev(i)))
ENDIF
wrevers=REFORM(w,N_ELEMENTS(w(0,*)),1600)
FOR i=0,1599 DO FOR j=0,N_ELEMENTS(w(0,*))-1 DO wrevers(j,i) = w (i,j)
erevers=wrevers/SQRT(wrevers+1.)
;erevers=SQRT(wrevers)
wrevnorm=REFORM(wnorm,N_ELEMENTS(wnorm(0,*)),1600)
FOR i=0,1599 DO FOR j=0,N_ELEMENTS(wnorm(0,*))-1 DO wrevnorm(j,i) = wnorm (i,j)
erevnorm=REFORM(enorm,N_ELEMENTS(enorm(0,*)),1600)
FOR i=0,1599 DO FOR j=0,N_ELEMENTS(enorm(0,*))-1 DO erevnorm(j,i) = enorm (i,j)

END
