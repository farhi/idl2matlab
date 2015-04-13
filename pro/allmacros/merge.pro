PRO merge,w,n,norm=norm,help=help

IF KEYWORD_SET(help) THEN BEGIN
  print,''
  print,'                         PRO Merge.PRO'
  print,''
  print,'LAMP-IDL Macro (Procedure) started: 03-Nov-97 by Th.Hansen, ILL-Grenoble (D20)'
  print,'Modification of a workspace w, merging each n single diagrams together'
  print,'Usefull tool to regroup thermodiffractometries where counting times were too short'
  print,'Afterwards the workspace w contains n times less single diagrams'
  print,'Monitor normalisation to the common monitor counting rate of the original workspace may be done by choosing the keyword norm'
ENDIF

take_datp,datp
n=ceil(n)>1
n=     n <N_ELEMENTS(w(0,*))
IF KEYWORD_SET(norm) THEN IF datp.n(0,0,0) EQ datp.n(0,0,N_ELEMENTS(w(0,*))-1) THEN norm=datp.n(0,0)
ww=     w   (*,0:N_ELEMENTS(w(0,*))/n-1)
ee=datp.e   (*,0:N_ELEMENTS(w(0,*))/n-1)
nn=datp.n (*,*,0:N_ELEMENTS(w(0,*))/n-1)
pv=datp.pv  (*,0:N_ELEMENTS(w(0,*))/n-1)
yy=datp.y   (  0:N_ELEMENTS(w(0,*))/n-1)
datp.other_tit=datp.other_tit+' merg'+STRING(n)
FOR i=0,N_ELEMENTS(w(0,*))/n-1 DO BEGIN
  ww   (*,i)=     TOTAL     (w  (*,  i*n:(i+1)*n-1),  2)
  ee   (*,i)=SQRT(TOTAL(datp.e  (*,  i*n:(i+1)*n-1)^2,2))
  pv   (*,i)=           datp.pv (*,  i*n)
  yy   (  i)=           datp.y  (    i*n)
  nn (0,0,i)=     TOTAL(datp.n  (0,0,i*n:(i+1)*n-1))
  nn (0,1,i)=     TOTAL(datp.n  (0,1,i*n:(i+1)*n-1))
  pv   (5,i)=     TOTAL(datp.pv (5,  i*n:(i+1)*n-1))
  pv  (30,i)=     TOTAL(datp.pv(30,  i*n:(i+1)*n-1))
  IF KEYWORD_SET(norm) THEN BEGIN
    ww(*,i)=ww(*,i)/nn(0,0,i)*norm
    ee(*,i)=ee(*,i)/nn(0,0,i)*norm
    nn(0,0,i)      =norm
  ENDIF
ENDFOR
datp.p( 5)=TOTAL(datp.pv( 5,*))
datp.p(30)=TOTAL(datp.pv(30,*))
mod_datp,datp,'e', ee
mod_datp,datp,'y', yy
mod_datp,datp,'n', nn
mod_datp,datp,'pv',pv
w=ww
give_datp,datp
END
