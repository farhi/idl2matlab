PRO exasc,w,filename,HELP=help
;
; Started: 02-Dec-96 by Th.Hansen, ILL-Grenoble
; Output of data 
; Last modification: 02-Dec-96 by Th.Hansen
;
IF KEYWORD_SET(help) THEN BEGIN
  print,''
  print,'                         PRO ExAsc.PRO'
  print,''
  print,'LAMP-IDL Macro (Procedure) started: 02-Dec-96 by Th.Hansen, ILL-Grenoble (D20)'
  print,'Output of data '
  print,''
  print,"Call:      EXASC,W[,'filename'][,/HELP]"
  print,"Variables: Workspace (necessary, won't be changed)"
  print,"           basic filename different from '' (optional, will be changed)"
  print,'Keywords:  HELP: this hopefully helping text'
  print,''
  print,'Last modification: 02-Dec-96 by Th.Hansen'
  print,''
ENDIF
IF n_elements(filename) EQ 0 THEN filename = ''
take_datp,datp
x=w
y=fltarr(N_ELEMENTS(w(0,*)))
IF N_ELEMENTS(datp.x(0,*)) GT 1 THEN x=datp.x ELSE FOR i=0,N_ELEMENTS(w(0,*))-1 DO x(*,i)=datp.x(*)
IF N_ELEMENTS(datp.y(0,*)) GT 1 THEN FOR i=0,N_ELEMENTS(w(0,*))-1 DO y(i)=datp.y(0,i) ELSE y=datp.y 
filename=string(filename)
filename = strcompress(filename,/REMOVE_ALL)
FOR i=0,n_elements(w(0,*))-1 DO BEGIN
  filext = string(long(datp.p(0)))+'.'+string(long(i))
  filext=strcompress(filext,/REMOVE_ALL)
;  print,'FileName: ',filename+filext
  OPENW,out,filename+filext,/get_lun
  PRINTF,out,'TwoTheta, Counts, Z=',y(i),', File: ',filename+filext
  FOR j=0,N_ELEMENTS(w(*,i))-1 DO PRINTF,out,x(j,i),w(j,i)
  FREE_LUN,out
ENDFOR
END

