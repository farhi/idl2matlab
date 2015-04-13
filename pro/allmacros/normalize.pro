pro normalize,w,n,TIME=time,NOPRINT=noprint,HELP=help

IF KEYWORD_SET(help) THEN BEGIN
  print,''
  print,' Macro (started 25/10/96 by TH.HANSEN) to effectuate a '
  print,' normalization by counting time '
  print,' or by monitor counting rate, by default to monitor (-000)'
  print,''
  print,' Last modification 17-Jun-97 by Th.Hansen'
  print,' Last modification 19-Feb-98 by Th.Hansen'
  print,' Last modification 29-May-01 by Th.Hansen'
  print,' Last modification 01-Jun-01 by Th.Hansen'
  print,' Last modification 02-May-02 by Th.Hansen: Normalization of data read with raw-button released'
  print,''
ENDIF
take_datp,datp
w=float(w)
IF N_ELEMENTS(datp.e) NE N_ELEMENTS(W) THEN mod_datp,datp,'e',W/SQRT(W+1)
nt = 0 & nm = 0
ft = 0 & fm = 0
IF  datp.p(39) lt 0.0 THEN BEGIN
  IF NOT KEYWORD_SET(noprint) THEN print,'Time normalization already effectuated: ',datp.p(39)
  ft = -1.0 /  datp.p(39) 
ENDIF
IF  datp.p(39) gt 0.0 THEN BEGIN
  IF NOT KEYWORD_SET(noprint) THEN print,'Monitor normalization already effectuated: ',datp.p(39)
  fm = 1.0 /  datp.p(39) 
ENDIF
IF N_ELEMENTS(n) NE 1 THEN BEGIN
  IF KEYWORD_SET (time) THEN BEGIN
    n=1 
    IF N_ELEMENTS(w(0,*,*)) GT 1 THEN n=TOTAL(datp.pv(5,*,*))/N_ELEMENTS(datp.pv(0,*,*))
  ENDIF ELSE BEGIN
    n=100000
    IF N_ELEMENTS(w(0,*,*)) GT 1 THEN n=TOTAL(datp.n(*,*))/N_ELEMENTS(datp.n(*,*))
  ENDELSE
ENDIF
IF KEYWORD_SET (time) THEN BEGIN
  IF MIN(datp.pv(5,*)) LE 0.0 THEN n = 0
  IF NOT KEYWORD_SET(noprint) THEN print, 'Normalization to counting time = ',n 
  nt = n
  datp.p(39) = -n 
ENDIF ELSE BEGIN
  IF MIN(datp.n(0,0,*)) LE 0.0 THEN n = 0
  IF NOT KEYWORD_SET(noprint) THEN print, 'Normalization to monitor counting rate = ',n
  nm = n
  datp.p(39) = n 
ENDELSE
nss = N_ELEMENTS (w(0,0,*))		; Number of Scans/slices per numor
ns =  N_ELEMENTS (w(0,*,0))		; Number of Scans/slices per numor
nn = datp.n
IF N_ELEMENTS(datp.n(0,*)) EQ N_ELEMENTS(w(0,*)) AND N_ELEMENTS(datp.n(*,0)) EQ 1 THEN BEGIN
  IF N_ELEMENTS(nn) GT 1 THEN nn=REFORM(nn,N_ELEMENTS(w(0,*)))
ENDIF
pv = datp.pv
ee = datp.e
FOR ss=0,nss-1 DO FOR s=0,ns-1 DO BEGIN
  IF fm eq 0.0 THEN fmm = 1.0 ELSE fmm = (fm*nn(0,0,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1)))
  IF nm eq 0.0 THEN nmm = 1.0 ELSE nmm = (nm/nn(0,0,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1)))
  IF ft eq 0.0 THEN ftt = 1.0 ELSE BEGIN
      ftt = (ft*nn(0,1,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1)))
  ENDELSE
  IF nt eq 0.0 THEN ntt = 1.0 ELSE BEGIN
      ntt = (nt/nn(0,1,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1)))
  ENDELSE
  w (*,s,ss)=w (*,s,ss)*fmm*nmm*ftt*ntt
  ee(*,s,ss)=ee(*,s,ss)*fmm*nmm*ftt*ntt
  nn  (0,0,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1))=nn  (0,0,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1))*fmm*nmm*ftt*ntt
  IF N_ELEMENTS(nn(0,*,0,0)) GE 2 THEN BEGIN
    nn  (0,1,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1))=nn  (0,1,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1))*fmm*nmm*ftt*ntt
    IF N_ELEMENTS(nn(0,*,0,0)) GE 3 THEN BEGIN
	  nn  (0,2,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1))=nn  (0,2,s<(N_ELEMENTS(nn(0,0,*,0))-1),ss<(N_ELEMENTS(nn(0,0,0,*))-1))*fmm*nmm*ftt*ntt
    ENDIF
  ENDIF
ENDFOR
datp.e  = ee
datp.n  = nn
give_datp,datp

END
