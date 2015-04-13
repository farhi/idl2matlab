function rdcon,n
;
; Macro (started 28/10/96 by TH.HANSEN) in order to effectuate a concatenating reading of multiple 
; numors
;
; Last modification 19-Jun-97 by Th.Hansen
;
IF n_params() ge 1 THEN BEGIN
  datp = 0
  w    = 0
  e    = 0
  x    = 0
  pv   = 0
  nn   = 0
  n = long (REFORM (n,n_elements(n)))
  FOR i=0,n_elements(n)-1 DO BEGIN
    wtmp = rdrun(n(i))
    take_datp,datptmp
    IF n_elements(w) le 1 THEN BEGIN
      w   = wtmp
      wt  = datptmp.w_tit		; choose a common title!
      IF i eq 0 THEN ot  = datptmp.other_tit	; choose a common title!
      p   = datptmp.p		; if there are differences in p, no concatenation possible
      pt  = datptmp.par_txt
      pv  = datptmp.pv
      e   = datptmp.e
      nn  = datptmp.n
      x   = datptmp.x		
      y   = datptmp.y		
      z   = datptmp.z
      xt  = datptmp.x_tit
      yt  = datptmp.y_tit
      zt  = datptmp.z_tit
      t   = datptmp.time
      IF n_elements(x) ne n_elements(wtmp) THEN BEGIN
        xtmptmp=wtmp*0.0
        FOR j=0,n_elements(wtmp(0,*))-1 DO xtmptmp(*,j)=x(*)
	x=xtmptmp
      ENDIF
      IF n_elements(y) ne n_elements(wtmp) THEN BEGIN
        ytmptmp=wtmp*0.0
        FOR j=0,n_elements(wtmp(*,0))-1 DO ytmptmp(j,*)=y(*)
	y=ytmptmp
      ENDIF
    ENDIF ELSE BEGIN
      etmp = datptmp.e
      xtmp = datptmp.x
      ytmp = datptmp.y
      ntmp = datptmp.n
      IF n_elements(xtmp) ne n_elements(wtmp) THEN BEGIN
        xtmptmp=wtmp*0.0
        FOR j=0,n_elements(wtmp(0,*))-1 DO xtmptmp(*,j)=xtmp(*)
	xtmp=xtmptmp
      ENDIF
      IF n_elements(ytmp) ne n_elements(wtmp) THEN BEGIN
        ytmptmp=wtmp*0.0
        FOR j=0,n_elements(wtmp(*,0))-1 DO ytmptmp(j,*)=ytmp(*)
	ytmp=ytmptmp
      ENDIF
      IF n_elements(w(*,0)) gt n_elements(wtmp(*,0)) THEN BEGIN
        wtmp=[wtmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
        etmp=[etmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
        xtmp=[xtmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
        ytmp=[ytmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
      ENDIF
      IF n_elements(w(*,0)) lt n_elements(wtmp(*,0)) THEN BEGIN
        w   =[w,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
        e   =[e,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
        x   =[x,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
        y   =[y,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
      ENDIF
      w =   [[w] ,[wtmp      ]]
      e   = [[e] ,[etmp      ]]
      x   = [[x] ,[xtmp      ]]
      y   = [[y] ,[ytmp      ]]
      pv  = [[pv],[datptmp.pv]]
      nn  = [ nn , ntmp       ]
    ENDELSE
  ENDFOR
  mod_datp,datptmp,'w_tit', wt
  mod_datp,datptmp,'other_tit', ot+strcompress(string(round(n(i-1))))
  mod_datp,datptmp,'p', p
  mod_datp,datptmp,'par_txt', pt
  mod_datp,datptmp,'pv', pv
  mod_datp,datptmp,'e', e
  mod_datp,datptmp,'n', nn
  mod_datp,datptmp,'x', x
  mod_datp,datptmp,'y', y
  mod_datp,datptmp,'z', z
  mod_datp,datptmp,'x_tit', xt
  mod_datp,datptmp,'y_tit', yt
  mod_datp,datptmp,'z_tit', zt
  mod_datp,datptmp,'time', t
  give_datp,datptmp  
ENDIF
return,w
END
