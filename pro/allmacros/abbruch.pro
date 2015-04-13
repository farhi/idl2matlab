PRO abbruch,x,a,y,ps=ps,portrait=portrait,landscape=landscape
;+
; NAME:
;	abbruch
;
; PURPOSE:
;	This procedure calculates the analogous neutron detection signal.
;
; CATEGORY:
;	Instrument.
;
; CALLING SEQUENCE:
;
;	ABBRUCH, X, A, Y
;
;
; INPUTS:
;   X   :
;   A(0): factor,
;   A(1): t0,
;   A(2): sigma
;   A(3): capture_ratio,
;   A(4): gamma_ratio,
;   A(5): gamma_exp,
;   Y   :
;
; OPTIONAL INPUTS:
;	none
;	
; KEYWORD PARAMETERS:
;	none
;
; OUTPUTS:
;	?
;
; OPTIONAL OUTPUTS:
;	none
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none?
;
; RESTRICTIONS:
;	none?
;
; PROCEDURE:
;	no foobar superfloatation method
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by:	Thomas Hansen, May 1997.
;	May, 1997	Documentation
;
;-

take_datp,datp
w=x*0.

FUNCT,x,[a(0)*(1-a(3)),a(1),a(2),0,0,0],y
FOR i=a(1)/4,a(1),a(1)/100. DO BEGIN
  FUNCT,x,[a(0)*a(3),i,a(2),0,0,0],w
  y = y+w/100.
ENDFOR
FOR i=3*a(1)/4,a(1),a(1)/100. DO BEGIN
  FUNCT,x,[a(0)*a(3),i,a(2),0,0,0],w
  y = y+w/100.
ENDFOR
y=y+a(0)*a(4)/x^a(5)
IF KEYWORD_SET(ps) THEN BEGIN
  IF NOT KEYWORD_SET(portrait) THEN portrait=0 ELSE landscape=0
  IF NOT KEYWORD_SET(landscape) THEN landscape=0
  i=1
  WHILE y(i) LE y(i-1) AND i LT N_ELEMENTS(y) DO i = i+1
  IF NOT (i LT N_ELEMENTS(y)) THEN i=0
  SET_PLOT,'ps'
  DEVICE,FILENAME='anode.ps',portrait=portrait,landscape=landscape
  PLOT,x,y,$
    TITLE='Signal histogram for detector anode '+systime(),$
    YRANGE=[0,max(y(i:N_ELEMENTS(y)-1))],$
    YTITLE='Number of counts',$
    XTITLE='Signal amplitude',$
    SUBTITLE='hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,4)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,4)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,4)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,4)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,4)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,4)
  DEVICE,/CLOSE
  SET_PLOT,'x'
ENDIF
mod_datp,datp,'w_tit','Signal histogram for detector anode'
mod_datp,datp,'other_tit','hght'+STRMID(STRCOMPRESS(STRING(a(0))),0,4)+$
             ', Pk'+STRMID(STRCOMPRESS(STRING(a(1))),0,4)+$
             ', sg'+STRMID(STRCOMPRESS(STRING(a(2))),0,4)+$
             ', cpt'+STRMID(STRCOMPRESS(STRING(a(3))),0,4)+$
             ', g_rt'+STRMID(STRCOMPRESS(STRING(a(4))),0,4)+$
             ', g_exp'+STRMID(STRCOMPRESS(STRING(a(5))),0,4)
take_datp,datp
END
