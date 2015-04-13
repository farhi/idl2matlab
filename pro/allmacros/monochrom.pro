FUNCTION monochrom,w,l,om,om1=om1,om2=om2,d1=d1,d2=d2,l1=l1,l2=l2
;+
; NAME:
;	MONOCHROM
; PURPOSE:
;  Creates a workspace of illumination/divergence of e.g. a sample
;  see also <A HREF="#DIVERGENCE" TARGET="_top">divergence</A>
; INPUTS:
;  W:  Workspace
;  L:  Distance from illuminator to illuminated object (monochromator)
;  Om: Angle of monochromator to axis normal (takeoff/2, pos.=reflection)
; PROJECTS:
;  two sets of slits between monochromator and sample
;  wavelength distribution and monochromator mosaic
; MODIFICATION HISTORY:
;  Written by Thomas Hansen, May 1998
;-
IF KEYWORD_SET(om2) THEN om=om2
take_datp,datp
x=datp.x
y=datp.y
wout=w*0
FOR i=0,N_ELEMENTS(y)-1 DO BEGIN
  FOR j=0,N_ELEMENTS(x)-1 DO BEGIN
    xx=ABS(x-(x(j)-l*tan(y(i)*!PI/180.)))
    xx=MIN(xx)
    wout(j,i) = cos(y(i)*!PI/180.)*w(!C,i)
  ENDFOR
ENDFOR
give_datp,datp
RETURN,wout
end
