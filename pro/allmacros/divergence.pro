FUNCTION divergence,dx,xmax,ddiv,divmax,dlam,lammax,ddelta,Deltamax,d,l,d2,l2,om,mos,dspac,takeoff,d3,l3,d4,l4,ds,ls,ld,lg,dsample,$
                    w1=w,x1=x,w2=w2,x2=x2,w3=w3,w4=w4a,ws=w5,wd=w6
;+
; NAME:
;	 DIVERGENCE
;
;	 <A HREF="divergence.pro">See source code!</A>
;
; PURPOSE:
;  Creates a workspace of illumination and divergence of e.g. a monochromator
;
; INPUTS:
;  Dx:       Spacial definition / mm
;  Xmax:     Beamwidth/2 / mm
;  Ddiv:     Divergence definition / degrees
;  Divmax:   Maximum positive divergence / degrees
;  Dlam:     Wavelength definition / Angstroem
;  Lammax:   Maximum positive delta(lambda) / Angstroem
;  Ddelta:   Deviation definition / degrees
;  Deltamax: Maximum positive deviation by monochromator / degrees
;  D:        Width of illuminating source
;  L:        Distance from illuminator to slits in front of the monochromator / mm
;  D2:       Width slit in front of monochromator / mm
;  L2:       Distance from illuminating source to center of the illuminated monochromator / mm
;  Om:       Angle of monochromator to axis normal (transmission: takeoff/2, reflection: (takeoff/2)-90 deg)
;  Mos:      Mosaic of monochromator (degrees)
;  Dspac:    d-spacing of monochromator / Angstroem
;  Takeoff:  Takeoff angle
;  D3:       Width of slits after monochromator / mm
;  L3:       Distance from monochromator to first secondary slit (monochromator) / mm
;  D4:       Width of slits before sample / mm
;  L4:       Distance from monochromator to second secondary slit (sample) / mm
;  Ds:       Diameter of sample / mm
;  Ls:       Distance from monochromator to sample center / mm
;  Ld:       Distance from sample center to detector entrance / mm
;  Lg:       Distance from detector entrance to microstrip detection plate (detection gap) / mm
;  Dsample:  d-spacing of Bragg reflection of sample / Angstroem
;
; KEYWORDS (OPTIONAL FOR OUTPUT):
;  W1:       Beam arrived at slit before monochromator
;  X1:       Beam arrived at slit before monochromator : position
;  W2:       Beam arrived at monochromator
;  X2:       Beam arrived at monochromator : position
;  W3:       Beam arrived at slit after monochromator
;  W4:       Beam arrived at slit before sample
;  Ws:       Beam arrived at cylindrical sample
;  Wd:       Beam arrived at detector entrance window
;
; PROJECTS:
;  second set of slits between source and monochromator
;  wavelength distribution
;  monochromator penetration
;  incident neutron spectrum
;  for-next-loops in c
;
; MODIFICATION HISTORY:
;  Written by: Thomas Hansen, May 1998.
;  June, 1998 Documentation, wavelength distribution, slits, sample to detector.
;
;-

ddelta=Dlam/Lammax*Deltamax 
PRINT,Dx,      " = Dx:       Spacial definition / mm"
PRINT,Xmax,    " = Xmax:     Beamwidth/2 / mm"
PRINT,Ddiv,    " = Ddiv:     Divergence definition / degrees"
PRINT,Divmax,  " = Divmax:   Maximum positive divergence / degrees"
PRINT,Dlam,    " = Dlam:     Wavelength definition / Angstroem"
PRINT,Lammax,  " = Lammax:   Maximum positive delta(lambda) / Angstroem"
PRINT,Ddelta,  " = Ddelta:   Deviation definition / degrees"
PRINT,Deltamax," = Deltamax: Maximum positive deviation by monochromator / degrees"
PRINT,D,       " = D:        Width of illuminating source"
PRINT,L,       " = L:        Distance from illuminator to slits in front of the monochromator / mm"
PRINT,D2,      " = D2:       Width slit in front of monochromator / mm"
PRINT,L2,      " = L2:       Distance from illuminating source to center of the illuminated monochromator / mm"
PRINT,Om,      " = Om:       Angle of monochromator to axis normal (transmission: takeoff/2, reflection: (takeoff/2)-90 deg)"
PRINT,Mos,     " = Mos:      Mosaic of monochromator (degrees)"
PRINT,Dspac,   " = Dspac:    d-spacing of monochromator / Angstroem"
PRINT,Takeoff, " = Takeoff:  Takeoff angle"
PRINT,D3,      " = D3:       Width of slits after monochromator / mm"
PRINT,L3,      " = L3:       Distance from monochromator to first secondary slit (monochromator) / mm"
PRINT,D4,      " = D4:       Width of slits before sample / mm"
PRINT,L4,      " = L4:       Distance from monochromator to second secondary slit (sample) / mm"
PRINT,Ds,      " = Ds:       Diameter of sample / mm"
PRINT,Ls,      " = Ls:       Distance from monochromator to sample center / mm"
PRINT,Ld,      " = Ld:       Distance from sample center to detector entrance / mm"
PRINT,Lg,      " = Lg:       Distance from detector entrance to microstrip detection plate (detection gap) / mm"
PRINT,Dsample, " = Dsample:  d-spacing of Bragg reflection of sample / Angstroem"
; The source: all wavelengths and divergences are equally distributed
take_datp,datp
x = FINDGEN (ROUND (2 * Xmax   / Dx  )) * Dx   - Xmax
y = FINDGEN (ROUND (2 * Divmax / Ddiv)) * Ddiv - Divmax
w = FLTARR(N_ELEMENTS(X),N_ELEMENTS(Y))
j=indgen(N_ELEMENTS(x))
FOR i=0,N_ELEMENTS(y)-1 DO BEGIN
  k=(x(j) GE l*tan(!pi/180.*( y(i)))-d/2 AND x(j) LE l*tan(!pi/180.*( y(i)))+d/2 AND x(j) GE -d2/2 AND x(j) LE  d2/2)
  w(j,i) = cos(y(i)*!PI/180.)*k(j)
ENDFOR
PRINT,'Beam profile at slits before monochromator (w1)'
x1=x

; Now we are at the slits of max(x)-min(x) at the distance l from the source ...
w2=w*0
x2=x/cos(om*!PI/180.)
y2=x*tan(om*!PI/180.)
maxj=N_ELEMENTS(x)-1
maxi=N_ELEMENTS(y)-1
FOR i=0,maxi DO BEGIN
  jj= ROUND(((x2(*)-((l2-l)+y2(*))*tan(( y(i))*!PI/180.))+Xmax)/Dx)
  w2(j,i) = w2(j,i)+cos(y(i)*!PI/180.)*w((jj>0)<maxj,i)*cos(om*!PI/180.)*(jj GE 0 AND jj LE maxj)
ENDFOR
PRINT,'Beam profile on monochromator (w2)'

; Now we are at the monochromator - its mosaic will change divergence and wavelength ...
; ATTENTION : Up to now the monochromator has a penetration depth of 0!
; ATTENTION : Up to now no FANKUCHEN effect!
x=x2*cos((takeoff-om)*!PI/180.)
y2=x2*sin((takeoff-om)*!PI/180.)
lam = 2.*dspac * sin (takeoff/2. * !pi / 180.)
PRINT,"Wavelength",lam
z = FINDGEN (ROUND (2 * Lammax / Dlam)) * Dlam - Lammax + lam
w3=fltarr(N_ELEMENTS(x),N_ELEMENTS(y),N_ELEMENTS(z))
int = 1.
pik = int*2./mos*sqrt(alog(2)/!pi)
sig = mos/(2.*sqrt(2.*alog(2.)))
gau = GAUSS(y,[pik,0,sig])
delta=FINDGEN (ROUND (2 * Deltamax / Ddelta)) * Ddelta - Deltamax 
maxk=N_ELEMENTS(z)-1
kk= ROUND((2.*dspac*sin((takeoff/2.-delta(*))*!pi/180.)-lam+lammax)/dlam)
FOR k=0,N_ELEMENTS(delta)-1 DO BEGIN      ; fuer Ablenkungen delta
  y3=y+delta(k)
  ;sumk=TOTAL(w3)
; ii= ROUND(((y(*))-delta(k)+DivMax)/DDiv)
  ii= ROUND((y3(*)+DivMax)/DDiv)
  iii= ROUND((-y(*)+delta(k)/2.+DivMax)/DDiv)
  FOR i=0,maxi DO BEGIN    ; resulting divergence
;   jj= ROUND(((x2(*)-((l2-l)+y2(*))*tan(( y (i))*!PI/180.)                           )+Xmax)/Dx)
;   jj= ROUND(((x (*)-( l3   -y2(*))*tan(( y3(i))*!PI/180.))/cos((takeoff-om)*!PI/180.)+Xmax)/Dx)  ; cosinus ?
    jj= ROUND(((x (*)-( l3   +y2(*))*tan(( y3(i))*!PI/180.)                           )+Xmax)/Dx)  
    help=(jj GE 0 AND jj LE maxj AND kk(k) GE 0 AND kk(k) LE maxk AND ii(i) GE 0 AND ii(i) LE maxi AND iii(i) GE 0 AND iii(i) LE maxi)
    w3(*,(ii(i)>0)<maxi,(kk(k)>0)<maxk) = w3(*,(ii(i)>0)<maxi,(kk(k)>0)<maxk)+cos((takeoff-om-y3(i))*!PI/180.)*w2((jj>0)<maxj,i)*GAU((iii(i)>0)<maxi)*help
  ENDFOR
  ;PRINT,'step',k,':',TOTAL(w3)-sumk,' counts at',delta(k),'deg beam deviation'
ENDFOR
jj=WHERE(x le -d3/2 OR x ge d3/2,count)
jj=(jj>0)<maxj
w3(jj,*,*)=(count LE 0)*w3(jj,*,*)
PRINT,'Beam profile a(f)t(er) monochromator slits (w3)'

; Now are at the first set of slits behind the monochromator, performing secondary collimation
; The next slit will just reduce the background at the sample
w4=w3*0.
FOR k=0,maxk DO BEGIN      ; for different wavelengths
  FOR i=0,maxi DO BEGIN    ; for different divergences
    jj= ROUND(((x(*)-(l4-l3)*tan(( y(i))*!PI/180.))+Xmax)/Dx)
    w4((jj>0)<maxj,i,k) = w4((jj>0)<maxj,i,k)+cos(y(i)*!PI/180.)*w3((jj>0)<maxj,i,k)*(jj GE 0 AND jj LE maxj)
  ENDFOR
ENDFOR
jj=WHERE(x le -d4/2 OR x ge d4/2,count)
jj=(jj>0)<maxj
w4(jj,*,*)=(count LE 0)*w4(jj,*,*)
PRINT,'Beam profile at sample slits (w4)'
w4a=w4

; Here we are at the cylindrical sample ...
; It has a certain scattering power, incoherent scattering and absorption ...
; Different wavelength will be diffracted to different directions ...
; Start with a single point in the center of the sample (x=0)
; The reference is x=0, y=0 (div.) and z=lam
;
jj=WHERE(x le -ds/2 OR x ge ds/2,count)   ; limitation to sample diameter
jj=(jj>0)<maxj
w4(jj,*,*)=(count LE 0)*w4(jj,*,*)
;
; ^ This has to be changed afterwards ^ ... (above)
;
w5=w4*0.
theta= ASIN(lam/dsample/2.)/!pi*180.
PRINT,'twotheta',theta*2.
; diffraction angle deviation from right angle
delta = 2. * (ASIN(z/dsample/2.)/!pi*180. - theta)
FOR k=0,maxk DO BEGIN      ; for different wavelengths
  ; The divergence will change depending on wavelength
  yy = y + delta(k)
  ii= ROUND((yy(*)+DivMax)/DDiv)
  FOR i=0,maxi DO BEGIN    ; for different divergences
    ;PRINT,k,delta(k),i,y(i),yy(i),ii(i)
    jj= ROUND(((x(*)*SIN((yy(i)+2*theta)/180.*!PI)-(ls-l4)*tan(( y(i))*!PI/180.))+Xmax)/Dx)
    w5((jj>0)<maxj,(ii(i)>0)<maxi,k) = w5((jj>0)<maxj,(ii(i)>0)<maxi,k)+cos(y(i)*!PI/180.)*w4((jj>0)<maxj,i,k)*(jj GE 0 AND jj LE maxj)*(ii(i) GE 0 AND ii(i) LE maxi)
   ;
   ; to be implemented :
   ; At x=0 scattering from -r to +r, at -/+r scattering only at x, at >+r/<-r nothing!
   ;
  ENDFOR
ENDFOR
PRINT,'Beam profile a(f)t(er) sample'

; The next step is the entrance to the detector
; There's some Aluminium scattering (wing effect) depending on wavelength etc.
;
; #####################################################################################################################
; # CHECK: A certain divergence at the sample should result in the same divergence on the PSD: D20: 0.1 deg = 2.5 mm! # 
; #####################################################################################################################
;
;
w6=w5*0.
FOR k=0,maxk DO BEGIN      ; for different wavelengths
  FOR i=0,maxi DO BEGIN    ; for different divergences
    jj= ROUND(((x(*)-(ld*tan(( y(i))	*!PI/180.)))+Xmax)/Dx)
    w6((jj>0)<maxj,i,k) = w6((jj>0)<maxj,i,k)+cos(y(i)*!PI/180.)*w5((jj>0)<maxj,i,k)*(jj GE 0 AND jj LE maxj)
  ENDFOR
ENDFOR
PRINT,'Beam profile at PSD'

; The next step is the capture reaction somewhere in the detection gap depending on wavelength 
; a T-p-trace will create an electron avalanche of a certain length

; Finally these electrons will be amplified and detected at the microstrip electrodes
; Only the cell receiving the maximum of electrons will count ...

mod_datp,datp,'x',x
mod_datp,datp,'y',y
mod_datp,datp,'z',z
give_datp,datp
RETURN,w6
end
