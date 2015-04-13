function qens_treat,w_in,emin=emin,emax=emax,de=de,arm=arm
;** **********************************************
;** S. Rols 11/01 srols@anl.gov
;** The call is w_out=qens_treat(w_in)
;** Treatment of the QENS (IPNS) data involving
;** -tof 2 nrj transformation and ki/kf correction
;** -arms grouping
;** -monitor normalization
;** **********************************************

common c_lamp_access, inst
common printing, iprint, outstring

take_datp, datp
par=datp.p
D_sd=datp.pv
D_mon_s=par(17) ;moderator to monitor distance

;** First of all check the energy range and increment
;** *************************************************

if (emin lt -2.5) then begin
print, 'QENS: Give an emin value -2.5 meV < emin < 0 meV'
w_out=w_in
goto, endret
endif else if (emax gt 200) then begin
print, 'QENS: Give an emax value 0 meV < emax < 200 meV'
w_out=w_in
goto, endret
endif else if (emin ge emax) then begin
print, 'QENS: emin should be < to emax !!!'
w_out=w_in
goto, endret
endif else if (n_elements(de) ne 0) then begin
if (de(0) le 0.005) then begin
print, 'QENS: problem ... de too small.// Typical values for de are 0.015 for quasielastic ([-1.5,5 meV]) or 0.1 for DOS ([-3,50meV]).)'
w_out=w_in
goto, endret
endif
endif

;** Donnees experimentales
;** ************************************************

x_in=datp.x
y_in=datp.y
e_in=datp.e
mon=datp.n
teta=y_in
iprint=0
n_channels=n_elements(x_in)
n_spectra=n_elements(D_sd)

;** Calcul of the scattered energy for each detector
;** ************************************************

D_md=8.04+D_sd ;moderator to detector distance in meter
D_ms=8.04 ;moderator to sample distance in meter
D_mm=D_ms-D_mon_s
ch_width=par(14)   ; channel width in microsecond for detectors
ch_width_mon=par(16)   ; channel width in microsecond for monitor
c_el=D_md*0.
T_md=(x_in-1)*ch_width+2000 ; tof for channels in microseconds
alpha=5.2267*1e6 ;E to v conversion factor

if iprint eq 0 then print,n_spectra,ch_width,D_mm,'n_spectra,ch_width,D_mod_mon'

for ispec=0,n_spectra-1 do begin
if iprint ne 0 then print,"Spectrum #",ispec
ce=where(w_in(*,ispec) eq max(w_in(*,ispec)))
if (n_elements(ce) le 3 and fix(total(ce)/n_elements(ce)) le 1300 and fix(total(ce)/n_elements(ce)) ge 600) then c_el(ispec)=x_in(fix(total(ce)/n_elements(ce))) else begin
print,'Elastic channel of Spectrum #',ispec+1,' has pb ... removed'
c_el(ispec)=0
endelse
endfor

;** we now remove the bad detectors
index_remo=where(c_el ne 0) ;indices of the spectra to be removed
n_spectra=n_elements(index_remo) ;new dimension of the arrays
w_buf_1=fltarr(n_channels,n_spectra) & w_buf_1=w_in(*,index_remo) ;intensities modifications
e_in_buf_1=fltarr(n_channels,n_spectra) & e_in_buf_1=e_in(*,index_remo) ;intensities errors modifications
c_el=c_el(index_remo) ;elastic channel array modification
D_md=D_md(index_remo) & D_sd=D_sd(index_remo) & y_out=y_in(index_remo) ;distances and angles arrays modifications
;** we now calculate the scattered energy and the elastic time of flight
T_md_el=c_el*ch_width+2000
e_d=alpha*(D_md/T_md_el)^2 & v_d=D_md/T_md_el ;scattered energy in meV and scattered celerity in m/us
e_i=fltarr(n_channels,n_spectra) & homega=e_i

;** Calculation of the incident energy ... the first channels are non-physical, and correspond
;** to a negative flight path distance ... we should not take them into account ... but for the moment, it
;** is simpler to take them and then reduce our energy range to reasonnable values.
for ispec=0,n_spectra-1 do begin
if ispec ne 2 then begin
e_i(*,ispec)=alpha*((D_ms*v_d(ispec))/(v_d(ispec)*T_md(*)-D_sd(ispec)))^2	;incident energy in meV
homega(*,ispec)=e_i(*,ispec)-e_d(ispec)
endif
endfor

;** The only spectrum# for which the value of v_d(ispec)*T_md(*)-D_sd(ispec)=0 is the spectrum ispec=2
;** for the 73rd channel. We gonna give it a slighly larger (non-0) value ... not perfect but it works
for ichan=0,n_channels-1 do begin
if ichan ne 73 then e_i(ichan,2)=alpha*((D_ms*v_d(2))/(v_d(2)*T_md(ichan)-D_sd(2)))^2	$
else e_i(ichan,2)=alpha*((D_ms*v_d(2))/(1.e-6))^2
homega(ichan,2)=e_i(ichan,2)-e_d(2)
endfor
if iprint ne 0 then print,e_d

;
;** Transformation from tof to energy
;** ************************************************
;1/ create the desired energy grid first
if (n_elements(de) eq 0) then begin
print,"QENS: Will interpol spectra to nrj grid defined by detector #1"
x_out=reverse(homega(*,0)) & x_out=x_out(where(x_out le emax and x_out ge emin))
nx_points=n_elements(x_out)-1
endif else begin
print,"QENS: Will interpol spectra to nrj grid defined user"
if (de(0) le 0.005) then begin
de=0.02
print,"QENS: Value of de too small ... put it to 0.02"
endif
nx_points=fix(abs(emax-emin)/abs(de))
x_out=emin+findgen(nx_points+1)*abs(de)
endelse
w_buf=fltarr(nx_points+1,n_spectra) & e_out=w_buf

;2/ Do the t2e transformation next for each detector

for ispec=0,n_spectra-1 do begin
homega_buf=homega(*,ispec) & e_i_buf=e_i(*,ispec)
homega_buf_2=homega_buf(where(homega_buf le emax and homega_buf ge emin)) & homega_buf_2=reverse(homega_buf_2)
e_i_buf_2=e_i_buf(where(homega_buf le emax and homega_buf ge emin)) & e_i_buf_2=reverse(e_i_buf_2)

; No remove a constant flat background determined by the min of the 10 first and 10 last channels.

i_buf=w_buf_1(*,ispec)
ra=total(i_buf(0:9))/10. & rb=total(i_buf(n_elements(i_buf)-10:n_elements(i_buf)-1))/10.
backg=ra < rb
if backg le 0 then backg=0.
w_buf_1(*,ispec)=w_buf_1(*,ispec)-backg
infzero=where(w_buf_1(*,ispec) LT 0.)
if infzero(0) ne -1 then w_buf_1(infzero,ispec)=0.

i_buf=w_buf_1(where(homega_buf le emax and homega_buf ge emin),ispec) & i_buf=reverse(i_buf)
i_buf_b=interpol(i_buf,homega_buf_2,x_out) & fact=interpol(e_i_buf_2,homega_buf_2,x_out)
infzero=where(fact le 0.)
if infzero(0) ne -1 then print,"Probem in fact calculation"
;i_buf_b=spline(homega_buf_2,i_buf,x_out) & fact=spline(homega_buf_2,e_i_buf_2,x_out)
;i_buf_b(where(i_buf_b) lt 0.)=0.

;3/ Now do the dt/dw correction (jacobian factor)

i_buf_b=i_buf_b*D_ms*sqrt(alpha)*0.5/(abs(fact)^(3/2))
e_in_buf=abs(i_buf_b)*D_ms*sqrt(alpha)*0.5/(abs(fact)^(3/2)) & indd=where(e_in_buf lt 0)
if indd(0) ge 0 then begin
print,'Probleme error inf eq 0'
stop
e_in_buf(indd)=0.
endif

;4/ Now do the kd/ki correction factor

if e_d(ispec) le 0. then print,"Probem in e_d ... in or eq 0"
fact=sqrt(abs((x_out+e_d(ispec))/(e_d(ispec))))
i_buf_b=i_buf_b*fact & e_in_buf=e_in_buf*fact

;5/ Loading into convenient workspaces

w_buf(*,ispec)=i_buf_b(*)
e_out(*,ispec)=sqrt(e_in_buf(*))

endfor

e_i_grid=fltarr(n_elements(x_out),n_spectra)
for ispec=0,n_spectra-1 do e_i_grid(*,ispec)=x_out(*)+e_d(ispec)

;** Now do the monitor normalization
;** *****************************************************

n_channels_mon=n_elements(mon)
tof_mon=600+findgen(n_channels_mon)*ch_width_mon
e_in_mon=alpha*((D_mm)/tof_mon)^2
infzero=where(e_in_mon le 0.)
if infzero(0) ne -1 then print,"problem in monitor normalization 1"
mon_buf=mon*D_mm*sqrt(alpha)*0.5/(e_in_mon^(3/2))

for ispec=0,n_spectra-1 do begin
e_in_mon_2=e_in_mon(where(e_in_mon le e_i_grid(n_elements(x_out)-1,ispec) and e_in_mon ge e_i_grid(0,ispec)))
mon_buf_2=mon_buf(where(e_in_mon le e_i_grid(n_elements(x_out)-1,ispec) and e_in_mon ge e_i_grid(0,ispec)))
YY=reverse(mon_buf_2) & XX=reverse(e_in_mon_2)
TT=fltarr(n_elements(x_out)) & TT=e_i_grid(*,ispec)
;mon_splined=spline(XX,YY,TT)
mon_splined=interpol(YY,XX,TT)
nfzero=where(mon_splined le 0.)
if infzero(0) ne -1 then print,"problem in monitor normalization 2"
w_buf(*,ispec)=1000.*w_buf(*,ispec)/mon_splined(*)
e_out(*,ispec)=1000.*e_out(*,ispec)/mon_splined(*)
endfor

;** Now do the Arm Grouping if required
;** *****************************************************

if ((n_elements(arm) eq 0) or (arm(0) eq 1)) then begin
print,"QENS: Grouping spectra by arms"
count=n_spectra & ngroup=0
ntot=0.
group=0.
while ntot ne n_spectra do begin
ang=y_out(ntot)
ind=where(y_out eq ang,count)
ntot=ntot+count
ngroup=ngroup+1
group=[group,ind(0),ind(n_elements(ind)-1),ang]
endwhile
group=group(1:n_elements(group)-1) & group=reform(group,3,ngroup)
w_buf_g=fltarr(n_elements(x_out),ngroup) & y_out_g=fltarr(ngroup) & e_out_g=w_buf_g
for ig=0,ngroup-1 do begin
imin=group(0,ig) & imax=group(1,ig) & y_out_g(ig)=group(2,ig)
w_buf_g(*,ig)=total(w_buf(*,imin:imax),2)/(imax-imin+1)
e_out_g(*,ig)=sqrt(total((e_out(*,imin:imax))^2,2))/(imax-imin+1)
endfor
w_buf=w_buf_g
e_out=e_out_g
y_out=y_out_g
endif

;** We return the workspace and modify the datp structure
;** *****************************************************

w_out=w_buf

mod_datp, datp, "e", e_out
mod_datp, datp, "x", x_out
mod_datp, datp, "y", y_out
mod_datp, datp, "pv", D_sd
mod_datp, datp, "x_tit", "Energy transfer (meV)"
give_datp, datp

endret: return, w_out

;** End of program
;** *****************************************************
end
