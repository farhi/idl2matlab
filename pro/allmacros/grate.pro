pro grate,w,dan,nb,dq,f,g,h

w=w*!pi/180.

lmin=0
lmax=39

tthmin=-2.5-dan
tthmax=2.5-dan

;dq=7e-5
nth=286.
dth=(tthmax-tthmin)/nth
g=fltarr(nth)
g=findgen(nth)*dth+tthmin

nl=300.
dl=(lmax-lmin)/nl
h=fltarr(nl)
h=findgen(nl)*dl+lmin

;nb=3.9e-6
;nb=0
qc=sqrt(16*!pi*nb)
lamc=4*!pi*sin(w)/qc
print,'critical lambda = ',lamc,'critical q = ',qc

mmax=8

f=fltarr(nth,nl)
for i=1,nth-1 do begin
  for j=1,nl-1 do begin
     f(i,j)=1
  endfor
endfor


print,nth,lmax-lmin+1


for m=-mmax,mmax do begin
   for l=0,nl-1 do begin
      
         
      if(w^2 + ((m*dq*l*dl+lmin)/!pi) gt 0) then begin
      
      
      tth1=-(w+sqrt(w^2+m*dq*(l*dl+lmin)/!pi))*180./!pi
      n1=fix(round((tth1-tthmin)/dth))
      if (n1 lt nth and n1 gt 0 )then begin
;         f(n1,l)=f(n1,l)+1
          f(n1,l)=-1
      endif


;      refraction before diffraction
;      if (cos(w)/(1-(l*dl+lmin)^2*nb/(2*!pi)) lt 1) then begin
;        neww=acos(cos(w)/(1-(l*dl+lmin)^2*nb/(2*!pi)))
;        tth2=-(neww-sqrt(neww^2+m*dq*(l*dl+lmin)/!pi))*180./!pi
;        if(m eq 0)then tth2=tth2+(w-neww)*(-180./!pi)
;        n2=fix(round((tth2-tthmin)/dth)) 
;        if(n2 lt nth and n2 gt 0)then begin
;          f(n2,l)=f(n2,l)+1
;           print,n2,m,l*dl+lmin,tth2,neww*180/!pi
;        endif
;      endif 

;      diffraction before refraction

       tth2=-(w-sqrt(w^2+m*dq*(l*dl+lmin)/!pi))
       ttt=(w+tth2)

       if (cos(ttt)/(1-(l*dl+lmin)^2*nb/(2*!pi)) lt 1) then begin
          newth=acos(cos(ttt)/(1-(l*dl+lmin)^2*nb/(2*!pi)))*ttt/abs(ttt)
          tth2=(newth-w)*180/!pi

        n2=fix(round((tth2-tthmin)/dth)) 
        if(n2 lt nth and n2 gt 0)then begin
;           f(n2,l)=f(n2,l)+1
            f(n2,l)=-1
;           print,n2,m,l*dl+lmin,tth2
        endif
       endif else begin
;         print,'whoops',l,m,(w*180/!pi),t(ttt*180/!pi)
       endelse

      endif    
   endfor
endfor
f(fix(-tthmin/dth),*)=-1


end

