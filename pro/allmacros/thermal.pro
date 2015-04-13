;**********D2B => Paolo RADAELLI *********************************

; ********************* TO CORRECT FOR THERMAL EXPANSION **********

pro thermal, coeff=coeff,  b=b, w=ws

take_w, ww, w=ws
take_datp, datp, w=ws
x=datp.x
e=datp.e
xn=360/!pi*asin(sin(x*!pi/360)*coeff)
G=(x-xn)/.1+findgen(n_elements(x))
wn=(interpolate(ww,g)-16000)*exp(-b*sin(x*!pi/360)^2/1.30^2)+16000
datp.x=x
give_datp, datp, w=ws
give_w, wn, w=ws
end

; ********************* TO CORRECT MULTIPLE DATA SETS FOR THERMAL EXPANSION **********

pro multherm, coeff=coeff, b=b, scale=s, wem=we, wmul=wm

take_w, wempty, w=we
take_w, wmult, w=wm
take_datp, datpe, w=we
take_datp, datpm, w=wm
xe=datpe.x
ee=datpe.e
xm=datpm.x
em=datpm.e
y=datpm.y
y=(y-27402)*4.7+400.
datpm.y=y
for i=0,n_elements(wmult)/1600-1 do begin
   t=y(i)
   xn=360/!pi*asin(sin(xe*!pi/360)/(1+coeff*T))
   g=(xe-xn)/.1+findgen(n_elements(xe))
   wn=(interpolate(wempty*s,g)-16000)*exp(-b*T*sin(xe*!pi/360)^2/1.30^2)+16000
   for j=0,1599 do wmult(j,i)=wmult(j,i)-wn(j)
endfor
give_datp, datpm, w=wm
give_datp, datpe, w=we
give_w, wmult, w=wm
give_w, wempty, w=we
end

