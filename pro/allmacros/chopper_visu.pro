pro chopper_visu


; Written by F. Descamps 1999

loadct,4

;Opens the window 
window,0,xsize=600,ysize=400,title='Chopper Visu D17b',retain=2
erase,32

histo1=intarr(1000)
histo2=intarr(1000)
ite=0

while(get_kbrd(0) NE 'q') do begin

; gets the parameters

rpm1=700
rpm2=700
phase1=180.00
phase2=180.00
pres1=10
pres2=10
phi1long=ite*2;
phi2long=571;

l1phi=0.3;
l2phi=0.25;



phi1=fix((float(phi1long)*float(rpm1)*6.0/24000000.0)*1000/l1phi)+500
phi2=fix((float(phi2long)*float(rpm2)*6.0/24000000.0)*1000/l2phi)+500

if(phi1 GE 0) AND (phi1 LT 1000) then histo1(phi1)=histo1(phi1)+1
if(phi2 GE 0) AND (phi2 LT 1000) then histo2(phi2)=histo2(phi2)+1

if(ite EQ 100) then begin
; display the parameters


polyfill,[400,595,595,400],[5,5,395,395],color=48,/device
plots,[400,595,595],[5,5,395],color=0,/device,thick=3
plots,[595,400,400],[395,395,5],color=64,/device,thick=3
; chopper1

xyouts,410,370,'Chopper 1 status',/device,charthick=2,charsize=1.5,color=219
xyouts,415,350,'Speed'+string( rpm1)+' rpm',/device,charthick=1,charsize=1.1,color=219
xyouts,415,335,'Phase'+string( phase1)+' deg',/device,charthick=1,charsize=1.1,color=219
xyouts,415,320,'Precision'+string( pres1)+' mdeg',/device,charthick=1,charsize=1.1,color=219

xyouts,410,200,'Chopper 2 status',/device,charthick=2,charsize=1.5,color=219
xyouts,415,170,'Speed'+string( rpm2)+' rpm',/device,charthick=1,charsize=1.1,color=219
xyouts,415,155,'Phase'+string( phase2)+' deg',/device,charthick=1,charsize=1.1,color=219
xyouts,415,140,'Precision'+string( pres2)+' mdeg',/device,charthick=1,charsize=1.1,color=219


polyfill,[420,575,575,420],[15,15,55,55],color=127,/device
polyfill,[420,575,575,420],[55,55,95,95],color=95,/device



; converts data into histo bin


phi1=fix((float(phi1long)*float(rpm1)*6.0/24000000.0)*1000/l1phi)+500
phi2=fix((float(phi2long)*float(rpm2)*6.0/24000000.0)*1000/l2phi)+500



if(phi1 GE 0) AND (phi1 LT 1000) then histo1(phi1)=histo1(phi1)+1
if(phi2 GE 0) AND (phi2 LT 1000) then histo2(phi2)=histo2(phi2)+1

plot,(indgen(1000)-500)/1000.0*l1phi,histo1,position=[30,230,385,395],/device,/noerase,xstyle=1,ystyle=4
plot,(indgen(1000)-500)/1000.0*l2phi,histo2,position=[30,30,385,205],/device,/noerase,xstyle=1,ystyle=4
ite=0
end;
ite=ite+1
end;



end; chopper visu