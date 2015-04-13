; $Id: test_true.pro,v 1.1 1993/04/02 18:38:01 idl Exp $

;
;	Procedure to test IDL true color operation
;
function col,r,g,b	;Return the color index, given R, G, and B
return,r + 256L*(g + 256L*b)
end

pro test_true

Print, 'Demonstration of True Color / IDL'
Read,'Enter 1 for retained windows, 0 for non-retained: ',retain
window,xs=512,ys=800,retain=retain	;Make a window

print,'Ramps'
v = [0,0,0]
for ic=0,2 do begin
  for iy=0,255 do begin	;Fill with vectors of 3 colors
	v = intarr(3)
	v(ic) = iy
	y = ic*256 + iy
	plots,[0,511],[y,y],/dev,col=col(v(0),v(1),v(2))
	endfor
   empty
endfor

; Make an image and output to Channels 0 thru 3.  Channel 0 is all channels.
; Then, read it back and compare.
print,'Output an image to all channels, then r,g,b'
a=bytscl(dist(200))
for i=0,3 do begin
	tv,a,i*100,i*100,channel=i	;Write it
	empty
	b = tvrd(i*100,i*100,200,200,channel=i) ;Read it
	if total(abs(a-b)) ne 0 then $
	  print, "Read Compare Error, Channel ",i
	endfor

Print,'Color Table Manipulations'
for i=0,15 do begin loadct,i,/silent & wait,0.5  & end
r = indgen(256)
s = [1,1,1]
for j=0,2 do begin
	for i=0,256,2 do begin
	tvlct,shift(r,i*s(0)),shift(r,i*s(1)),shift(r,i*s(2))
	wait,.01
	endfor
  s(j) = -1
endfor
loadct,0,/silent

print,'Reading image'
close,1

fname = filepath('p077h3b.equ',subdir='images') ;the demo true color image 
qq = findfile(fname,count = i)
if i eq 0 then begin
	print,'Cant find true color image file ',fname
	print,'Skipping this part of the demo.'
	goto, no_file
	endif
openr,1, fname
a=bytarr(500,500,4)
readu,1,a
close,1

print,'Displaying Image'
erase
tv,a,true=3	;Output 1st three images of A into R,G,B
wait,1
tv,a(*,*,3),channel=2	;Output Fourth into Green channel

no_file: 
print,'Computing Hue-Saturation Image'
siz = 150
x = indgen(siz) # replicate(1,siz) - siz/2.
y = replicate(1,siz) # indgen(siz) - siz/2.
r = sqrt(x^2 + y^2) / (siz/2.) < 1 ;From 0 to 1.
hue = atan(y,x) * (180./!pi)
color_convert,hue, r, replicate(1.,siz,siz), r,g,b,/hsv_rgb

print,'Displaying Hue-Saturation Image'
tv,r,0,channel=1
tv,g,0,channel=2
tv,b,0,channel=3

print,'Done'
end
