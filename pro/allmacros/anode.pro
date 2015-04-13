pro anode,det,rdata

xp=0l
xm=0l
ym=0l
yp=0l


chan=1l
time=1l
data=lonarr(25000,4)
rdata=lonarr(25000,2)
close,3
xsize=286
ysize=276
det=lonarr(286,276)
xx=216
xd=20
yy=460
yd=20

for n=0,9 do begin
f=strtrim(string(n),2)
print,'zill open','/users/d17/rawdata.'+f
openr,3,'/users/d17/rawdata.'+f

print,'opened file','/users/d17/rawdata.'+f
neut=0
reader=0


while (eof(3) eq 0) do begin

   readf,3,chan,time
;   print,chan,time
   if(chan ne 0) then begin
   
     if(chan eq 11) then rdata(reader,0)=1
     if(chan eq 12) then rdata(reader,0)=2
     if(chan eq 21) then rdata(reader,0)=4
     if(chan eq 22) then rdata(reader,0)=8     
     rdata(reader,1)=time   
     reader=reader+1
   endif
end

for i=0,reader do begin

   if ((rdata(i,0)+rdata(i+1,0)+rdata(i+2,0)+rdata(i+3,0) eq 15) and $
       (max([rdata(i,1),rdata(i+1,1),rdata(i+2,1),rdata(i+3,1)])- $
       min([rdata(i,1),rdata(i+1,1),rdata(i+2,1),rdata(i+3,1)])) $
       lt 1000) then begin

      for k=0,3 do begin
        if(rdata(i+k,0) eq 1) then xp=rdata(i+k,1)
        if(rdata(i+k,0) eq 2) then xm=rdata(i+k,1)
        if(rdata(i+k,0) eq 4) then yp=rdata(i+k,1)
        if(rdata(i+k,0) eq 8) then ym=rdata(i+k,1)
      end
        data(neut,0)=xp
        data(neut,1)=xm
        data(neut,2)=yp
        data(neut,3)=ym
        neut=neut+1
        
        ;if(neut lt 21)then begin
        ;print,xp,xm,yp,ym,rdata(i,0),rdata(i+1,0),rdata(i+2,0),rdata(i+3,0)
        ;endif
       i=i+4

    endif

end


print,'xpos',moment(data(0:neut-1,0)-data(0:neut-1,1))
print,'ypos',moment(data(0:neut-1,2)-data(0:neut-1,3))


;window,1
;plot,data(*,0),data(*,1),psym=6
;window,2
;plot,data(*,2),data(*,3),psym=6
;window,3
;plot,data(*,0)+data(*,1),data(*,2)+data(*,3),psym=4

print,'good neuts= ',neut,' total cathodes read= ',reader
print,'cathodes/4 =',reader/4.

xsize=286
ysize=276
print,total(det)

xdel=270
ydel=550
xf=2
yf=4
posx=(data(*,0)-data(*,1)+xdel)/xf
posy=(data(*,2)-data(*,3)+ydel)/yf
help,posx,posy,det
flag=0

for i=0,neut-1 do begin

    if (posx(i) ge 0 and posx(i) lt xsize and posy(i) ge 0 and posy(i) lt ysize) then $
    det(posx(i),posy(i))= det(posx(i),posy(i))+1
endfor




close,3

endfor

;window,1
;plot,data(*,0),data(*,1),psym=6
;window,2
;plot,data(*,2),data(*,3),psym=6
;window,3
;plot,d


end
