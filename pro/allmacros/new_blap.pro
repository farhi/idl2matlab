pro blap
openr,3,'data.'
x=indarr(50000)
y=indarr(50000)
picture=indarr(1000,1000)
readf,3,x,y

for i=1,50000 do begin
  picture(x(i),y(i))=picture(x(i),y(i))+1 
endfor

window,0,retain=2,xsize=2000,ysize=2000
contour,picture,levels=ll,c_colors=indgen(10)*25
close,3
end
