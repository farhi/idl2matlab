pro test_polyshade

SPHERE = FLTARR(20, 20, 20)

FOR X=0,19 DO begin
	FOR Y=0,19 DO begin
		FOR Z=0,19 DO begin
   			SPHERE(X, Y, Z) = SQRT((X-10)^2 + (Y-10)^2 + (Z-10)^2)
   		endfor
   	endfor
endfor

SHADE_VOLUME, SPHERE,8, V, P,/verbose
r=size(P)

f=fltarr(r(1))
j=0
for i=0,r(1)-1 DO begin
	if j ge 255 then j=0
	f(i)=j
	j=j+1
endfor


image = POLYSHADE(V, P,xsize=200)


TV, image
end
