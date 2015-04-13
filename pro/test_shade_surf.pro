pro test_shade_surf



D=dist(30)
S = SIN(D)

window,0
loadct,10
SHADE_SURF, D, SHADES = BYTSCL(S)
window,1
SHADE_SURF, D, /save
print,!p.t
window,2
shade_surf, D,ax=25, az=89, image=img
window,3
shade_surf, D, max_value=10, min_value=5
window,4
shade_surf, D, /XLOG, /YLOG

X=findgen(30)^2
Y=findgen(30)*10/1.5
window,5
shade_surf,D,X,Y,SHADES = BYTSCL(S),ax=50,/save
print,!p.t





end
