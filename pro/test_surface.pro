pro test_surface

x=findgen(20)
y=findgen(20)^2
z=findgen(20,20)-30

loadct,0

window,0
surface, dist(30)
window,1
surface,z,x,y
window,2
surface, dist(30),ax=50,az=90
window,3
SURFACE, dist(30), SKIRT=0.0
loadct,4
window,4
surface,dist(30),shades=findgen(30,30)
loadct,0
window,5
surface,dist(30),max_value=15,min_value=7,ax=15,/save
print,!p.t
window,6
surface,z,x,y,/horizontal
window,7
surface,z,x,y,/xlog,/ylog,/zlog
loadct,3
window,8
surface,dist(30),skirt=0.0,shades=findgen(30,30),max_value=15

end
