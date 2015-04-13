pro testoplot

a=findgen(100)
y=3*a+11
z=2*a+14

window,1
plot,a,z, thick=2.5, min_value=50
oplot, y

window,2
plot,a,z, thick=2.5, min_value=50
oplot,a,y, thick=3.5, max_value=150

window,3
plot,a,z, thick=2.5, min_value=50
oplot,y, thick=4, min_value=50

window,4
plot,a,z, thick=2.5, min_value=50
oplot,y, thick=4, min_value=50, max_value=150

window,5
plot,a,z, thick=2.5, min_value=50
oplot,y,y,/polar

window,6
plot,a,z, thick=2.5, min_value=50
oplot,y,y,/polar, thick=2

window,7
plot,a,z, thick=2.5, min_value=50
oplot,y,y,/polar, thick=2, min_value=10

window,8
plot,a,z, thick=2.5, min_value=50
oplot,y,y,/polar, thick=2, min_value=10, max_value=150

end
