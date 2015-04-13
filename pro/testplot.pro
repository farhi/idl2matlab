pro testplot

x=findgen(100)
y=2*x+14

window,1
plot, y

window,2
plot,y, thick=4

window,3
plot,y, thick=4, min_value=50

window,4
plot,y, thick=4, min_value=50, max_value=150

window,5
plot,y, thick=4, min_value=50, max_value=150, /ynozero

window,6
plot,y, thick=4, min_value=50, max_value=150, /ynozero, /isotropic

window,7
plot,y, thick=4, min_value=50, max_value=150,/ylog

window,8
plot,y, thick=4, min_value=50, max_value=150,/xlog

window,9
plot,y, thick=4, min_value=50, max_value=150,/ylog, /xlog

window,10
plot,y,y,/polar

window,11
plot,y,y,/polar, thick=2, min_value=10, /ylog

window,12
plot,y,y,/polar, thick=2, min_value=10, /isotropic

window,13
plot,y,y,/polar, thick=2, min_value=10, /ylog, /ynozero

end
