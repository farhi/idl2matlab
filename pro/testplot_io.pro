pro testplot_io

x=findgen(100)
y=2*x+14

window,1
plot_io, y

window,2
plot_io,y, thick=4

window,3
plot_io,y, thick=4, min_value=50

window,4
plot_io,y, thick=4, min_value=50, max_value=150

window,5
plot_io,y, thick=4, min_value=50, max_value=150, /ynozero

window,6
plot_io,y, thick=4, min_value=50, max_value=150, /ynozero, /isotropic

window,7
plot_io,y, thick=4, min_value=50, max_value=150,/ylog

window,8
plot_io,y, thick=4, min_value=50, max_value=150,/xlog

window,9
plot_io,y, thick=4, min_value=50, max_value=150,/ylog, /xlog

window,10
plot_io,y,y,/polar

window,11

!P.MULTI = [0,0,2]
plot_io, y
plot, y

!P.MULTI = 0

end
