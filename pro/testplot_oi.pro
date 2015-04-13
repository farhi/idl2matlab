pro testplot_oi

x=findgen(100)
y=2*x+14

window,1
plot_oi, y

window,2
plot_oi,y, thick=4

window,3
plot_oi,y, thick=4, min_value=50

window,4
plot_oi,y, thick=4, min_value=50, max_value=150

window,5
plot_oi,y, thick=4, min_value=50, max_value=150, /ynozero

window,6
plot_oi,y, thick=4, min_value=50, max_value=150,/ylog

window,7
plot_oi,y, thick=4, min_value=50, max_value=150,/xlog

window,8
plot_oi,y, thick=4, min_value=50, max_value=150,/ylog, /xlog

window,9
plot_oi,y,y,/polar

end
