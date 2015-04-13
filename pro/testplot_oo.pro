pro testplot_oo

x=findgen(100)
y=2*x+14

window,1
plot_oo, y

window,2
plot_oo,y, thick=4

window,3
plot_oo,y, thick=4, min_value=50

window,4
plot_oo,y, thick=4, min_value=50, max_value=150

window,5
plot_oo,y, thick=4, min_value=50, max_value=150, /ynozero

window,6
plot_oo,y, thick=4, min_value=50, max_value=150,/ylog

window,7
plot_oo,y, thick=4, min_value=50, max_value=150,/xlog

window,8
plot_oo,y, thick=4, min_value=50, max_value=150,/ylog, /xlog

end
