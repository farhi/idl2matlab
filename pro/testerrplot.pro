pro testerrplot

x=findgen(100)
y=2*x+14
y1=y-5
y2=y+5

window,1
plot, y
errplot,y1,y2

window,2
plot,x, y
errplot,x,y1,y2

window,3
plot,x,y
errplot,x,y1,y2, WIDTH=2

window,4
errplot,x,y1,y2, WIDTH=2
end
