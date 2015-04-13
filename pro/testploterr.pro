pro testploterr

x=findgen(100)
y=2*x+14
y1=y-5
y2=y+5

window,1
ploterr,x,y,y2

window,2
ploterr,y,y1

window,3
ploterr,x,y,y1,PSYM=4

window,4
ploterr,x,y,y1,PSYM=4,TYPE=1

window,5
ploterr,x,y,y1,PSYM=4,TYPE=2

end
