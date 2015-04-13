

pro testorientation

x = findgen(10)


!P.MULTI = [3,2,0,2,1]

plot,x
xyouts,2,5,'orientation = 35',ORIENTATION = 35
xyouts,5,2,'orientation = -60',ORIENTATION = -60
xyouts,7,1,'orientation = 0',ORIENTATION = 0
plot,2*x
xyouts,1,7,'orientation = 180',ORIENTATION = 180


!P.MULTI = 0

end
