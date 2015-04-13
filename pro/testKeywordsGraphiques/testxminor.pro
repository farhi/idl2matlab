

pro testxminor

x = findgen(10)

d = dist(25)

window, 0

!P.MULTI = [0,2,0,0,1]

plot,x, xminor = -1, xtitle = 'KG xminor = -1'
oplot, x + 10

!X.MINOR = -1

plot,x, xminor = 0, xtitle = 'KG xminor = 0'
oplot, x + 10



window, 1

!X.MINOR = 2
contour,d,NLEVELS=11,xtitle = 'KG xminor = -1',xminor = -1
contour,d,NLEVELS=11,xtitle = 'VS XMINOR = 2'

window,2
surface, d, xminor = -0.5,xtitle = 'KG xminor = -0.5'
surface, d, xminor = -1.5,xtitle = 'KG xminor = -1.5'

window, 3
shade_surf,d,xtitle = 'KG xminor = 1', xminor = 1
!X.MINOR = -1.5
shade_surf,d,xtitle = 'VS xminor = -1.5'

!P.MULTI = 0
!X.MINOR = 0

end

