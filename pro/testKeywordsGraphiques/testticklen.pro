
pro testticklen

x = findgen(100)
y = x^2 - x + 5

c=[[16,2,3,13],[5,11,10,8],[9,7,6,12],[4,14,15,1]]
d = dist(25)

!P.TICKLEN = -1

window,1
surface, d, title='surface ticklen = 0.5', ticklen = 0.5

window,2
shade_surf, d, title='shade surf ticklen = -0.2',ticklen = -0.2

window,3
contour,d,NLEVELS=11,/fill,ticklen = 0.02,title='ticklen = 0.02'

window,4
plot, y, title='ticklen = -1'
oplot, 2*y, thick = 2
oplot, 3*y, thick = 3

!P.TICKLEN = 0.02

end
