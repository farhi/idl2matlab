

pro testposition

x = findgen(100)
y = x^2 - x + 5

c=[[16,2,3,13],[5,11,10,8],[9,7,6,12],[4,14,15,1]]
d = dist(25)

!P.POSITION = [0,0,0.5,0.5]

window,1
surface, d, title='surface position=[0 0 1 1]', position = [0,0,1,1]

window,2
shade_surf, d, title='shade surf position=[0.5,0.5,1,1]',position=[0.5,0.5,1,1]

window,3
contour,d,NLEVELS=11,/fill,position = [0.2,0.5,0.8,0.8],title='position = [0.2,0.5,0.8,0.8]'

window,4
plot, y, title='position = [0 0 0.5 0.5]'
oplot, 2*y, thick = 2
oplot, 3*y, thick = 3

!P.POSITION = 0

end
