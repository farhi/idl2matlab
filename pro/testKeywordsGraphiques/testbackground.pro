

pro testbackground

x = findgen(10)

d = dist(25)

red = [0,1,1,0,0,1]
green = [0,1,0,1,0,1]
blue=[0,1,0,0,1,0]

tvlct,255*red,255*green,255*blue

window, 1

!P.MULTI = [0,2,0,0,1]

plot,x, background = 4
oplot, x + 10

contour,d,NLEVELS=11,background = 2
;surface, d, background = 25
;plot,x^2,background = 15

window, 2
;!P.BACKGROUND = 255
!P.MULTI = [0,2,2,5,0]

;plot,-x,background = 25
;plot,-x +5,background = 25

shade_surf,d,background = 5
contour,d,NLEVELS=11, background = 3
;surface, d, background = 25


!P.MULTI = 0

window,3
plot,x,background = 2

;!P.BACKGROUND = 0
end
