
pro testcharsize

x = findgen(100)
y = x^2 - x + 5

!P.MULTI = [0,2,2,0,0]

!P.CHARSIZE = 2
!X.CHARSIZE = 0.5

plot, y, title='charsize = 1', charsize = 1
xyouts, 15,30,'charsize = 2'

plot, y,title='charsize = 2'
xyouts, 15,30,'charsize = 1', charsize=1
xyouts, 30,45,'charsize = 3',charsize=3

plot,y
!P.CHARSIZE = 0

end
