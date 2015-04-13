
pro testcharthick

x = findgen(100)
y = x^2 - x + 5

!P.MULTI = [0,2,2,0,0]

!P.CHARTHICK = 2

plot, y, title='charthick = 1', charthick = 1
xyouts, 15,30,'charthick = 2'

plot, y,title='charthick = 2'
xyouts, 15,30,'charthick = 1', charthick=1

plot,y

end
