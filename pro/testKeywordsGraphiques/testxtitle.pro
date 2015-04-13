pro testxtitle

x = findgen(100)
y = x^2 - x + 5

window, 1

!X.TITLE = ''

plot, y, xtitle='YOP !C keyword!!'

window, 2

!X.TITLE = 'YOP !C Varsys!!'

plot, y, xtitle='YOP !C keyword!!'

window,3

plot,y

end