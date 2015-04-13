pro testytitle

x = findgen(100)
y = x^2 - x + 5

window, 1

!Y.TITLE = ''

plot, y, ytitle='YOP !C keyword!!'

window, 2

!Y.TITLE = 'YOP !C Varsys!!'

plot, y, ytitle='YOP !C keyword!!'

window,3

plot,y

end