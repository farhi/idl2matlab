pro testtittle

x = findgen(100)
y = x^2 - x + 5

window, 1

!P.TITLE = ''

plot, y, title='YOP !C keyword!!'

window, 2

!P.TITLE = 'YOP !C Varsys!!'

plot, y,title='YOP !C keyword!!'

window,3

plot,y

end
