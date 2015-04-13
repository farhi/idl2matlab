
pro testthick

x = findgen(100)
y = x^2 - x + 5

d = dist(25)

!P.MULTI = [0,2,2,0,0]

!P.THICK = 1

;surface, d, thick = 3, title='surface thick = 3'

;shade_surf, d, thick = 2, title='shade surf thick = 2'

plot, y
oplot, 2*y, thick = 2
oplot, 3*y, thick = 3

;!P.MULTI = 0

;window, 2

plot,x
plots, 20,22
plots, 40,50,/continue,thick = 2
plots, 40,80,/continue

plot,x^2

end
