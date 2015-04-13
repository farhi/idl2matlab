

pro testlinestyle

x = findgen(10)
y = x^2 - x + 5

d = dist(25)

!P.LINESTYLE = 3
!P.PSYM = 1

;window,1
;surface, d, title='Linestyle = 5', linestyle = 5

window,2
plot, x, psym = 0, linestyle =0
oplot, 2*x, thick = 2 , linestyle = 2, psym = 3
oplot, 3*x, thick = 3, linestyle = 4, psym = 5

!P.LINESTYLE = 0


end
