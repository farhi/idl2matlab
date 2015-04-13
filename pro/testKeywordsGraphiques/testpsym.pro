

pro testpsym

x = findgen(100)
y = x^2 - x + 5

!P.LINESTYLE = 0
!P.PSYM = 0

;window, 1

;plot, x, psym = 0, linestyle =0
;oplot, 2*x, thick = 2, linestyle = 2, psym = 3
;oplot, 3*x, thick = 3, linestyle = 4

window, 2
plot,x, psym = 0
plots, 20,22, psym = 1
plots, 40,50,/continue, psym = 5
plots, 40,80,/continue, psym = 0

!P.PSYM = 0

end
