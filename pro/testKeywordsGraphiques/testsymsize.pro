
pro testsymsize

x = findgen(10)


!P.MULTI = [0,2,0,0,1]


plot,x, psym = 2, symsize = 3
plots, 2,3, psym = 1, symsize = 4
plots, 4,5,/continue, psym = 1, symsize = 10
plots, 4,8,/continue, psym = 5, symsize = 2
plot,x



!P.MULTI = 0

end
