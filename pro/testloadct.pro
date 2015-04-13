
pro testloadct

!P.MULTI = [0,2,3,0,0]

t = 4
loadct,t , /silent
shade_surf,dist(25)


loadct,get_names = d
print,d

;loadct, file = 'colors2.tbl'

loadct,0,bottom = 250
shade_surf,dist(25)

loadct,0
loadct,15,ncolors=15
shade_surf,dist(25)

loadct,0
loadct,13,bottom=245,ncolors = 5
shade_surf,dist(25)

loadct,0
loadct,26,bottom=245,ncolors=40
shade_surf,dist(25)

!P.MULTI = 0

end
