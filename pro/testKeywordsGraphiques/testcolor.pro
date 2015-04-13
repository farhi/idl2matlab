


pro testcolor

x = findgen(10)

d = dist(25)

red = [0,1,1,0,0,1]
green = [0,1,0,1,0,1]
blue=[0,1,0,0,1,0]

!P.MULTI = [0,2,2]

tvlct,255*red,255*green,255*blue

!P.COLOR = 4

contour,d,NLEVELS=11,color=2,title='test contour rouge',xtitle = 'axe des x rouge',ytitle = 'axe des y rouge'

surface, d, color = 3,title='test surface vert',ztitle = 'axe des z vert',ytitle = 'axe des y vert',xtitle = 'axe des x vert'
shade_surf,d,color = 5,title='test shade_surf jaune',ztitle = 'axe des z jaune',ytitle = 'axe des y jaune',xtitle = 'axe des x jaune'

plot,x,color = 1 ,title = 'test plot blanc',xtitle = 'axe X',ytitle = 'axe Y'
oplot, x+5,color=2
xyouts,1,2,'rouge keyword',color=2
xyouts,1,3,'bleu systeme'
!P.COLOR = 5
xyouts, 1,4,'jaune systeme'
xyouts, 1,5,'vert keyword',color=3
xyouts, [5,5],[1,2],['bleu keyword','rouge keyword'],color=[4,2]
testtxt = ['bleu keyword','rouge keyword']
plots, 8,2, color = 2
plots, [8,9],[5,6],/continue
plots, 8,8,/continue, color = 4

!P.MULTI = 0

end
