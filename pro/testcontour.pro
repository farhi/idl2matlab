pro testcontour

;REMPLACER CES 2 LIGNES PAR LA FONCTION MESHGRID DE MATLAB
;[x,y] = meshgrid(-2:0.2:2,-2:0.2:3);
x=matx(21,26,-2,0.2)
y=maty(21,26,-2,0.2)
Z =x*exp(-x^2-y^2)
V=[-0.3,-0.2,0,0.1,0.3,0.35,0.36]
window,1
contour,Z,x,y, LEVELS=V, C_LINESTYLE=[0,1,2,3], C_ANNOTATION=['n1','n2','n3','n4'], /FOLLOW
window,2
contour,Z,x,y, LEVELS=V, C_THICK=[1,2,3] , C_LABELS=[0,0,0,1,1,1], C_CHARSIZE=1.1
window,3
contour,Z,x,y, NLEVELS=13
window,4
contour,Z,x,y, NLEVELS=13,LEVELS=V,/FOLLOW
window,5
contour,Z,x,y, NLEVELS=13,/XLOG,/YLOG
window,6
contour,Z,x,y, NLEVELS=13,/ISOTROPIC
window,7
contour,Z,x,y, NLEVELS=13,/FILL
window,8
contour,Z,x,y, NLEVELS=13,/CELL_FILL
window,9
contour,Z,x,y, NLEVELS=13,/FILL,/CELL_FILL
window,10
contour,Z,x,y, NLEVELS=13,/FILL,/CELL_FILL,C_LABELS=[0,0,0,1,1,1,1],C_ANNOTATION=['n1','n2','n3','n4'],C_LINESTYLE=[0,1,2,3]
window,11
contour,Z,x,y, NLEVELS=13,/FILL,/CELL_FILL,/XLOG,/YLOG,/ISOTROPIC,/FOLLOW
window,12
contour,Z,x,y, NLEVELS=13,MAX_VALUE=0,/FOLLOW
window,13
contour,Z,x,y, NLEVELS=13,MIN_VALUE=0,/FOLLOW
window,14
contour,Z,x,y, NLEVELS=13,MIN_VALUE=-0.2,MAX_VALUE=0.3,/FOLLOW
contour,Z,x,y, LEVELS=V,C_LINESTYLE=[1],/OVERPLOT
window,15
contour,Z,x,y, NLEVELS=13,/DOWNHILL
window,16
contour,Z,x,y, NLEVELS=13,LEVELS=V,MIN_VALUE=-0.1,MAX_VALUE=0.3,C_CHARSIZE=1,C_THICK=[1,2],C_LINESTYLE=[0,1],/FOLLOW


d=[[16,2,3,13],[5,11,10,8],[9,7,6,12],[4,14,15,1]]
window,17
contour,d,NLEVELS=11,/fill
window,18
contour,d,NLEVELS=11,/fill,/DOWNHILL

end
