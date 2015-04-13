pro test_polar_contour

 nr=12
 nt=18
 r=findgen(nr)/(nr-1)
 theta=2*!pi*findgen(nt)/(nt-1)
 z=cos(theta*3)
 ;Penser a modifier cette ligne par ((r-0.5)^2).'*z en Matlab
 z=z#((r-0.5)^2)

V=[-0.3,-0.2,0,0.1,0.3,0.35,0.36]

 window,0
 polar_contour,z,theta,r,nlevels=15
 window,1
polar_contour,z,theta,r, LEVELS=V, C_LINESTYLE=[0,1,2,3], C_ANNOTATION=['n1','n2','n3','n4'], /FOLLOW
window,2
polar_contour,z,theta,r, LEVELS=V, C_THICK=[1,2,3] , C_LABELS=[0,0,0,1,1,1], C_CHARSIZE=1.1
window,3
polar_contour,z,theta,r, NLEVELS=13
window,4
polar_contour,z,theta,r, NLEVELS=13,LEVELS=V,/FOLLOW
window,5
polar_contour,z,theta,r, NLEVELS=13,/XLOG,/YLOG
window,6
polar_contour,z,theta,r, NLEVELS=13,/ISOTROPIC
window,7
polar_contour,z,theta,r, NLEVELS=13,/FILL
window,8
polar_contour,z,theta,r, NLEVELS=13,/CELL_FILL
window,9
polar_contour,z,theta,r, NLEVELS=13,/FILL,/CELL_FILL
window,10
polar_contour,z,theta,r, NLEVELS=13,/FILL,/CELL_FILL,C_LABELS=[0,0,0,1,1,1,1],C_ANNOTATION=['n1','n2','n3','n4'],C_LINESTYLE=[0,1,2,3]
window,11
polar_contour,z,theta,r, NLEVELS=13,/FILL,/CELL_FILL,/XLOG,/YLOG,/ISOTROPIC,/FOLLOW
window,12
polar_contour,z,theta,r, NLEVELS=13,MAX_VALUE=0,/FOLLOW
window,13
polar_contour,z,theta,r, NLEVELS=13,MIN_VALUE=0,/FOLLOW
window,14
polar_contour,z,theta,r, NLEVELS=13,MIN_VALUE=-0.2,MAX_VALUE=0.3,/FOLLOW
polar_contour,z,theta,r, LEVELS=V,C_LINESTYLE=[1],/OVERPLOT
window,15
polar_contour,z,theta,r, NLEVELS=13,/DOWNHILL
window,16
polar_contour,z,theta,r, NLEVELS=13,LEVELS=V,MIN_VALUE=-0.1,MAX_VALUE=0.3,C_CHARSIZE=1,C_THICK=[1,2],C_LINESTYLE=[0,1],/FOLLOW



end
