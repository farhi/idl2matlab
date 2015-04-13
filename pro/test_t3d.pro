pro test_t3d

T3D, /RESET, ROT = [ 30,0,0], PERSPECTIVE = 1.
print,'!p.t=',!P.T

T3D, /RESET, TRANS = [-.5,-.5,0], ROT = [0,0,45]
print,'!p.t=',!P.T

T3D, TRANS = [.5,.5,0]
print,'!p.t=',!P.T

T3D,MATRIX=mat,/reset,TRANS = [-.5,-.5,0]
print,'!p.t=',!P.T
print,'Mat=',mat

T3D, /reset, scale=[1,2,0.5]
print,'!p.t=',!P.T

T3D,/reset,oblique=[0.1,3.2]
print,'!p.t=',!P.T

T3D,TRANS = [-.5,-.5,0], ROT = [0,0,45] ,XYEXCH
print,'!p.t=',!P.T

end
