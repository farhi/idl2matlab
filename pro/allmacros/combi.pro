FUNCTION combi, w1, w2, dim

take_datp,p1
take_datp,p2,/third

cells=N_ELEMENTS(w1(*,0,0))
inner=N_ELEMENTS(w1(0,*,0))
outer=N_ELEMENTS(w1(0,0,*))

IF N_PARAMS() LT 3 THEN BEGIN
  dim=2
		IF outer GT 1 THEN dim=3
ENDIF

IF dim EQ 1 THEN BEGIN
  print,'Concatenation in one dimensions (funny)'
  w =[w1   ,w2   ]
  e =[p1.e ,p2.e ]
  x =[p1.x ,p2.x ]
  pv= p1.pv
  n = p1.n 
  y = p1.y 
  z = p1.z 
ENDIF ELSE BEGIN
  IF dim EQ 2 THEN BEGIN
    print,'Concatenation in two dimensions (normal)'
    w =[[w1   ],[w2   ]]
    e =[[p1.e ],[p2.e ]]
    x =[[p1.x ],[p2.x ]]
    pv=[[p1.pv],[p2.pv]]
    y =[ p1.y  , p2.y  ]
    n =[ p1.n  , p2.n  ]
    z =[[p1.z ],[p2.z] ]
  ENDIF ELSE BEGIN
    print,'Concatenation in three dimensions (extravagant)'
    w =[[[w1   ]],[[w2   ]]]
    e =[[[p1.e ]],[[p2.e ]]]
    x =[[[p1.x ]],[[p2.x ]]]
    pv=[[[p1.pv]],[[p2.pv]]]
    y =[[ p1.y  ],[ p2.y  ]]
    n =[[ p1.n  ],[ p2.n  ]]
    z =[[[p1.z ]],[[p2.z ]]]
  ENDELSE
ENDELSE
help,w,x,y,z,n,e,pv
mod_datp,p1,'x' ,x
mod_datp,p1,'y' ,y
mod_datp,p1,'z' ,z
mod_datp,p1,'n' ,n
mod_datp,p1,'e' ,e
mod_datp,p1,'pv',pv
give_datp,p1
RETURN,w
END
