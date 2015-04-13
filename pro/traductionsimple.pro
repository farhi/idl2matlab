pro traductionsimple, x, y

  if (x LT 0) then x = -x
  if (y GE 0) then y = +y $
      	      else y = -y

  for i = 1,y do $
  begin
    x = x*y^1 & y = y+x
  end
  
  z = 3 < x
  
  ; Action differente suivant la valeur de y
  case y of
    1 : repeat y=y/0.5 until (y LE 0) or (y EQ z)
    2 : while (x NE z) do x=x-1
    Else : if ((x GT y) AND (x GT z)) then x = (y-2) > z 
  end
  
  ; Affichage du sinus de x et du cosinus de y
  print,sin(x)
  print,cos(y)
  print,alog(z)

end
