pro testsimple, x, y

;a = findgen(p)
;b=5
;For i=0,b do a=a+1
;print,a
a=1

if (x LT 0) then x = -x
  if (y GE 0) then y = +y $
      	      else y = -y

a=a*x & x=x+y

z = 3 < x

  ; Action differente suivant la valeur de y
  case y of
    1 : repeat y=y/0.5 until (8 LE y) or (y EQ z)
    2 : while (x NE z) and (x GT z) do x=x-1
    Else : if ((x GT y) AND (x GT z)) then x = (y-2) > z 
  end

  
  print,x
  print,y
  ; Affichage du sinus de x et du cosinus de y
  print,sin(x)
  print,cos(y)

end


