function c = lt(a,b)


taille_a = size(a,2);
taille_b = size(b,2);


if (taille_a < taille_b); taille = taille_a;
else taille = taille_b; end

i = 1;
comp = -1;
while (i <= taille & comp == -1)
  if builtin('lt',a(i),b(i)) == 1 comp = 1; end
  if builtin('gt',a(i),b(i)) == 1 comp = 0; end
  i = i + 1;
  
end

if (comp == -1) 
  if taille_a < taille_b c = 1; 
  else c = 0; end
     	      	
else c = comp; end
