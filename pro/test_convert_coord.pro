pro test_convert_coord

surface,dist(30)

;Premiere partie de test : on a x = matrice de dim3*3
;On verifie si les differentes conversions fonctionnent bien
t=dist(3)

r=convert_coord(t,/data,/to_normal)
r2=convert_coord(t,/data,/to_device)
r3=convert_coord(r,/normal,/to_data)
r4=convert_coord(r,/normal,/to_device)
r5=convert_coord(r2,/device,/to_data)
r6=convert_coord(r4,/device,/to_normal)

;print,'Verification : on doit avoir r3=r5=dist(3), r4=r2 et r6=r'

if ((total(r3-r5) EQ 0.0) and (total(r3-t) LE 0.0001) and (total(r4-r2) EQ 0.0) and (total(r6-r) EQ 0.0)) then $
	print,'1er jeu de test : OK'   $
else $
	print,'1er jeu de test : KO'

;Deuxieme partie de test : on a x = un vecteur, y = un vecteur et z = un vecteur
;x, y et z ont tous la meme dimension
x=findgen(5)*2
y=findgen(5)/2
z=findgen(5)
m=findgen(3,5)
m(0,*)=x
m(1,*)=y
m(2,*)=z
s=convert_coord(x,y,z,/data,/to_normal)
s2=convert_coord(x,y,/data,/to_device)
s3=convert_coord(s,/normal,/to_data)
s4=convert_coord(s2,/device,/to_data)

;print,'Verification : on doit avoir s3=[x,y,z] et s4=[x,y,0]'

if ( (total(s3-m) LE 0.0001) and (total(s4-m) LE 0.0001)) then $
	print,'2eme jeu de test : OK' $
else $
	print, '2eme jeu de test : KO'

end
