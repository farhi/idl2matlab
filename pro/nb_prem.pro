function verif_prem, cour_int

int_test = cour_int -2 
while ((int_test GE 1) AND ( cour_int mod int_test GT 0)) do int_test = int_test-2
return,int_test EQ 1

end



pro nb_prem,lim

cour_int = 1
compt = 1

while (compt LT lim or compt EQ lim) do $
begin
if verif_prem(cour_int) then $
  begin
    print,cour_int  
    compt= compt + 1
  end
cour_int = cour_int + 2
 
end
end
