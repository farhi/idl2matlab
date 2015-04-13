function  [mat,dir]=rotat(mat,dir)
%*******            *****
%**

p=mod(dir,8);
switch p,
case 0;
case 1; mat=rot90         (mat,1);
case 2; mat=rot90         (mat,2);
case 3; mat=rot90         (mat,3);
case 4; mat=      transpos(mat);
case 5; mat=rot90(transpos(mat),1);
case 6; mat=rot90(transpos(mat),2);
case 7; mat=rot90(transpos(mat),3);
otherwise;
end;
