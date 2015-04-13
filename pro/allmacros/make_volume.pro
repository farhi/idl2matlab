FUNCTION make_volume, Win,thresh=thresh
;******* ***********
;**
;**
s=SIZE(Win)
if s(0) ne 3 then return,Win

mx=max(Win,min=mi)
if n_elements(thresh) ne 1 then thresh=mi+(mx-mi)/6.
if (thresh lt mi) or (thresh gt mx) then return,Win

shade_volume,Win,thresh,v,p

v=reform(v,3,n_elements(v)/3)
TAKE_DATP,datp

x=reform(v(0,*)) & MOD_DATP,datp,'X',x
y=reform(v(1,*)) & MOD_DATP,datp,'Y',y
z=reform(v(2,*)) & MOD_DATP,datp,'Z',z

MOD_DATP,datp,'PV',p
MOD_DATP,datp,'PAR_TXT',['OBJECT: 1:Mol 2:Shape ','Thresh ']
MOD_DATP,datp,'P'      ,[               2.       , Thresh  ]
GIVE_DATP,datp
help,datp.x,datp.y,datp.z
return, x*0 +1
end
