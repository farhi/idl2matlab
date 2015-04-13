pro geokey, key
;** ******

key=" "+key+" " & TB=byte(key)-1 & ntb=n_elements(TB)-1
key=""  &  if ntb lt 4  then return  else TB=long(TB)
TB(0)=TB(2)     & TB(ntb)=TB(ntb-2)
tmp  =TB(1)+2   & TB(1)  =TB(ntb-1)+2 & TB(ntb-1)=tmp
for i=0,ntb do key=key+string(TB(i))
key  =strtrim(strcompress(key),2)+" "
end
