function bobit,name
loadct,3
print,'cuicui'
XBIN=40
YBIN=39
TSLICE=500
det=intarr(XBIN,YBIN,TSLICE)

print,name
openr,3,name
readf,3,det
close,3

w=det

return,w
end 
