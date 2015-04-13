pro d17ratio,count
range=5
;dummy runs
co,2,'t n'
d17data,data
wait,.5
co,2,'t n'
wait,.5
d17data,data
wait,1
d17mad,'par sub flipping ratio'
print,'turning flippers on'
d17mad,'b1 on'
wait,1
d17mad,'b2 on'
wait,2

co,count,' t'
wait,2
d17data,data
wait,2
b=size(data)
if b(0) gt 1 then dat=total(data,2)

a=max(dat,c)
print,'sum range= ',c-range,c+range
on=float(total(dat(c-range:c+range)))

eron=sqrt(on)/on
print,'total rate =',total(float(dat))/float(count)
print,'box counts =',on, '+/- ',eron*on

print,'turning flippers off'
d17mad,'b1 off'
wait,1
d17mad,'b2 off'
wait,2

co,count,' t'
wait,2
d17data,data
wait,2
b=size(data)
if b(0) gt 1 then dat=total(data,2)

a=max(dat,c)
print,'sum range= ',c-range,c+range
off=float(total(dat(c-range:c+range)))
eroff=sqrt(off)/off
print,'sum range= ',c-range,c+range
print,'total rate =',total(float(dat))/float(count)
print,'box counts =',off, '+/- ',eroff*off

ratio=off/on

error=ratio*sqrt(eron^2+eroff^2)

print,'Flipping ratio =',ratio,' +/- ',error

end

