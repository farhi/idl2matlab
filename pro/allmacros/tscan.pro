PRO tscan,count 

w=900
;tstart=20
;tstop=60
;tstep=20
;count=1


co,2,'t n'
wait,.5

t=[8,20,50,80,110,130,150,170,210,250,280,300]
points=12

;for i=tstart,tstop,tstep do begin
;d17mad,'pte '+strtrim(string(i),2)

for i=0,points-1 do begin
d17mad,'pte '+strtrim(string(t(i)),2)
wait,w

d17mad,'temp'
mv,'trs',0
mv,'san',0

dial_autoalign,20.,0.4

setup

d17mad,'par sub 0T '+strtrim(string(t(i)),2)+' K'
wait,.5
co,2,'t n'
wait,.5
co,2,'t n'
wait,.5
d17mad,'temp'
co,count,' t'
d17mad,'temp'

endfor

end
