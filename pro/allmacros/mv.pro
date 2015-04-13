pro mv,mot,value


valstr=strtrim(string(value),2)
dummy=dial_mad_send('',0,mot+' '+valstr,'')
print,'sent command: '+mot+' '+valstr
   
wait,.8
stat=DIAL_MAD_READ('status')
;print,stat
 stat=strtrim(stat,2)
 while stat eq 'POSITIONNING' do begin
  stat=DIAL_MAD_READ('status')
  stat=strtrim(stat,2)
;  print,stat
 endwhile
wait,.5

end
