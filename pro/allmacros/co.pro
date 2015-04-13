pro co,value,tm


valstr=strtrim(string(value),2)
dummy=dial_mad_send('',0,'run '+valstr+' '+tm,'')
print,'sent command: run '+valstr+' ',tm
   
wait,.2
stat=DIAL_MAD_READ('status')
 stat=strtrim(stat,2)
 while stat eq 'COUNTING' do begin
  stat=DIAL_MAD_READ('status')
  stat=strtrim(stat,2)
 endwhile
wait,2.2
end


