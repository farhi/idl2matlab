PRO titcat,num0,num1
datp=1
OPENW,tit,'titl_'+strcompress(num0,/re)+'_'+strcompress(num1,/re)+'.cat',/get_lun
OPENW,tem,'temp_'+strcompress(num0,/re)+'_'+strcompress(num1,/re)+'.cat',/get_lun
printf,tem,'# Numor     T(s.p.)/K    T(reg.)/K  T(sample)/K     Monitor  time/(s/100) total cnts.'
printf,tem,'# Numor  T(s.p.)  T(reg.) T(sample) Monitor time/(s/100) total cnts.'
for num=num0, num1 do begin
    w=rdrun(num,datp=datp)
    printf,tit,strcompress(num),' ',datp.w_tit
    printf,tit,'       ',datp.other_tit,strcompress([datp.p(5:7),datp.n(0:1),total(w)])
    printf,tem,FORMAT = '(1i7,3f9.2,3i12)',num,datp.p(5:7),datp.n(0:1),total(w)
    print,strcompress(num),' ',datp.w_tit
    print,'       ',datp.other_tit,strcompress([datp.p(5:7),datp.n(0:1),total(w)])
    print,FORMAT = '(1i7,3f9.2,3i12)',num,datp.p(5:7),datp.n(0:1),total(w)
endfor
free_lun,tit
free_lun,tem
end
