pro did_readf,line,fmt,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10
;** *********
;**
for i=1,n_elements(fmt) do begin 
    if fmt(i-1) gt 0 then line=strmid(line,fmt(i-1),200)
    ii=execute('reads,line,v'+strtrim(string(i),2)+',line')
endfor
end

pro READ_ORG, pthfil, w,x,y,z,e, head,step,m,n,f,a,b,c,d, org,pair,mxi, status=pp2,ws=ws,strtit=strtit
;** ********
;**
on_ioerror,misopen
OPENR,u,pthfil,/get_lun & line=' '
on_ioerror,misread

case org of
0:	fmt=[a,b]
1:	fmt=[a,b,c]
2:	fmt=[a,b,c]
3:	fmt=[a,b,c,d]
4:	fmt=[a,b,c,d]
else:
endcase

if head gt 0 then begin strtit=strarr(head<10)         &    readf,u,strtit
                        if head gt 10 then for i=11L,head do readf,u,line
endif

clearpar,fix(ws),ws

if org  eq 5 then begin w=fltarr(m,n)   & readf,u,w & x=lindgen(m)+1 & y=lindgen(n)+1
endif else $
if org  eq 6 then begin w=fltarr(m,n,f) & readf,u,w & x=lindgen(m)+1 & y=lindgen(n)+1 & z=lindgen(f)+1
endif else $
if pair eq 1 then begin w=fltarr(n)     & x=fltarr (n)
   case org of
   0:   begin readf,u,x & if step gt 1 then for i=2,step do readf,u,line
              readf,u,w
        end
   1:   begin readf,u,x & if step gt 1 then for i=2,step do readf,u,line
              readf,u,w & if step gt 1 then for i=2,step do readf,u,line & e=fltarr(n)
              readf,u,e
        end
   2:   begin y=fltarr (n) & z=fltarr(n) & w(*)=1
              readf,u,x & if step gt 1 then for i=2,step do readf,u,line
              readf,u,y & if step gt 1 then for i=2,step do readf,u,line
              readf,u,z
        end
   3:   begin y=fltarr(n) & z=fltarr(n)
              readf,u,x & if step gt 1 then for i=2,step do readf,u,line
              readf,u,y & if step gt 1 then for i=2,step do readf,u,line
              readf,u,z & if step gt 1 then for i=2,step do readf,u,line
              readf,u,w
        end
   4:   begin y=fltarr(n) & z=fltarr(n)
              readf,u,w & if step gt 1 then for i=2,step do readf,u,line
              readf,u,x & if step gt 1 then for i=2,step do readf,u,line
              readf,u,y & if step gt 1 then for i=2,step do readf,u,line
              readf,u,z
        end
   else:
   endcase
endif else begin
  on_ioerror,miseof & cnt=0L & dim=500 & tmp=fltarr(dim) & w=fltarr(dim) & x=fltarr(dim)
                      cnp=0L &  v1=0.  & v2=0.  & v3=0.  & v4=0.
                      if org eq 1 then  e=fltarr(dim)
                      if org ge 2 then  begin y =fltarr(dim) & z=fltarr(dim) & endif
                      if total(fmt) eq 0 then sp=1 else sp=0
  trou=1
  while (trou) do begin print
   case org of
   0:   begin if sp then readf,u,v1,v2       else begin readf,u,line & did_readf,line,fmt,v1,v2       & endelse
              x(cnp)=v1 & w(cnp)=v2
              if cnt eq dim-1 then begin w=[w,tmp] & x=[x,tmp]                         & cnt=0  & endif else cnt=cnt+1
        end
   1:   begin if sp then readf,u,v1,v2,v3    else begin readf,u,line & did_readf,line,fmt,v1,v2,v3    & endelse
              x(cnp)=v1 & w(cnp)=v2 & e(cnp)=v3
              if cnt eq dim-1 then begin w=[w,tmp] & x=[x,tmp] & e=[e,tmp]             & cnt=0  & endif else cnt=cnt+1
        end
   2:   begin if sp then readf,u,v1,v2,v3    else begin readf,u,line & did_readf,line,fmt,v1,v2,v3    & endelse
              x(cnp)=v1 & y(cnp)=v2 & z(cnp)=v3
              if cnt eq dim-1 then begin x=[x,tmp] & y=[y,tmp] & z=[z,tmp]             & cnt=0  & endif else cnt=cnt+1
        end
   3:   begin if sp then readf,u,v1,v2,v3,v4 else begin readf,u,line & did_readf,line,fmt,v1,v2,v3,v4 & endelse
              x(cnp)=v1 & y(cnp)=v2 & z(cnp)=v3 & w(cnp)=v4
              if cnt eq dim-1 then begin w=[w,tmp] & y=[y,tmp] & x=[x,tmp] & z=[z,tmp] & cnt=0  & endif else cnt=cnt+1
        end
   4:   begin if sp then readf,u,v1,v2,v3,v4 else begin readf,u,line & did_readf,line,fmt,v1,v2,v3,v4 & endelse
              w(cnp)=v1 & x(cnp)=v2 & y(cnp)=v3 & z(cnp)=v4
              if cnt eq dim-1 then begin w=[w,tmp] & y=[y,tmp] & x=[x,tmp] & z=[z,tmp] & cnt=0  & endif else cnt=cnt+1
        end
   else:
   endcase
   cnp=cnp +1
   if step gt 1 then for i=2,step do readf,u,line
   if mxi  gt 0 then if  cnp  eq mxi then  trou=0
  endwhile
  miseof:
  if org eq 0 then begin w=w(0:cnp-1) & x=x(0:cnp-1) & endif
  if org eq 1 then begin w=w(0:cnp-1) & x=x(0:cnp-1) & e=e(0:cnp-1)   & endif
  if org eq 2 then begin x=x(0:cnp-1) & y=y(0:cnp-1) & z=z(0:cnp-1)   & w=fltarr(cnp) & w(*)=1 & endif
  if org eq 3 then begin x=x(0:cnp-1) & y=y(0:cnp-1) & z=z(0:cnp-1)   & w=w(0:cnp-1)  & endif
  if org eq 4 then begin x=x(0:cnp-1) & y=y(0:cnp-1) & z=z(0:cnp-1)   & w=w(0:cnp-1)  & endif
endelse
misread:free_lun,u
pp2=0
misopen:
end
