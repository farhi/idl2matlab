pro matovr ,pw,px,py ,row=row,column=col,poly=poly,iso=thresh
;** ******
;Surface to VRML file "lamp.wrl" (D.Richard oct 96)

if n_elements(pw) lt 3 then return

s   =size(pw) & if s(0) eq 1 then begin s(2)=1 & poly=0 & endif
p=75. & d=1.
if keyword_set(poly) then d=3.
fmt='(f4.1)'


IF s(0) LT 3 THEN BEGIN
;**************************************************************************************************
;****************
;TWO DIMENSIONAL*
;****************
;****************

w=pw		 & if n_elements(px) lt s(1) then x=findgen(s(1)) else x=px
if s(0) eq 2 then  if n_elements(py) lt s(2) then y=findgen(s(2)) else y=py else y=1
sx=(size(x))(0) & sy=(size(y))(0)

;*******
;RESIZE*
;*******
rz=200 & if keyword_set(poly) then rz=100
n1=(s(1)/rz)>1 & n2=(s(2)/rz)>1
if  n1+n2 gt 2 then begin if s(0) eq 2  then w=congrid(w,s(1)/n1,s(2)/n2) else $
			  if n1   gt 1  then w=congrid(w,s(1)/n1)
			  if sx   eq 2  then x=congrid(x,s(1)/n1,s(2)/n2) else $
			  if n1   gt 1  then x=congrid(x,s(1)/n1)
			  if sy   eq 2  then y=congrid(y,s(1)/n1,s(2)/n2)
			  if n2   gt 1  then y=congrid(y,s(2)/n2)
			  endif
;**********
;NORMALIZE*
;**********
maxw=max( w,min=minw) &  maxx=max( x,min=minx) & maxy=max( y,min=miny)
w   =( w-minw)*p/d/(maxw-minw)
x   =( x-minx)*p  /(maxx-minx)
if s(0) gt 1 then y   =( y-miny)*p  /(maxy-miny) else y=0

;*******
;COLUMN*
;*******
if keyword_set(col) and (s(0) eq 2) then begin w=transpose(w) & a=x
			if sx eq 2  then x=transpose(y) else x=y
			if sy eq 2  then y=transpose(a) else y=a & endif

s  =size(w) & s1=s(1) & if s(0) eq 2 then s2=s(2) else s2=1L

;************
;DERIVATIVES*
;************
w   =fix(w/2*10)/10.*d
if s(0) eq 2		then wx=   (w-shift(w,1,0)) + (w-shift(w,-1,0))
if s(0) eq 1		then wx=   (w-shift(w,1))   + (w-shift(w,-1)) else $
if keyword_set(poly)	then begin
			     wx=wx+(w-shift(w,0,1)) + (w-shift(w,0,-1))
			     wx=(abs(wx)-.5)>0 & endif
wx(0,*)=1 & wx(s1-1,*)=1

;********************
;ASCII TRANSCRIPTION*
;********************
buf=strarr(3,s1*s2)
if sx lt 2 then xb =transpose(string(x,format=fmt)+' ')
if sy lt 2 then y  =string(y,format=fmt)+' '
if sy lt 2 then yb =strarr(1,s1)

;POLYGONS*
;*********
if keyword_set(poly) and (s(0) eq 2) then begin
   bof=strarr((s1-1)*4*s2/2)
   tr4=lonarr(3,4)+s1
   trs=[[0,-s1-1,-s1],[0,-1,-s1-1],[0,s1-1,-1],[0,s1,s1-1]]
   j=1L & p=0L
   WHILE j le s2-2 do begin
	 i=1L & trj=tr4*j & a1=s1*(j+1) & a2=s1*(j-1)
	 WHILE i le s1-1 do begin
	   k=i
	   while (k le s1-2) and (wx(k,j) eq 0) and (wx(k,j+1) eq 0) do k=k+1

	   if k eq i then begin t=strtrim(string(trj+trs+k),1)
				for q=0,3 do bof(p+q)=t(0,q)+','+t(1,q)+','+t(2,q)
				p=p+4

	   endif     else begin trq=strtrim(string([a1+k ,a1+i-1 ,a2+i-1 ,a2+k]),1)
				bof(p)=trq(0)+','+trq(1)+','+trq(2)+','+trq(3)
				p=p+1
	   			endelse
	   i=k+1
	 ENDWHILE
	 for k=j-1,j do begin q=k*s1
	     if sx eq 2 then xb   =transpose(string(x(*,k),format=fmt)+' ')
	     if sy eq 2 then yb   =transpose(string(y(*,k),format=fmt)+' ') $
		        else yb(*)=y(k)
	     buf(0,q)=xb
	     buf(1,q)=yb
	     buf(2,q)=transpose(string(w(*,k),format=fmt))+','
	 endfor
	 j=j+2
   ENDWHILE
   for k=j-1,j-1+s2-j do begin q=k*s1
	     if sx eq 2 then xb   =transpose(string(x(*,k),format=fmt)+' ')
	     if sy eq 2 then yb   =transpose(string(y(*,k),format=fmt)+' ') $
		        else yb(*)=y(k)
	     buf(0,q)=xb
	     buf(1,q)=yb
	     buf(2,q)=transpose(string(w(*,k),format=fmt))+','
   endfor
   if j lt s2 then begin trj=lonarr(3,2)+s1*j & trs=trs(*,0:1)
	     for i=1,s1-1 do begin t=strtrim(string(trj+trs+i),1)
				   for q=0,1 do bof(p+q)=t(0,q)+','+t(1,q)+','+t(2,q)
				   p=p+2
	     endfor & endif
   bof=bof(0:p-1)+',-1,'

;LINES*
;******
endif else begin
  bof=strarr(2,s1*s2)
  a  =indgen(1,s1-1)
  i  =0L
  FOR j=0L,s2-1 do begin idx=where(wx(*,j) ne 0)
			 n  =n_elements(idx)
			 if sx eq 2 then xb   =transpose(string(x(*,j),format=fmt)+' ')
			 if sy eq 2 then yb   =transpose(string(y(*,j),format=fmt)+' ') $
				    else yb(*)=y(j)
			 buf(0,i)=xb(0,idx)
			 buf(1,i)=yb(0,idx)
			 buf(2,i)=transpose(string(w(idx,j),format=fmt))+','
			 a	 =indgen(1,n-1)+i
			 bof(0,i)=strtrim(string(a)  ,1)+','
			 bof(1,i)=strtrim(string(a+1),1)+','
			 i=i+n
			 bof(1,i-1)=bof(1,i-1)+'-1,' &  ENDFOR
  buf=buf(*,0:i-1)
  bof=bof(*,0:i-1)
endelse

ENDIF ELSE BEGIN
;**************************************************************************************************
;******************
;THREE DIMENSIONAL*
;******************
;******************
buf='' & bof=''
if n_elements(thresh) ne 1 then begin maxw=max(pw,min=minw) & thresh=minw+(maxw-minw)/3 & endif
shade_volume, pw  ,thresh,v,po

sp=n_elements(po)
if sp gt 3 then begin
;**********
;NORMALIZE*
;**********
   sv =size(v) & maxv=max(v(0,*),min=minv) & v(0,*)=transpose((v(0,*)-minv)*p*10/(maxv-minv))
		 maxv=max(v(1,*),min=minv) & v(1,*)=transpose((v(1,*)-minv)*p*10/(maxv-minv))
		 maxv=max(v(2,*),min=minv) & v(2,*)=transpose((v(2,*)-minv)*p*10/(maxv-minv))
   v   =round(v)
   bof=strarr(sp/3)
   buf=strarr(3,sv(2))
   buf(0,*)=strtrim(string(v(0,*)),1)+' '
   buf(1,*)=strtrim(string(v(1,*)),1)+' '
   buf(2,*)=strtrim(string(v(2,*)),1)+','
   i=0L & k=0L
   WHILE i lt sp do begin
	 j=po(i) & str=''
	 for n=i+1,i+j do str=str+strtrim(string(po(n)),1)+','
	 bof(k)=str
	 i=i+j+1 & k=k+1
   ENDWHILE
   bof=bof(0:k-1)+'-1,'
endif
ENDELSE

;**************************************************************************************************
;***********
;WRITE VRML*
;***********
;***********
head='#VRML V1.0 ascii'
tail='}'

ON_IOERROR,prob
OPENW,u,'lamp.wrl',/get_lun

PRINTF,u,head
PRINTF,u,'Separator {'

PRINTF,u,'DirectionalLight { direction 0  0 -1  }'
PRINTF,u,'PointLight       { location  0  0 200 }'

 PRINTF,u,'Material  { ambientColor    .4 0. 0.'
if keyword_set(poly) then $
 PRINTF,u,'            diffuseColor    .6 .6 .1' else $
 PRINTF,u,'            diffuseColor    1. 0. 0.'
 PRINTF,u,'            specularColor   1. 0. 0.'
if keyword_set(poly) then $
 PRINTF,u,'            emissiveColor   .6 0. 0.' else $
 PRINTF,u,'            emissiveColor   1. 0. 0.'
 PRINTF,u,'            shininess       1.'
 PRINTF,u,'            transparency    0. }'

PRINTF,u,'MaterialBinding { value OVERALL }'

;PRINTF,u,'SpotLight { on TRUE'
;PRINTF,u,'            intensity 1.'
;PRINTF,u,'            color     1. 1. 1.'
;PRINTF,u,'            location  0  0 300'
;PRINTF,u,'            direction 0  0 -1 }'

PRINTF,u,'DEF Surface Separator {'

PRINTF,u,'DEF SurfCoord Coordinate3 { point ['
PRINTF,u,buf
PRINTF,u,']}'

if keyword_set(poly) then PRINTF,u,'IndexedFaceSet { coordIndex [' $
		     else PRINTF,u,'IndexedLineSet { coordIndex ['
PRINTF,u,bof
PRINTF,u,']}'

PRINTF,u,tail & PRINTF,u,tail

FREE_LUN,u
prob:
end
