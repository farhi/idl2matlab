FUNCTION filters,n1,n2,a=aa,x=xxx,y=yyy,zz12=zzz,z1=z1,z2=z2,t1=trans1,t2=trans2
;
; Started  14-Nov-96 by Thomas Hansen
; Macro for analysis of Graphite filter plates for the HOPG Monochromator of D20
; Last changed 23-Nov-96 by Thomas Hansen
;
IF n_elements(n2) EQ 0 THEN n2=n1
result=fltarr(252,252)
trans1=fltarr(252,252)
trans2=fltarr(252,252)
xx=fltarr(252,252)
yy=fltarr(252,252)
numors=long(indgen(n2-n1+1)+n1)
IF n_elements(aa) THEN a=aa ELSE a=[[185,45.8,.22,0,0,0],[43,37.1,.22,0,0,0],[23,43.8,.22,0,0,0]]
w=concat(numors)
take_datp,datp
give_datp,datp
pretreat,w,126
take_datp,datp
calib,w
take_datp,datp
normalize,w,1.95646e+07,/noprint
take_datp,datp
;IF n_elements(w(0,*)) GT 1 THEN surface,w,datp.x,indgen(n_elements(w(0,*))) ELSE plot,w,datp.x
w=fit(w,a)
take_datp,datp
;IF n_elements(w(0,*)) GT 1 THEN surface,w,datp.x,indgen(n_elements(w(0,*))) ELSE plot,w,datp.x
FOR j=0,n_elements(datp.pv(17,*))-1 DO BEGIN
  t1=round(datp.pv(17,j))
  t2=round(datp.pv(18,j))
  xx(t1,t2)=t1
  yy(t1,t2)=t2
  result(t1,t2)=a(0,0,j)*a(2,0,j)/(a(0,1,j)*a(2,1,j))
  trans1(t1,t2)=a(0,0,j)*a(2,0,j)/4.23305
  trans2(t1,t2)=a(0,1,j)*a(2,1,j)/2.27065
ENDFOR
print
t1=where(total(result,2))
t2=where(total(result,1))
result=result(t1(*),*)
result=result(*,t2(*))
trans1=trans1(t1(*),*)
trans1=trans1(*,t2(*))
trans2=trans2(t1(*),*)
trans2=trans2(*,t2(*))
xx=xx(t1(*),*)
xx=xx(*,t2(*))
yy=yy(t1(*),*)
yy=yy(*,t2(*))
mod_datp,datp,'x',xx
mod_datp,datp,'y',yy
mod_datp,datp,'x_tit','T1/mm'
mod_datp,datp,'y_tit','T2/mm'
mod_datp,datp,'w_tit','[(111)/(lambda)]/[(220)/(lambda/2)]: Run'+string(round(n1))+' to'+string(round(n2))
mod_datp,datp,'other_tit','FILTERS.pro'
mod_datp,datp,'e',0*result
mod_datp,datp,'n',0*t2
give_datp,datp
IF KEYWORD_SET(xxx) THEN BEGIN
  xx=REFORM(xx,n_elements(result))
  yy=REFORM(yy,n_elements(result))
  ww=REFORM(result,n_elements(result))
  xx=xx(WHERE(ww))
  yy=yy(WHERE(ww))
  ww=ww(WHERE(ww))
  triangulate,xx,yy,zz
  xxx=min(xx)+float(indgen(max(xx)-min(xx)+1))
  yyy=min(yy)+float(indgen(max(yy)-min(yy)+1))
  zzz = trigrid(xx,yy,ww,zz,[1,1],/smooth)
  z1=REFORM(trans1,n_elements(trans1))
  z1=z1(WHERE(z1))
  z1 = trigrid(xx,yy,z1,zz,[1,1],/smooth)
  z2=REFORM(trans2,n_elements(trans2))
  z2=z2(WHERE(z2))
  z2 = trigrid(xx,yy,z2,zz,[1,1],/smooth)
 ;contour,zzz,xxx,yyy,/follow,nlevels=16
 ;surface,zzz,xxx,yyy
ENDIF
output=[reform(t2,1,n_elements(t2)),result]
output=[[0,t1],[output]]
print,strmid(strcompress(string(output)),1,4)
FOR i = 0,3 DO BEGIN
  index=WHERE(datp.x GE i*60 AND datp.x LT (i+1)*60 AND result NE 0.0 AND datp.y GE 150 AND datp.y LE 230,count)
  IF count gt 1 THEN m=moment(result(index),sdev=s)
  IF count gt 1 THEN print,i,':',m(0),' +/-',s
ENDFOR
print
output=[reform(t2,1,n_elements(t2)),trans1]
output=[[0,t1],[output]]
print,strmid(strcompress(string(output)),1,4)
FOR i = 0,3 DO BEGIN
  index=WHERE(datp.x GE i*60 AND datp.x LT (i+1)*60 AND result NE 0.0 AND datp.y GE 150 AND datp.y LE 230,count)
  IF count gt 1 THEN m=moment(trans1(index),sdev=s)
  IF count gt 1 THEN print,i,':',m(0),' +/-',s
ENDFOR
print
output=[reform(t2,1,n_elements(t2)),trans2]
output=[[0,t1],[output]]
print,strmid(strcompress(string(output)),1,4)
FOR i = 0,3 DO BEGIN
  index=WHERE(datp.x GE i*60 AND datp.x LT (i+1)*60 AND result NE 0.0 AND datp.y GE 150 AND datp.y LE 230,count)
  IF count gt 1 THEN m=moment(trans2(index),sdev=s)
  IF count gt 1 THEN print,i,':',m(0),' +/-',s
ENDFOR
;surface,zzz,xxx,yyy
return,result
END
