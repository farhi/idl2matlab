PRO plotdfil,xmin,xmax,$
             thresh=thresh,max=max,min=min,cmin=cmin,cmax=cmax,$
             ps=ps,noprint=noprint,printer=printer,eps=eps,$
             multi=multi,scale=scale,w=w11,x=x11,overlap=overlap,$
             portrait=portrait,landscape=landscape,ysize=ysize,$
             xoffset=xoffset,yoffset=yoffset,intensity=intensity,$
             fwhm=width,position=position,cellno=cellno,$
             intersection=intersection,interwidth=interwidth,points=points,$
             sum=w10

;+
; EXAMPLE :
;  plotdfil,59,61,max=25000,/ps,/nop
;-
Window,0
IF NOT KEYWORD_SET(portrait)  THEN portrait =0
IF NOT KEYWORD_SET(landscape) THEN landscape=0
IF NOT KEYWORD_SET(landscape) AND NOT  KEYWORD_SET(portrait) THEN BEGIN
  IF KEYWORD_SET(eps) THEN portrait=1 ELSE landscape=1
ENDIF
IF NOT KEYWORD_SET(multi)     THEN multi    =5
IF NOT KEYWORD_SET(xoffset)   THEN BEGIN
  IF KEYWORD_SET(portrait)THEN xoffset=1 ELSE xoffset=1
ENDIF
IF NOT KEYWORD_SET(yoffset)   THEN BEGIN
  IF KEYWORD_SET(portrait)THEN yoffset=0 ELSE yoffset=10
ENDIF
IF NOT KEYWORD_SET(ysize)     THEN BEGIN
  IF KEYWORD_SET(landscape)THEN ysize=19./multi ELSE ysize=29./multi
ENDIF
IF NOT KEYWORD_SET(overlap)   THEN overlap  =1
IF NOT KEYWORD_SET(scale)     THEN scale    =2.5
IF KEYWORD_SET(eps) THEN ps=1 ELSE eps      =0
white=0
IF N_ELEMENTS(xmax) EQ 0 THEN IF NOT KEYWORD_SET(cmax) THEN xmax=159.9 ELSE xmax=FLOAT(cmax)/10.
IF N_ELEMENTS(xmin) EQ 0 THEN IF NOT KEYWORD_SET(cmin) THEN xmin=0.    ELSE xmin=FLOAT(cmin)/10.
;print,xmin,xmax
IF NOT KEYWORD_SET(max)  THEN max=25000.
IF NOT KEYWORD_SET(cmin) THEN cmin=FLOOR(xmin*10)-10
IF NOT KEYWORD_SET(cmax) THEN cmax=CEIL (xmax*10)+10
cmin=cmin>0
cmax=cmax<1599
IF N_ELEMENTS(cellno)       NE 1600 THEN cellno      =INDGEN(1600)
IF N_ELEMENTS(position)     NE 1600 THEN position    =fltarr(1600)
IF N_ELEMENTS(width)        NE 1600 THEN width       =fltarr(1600)
IF N_ELEMENTS(intersection) NE 1600 THEN intersection=fltarr(1600)
IF N_ELEMENTS(interwidth)   NE 1600 THEN interwidth  =fltarr(1600)
cell=0
overlap=FLOAT(overlap)
range=((xmax-xmin)+FLOAT(2.*overlap*((multi-1))))/FLOAT(multi)
PRINT,((xmax-xmin))/FLOAT(multi),FLOAT(overlap*((multi-1))),range
ang1=xmin+2.*overlap
cells=((cmax-cmin+4*(overlap*10)*((multi+1))))/multi
cel1=cmin+2*(overlap*10)
!P.Multi=[0,1,multi]
IF keyword_set(ps) THEN BEGIN
    IF KEYWORD_SET(eps) THEN extension='.eps' ELSE  extension='.ps' 
    mydevice=!D.NAME
    set_plot,'ps'
    psnam='def_'+strcompress(cmin,/RE)+'_'+strcompress(cmax,/re)+extension
    PRINT,'Create ',psnam, ' - XSIZE (range*scale) =',range*scale,' cm, YOFFSET=',yoffset, 'cm'
    device,portrait=portrait,landscape=landscape,/color,fil=psnam,encapsulated=eps,$
           YSIZE=ysize,XSIZE=range*scale,xoffset=xoffset,yoffset=yoffset,/SYMBOL
ENDIF
colors,color
TVLCT,[0,255,0,0],[0,0,255,0],[0,0,0,255]
color=[0,1,2,3,255]
xmin=ROUND(xmin*100)/100.
;print,xmin,xmax
xmax=ROUND(xmax*100)/100.
;print,xmin,xmax
w10=FLTARR(CEIL(xmax-xmin)*100)
w11=FLTARR(CEIL(xmax-xmin)*100,5)
x11=INTARR(CEIL(xmax-xmin)*100,5)
FOR page=1,multi DO BEGIN
 ang0=ang1-2.*overlap
 ang1=ang0+range
 plot,w10*0.,yr=[0,max],xr=[ang0,ang1],color=color(0),background=color(4),FONT=0,XSTYLE=1,yticklen=1,ygridstyle=2,yminor=0,xminor=9
 cel0=cel1-6*(10*overlap)
 cel1=cel0+cells
 ;print,cel0,cel1
 ;cel0=ang0*10.-10.
 ;cel1=ang1*10.+10.
 ;print,ang0,ang1
 ;print,cel0,cel1
 print,page,ang0,ang1,range,cel0,cel1,cells
 PRINT,'   cell, left, center, right, fwhm,  center-i/10.,  totalw,   maxw'
 ;PRINT,cel0,cmin,cmax,cel1
 for i=ROUND(cel0>cmin),ROUND(cel1<cmax) do BEGIN
  filnam='C'+STRMID(STRCOMPRESS(i+10000,/RE),1,4)+'.def'
  ;PRINT,filnam
  bid=FindFile(filnam,COUNT=counts)
  IF counts GE 1 THEN BEGIN
    OPENR,in,filnam,/GET_LUN
    number=0
    READF,in,number
    ;print,'Read ',filnam,number, ' points'
    x=FLTARR(number)
    w=FLTARR(number)
    READF,in,x,w
    FREE_LUN,in
    xnew=ROUND(x*100)/100.
    ;help,wnew,w,x,xnew
    wnew=INTERPOL(w,x,xnew)
    w=wnew
    x=xnew
    oplot,x,w,line=ABS(((i mod 32)<1)-1),color=color(i mod 4)
    maxw=max(w,pos)
    pos=x(pos)
    IF maxw ge max/2. AND pos LT ang1 AND pos GT ang0 THEN BEGIN
      XYOUTS, pos,maxw,charsize=.6, ' '+strcompress(i,/re),align=.0,orient=90,color=color(i mod 4),FONT=0
      IF N_ELEMENTS(thresh) EQ 1600 THEN BEGIN
        IF KEYWORD_SET(intensity) THEN BEGIN
          XYOUTS,pos,maxw,charsize=.5, strcompress(round(thresh(i)),/re)+' '+strcompress(round(TOTAL(w)/intensity),/re)+'    ',align=1,orient=90,color=color(i mod 4),FONT=0
        ENDIF ELSE BEGIN
          XYOUTS,pos,maxw,charsize=.5, strcompress(round(thresh(i)),/re)+'    ',align=1,orient=90,color=color(i mod 4),FONT=0
        ENDELSE
      ENDIF ELSE BEGIN
        IF KEYWORD_SET(intensity) THEN BEGIN
          XYOUTS,pos,maxw,charsize=.5, strcompress(round(TOTAL(w)/intensity),/re)+'    ',align=1,orient=90,color=color(i mod 4),FONT=0
        ENDIF 
      ENDELSE
    ENDIF
    IF i GT cell THEN BEGIN
     integral=FLTARR(number)
     FOR j=0,number-1 DO integral(j)=integral((j-1)>0)+w(j)
     totalw=integral(number-1)
     center=INTERPOL(x,integral,[totalw/2.,totalw/4.,totalw*3./4.])
     center=center(0)
     maxw=max(w,pos)
     x1=max(x(where(x le x(pos) AND w lt maxw/2)>0),ind)
     y1=w(where(x le x(pos) AND w lt maxw/2)>0)
     y1=y1(ind>0)
     x2=min(x(where(x lt x(pos) AND w gt maxw/2)>0),ind)
     y2=w(where(x le x(pos) AND w gt maxw/2)>0)
     y2=y2(ind>0)
     left=(x2-x1)/(y2-y1)*(maxw/2-y1)+x1
     x1=min(x(where(x gt x(pos) AND w lt maxw/2)>0),ind)
     y1=w(where(x ge x(pos) AND w lt maxw/2)>0)
     y1=y1(ind>0)
     x2=max(x(where(x gt x(pos) AND w gt maxw/2)>0),ind)
     y2=w(where(x ge x(pos) AND w gt maxw/2)>0)
     y2=y2(ind>0)
     right=(x2-x1)/(y2-y1)*(maxw/2-y1)+x1
     fwhm=  right-left
     width(i)=fwhm
     position(i)=center-i/10.
     PRINT,i,left,center,right,fwhm,center+i/10.,totalw,maxw
     for j=0,number-1 do begin
      k=ROUND(100*(x(j)-xmin))
      IF k GE 0 AND k LT N_ELEMENTS(W10) THEN BEGIN
        w10(k)=w10(k)+w(j)
        IF w(j) GT w11(k,0) THEN BEGIN
          w11(k,4)=w11(k,3)
          w11(k,3)=w11(k,2)
          w11(k,2)=w11(k,1)
          w11(k,1)=w11(k,0)
          w11(k,0)=w(j)
          x11(k,4)=x11(k,3)
          x11(k,3)=x11(k,2)
          x11(k,2)=x11(k,1)
          x11(k,1)=x11(k,0)
          x11(k,0)=i
        ENDIF ELSE IF w(j) GT w11(k,1) THEN BEGIN
            w11(k,4)=w11(k,3)
            w11(k,3)=w11(k,2)
            w11(k,2)=w11(k,1)
            w11(k,1)=w(j)
            x11(k,4)=x11(k,3)
            x11(k,3)=x11(k,2)
            x11(k,2)=x11(k,1)
            x11(k,1)=i
          ENDIF ELSE IF w(j) GT w11(k,2) THEN BEGIN
              w11(k,4)=w11(k,3)
              w11(k,3)=w11(k,2)
              w11(k,2)=w(j)
              x11(k,4)=x11(k,3)
              x11(k,3)=x11(k,2)
              x11(k,2)=i
            ENDIF ELSE IF w(j) GT w11(k,3) THEN BEGIN
                w11(k,4)=w11(k,3)
                w11(k,3)=w(j)
                x11(k,4)=x11(k,3)
                x11(k,3)=i
              ENDIF ELSE IF w(j) GT w11(k,4) THEN BEGIN
                  w11(k,4)=w(j)
                  x11(k,4)=i
                ENDIF 
      ENDIF
    ENDFOR
    cell=i
   ENDIF
  ENDIF
 ENDFOR
 IF NOT KEYWORD_SET(points) THEN points=0
 IF NOT KEYWORD_SET(min) THEN min=0
 oplot,Findgen(N_ELEMENTS(W10))/100.+xmin,w10,color=color(1),psym=points,min_value=min,max_value=max
 oplot,Findgen(N_ELEMENTS(W10))/100.+xmin,TOTAL(w11(*,0:2),2),color=color(2),psym=points,min_value=min,max_value=max
 oplot,Findgen(N_ELEMENTS(W10))/100.+xmin,TOTAL(w11(*,0:4),2),color=color(3),psym=points,min_value=min,max_value=max
 axis,yr=[0,max],xr=[ang0,ang1],color=color(0),FONT=0,XSTYLE=1,yticklen=.1/(range*scale)<0.002,/noerase,xminor=9
ENDFOR
IF keyword_set(ps) THEN BEGIN
    device,/close
    set_plot,mydevice
    IF NOT keyword_set(noprint) THEN BEGIN
      IF NOT keyword_set(printer) THEN bid=sys_dep('PRT_DEF',psnam)
      IF NOT keyword_set(printer) THEN printer='dj1_d20'
      bid=sys_dep('PRINT',printer,psnam)
    ENDIF
ENDIF
PRINT,'Intersection Analysis'
points=N_ELEMENTS(w11(*,0))
FOR i=1,points-1 DO BEGIN
  ;PRINT,i
  IF x11(i,0) GT x11(i-1,0) THEN BEGIN
    z2=w11(i,0)
    z1=w11(i-1,1)
    y2=w11(i,1)
    y1=w11(i-1,0)
    delta=(z1-y1)/(y2-y1-z2+z1)
    intersection(x11(i,0))=x11(i-1,0)/10.-(i-1.+delta)/100.+xmin
    interwidth(x11(i-1,0))=-(intersection(x11(i-1,0))-intersection(x11(i,0)))
    PRINT,'At',  intersection(x11(i,0)),' between',x11(i,0),' and',x11(i-1,0),interwidth(x11(i-1,0))
  ENDIF 
ENDFOR

END
