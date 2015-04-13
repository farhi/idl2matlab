;d17flip,coil,first,last,step,count

pro fit_parabola, xp,yp, bestfit, xymin
;** ************
	s=SIZE(xp)	& np=s(1)
	dx=xp(1)-xp(0)

	A0=INT_TABULATED(xp,yp)
	A1=INT_TABULATED(xp,yp*xp)
	A2=INT_TABULATED(xp,yp*xp^2)

	x1=xp(0)	& x2=xp(np-1)
	d1=x2-x1
	d2=(x2^2-x1^2)/2.
	d3=(x2^3-x1^3)/3.
	d4=(x2^4-x1^4)/4.
	d5=(x2^5-x1^5)/5.

	A=[[d3,d2,d1],[d4,d3,d2],[d5,d4,d3]]
	B=[A0,A1,A2]
	R=CRAMER(A,B)

	bestfit=R(0)*xp^2+R(1)*xp+R(2)
	x0=-0.5*R(1)/R(0)
	ymin=R(0)*x0^2+R(1)*x0+R(2)
        print,'in fit'
	xymin=[x0,ymin]
END
 
pro d17flip,coil,first,last,step,count,flux,cur,er


; coil is the number of the coil to be scanned
; first last and step define the current range in the scan
;count is the time to count each point
;xmin, xmax define the area of the detector in x to sum over

;dummy runs
co,2,'t n'
d17data,data
wait,.5
co,2,'t n'
wait,.5
d17data,data

range=5
points=fix((float(last)-float(first))/float(step))+1
print,'points in scan = ',points

cur=fltarr(points)
flux=fltarr(points)
er=fltarr(points)
cur=findgen(points)*step+float(first)
print,cur
print,count,float(count)
for i=0,points-1 do begin
  
  mv,'b'+strtrim(string(coil),2),cur(i)
  wait,1 
  co,count,' t n'
  wait,1  
  d17data,data
  wait,1
  b=size(data)
  if b(0) gt 1 then dat=total(data,2)
  a=max(dat,c)
  print,a,c
  xmin=c-range & xmax=c+range
  print,a,c,xmin,xmax
  fer=sqrt(float(total(dat(xmin:xmax))))/float(total(dat(xmin:xmax)))
  flux(i)=float(total(dat(xmin:xmax)))/float(count)
  er(i)=fer*flux(i)  
  print,cur(i),flux(i),er(i)
  plot, cur, flux, PSYM=2, XRANGE=[float(first),float(last)], $
  YRANGE=[0,MAX(flux)], XTITLE='b'+strtrim(string(coil),2), $
  YTITLE='Count Rate', TITLE=' scan of '+strtrim(string(coil),2)
  ERRPLOT, cur, flux-er, flux+er
endfor
print,'fit'
fit_parabola, cur,flux, bestfit, xymin
print,'did'
OPLOT, cur, bestfit, PSYM=0, LINESTYLE=0
print,'bestfit= ',xymin

mv,'b'+strtrim(string(coil),2),xymin(0)
end
