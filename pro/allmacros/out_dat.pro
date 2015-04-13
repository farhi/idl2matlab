pro out_dat, w, file_name_base



take_datp, datp


;	**** filenames should be given with the base command ***
;	**** if not, the default name is "outfile" ***
if file_name_base eq '' then file_name_base = "outfile"

; 	**** set the two file names and get the corresponding units ****
o_file_dat  = file_name_base+'.dat'
o_file_gsas = file_name_base+'.gsas'
get_lun, unit1
get_lun, unit2

;	**** initialize the useful variables from the pv array ****
d2th = round(datp.p(11)*1000)/1000.
monitor = datp.p(12)
x = round(datp.x*1000)/1000. ;	x-axis (round up possible errors)
e = datp.e ;	errors
nruns = datp.n(0)/monitor ; (no of runs: not necessarily an integer)

;	        w contains the intensities

;	**** cut off negative points *****

range = where (x ge -1.e-4)
xx = x(range)
ww = w(range)
ee = e(range)


;	**** calculate final number of points (voids are absent in xx ****
;	**** but must be present in the data files), and initialize the new
;	**** variables *****
 
npoints = round(fix((xx(n_elements(xx)-1)-xx(0))/d2th+1)/10)*10
xnew=findgen(npoints)*d2th+xx(0)
wnew=fltarr(npoints)
enew=fltarr(npoints)
iout = intarr(2,npoints)	; the final output matrix

;	*** now loop over the points of xnew and look for matches with xx ***
for j=0, npoints-1 do $
	begin

	pt = where(abs(xx-xnew(j)) lt 1e-4 )
;		*** if a match is found, set the values of ww, ee and iout ***
;		*** (otherwise, points will be left at 0) ****

       	if pt(0) ne -1 then $
	  begin
		wnew(j)   = ww(pt(0))
		enew(j)   = ee(pt(0))
;		*** average values are printed
		iout(0,j) = round(wnew(j)/(enew(j)^2)*nruns)
		iout(1,j) = round(wnew(j)/nruns)
	  endif 
endfor

;	**** now let the final output begin .... ******
flag=''
on_ioerror, mis_write 
openw, unit1, o_file_dat
openw, unit2, o_file_gsas
printf, unit1, datp.w_tit, format='(A62,17X," ")'
printf, unit2, datp.w_tit, format='(A62,17X," ")'
printf, unit1 ,64, 0, d2th, 1, 0, 0, 0,format= '(2I8,F8.4,4I8)'
printf, unit2, 1,npoints,npoints/10,' CONST', xnew(0)*100,D2th*100,0.,0.,$
	format = '("BANK",3I12,A,4F15.7," STD")'
printf, unit1, xnew(0), format = '(F8.3)'
printf, unit1, monitor, format = '(F8.0)'
printf, unit1, iout, format = '(10(I2, I6))'
printf, unit2, iout, format = '(10(I2, I6))'      
printf, unit1, format ='("   -1000",71X," ")'
printf, unit1, format ='("   -10000",70X," ")'

;   	*** and do not forget to free the units ... ****
free_lun, unit1
free_lun, unit2
flag='ok'

mis_write: if flag ne 'ok' then print,string(7b)+!err_string

end
