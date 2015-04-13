; $Id: time_test.pro,v 1.5 1995/01/28 20:52:41 dave Exp $

;	Time test procedure.. Print values of commonly used timings.
pro timer, name	;Print timing information, name = descriptive
;	string for message.

common timer_common, time, lunno, total_time, geom_time, ntest

t = systime(1)		;Get current time.
ntest = ntest + 1
tt = t - time
total_time = total_time + tt
geom_time = geom_time + alog(tt)
printf, lunno, ntest, float(tt), ' ',name
time = t
end

pro init_time,file	;Initialize timer, file = optional param
;	containing name of file to write info to
common timer_common, time, lunno, total_time, geom_time, ntest

on_error,2              ;Return to caller if an error occurs
total_time = 0.
geom_time = 0.
ntest = 0
if n_params(0) ge 1 then begin
	get_lun, lunno	;Get a lun
	openw,lunno,file
  end else lunno = -1	;Set to stdout
time = systime(1)
return
end



pro time_compare, file1, file2, outfile	;Compare results of two time tests...
; File1 = file name containing output of first time test.
; File2 = file name containing output of 2nd time test.
; Outfile = filename for output. If omitted, only output to log window.
; A report is printed..
;
openr,lun1, /get, file1
openr,lun2, /get, file2
nmax = 100			;Max number of tests.
t1 = fltarr(nmax)
t2 = fltarr(nmax)
names = strarr(nmax)
for i=0, nmax-1 do begin	;Read times
    a1 = '' & a2 = ''
    readf, lun1, a1
    readf, lun2, a2
    if strpos(a1, 'Total Time') ge 0 then goto, done_reading
    a1 = strcompress(a1)
    j = strpos(a1, ' ', 1)
    t1(i) = float(strmid(a1, j+1, 100))
    j = strpos(a1, ' ', j+1)
    names(i) = strmid(a1, j+1, 100)
    a1 = strcompress(a2)	;2nd time
    j = strpos(a1, ' ', 1)
    t2(i) = float(strmid(a1, j+1, 100))
    endfor

done_reading: free_lun, lun1, lun2
t1 = t1(0:i)
t2 = t2(0:i)
names = names(0:i)
t1(i) = total(t1)
t2(i) = total(t2)
r = t1/t2
names(i) = 'Total Time'
luns = -1
if n_elements(outfile) gt 0 then begin
    openw, lun, /get, outfile
    luns = [luns, lun]
    endif
for file = 0, n_elements(luns)-1 do begin 
  printf, luns(file) , strmid(file1, 0, 7), ' ', strmid(file2,0,7), '   Ratio'
  for j=0,i do printf, luns(file), $
		format='(3(f7.2, 1x), a)', t1(j), t2(j), r(j), $
		strmid(names(j), 0, 48)
  endfor
if n_elements(outfile) gt 0 then free_lun, lun
end





pro reset,dummy	;Reset timer, used to ignore set up times...
; No-op this procedure to include setup times mainly comprise
; the time required to allocate arrays and to set them to
; a given value.

common timer_common, time, lunno, total_time, geom_time, ntest

time = systime(1)
return
end

pro dummy,dummy
return
end



pro graphics_times2, filename
; Time common graphics operations in the same manner as time_test  (REVISED)

common timer_common, time, lunno, total_time, geom_time, ntest

on_error,2                      ;Return to caller if an error occurs
if (!d.x_size ne 640) or (!d.y_size ne 512) then $
	window, xs=640, ys=512	;Use the same size window for fairness.
if n_params() gt 0 then init_time,filename else init_time

for i=1,10 do begin
	plot,[0,1]+i
	empty
	endfor
timer,'Simple plot, 10 times'

n = 1000
x = randomu(seed, n) * (2 * !pi)
y = fix((sin(x)+1) * (0.5 * !d.y_vsize))
x = fix((cos(x)+1) * (0.5 * !d.x_vsize))
for i=1,20 do begin
	erase
	empty
	plots,x,y,/dev
	empty
	endfor
timer,strtrim(n,2) + ' vectors x 100'
 
n = 50
plot,[-1,1],[-1,1]

for i=3,n do begin
	x = findgen(i) * ( 2 * !pi / i)
	erase
	polyfill,sin(x),cos(x)
	for j=1,i do begin
	    xx = randomu(seed, 3)
	    yy = randomu(seed, 3)
	    polyfill, xx, yy, /norm, color = !d.n_colors/2
	    endfor
	empty
	endfor
timer,'Polygon filling'

n = 512
a = findgen(n) * (8 * !pi / n)
c = bytscl(sin(a) # cos(a), top = !d.n_colors-1)
d = not c
erase
reset
for i=1,5 do begin
	tv,c
	empty
	tv,d
	empty
	endfor
timer,'Display 512 x 512 image, 10 times'
;for i=1,10 do begin
;	c = 0
;	c = tvrd(0,0,512,512)
;	endfor
;timer,'Read back 512 by 512 image, 10 times'

printf,lunno, float(total_time),'=Total Time, ', $
	exp(geom_time / ntest), '=Geometric mean,',ntest,' tests.'
if lunno gt 0 then free_lun,lunno
end

pro time_test2, filename		;Time_test revised....
common timer_common, time, lunno, total_time, geom_time, ntest

on_error,2                      ;Return to caller if an error occurs
if n_params() gt 0 then init_time,filename else init_time

;	Empty for loop
nrep = 2000000
for i=1L, nrep do begin & end

timer,'Empty For loop,' + string(nrep)+ ' times'

for i=1L,100000 do dummy, i
timer,'Call empty procedure (1 param) 100,000 times'

;	Add 100000 scalar ints:...
for i=0L,99999 do a=i+1
timer,'Add 100,000 integer scalars and store'

;	Scalar arithmetic loop:
for i=0L,25000 do begin
	a = i + i -2
	b = a / 2 + 1
	if b ne i then print,'You screwed up',i,a,b
	endfor
timer,'25,000 scalar loops each of 5 ops, 2 =, 1 if)'

a=replicate(2b,512,512)
reset
for i=1,10 do b=a*2b
timer,'Mult 512 by 512 byte by constant and store, 10 times'
for i=1,100 do c = shift(b,10,10)
timer,'Shift 512 by 512 byte and store, 100 times'
for i=1,50 do b=a+3b
timer,'Add constant to 512 x 512 byte array and store, 50 times'
for i=1,30 do b=a+b
timer,'Add two 512 by 512 byte images and store, 30 times'

a = float(a)
reset
for i=1,30 do b=a*2b
timer,'Mult 512 by 512 floating by constant and store, 30 times'
for i=1,30 do c = shift(b,10,10)	
timer,'Add constant to 512 x 512 floating and store, 30 times'
for i=1,30 do b=a+b
timer,'Add two 512 by 512 floating images and store, 30 times'

reset
for i=1,10 do a=randomu(qqq, 150, 150)	;Random number matrix
timer, 'Generate 225000 random numbers'

reset
b = invert(a)
timer,'Invert a 150 by 150 random matrix'

reset
nr_ludcmp, a, index
timer, 'LU Decomposition of a 150 by 150 random matrix'

a=bindgen(256,256) & b=a
reset
for i=0,255 do for j=0,255 do b(j,i)=a(i,j)
timer,'Transpose 256 x 256 byte, FOR loops'
for i=0,255 do begin
	b(0,i) = transpose(a(i,*))
	end
timer,'Transpose 256 x 256 byte, row and column ops'
b=transpose(a)
timer,'Transpose 256 x 256 byte, transpose function'

a=findgen(100000)+1
c=a
b = a
reset
for i=0L,n_elements(a)-1 do b(i) = alog(a(i))
timer,'Log of 100,000 numbers, FOR loop'
b = alog(a)
timer,'Log of 100,000 numbers, vector ops'

n = 2L^17
a = findgen(n)
reset
b = fft(a,1)
b = fft(b,-1)
timer,string(n) + ' point forward plus inverse FFT'

a=bytarr(512,512)
a(200:250,200:250)=10b
reset
for i=1,10 do b=smooth(a,5)
timer,'Smooth 512 by 512 byte array, 5x5 boxcar, 10 times'

a=float(a)
reset
for i=1,2 do b=smooth(a,5)
timer,'Smooth 512 by 512 floating array, 5x5 boxcar, 2 times'

a=bindgen(512,512)
aa =assoc(1,a)
reset
nrecs = 20
openw,1,'test.dat',512, initial = 512L*nrecs	;Must be changed for vax
for i=0,nrecs-1 do aa(i) = a
for i=0,nrecs-1 do a=aa(i)
timer,'Write and read 512 by 512 byte array x '+strtrim(nrecs,2)
close,1

printf,lunno, float(total_time),'=Total Time, ', $
	exp(geom_time / ntest), '=Geometric mean,',ntest,' tests.'
;  Remove the data file
openw,2,'test.dat',/DELETE
close,2
if lunno gt 0 then free_lun,lunno
end

pro graphics_times, filename
; Time common graphics operations in the same manner as time_test

common timer_common, time, lunno, total_time, geom_time, ntest

on_error,2                      ;Return to caller if an error occurs
if (!d.x_size ne 640) or (!d.y_size ne 512) then $
	window, xs=640, ys=512	;Use the same size window for fairness.
if n_params() gt 0 then init_time,filename else init_time

for i=1,10 do begin
	plot,[0,1]+i
	empty
	endfor
timer,'Simple plot, 10 times'

n = 1000
x = randomu(seed, n) * (2 * !pi)
y = fix((sin(x)+1) * (0.5 * !d.y_vsize))
x = fix((cos(x)+1) * (0.5 * !d.x_vsize))
for i=1,5 do begin
	erase
	empty
	plots,x,y,/dev
	empty
	endfor
timer,'vectors'
 
n = 24
plot,[-1,1],[-1,1]

for i=3,n do begin
	x = findgen(i) * ( 2 * !pi / i)
	erase
	polyfill,sin(x),cos(x)
	empty
	endfor
timer,'Polygon filling'

n = 512
a = findgen(n) * (8 * !pi / n)
c = bytscl(sin(a) # cos(a), top = !d.n_colors-1)
d = not c
erase
reset
for i=1,5 do begin
	tv,c
	empty
	tv,d
	empty
	endfor
timer,'Display 512 x 512 image, 10 times'
;for i=1,10 do begin
;	c = 0
;	c = tvrd(0,0,512,512)
;	endfor
;timer,'Read back 512 by 512 image, 10 times'

printf,lunno, float(total_time),'=Total Time, ', $
	exp(geom_time / ntest), '=Geometric mean,',ntest,' tests.'
if lunno gt 0 then free_lun,lunno
end



pro time_test, filename		;Run some time tests.
; filename = name of listing file or null for terminal output.
;
;+
; NAME:
;	TIME_TEST
;
; PURPOSE:
;	General purpose IDL benchmark program that performs
;	approximately 20 common operations and prints the time
;	required.
;
; CATEGORY:
;	Miscellaneous.
;
; CALLING SEQUENCE:
;	TIME_TEST [, Filename]
;
; OPTIONAL INPUTS:
;    Filename:	The string containing the name of output file for the 
;		results of the time test.
;
; OUTPUTS:
;	No explicit outputs.  Results of the test are printed to the screen 
;	or to a file.
;
; OPTIONAL OUTPUT PARAMETERS:
;	None.
;
; COMMON BLOCKS:
;	TIMER_COMMON
;
; SIDE EFFECTS:
;	Many operations are performed.  Files are written, etc.
;
; RESTRICTIONS:
;	Could be more complete, and could segregate integer, floating
;	point and file system IO times into separate figures.
;
; PROCEDURE:
;	Straightforward.
;	See also the procedure GRAPHICS_TEST, in this file, which
;	times a few of the common graphics operations.
;
;	We make no claim that these times are a fair or accurate
;	measure of computer performance.  In particular, different
;	versions of IDL were used.
;
;	Graphics performance varies greatly, depending largely on the
;	window system, or lack of thereof.
;	
;	Typical times obtained to date include:
; 	 (where	Comp.     = computational tests
; 		Graphics  = graphics tests
;		Geo. Avg. = geometric average)
;
; Machine / OS / Memory            Comp.   Geo. Avg.   Graphics Geo. Avg.
;
; MicroVAX II, VMS 5.1, 16MB        637     14.4        39.9    6.57
; MicroVAX II, Ultrix 3.0, 16MB     616     13.9        58.1    8.27
; Sun 3/110, SunOS 4.0, 12MB        391      8.19       32.0    7.81
; Sun 3/80, 12MB, 24 bit color      282      6.03       89.3   21.7
; PC 386 25MHz, 80387, MSDOS, 4MB   276      6.9        29.5    5.94
; Mips R2030, RISC/os 4.1, 8MB      246      3.67       14.6    2.62
; VAXStation 3100, VMS 5.1, 16MB    235      5.13       24.3    3.71
; HP 9000, Model 375, ?? ??         163      4.14       20.8    3.37
; DecStation 3100, UWS 2.1, 16MB    150      4.00       17.6    3.23
; 486 33mhz Clone, MS Windows, 8MB   70      1.81       12.9    3.00
; Sun 4/65, SunOS 4.1, 16MB          66      1.81        7.0    1.64
; Silicon Graphics 4D/25, ??         51      1.38       19.4    2.44
; Sun 4/50 IPX, 16MB		     40	     1.03	 7.7	0.80
; IBM 6000 Model 325 24MB            40      0.87        5.8    1.21
; HP 9000 / 720 48 MB                20      0.52        5.0    0.70
; SGI Indigo XS4000, 32MB	     20      0.46	 2.1    0.44
; SGI Indigo2, 150Mhz, 32MB	     16	     0.32        2.4    0.51
; DEC Alpha 3000/500, 224MB	     13      0.30        2.3    0.43
;
;
; MODIFICATION HISTORY:
;	DMS, 1986.
;
;	DMS, Revised July 1990,  Increased the size of arrays and the number
;		of repetitions to make most of the tests take longer.
;		This is to eliminate the effect of clock granularity
;		and to acknowledge that machines are becoming faster.
;		Many of the tests were made longer by a factor of 10.
;		
;-

common timer_common, time, lunno, total_time, geom_time, ntest

on_error,2                      ;Return to caller if an error occurs

do_floating = 1	;Do floating point array tests


if n_params() gt 0 then init_time,filename else init_time

;	Empty for loop
for i=0L, 999999l do begin & end

timer,'Empty For loop, 1 million times'

for i=1L,100000 do dummy, i
timer,'Call empty procedure (1 param) 100,000 times'

;	Add 100000 scalar ints:...
for i=0L,99999 do a=i+1
timer,'Add 100,000 integer scalars and store'

;	Scalar arithmetic loop:
for i=0L,25000 do begin
	a = i + i -2
	b = a / 2 + 1
	if b ne i then print,'You screwed up',i,a,b
	endfor
timer,'25,000 scalar loops each of 5 ops, 2 =, 1 if)'

a=replicate(2b,512,512)
reset
for i=1,10 do b=a*2b
timer,'Mult 512 by 512 byte by constant and store, 10 times'
for i=1,10 do c = shift(b,10,10)
timer,'Shift 512 by 512 byte and store, 10 times'
for i=1,10 do b=a+3b
timer,'Add constant to 512 x 512 byte array and store, 10 times'
for i=1,10 do b=a+b
timer,'Add two 512 by 512 byte images and store, 10 times'

if do_floating then begin
	a = float(a)
	reset
	for i=1,10 do b=a*2b
	timer,'Mult 512 by 512 floating by constant and store, 10 times'
	for i=1,10 do c = shift(b,10,10)	
	timer,'Add constant to 512 x 512 floating and store, 10 times'
	for i=1,10 do b=a+b
	timer,'Add two 512 by 512 floating images and store, 10 times'
	endif


a=randomu(qqq,100,100)	;Random number matrix
reset
b = invert(a)
timer,'Invert a 100 by 100 random matrix'

a=bindgen(256,256) & b=a
reset
for i=0,255 do for j=0,255 do b(j,i)=a(i,j)
timer,'Transpose 256 x 256 byte, FOR loops'
for i=0,255 do begin
	b(0,i) = transpose(a(i,*))
	end
timer,'Transpose 256 x 256 byte, row and column ops'
b=transpose(a)
timer,'Transpose 256 x 256 byte, transpose function'

a=findgen(100000)+1
c=a
b = a
reset
for i=0L,n_elements(a)-1 do b(i) = alog(a(i))
timer,'Log of 100,000 numbers, FOR loop'
b = alog(a)
timer,'Log of 100,000 numbers, vector ops'

for i=0L,n_elements(a)-1 do c(i)=a(i)+b(i)
timer,'Add two 100000 element floating vectors, FOR loop'

c=a+b
timer,'Add two 100000 element floating vectors, vector op'

a = findgen(65536L)
reset
b=fft(a,1)
timer,'65536 point real to complex FFT'

a=bytarr(512,512)
a(200:250,200:250)=10b
reset
b=smooth(a,5)
timer,'Smooth 512 by 512 byte array, 5x5 boxcar'

a=float(a)
reset
b=smooth(a,5)
timer,'Smooth 512 by 512 floating array, 5x5 boxcar'

a=bindgen(512,512)
aa =assoc(1,a)
reset
openw,1,'test.dat',512, initial = 5120	;Must be changed for vax
for i=1,10 do aa(0) = a
for i=1,10 do a=aa(0)
timer,'Write and read 10 512 by 512 byte arrays'
close,1

printf,lunno, float(total_time),'=Total Time, ', $
	exp(geom_time / ntest), '=Geometric mean,',ntest,' tests.'
;  Remove the data file
openw,2,'test.dat',/DELETE
close,2
if lunno gt 0 then free_lun,lunno
end
