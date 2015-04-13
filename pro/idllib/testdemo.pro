;
;	Main program to run a demonstration program.
;
; This IDL program reads a script file, showing each line that is executed
;	and waiting a fixed period of time after each statement is executed.
; The first character of each line has the following significance:
;	;  for comment, line is printed and there is no wait.
;	@  to execute a new file.
;	All others, statement to be executed.
; You can run it with a different delay (default = 1.0 seconds) by setting
;	demo$delay to the desired period.
;
demo$line  = ''			;null string for input strings
demo$msg = 'IO Error occured'	;assume error
if n_elements(demo$delay) eq 0 then demo$delay = 1.0 ;wait between statements

if n_elements(demo$file) eq 0 then begin ;Define the default script
	demo$file = filepath('testdemo.demo',subdirectory='lib')
	qq = findfile(demo$file, count=i)
	if i eq 0 then begin	;Cant find script.
		print,"Can't find the demo script ",demo$file
		read,'Enter script file path name: ',demo$file
		endif
	endif

if n_elements(demo$rept) eq 0 then demo$rept = 1	;Def Repetition count

if n_elements(demo$color) eq 0 then begin	;defining config?
	demo$imagedir = filepath('',subdir=['examples', 'data'])
	if n_elements(findfile(demo$imagedir + '*.dat')) le 2 then begin
		print,'Cant find images on: ',demo$imagedir
		read,'Enter directory containing images: ', demo$imagedir
		endif
	case !version.os of
		'vms':
		'Win32':
		'MacOS':
		else: demo$imagedir=demo$imagedir+'/'
	endcase
	if !d.name eq 'SUN' then window,col=240	;don't use all colors
	if !d.name eq 'X' then window
	demo$color = 1		;always do images
endif				;config

on_ioerror, io_err	;Error branch
get_lun, demo$lun	;get a unit for program file
t0 = systime(1)		;Beginning time
wait_time = 0		;Time spent waiting


for demo$irept = 1L,demo$rept do begin	;main loop
	demo$done = 0			;set to quit
	openr,demo$lun,demo$file,err=i	;open program file
	if i ne 0 then begin		;Cant find file
file_err:	print," "
		print,"Can't find file ",demo$file," in current directory."
		print,"You should 'cd' to the directory containing this file and then run idl."
		print," Which is usually ",$
                        filepath('', subdir='lib')
		stop
		endif
	while not (eof(demo$lun) or demo$done) do begin	;statement loop
		readf,demo$lun,demo$line	;read statement
			;print comments without delay
		demo$char = strmid(demo$line,0,1) ;get 1st char
		case demo$char of
';':		 print,demo$line	;just print comments
'@':		 begin			;open new file
		  close,demo$lun
		  demo$file = strmid(demo$line,1,100) ;New file name
		  ofile = demo$file	;orig name
		  junk = findfile(demo$file,count=i)
			;Assume in library if not found
		  if i eq 0 then demo$file = filepath(demo$file, subdir='lib')
		  junk = findfile(demo$file,count=i)
		  if i eq 0 then begin
			print,"Cant find script file ",ofile
			read,"Enter pathname of script file ",demo$file
			endif
		  openr,demo$lun,demo$file,err=i
		  if i ne 0 then goto, file_err
		  endcase
else:		 begin		;execute statement
		  print, demo$line
		  demo$istat = execute(demo$line) ;execute it
		  wait, demo$delay	;allow it to sink in
		  wait_time = wait_time + 1
		  endcase
		endcase			
		endwhile		;eof
	close, demo$lun			;done with file
endfor			;execute loop

demo$msg = 'Done.'
t0 = fix(systime(1) - t0)	;Total time
wait_time = fix(wait_time * demo$delay)
print,'Times: Elapsed =',t0,', Waiting =',wait_time,', Executing =',$
	t0-wait_time
io_err:	stop, demo$msg
end
