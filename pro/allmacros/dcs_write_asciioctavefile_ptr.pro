; Written by J.R.D. Copley.  Last revision 07/31/01.
;************************************************************************************************
pro dcs_write_asciiOctavefile_ptr,filename,datafile_ptr=datafile_ptr,compress=compressed,$
	choice=choice
;************************************************************************************************
;
compile_opt strictarr
;
; This is a procedure that writes an ascii Octave file.
;
if (filename eq "") then stop
;
on_ioerror, no_file
get_lun,unit
if (!VERSION.OS_FAMILY eq "Windows") then openw,unit,filename,/binary,/noautomode,compress=compressed
if (!VERSION.OS_FAMILY eq "unix") then openw,unit,filename,compress=compressed
;
nvars=n_tags(*datafile_ptr)
tagnames=tag_names(*datafile_ptr)
;
for ivar=0,nvars-1 do begin
	if (choice[ivar]) then begin
		name=tagnames[ivar]
		data=(*datafile_ptr).(ivar)
		nsize=size(data)
		ns0=nsize[0]
		nst=nsize[ns0+1]
		if (ns0 eq 0) then begin
			case nst of
				7: dt=5; string
				else: dt=1; scalar
			endcase
		endif
		if (ns0 eq 1) then begin
			case nst of
				7: dt=7; string array
				else: dt=2
			endcase
			if (dt eq 2) then begin & n1=nsize[1] & n2=1 & end
		endif
		if (ns0 eq 2) then begin
			case nst of
				7: dt=0
				else: dt=2
			endcase
			if (dt eq 2) then begin & n1=nsize[1] & n2=nsize[2] & end
		endif
;
		if (dt eq 0) then stop,"No good."
;
		printf,unit,"# name: "+name
		case dt of
			1: begin; write scalar
				printf,unit,"# type: scalar"
				printf,unit,data
			end
			2: begin; write matrix
				printf,unit,"# type: matrix"
				printf,unit,"# rows:"+strcompress(n1)
				printf,unit,"# columns:"+strcompress(n2)
				printf,unit,data
			end
			5: begin; write string
				nchars=strlen(data)
				printf,unit,"# type: string array"
				printf,unit,"# elements: 1"
				printf,unit,"# length:"+strcompress(nchars)
				printf,unit,data
			end
			7: begin; write string array
				printf,unit,"# type: string array"
				nstrgs=nsize[1]
				printf,unit,"# elements:"+strcompress(nstrgs)
				for j=0,nstrgs-1 do begin
					nchars=strlen(data[j])
					printf,unit,"# length:"+strcompress(nchars)
					printf,unit,data[j]
				endfor
			end
		endcase
	endif
endfor
;
free_lun,unit
return
;
no_file: begin
	free_lun,unit
	error="There is no file named "+filename
end
;
end
