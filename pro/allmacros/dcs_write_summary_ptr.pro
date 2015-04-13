; Written by J.R.D. Copley.  Last revision 08/01/01.
;************************************************************************************************
pro dcs_write_summary_ptr,filename,datafile_ptr=datafile_ptr,choice=choice,icall,delim,short_form
;************************************************************************************************
;
compile_opt strictarr
;
; This procedure writes a delimited file summary.
;
if (filename eq "") then stop
;
on_ioerror, no_file
get_lun,unit
if (icall eq 0) then openw,unit,filename else openw,unit,filename,/append
;
nvars=n_tags(*datafile_ptr)
tagnames=tag_names(*datafile_ptr)
;
ivar_written=0
;
if (icall eq 0) then summary_line="File name" else summary_line=short_form
;
for ivar=0,nvars-1 do begin
	if (choice[ivar]) then begin
		ivar_written=ivar_written+1
		name=tagnames[ivar]
		data=(*datafile_ptr).(ivar)
		nsize=size(data)
		ns0=nsize[0]
		nst=nsize[ns0+1]
		n_el=n_elements(data)
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
		if (icall eq 0) then begin
			if (n_el eq 1) then summary_line=summary_line+delim+name else $
				for j=0,n_el-1 do summary_line=summary_line+delim+name+strcompress(j,/remove_all)
		endif else begin
			case dt of
				1: begin; write scalar
					summary_line=summary_line+delim+strcompress(string(format=((f)),data))
				end
				2: begin; write matrix
					for j=0,n_el-1 do summary_line=summary_line+delim+strcompress(string(format=((f)),data))
				end
				5: begin; write string
					summary_line=summary_line+delim+strcompress(data)
				end
				7: begin; write string array
					for j=0,n_el-1 do summary_line=summary_line+delim+strcompress(data[j])
				end
			endcase
		endelse
	endif
endfor
;
printf,unit,summary_line
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
