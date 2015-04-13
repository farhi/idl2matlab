; Written by J.R.D. Copley.  Last revision 07/29/01.
;************************************************************************************************
pro dcs_read_binaryOctavefile_ptr_done_reading,unit,ivar,ivaru,variables_read,unknowns
;************************************************************************************************	;
;
compile_opt strictarr
;
if (ivar ge 0) then variables_read=variables_read[0:ivar] else variables_read=["None"]
if (ivaru ge 0) then unknowns=unknowns[0:ivaru] else begin
	unknowns=["None"]
endelse
;
free_lun,unit
return
end


;************************************************************************************************
pro dcs_read_binaryOctavefile_ptr,filepath,nofirstline,omithisto,variables_read,unknowns,error,$
	datafile_ptr=datafile_ptr
;************************************************************************************************
;
compile_opt strictarr
;
; This is a procedure that reads a binary Octave file.
; If omithisto=0, nothing is omitted, everything is read.
; If omithisto=1, histohigh is read but histodata is omitted.
; If omithisto=2, histohigh and histodata are both omitted.
;
on_ioerror, no_file
get_lun,unit
if (filepath eq "") then begin
	filepath="h:\octave\octave.bin"
	filepath="f:\data\20000318_01.dcs"
endif
;print,"Reading from ",filepath
;
; Determine whether or not file is compressed.
ending=strmid(filepath,2,3,/reverse_offset)
compressed=(ending eq ".gz")
;
; Read header to determine byte ordering ("endianness")
openr,unit,filepath,compress=compressed
header=bytarr(11)
readu,unit,header
close,unit
free_lun,unit
;
case string(header[9]) of
	"B": openr,unit,filepath,/swap_endian,compress=compressed
	"L": openr,unit,filepath,compress=compressed
	else: begin
		error="Cannot determine byte ordering of file named "+filepath
		return
	end
endcase
;
readu,unit,header
;print,"Header line: ",string(header)
;
; Define the types of some variables
nlgth=0l & dldocgf=bytarr(5) & dt=0b & padding=0b & scalar=0.0d
n1=0l & n2=0l & nstrgs=0l & nchars=0l
;
variables_read=strarr(100)
unknowns=strarr(100)
error=""
ivar=-1
ivaru=-1
;
while (not eof(unit)) do begin
	readu,unit,nlgth; read name_length
	name=bytarr(nlgth) & readu,unit,name & name=string(name); read name
	if ((omithisto eq 2 and name eq "histohigh") or $
		(omithisto eq 1 and name eq "histodata")) then begin
		dcs_read_binaryOctavefile_ptr_done_reading,unit,ivar,ivaru,variables_read,unknowns
		return
	endif
;	print,"Variable name: ",name
	readu,unit,dldocgf; read 5 bytes of doc_length, doc, and global flag
	readu,unit,dt; read data type
	case dt of
		1: begin; read scalar
			readu,unit,padding
			readu,unit,scalar
;			print,"Scalar read: ",scalar
				case name of
					"badbrd": (*datafile_ptr).badbrd=float(scalar)
					"baddet": (*datafile_ptr).baddet=float(scalar)
					"ch_ms": (*datafile_ptr).ch_ms=float(scalar)
					"ch_res": (*datafile_ptr).ch_res=fix(scalar)
					"ch_srdenom": (*datafile_ptr).ch_srdenom=fix(scalar)
					"ch_srmode": (*datafile_ptr).ch_srmode=fix(scalar)
					"ch_wl": (*datafile_ptr).ch_wl=float(scalar)
					"coll_amp": (*datafile_ptr).coll_amp=float(scalar)
					"coll_mean": (*datafile_ptr).coll_mean=float(scalar)
					"coll_osc": (*datafile_ptr).coll_osc=fix(scalar)
					"datamax": (*datafile_ptr).datamax=float(scalar)
					"det_dis": (*datafile_ptr).det_dis=float(scalar)
					"duration": (*datafile_ptr).duration=float(scalar)
					"highmax": (*datafile_ptr).highmax=float(scalar)
					"nframes": (*datafile_ptr).nframes=long(scalar)
					"temp_setpoint": (*datafile_ptr).temp_setpoint=float(scalar)
					"tsdmin": (*datafile_ptr).tsdmin=float(scalar)
					else: begin & ivaru=ivaru+1 & unknowns[ivaru]=name & end
				endcase
				ivar=ivar+1
				variables_read[ivar]=name
		end
		2: begin; read matrix
			readu,unit,n1,n2
			nhi=n1>n2
			nlo=n1<n2
			if (nlo eq 1) then data=dblarr(nhi) else data=dblarr(n1,n2)
			readu,unit,padding
			readu,unit,data
				case name of
					"ch_bid" : (*datafile_ptr).ch_bid=float(data)
					"ch_delay": (*datafile_ptr).ch_delay=float(data)
					"ch_dis": (*datafile_ptr).ch_dis=float(data)
					"ch_input": (*datafile_ptr).ch_input=float(data)
					"ch_phase": (*datafile_ptr).ch_phase=float(data)
					"ch_slots": (*datafile_ptr).ch_slots=float(data)
					"detsum": (*datafile_ptr).detsum=ulong(data)
					"fc_dis":  (*datafile_ptr).fc_dis=float(data)
					"grandsum": (*datafile_ptr).grandsum=ulong(data)
					"histodata": (*datafile_ptr).histodata=float(data)
					"histohigh": (*datafile_ptr).histohigh=float(data)
					"motor_pos": (*datafile_ptr).motor_pos=float(data)
					"repeats": (*datafile_ptr).repeats=ulong(data)
					"resets": (*datafile_ptr).resets=ulong(data)
	;				"sample_envi": (*datafile_ptr).sample_envi=float(data)
	;				"sample_geom": (*datafile_ptr).sample_geom=float(data)
					"tchanlook": (*datafile_ptr).tchanlook=ulong(data)
					"temp_control": begin
						(*datafile_ptr).temp_control=-1.0
						(*datafile_ptr).temp_control=float(data)
					end
					"temp_sample": begin
						(*datafile_ptr).temp_sample=-1.0
						(*datafile_ptr).temp_sample=float(data)
					end
					"timsum": (*datafile_ptr).timsum=ulong(data)
					"totals": (*datafile_ptr).totals=ulong(data)
					else: begin & ivaru=ivaru+1 & unknowns[ivaru]=name & end
				endcase
				ivar=ivar+1
				variables_read[ivar]=name
		end
		5: begin; read string
			readu,unit,nchars; read number of characters in string element
			if (nchars gt 0) then begin
				str=bytarr(nchars) & readu,unit,str; read string element
				str=string(str)
				case name of
					"command": (*datafile_ptr).command=str
					"comments": (*datafile_ptr).comments=str
					"sample_desc": (*datafile_ptr).sample_desc=str
					"start_date": (*datafile_ptr).start_date=str
					"startchoice": (*datafile_ptr).startchoice=str
					"stop_date": (*datafile_ptr).stop_date=str
					else: begin & ivaru=ivaru+1 & unknowns[ivaru]=name & end
				endcase
				ivar=ivar+1
				variables_read[ivar]=name
			endif
		end
		7: begin; read string array
			readu,unit,nstrgs; read number of strings in array
			strarray=strarr(nstrgs)
			for j=1,nstrgs do begin
				readu,unit,nchars; read number of characters in string element
				str=bytarr(nchars) & readu,unit,str; read string element
				strarray[j-1]=strjoin(string(str)); store, joined, in strarray
			endfor
			case name of
				"highsource": (*datafile_ptr).highsource=strarray
				else: begin & ivaru=ivaru+1 & unknowns[ivaru]=name & end
			endcase
			ivar=ivar+1
			variables_read[ivar]=name
		end
		else: error="N.B. An unsupported variable type has been encountered: "+strcompress(dt)
	endcase
endwhile

;print,"Reading completed."
dcs_read_binaryOctavefile_ptr_done_reading,unit,ivar,ivaru,variables_read,unknowns
return
;
no_file: free_lun,unit
	error="There is no file named "+filepath
end