; Written by J.R.D. Copley.  Last revision 08/01/01.
;************************************************************************************************
pro dcs_binary2ascii
;************************************************************************************************
; This procedure converts DCS data files from Octave binary to Octave ascii.
; The input and output files may be compressed or uncompressed.
; The user chooses which variables to place in the output files.
; Instead, or in addition, the procedure creates a summary file (in ascii),
; listing the values of selected variables for each of several runs.
;
compile_opt strictarr
;
; Define raw data structure and pointer.
dcs_rawdata_ptr,datafile_ptr=datafile_ptr
;
; Get tag names of structure.
tagnames=strlowcase(tag_names(*datafile_ptr))
ntags=n_tags(*datafile_ptr)
;
; Find out which variables are to be written to the output file(s),
; whether to create ascii output files and/or a summary file, and
; what delimiter to use for the summary file.
dcs_rawdata_variables_choose,tagnames,ntags,choice,action,delim
dummy=where(choice eq 1,count)
if (count eq 0) then begin
	result=dialog_message(["No variables chosen.","Exiting ..."])
	ptr_free,datafile_ptr
	return
endif
;
nofirstline=0
omithisto=0
printinfo=0
;
files=dialog_pickfile(path="f:\data",filter=["*.dcs","*.dcs.gz"],/multiple_files,$
	title="Select one or more raw data files")
files=files[sort(files)]
fileinfo=size(files)
nfiles=fileinfo[1]
if (files[0] eq "") then begin
	result=dialog_message(["No files selected.","Exiting ..."])
	return
endif
;
; Decide whether output files are to be compressed or not.
if (action.make) then begin
	result=dialog_message("Do you want compressed (gzip'd) output files ?",/question,/default_no)
	out_compressed=(result eq "Yes")
	if (out_compressed) then out_exten=".asc.gz" else out_exten=".asc"
endif
;
if (action.summ) then begin
	repeat begin
		summary_file=dialog_pickfile(path="f:\data",$
			filter="*.txt",title="Select name of summary file")
	endrep until (summary_file ne "" and strpos(summary_file,"\00_") eq -1)
	print,"Writing summary information to ",summary_file
	dcs_write_summary_ptr,summary_file,datafile_ptr=datafile_ptr,choice=choice,0,delim,""
endif
;
for i=0,nfiles-1 do begin
	file_path=files[i]
	compressed=(strmid(file_path,2,3,/reverse_offset) eq ".gz")
	if (compressed) then rootlength=strlen(file_path)-7 else rootlength=strlen(file_path)-4
	file_root=strmid(file_path,0,rootlength)
;
	dcs_read_binaryOctavefile_ptr,file_path,nofirstline,omithisto,variables_read,unknowns,error,$
		datafile_ptr=datafile_ptr
	print,"Binary file ",file_path," has been read."
;
	if (strlen(error) gt 0) then begin
		result=dialog_message(["Error reading binary file.","Exiting ..."])
		return
	endif
;
	if (action.make) then begin
		file_path=file_root+out_exten
		dcs_write_asciiOctavefile_ptr,file_path,datafile_ptr=datafile_ptr,$
			compress=out_compressed,choice=choice
		print,"Ascii file ",file_path," has been written."
	endif
;
	if (action.summ) then begin
		lgth=strlen(file_path)
		pos=strpos(file_path,'\',/reverse_search)+1
		short_form=strmid(file_path,pos,11)
		dcs_write_summary_ptr,summary_file,datafile_ptr=datafile_ptr,choice=choice,1,delim,$
			short_form
	endif
;
endfor
;
ptr_free,datafile_ptr
result=dialog_message(["Success!"],/information);
end