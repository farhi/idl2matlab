; Written by J.R.D. Copley.  Last revision 08/01/01.
;************************************************************************************************
pro dcs_rawdata_variables_choose_handler,event
;************************************************************************************************
;
;
compile_opt strictarr
;
widget_control,event.top,get_uvalue=pstate
;
case event.id of
	(*pstate).none: begin
		for i=0,(*pstate).ntags-1 do widget_control,(*pstate).butid[i],set_button=0
		(*(*pstate).pbut)=intarr((*pstate).ntags)
	end
	(*pstate).all: begin
		for i=0,(*pstate).ntags-1 do widget_control,(*pstate).butid[i],set_button=1
		(*(*pstate).pbut)=intarr((*pstate).ntags)+1
	end
	(*pstate).save: begin
		z=(*(*pstate).pbut)
		filename=dialog_pickfile(path="C:\rsi\dave\dcs\text files",$
			filter="*.txt",title="Select name of save file",$
			file="ascii_variables.txt")
		if (file_test(filename) and filename ne "") then begin
			save,filename=filename,z
			result=dialog_message("Variable names saved to file "+filename,/information)
		endif	else print,"Invalid save file name."
	end
	(*pstate).recall: begin
		filename=dialog_pickfile(path="C:\rsi\dave\dcs\text files",$
			filter="*.txt",title="Select name of restore file",$
			file="ascii_variables.txt")
		print,filename
		if (file_test(filename) and filename ne "") then begin
			restore,filename=filename
			for i=0,(*pstate).ntags-1 do widget_control,(*pstate).butid[i],set_button=z[i]
				(*(*pstate).pbut)=z
			print,"Variable names restored from file ",filename
		endif else print,"Invalid restore file name."
	end
	(*pstate).done: begin
		widget_control,event.top,/destroy
		return
	end
	(*pstate).cancel: begin
		(*(*pstate).pbut)=intarr((*pstate).ntags)
		widget_control,event.top,/destroy
		return
	end
	(*pstate).make: begin
		(*(*pstate).pact).make=event.select
		widget_control,(*pstate).done,sensitive=(*(*pstate).pact).make or (*(*pstate).pact).summ
	end
	(*pstate).summ: begin
		(*(*pstate).pact).summ=event.select
		widget_control,(*pstate).bottom,sensitive=event.select
		widget_control,(*pstate).other,sensitive=event.select and (*pstate).obut
		widget_control,(*pstate).done,sensitive=(*(*pstate).pact).make or (*(*pstate).pact).summ
	end
	(*pstate).tab: *(*pstate).pdelim=string(format='(%"\t",a0)',"")
	(*pstate).com: *(*pstate).pdelim=","
	(*pstate).sem: *(*pstate).pdelim=";"
	(*pstate).spa: *(*pstate).pdelim=" "
	(*pstate).oth: begin
		(*pstate).obut=event.select
		widget_control,(*pstate).other,sensitive=(*pstate).obut
		widget_control,(*pstate).other,get_value=charact
		*(*pstate).pdelim=charact
	end
	(*pstate).other: begin
		widget_control,(*pstate).other,get_value=charact
		*(*pstate).pdelim=charact
	end
	else: begin
		k=where(event.id eq (*pstate).butid)
		(*(*pstate).pbut)[k]=event.select
	end
endcase
;
widget_control,event.top,get_uvalue=pstate
end


;************************************************************************************************
pro dcs_rawdata_variables_choose,tagnames,ntags,choice,action,delim
;************************************************************************************************
;
compile_opt strictarr
;
tlb=widget_base(/col,title='Click on variables to be written to output file.')
	upper=widget_base(tlb,/row,/frame)
		none=widget_button(upper,value="NONE")
		all=widget_button(upper,value="ALL")
		save=widget_button(upper,value="SAVE")
		recall=widget_button(upper,value="RECALL")
		done=widget_button(upper,value="DONE",sensitive=0)
		cancel=widget_button(upper,value="CANCEL")
	middle=widget_base(tlb,/row,/frame)
		ncols=5
		bbase=intarr(ncols)
		butid=intarr(ntags)
		for i=0,ncols-1 do bbase[i]=widget_base(middle,/col,/nonexclusive)
		ndisp=-1
		col=0
		for j=0,ntags-1 do begin
		  ndisp=ndisp+1
			if (ndisp ge 10) then begin
				ndisp=0
				col=col+1
			endif
		  butid[j]=widget_button(bbase[col],value=tagnames[j])
		endfor
	lower=widget_base(tlb,/row,/nonexclusive,/frame)
		make=widget_button(lower,value="MAKE INDIVIDUAL FILES")
		summ=widget_button(lower,value="MAKE SUMMARY FILE")
	bottom=widget_base(tlb,/col,/frame,sensitive=0)
		void=widget_label(bottom,$
			value="Select a field delimiter for the summary file.")
		bottom2=widget_base(bottom,/row)
			delims=widget_base(bottom2,/row,/exclusive)
				tab=widget_button(delims,value="Tab")
				com=widget_button(delims,value="Comma")
				sem=widget_button(delims,value="Semicolon")
				spa=widget_button(delims,value="Space")
				oth=widget_button(delims,value="Other-->")
			other=widget_text(bottom2,/editable,/all_events,xsize=2,ysize=1)
;
widget_control,tlb,/realize
widget_control,tab,/set_button
;
pbut=ptr_new(intarr(ntags))
pact=ptr_new({make:0,summ:0})
pdelim=ptr_new("")
state={none:none,all:all,done:done,save:save,recall:recall,cancel:cancel,$
	make:make,summ:summ,pact:pact,$
	bottom:bottom,tab:tab,com:com,sem:sem,spa:spa,oth:oth,other:other,pdelim:pdelim,$
	ntags:ntags,butid:butid,pbut:pbut,obut:0}
widget_control,tlb,set_uvalue=ptr_new(state),/no_copy
xmanager,'dcs_rawdata_variables_choose',tlb,event_handler='dcs_rawdata_variables_choose_handler'
choice=*pbut
ptr_free,pbut
action=*pact
ptr_free,pact
delim=*pdelim
ptr_free,pdelim
end


