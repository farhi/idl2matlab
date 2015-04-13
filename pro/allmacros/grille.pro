;written by Floris Andre, August 2001; modifications by Roland May

;*******************************************************************************
;*******************************************************************************
pro time_display
;*******************************************************************************
;*******************************************************************************
common nb,n
common tablo,tabb
common total,ti,m,ti2,tx,txx,time_result_string
common totaltime,ttime			;to calculate total time

;print,"new version of time_display"

tx1=long(0)
tx2=long(0)
tx3=long(0)
a=0
b=0
m=0
ti=0
for i=1,n[0] do begin
  if tabb[i+500] ne 'x' then begin
    if tabb[i+300] eq 't' then begin
      a=tabb[i+400]
      for j=1,a do begin
	ti=ti+float(strtrim(tabb[i+200],2))
	; print,"ti: ",ti
      endfor
    endif
    ti2=0.
    if tabb[i+300] eq 'm' then begin
      if strlen(tabb[i+200]) ne 0 then begin
        l=float(tabb[1099])
        b=tabb[i+400]
	for j=1,b do begin
	  m=m+float(tabb[i+200])
	  arg=0.30449/l-0.017049-0.00011906*l+0.00033876*l^2
	  ti2=float(m)*float(arg)
	  ; print,"ti2: ",ti2
;	  ti2=long(ti2)
	endfor
      endif
    endif
  endif
endfor
tx=ti+ti2
;------------------ TIME FORMAT H/MIN/S -----------------------------------
tx1=tx/3600
tx1=strtrim(long(tx1),2)
tx=tx mod 3600

tx2=tx/60
tx2=strtrim(long(tx2),2)
tx=tx mod 60
tx3=strtrim(long(tx),2)
;--------------------- DISPLAY TIME OF EXPERIMENT ------------------------------
th=string(' h ')
tm=string(' min ')
ts=string(' s ')
if tx1 eq 0 then begin
  tx1=string(format='(A1)','')
  th=string(format='(A1)','')
endif 
if tx2 eq 0 then begin
  tx2=string(format='(A1)','')
  tm=string(format='(A1)','')
endif 
if tx eq 0 then begin
  tx=string(format='(A1)','')
  ts=string(format='(A1)','')
endif 
time_result_string=string('estimated time: ')+string(tx1) $
+string(th)+string(tx2)+string(tm)+string(tx3)+string(ts) $
+string('(without instrument settings)')

end


;*******************************************************************************
;*******************************************************************************
pro grille_event,ev
;*******************************************************************************
;*******************************************************************************

; Tabb   1-100: sample position  in experiment list
; Tabb 101-200: sample name		"
; Tabb 201-300: preset			"
; Tabb 301-400: t/m			"
; Tabb 401-500: # of repetitions	"
; Tabb 501-600: save/nosave		"

; modification (physical sample list is maintained in a separate list):
; Tabb 601-700: sample list in rack

widget_control,ev.id,get_uvalue=u

common tablo,tabb
common tabla,tabu
common nb,n
common bases,pre,t,rep,sav,pos
common sa,sam

common erreur,error2,error3
;------ var for repetition of a line -----------------------

common rrr,indicedeb,ind
common tablo2,tabb2
common stopit,stop
stop=1
;-----------------------------------------------------------
common nofile,exist_file,ex


;pos=strarr(100)
;sam=strarr(100)
;pre=strarr(100)
;t  =strarr(100)
;rep=strarr(100)
;sav=strarr(100)


;-------- condition for checking fields ----------------------------------------
ok=1
ok2=1
;-------------------------------------------------------------------------------

;--------------- to calculate duration of experiment ---------------------------
common total,ti,m,ti2,tx,txx,time_result_string
common totaltime,ttime

;-------------------------------------------------------------------------------
;++++++++++++++++++++ VALUE OF WAVELENGTH FOR AN EXISTING FILE +++++++++++++++++

;common check,chec
;if chec eq 1 and ex eq 0 then begin
;tabb[1099]=8
;endif
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; keep old values for position, samples, etc.
for i=1,700 do begin
  tabb2[i]=strtrim(tabb[i],2)
endfor
	
if u(0) lt 1000 then begin

;-------- POS EVENT: a position has changed; 
;-------- replace the sample name by the name corresponding to this position
;---------------------------------------------------------------------
  if u(0) gt 0 and u(0) le 100 then begin
	;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	for j=1,n[0] do begin
	  ; print, "n[0]: ",n[0],"  - pos(j): ",pos(j)
	  widget_control,pos(j),get_value=ech
	  tabb[j]=strtrim(ech[0],2)
	  j1=fix(tabb(j))
	  j2=fix(tabb2(j))
	  if j2 ne j1 then begin
	    ; print,'# ',j,' position has changed from ',j2,' to ',j1
	    widget_control,pos(j),set_value=tabb(j)		;position
	    ; print,'sample in position ',j1,' is [',tabb[j1+600],']'
	    if tabb[j1+600] ne '' then begin
	      widget_control,sam(j),set_value=tabb[j1+600]	;par sub
	    endif else begin
	      widget_control,sam(j),set_value=''		;par sub
	    endelse
	    tabb[j+100]=tabb[j1+600]				;par sub
	  endif
	endfor
	;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	cpt=0
	for i=1,n[0]-1 do begin
	  i1=i+1
	  for j=i1,n[0] do begin
	    if fix(tabb[i]) eq fix(tabb[j]) then begin
		ind=i
		cpt=cpt+1
	    endif
	  endfor
	endfor
;	print, 'ok: ', ok,' - cpt: ',cpt
 	if ok eq 1 then begin
	  if cpt ge 1 then begin
;--------------------------------- ERROR ---------------------------------------
	    err2='*** warning: position '
	    widget_control,error2,set_value=string(err2)$
	     +string(tabb[ind])+string(' is repeated ***')
	  endif else begin
	    widget_control,error2,set_value=''
	  endelse
;-------------------------------------------------------------------------------
	  ok=0
	endif
  endif


;-------- SAMPLE EVENT: a sample title has changed; 
;-------- replace the sample title in the sample list and change the sample 
;-------- title in all occurrences of the same position
if u(0) gt 100 and u(0) le 200 then begin
	ok=1
	for j=1,n[0] do begin
	  widget_control,pos(j),get_value=ech
	  tabb[j]=strtrim(ech,2)
	  ind1=fix(tabb[j])				
	  widget_control,sam(j),get_value=ech
	  tabb[j+100]=strtrim(ech,2)					
	  tabb[ind1+600]=strtrim(ech,2)					
	  snew=tabb(j+100)
	  sold=tabb2(j+100)
	  if snew ne sold then begin
	    ; print,'sample in position ',tabb[j],' has changed from [',$
	    ;  sold,'] to [',snew,']'
	    tabb[600+ind1]=snew
	    for i=1,n[0] do begin
	      widget_control,pos(i),get_value=ech
	      tabb[i]=strtrim(ech,2)
	      ind2=fix(tabb[i])
	      ; print, 'i: ',j,' - ind1: ',ind1,' - ind2: ',ind2
	      if ind1 eq ind2 then begin				
	        ; print,'sample in position ',ind2,' is [',snew,']'
	        widget_control,sam(i),set_value=tabb(600+ind1)
	        tabb[600+ind2]=snew
	        tabb(i+100)=snew
	      endif
	    endfor
	  endif
	endfor

	cpt=0
	for i=1,n[0]-1 do begin
	  i1=i+1
	  for j=i1,n[0] do begin
	    if tabb[100+i] eq tabb[100+j] then begin
		ind=i
		if tabb[i] ne tabb [j] then begin
		  cpt=cpt+1
		endif
	    endif
	  endfor
	endfor
;print, 'ok: ', ok,' - cpt: ',cpt
 	if ok eq 1 then begin
	  if cpt ge 1 then begin
;--------------------------------- ERROR ---------------------------------------
	    err2='*** warning: sample '
	    widget_control,error2,set_value=string(err2)$
	     +string(tabb[100+ind])+string(' is repeated ***')
	  endif else begin
	    widget_control,error2,set_value=''
	  endelse
;-------------------------------------------------------------------------------
	  ok=0
	endif
endif	


;-------- PRE EVENT (to calculate time at every change in the grid)
	;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if u(0) gt 200 and u(0) le 600 then begin
	for j=1,n[0] do begin
;		widget_control,pos(j),get_value=ech
;		tabb[j]=strtrim(ech,2)			
;		widget_control,sam(j),get_value=ech
;		tabb[j+100]=strtrim(ech,2)			
		widget_control,pre(j),get_value=ech
		tabb[j+200]=strtrim(ech,2)				
		widget_control,t(j),get_value=ech
		tabb[j+300]=strtrim(ech,2)
		widget_control,rep(j),get_value=ech
		tabb[j+400]=strtrim(ech,2)
 		widget_control,sav(j),get_value=ech
		tabb[j+500]=strtrim(ech,2)
	endfor
	;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	time_display
	widget_control,u(2),set_value=time_result_string
  endif

endif else begin
	;-------------------------------------

	;--------- close button ---------------
	if u(0) eq 1000 then begin
	  widget_control,ev.top,/destroy
	endif
	
	;--------- new button ----------------
	if u(0) eq 1001 then begin
	  widget_control,ev.top,/destroy
	  grille

	;--------------- REMOVE VALUES IN THE SAMPLE AND PRESET FIELDS ---------
	  for i=1,n[0] do begin
	    ech=string(format='(A1)','')
	    ech=strtrim(ech,2)
	    widget_control,sam(i),set_value=ech
	    tabb[i+100]=ech
	    widget_control,pre(i),set_value=ech
	    tabb[i+200]=ech
	  endfor
	;-----------------------------------------------------------------------
	endif


	;------------------ add line button ------------------------------------
	if u(0) eq 1008 then begin
		b=string(format='(I2)',strtrim(n,2))
		bb=fix(b)

		;------ if n < 100 (nb max of samples) -----------
		if b lt 100 then begin
		;-------------------------------------------
			common newl,newline
			common newbut,base182
			newline=1
	;------------------ to keep value in memory -------------------	
			for i=1,n[0] do begin
				widget_control,u(i+1),get_value=ech
				tabb[i]=strtrim(ech,2)
				; pos[i]=strtrim(ech,2)
			endfor
			for i=101,100+n[0] do begin
				widget_control,u(i+1),get_value=ech
				tabb[i]=strtrim(ech,2)
				; sam[i-100]=strtrim(ech,2)
			endfor

			for i=201,200+n[0] do begin
				widget_control,u(i+1),get_value=ech
				tabb[i]=strtrim(ech,2)
				; pre[i-200]=strtrim(ech,2)
			endfor	
	
			for i=301,300+n[0] do begin
				widget_control,u(i+1),get_value=ech
				tabb[i]=strtrim(ech,2)	
				; t[i-300]=strtrim(ech,2)
			endfor
			
			for i=401,400+n[0] do begin
				widget_control,u(i+1),get_value=ech
				tabb[i]=strtrim(ech,2)
				; rep[i-400]=strtrim(ech,2)
			endfor

			for i=501,500+n[0] do begin
				widget_control,u(i+1),get_value=ech
				tabb[i]=strtrim(ech,2)	
				; sav[i-500]=strtrim(ech,2)
			endfor

			; for i=1,n[0] do begin
			;	ind=tabb(i)
			;	print, 'tabb[', ind,']: ',tabb(600+ind)
			; endfor

			widget_control,ev.top,/destroy
			n=n[0]+1
			tabb[1080]=n
		
			e=string(format='(I2)',n)
			e=strtrim(e,2)
			; print, "n: ",n, " - e: ", e
			none=string(format='(A1)','')

			grille

			; for i=1,n[0] do begin
			;	ind=tabb(i)
			;	print, 'tabb[', ind,']: ',tabb(600+ind)
			; endfor

			widget_control,pos(n),set_value=none
			widget_control,sam(n),set_value=string(none)
			widget_control,pre(n),set_value=string(none)
			widget_control,t(n),set_value='t'
			widget_control,rep(n),set_value='1'
			widget_control,sav(n),set_value='s'
	;----------- INITIALISATION ------------
			for i=1,700 do begin
			  tabb2[i]=tabb[i]
			endfor
			tabb[100+n[0]]='nonenonenone'
	;---------------------------------------
		endif else begin
			widget_control,base182,sensitive=0
		endelse
	endif

	;------------------ delete line button ---------------------------------
	
	if u(0) eq 1009 then begin
	
		b=string(format='(I2)',strtrim(n,2))
		bb=fix(b)
		
		;------ if n > 2 (1 = min nb of samples) -----------
		if b gt 1 then begin
		;---------------------------------------------------


		;------------------ to keep value in memory -------------------	
		
		for i=1,n[0] do begin
		  widget_control,u(i+1),get_value=ech
		  tabb[i]=strtrim(ech,2)
		  ; pos[i]=strtrim(ech,2)
		endfor
		
		for i=101,100+n[0] do begin
		  widget_control,u(i+1),get_value=ech
		  tabb[i]=strtrim(ech,2)
	  	  ind=fix(tabb[i-100])
		  ; sam[i-100]=strtrim(ech,2)
	  	  tabb[ind+600]=strtrim(ech,2)					
		endfor

		for i=201,200+n[0] do begin
		  widget_control,u(i+1),get_value=ech
		  tabb[i]=strtrim(ech,2)
		  ; pre[i-200]=strtrim(ech,2)
		endfor	
	
		for i=301,300+n[0] do begin
		  widget_control,u(i+1),get_value=ech
		  tabb[i]=strtrim(ech,2)	
		  ; t[i-300]=strtrim(ech,2)
		endfor
			
		for i=401,400+n[0] do begin
		  widget_control,u(i+1),get_value=ech
		  tabb[i]=strtrim(ech,2)
		  ; rep[i-400]=strtrim(ech,2)
		endfor

		for i=501,500+n[0] do begin
		  widget_control,u(i+1),get_value=ech
		  tabb[i]=strtrim(ech,2)	
		  ; sav[i-500]=strtrim(ech,2)
		endfor
		;--------------------------------------------------------------
		
		widget_control,ev.top,/destroy
		n=n-1
		tabb[1080]=n

		grille

		for i=1,n[0] do begin
			widget_control,pos(i),set_value=string(tabb[i])
; 2 lines inserted RPM
			ind=fix(tabb(i))
			widget_control,sam(i),set_value=string(tabb[ind+600])
			widget_control,pre(i),set_value=string(tabb[i+200])
			widget_control,t(i),set_value=string(tabb[i+300])
			widget_control,rep(i),set_value=string(tabb[i+400])
			widget_control,sav(i),set_value=string(tabb[i+500])
		endfor
		
		endif else begin

;--------------------------------- ERROR ---------------------------------------

			err2='*** cannot delete the last line ***'
			widget_control,error2,set_value=string(err2)
;-------------------------------------------------------------------------------
		endelse
		
	endif


;------------------ wavelength -------------------------------------------
	
	if u(0) eq 1010 then begin
		widget_control,u(1),get_value=l
		l=strtrim(strmid(l,0,4),2)
		widget_control,u(1),set_value=string(l)
		tabb[1099]=strtrim(l,2)
		for i=1,n[0] do begin
			widget_control,u(i+2),get_value=ech
			tabb[i+200]=strtrim(ech,2)
		endfor
		for i=101,n[0]+100 do begin
			widget_control,u(i+2),get_value=ech
			tabb[i+300-100]=strtrim(ech,2)
		endfor
		for i=201,n[0]+200 do begin
			widget_control,u(i+2),get_value=ech
			tabb[i+400-200]=strtrim(ech,2)
		endfor
		;for i=501,500+n[0] do begin
		;  widget_control,u(i+1),get_value=ech
		;  tabb[i]=strtrim(ech,2)	
		;  sav[i-500]=strtrim(ech,2)
		;endfor

	time_display			
	widget_control,u(302),set_value=time_result_string

;-------------------------------------------------------------------------------
		print,'l= ',l
	endif


;----------------- BUTTON =PRE1 FOR PRESET (PUT THE VALUE OF PRE1 EVERYWHERE) --
	
	if u(0) eq 1004 then begin
		widget_control,u(3),get_value=ech
		tabb[1+200]=strtrim(ech,2)
			
		
		if ev.select eq 1 then begin
			
			for i=1,n[0] do begin
			widget_control,pre(i),set_value=string(ech)
			tabb[i+200]=strtrim(ech,2)
			endfor

			for i=101,n[0]+100 do begin
			widget_control,u(i+2),get_value=string(ech)
			tabb[i+300-100]=strtrim(ech,2)
			endfor


			for i=201,n[0]+200 do begin
			widget_control,u(i+2),get_value=string(ech)
			tabb[i+400-200]=strtrim(ech,2)
			endfor

	time_display
	widget_control,u(302),set_value=time_result_string
	
;-------------------------------------------------------------------------------
	
	
;++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	
		endif
	endif






;----------------- BUTTON =T/M1 FOR T/M (PUT THE VALUE OF T/M1 EVERYWHERE)------
	
	if u(0) eq 1005 then begin
		widget_control,u(3),get_value=ech
		tabb[1+300]=strtrim(ech,2)
			
		
		if ev.select eq 1 then begin
			for i=1,n[0] do begin
				widget_control,t(i),set_value=string(ech)
				tabb[i+300]=strtrim(ech,2)
			endfor

			time_display
			widget_control,u(102),set_value=time_result_string
		endif 
	endif
;-------------------------------------------------------------------------------


;----------------- BUTTON =REP1 FOR REP (PUT THE VALUE OF REP1 EVERYWHERE)------
	if u(0) eq 1006 then begin
		widget_control,u(3),get_value=ech
		tabb[1+400]=ech
		if ev.select eq 1 then begin
			for i=1,n[0] do begin
				widget_control,rep(i),set_value=string(ech)
				tabb[i+400]=ech
			endfor
		endif 
	  	time_display
;		widget_control,u(102),set_value=time_result_string
	endif


;----------------- BUTTON =S/N1 FOR S/N (PUT THE VALUE OF S/N1 EVERYWHERE) -----
	if u(0) eq 1007 then begin
	  widget_control,u(3),get_value=ech
	  tabb[1+500]=strtrim(ech,2)
	  if ev.select eq 1 then begin
	    for i=1,n[0] do begin
	      widget_control,sav(i),set_value=string(ech)
	      tabb[i+500]=strtrim(ech,2)
	    endfor
	  endif 

	  time_display
	  widget_control,u(102),set_value=time_result_string

	endif

;-------------------------------------------------------------------------------
;------------------------ CREATION OF COMMAND FILE -----------------------------
	if u(0) eq 1003 then begin
		widget_control,error2,set_value=''
		print, "n[0]: ", n[0]
		for i=1,n[0] do begin
			widget_control,u(i+1),get_value=ech
			tabb[i]=strtrim(ech,2)
			; print, "tabb[",i,"]: ",tabb[i]
		endfor
		for i=101,100+n[0] do begin
			widget_control,u(i+1),get_value=ech
			tabb[i]=strtrim(ech,2)
			; print, "tabb[",i,"]: ",tabb[i]
		endfor
		for i=201,200+n[0] do begin
			widget_control,u(i+1),get_value=ech
			ech=strtrim(ech,2)
			if strlen(ech[0]) gt 0 then begin
				tabb[i]=strtrim(ech,2)
			endif else begin
				ok=0
;--------------------------------- ERROR ---------------------------------------
			err2='*** no value for the preset field for position '
			widget_control,error2,set_value=string(err2) $
			+string(tabb[i-200])+string(' ***')
;-------------------------------------------------------------------------------
			endelse
			; print, "tabb[",i,"]: ",tabb[i]
		endfor	
		for i=301,300+n[0] do begin
			widget_control,u(i+1),get_value=ech
			if ech[0] eq 't' or ech[0] eq 'm' then begin
				tabb[i]=strtrim(ech,2)
			endif else begin
				ok=0
;--------------------------------- ERROR ---------------------------------------
			err2='*** wrong value for the t/m field for position '
			widget_control,error2,set_value=string(err2) $
			+string(tabb[i-300])+string(' ***')
;-------------------------------------------------------------------------------
			endelse
			; print, "tabb[",i,"]: ",tabb[i]
		endfor
		for i=401,400+n[0] do begin
			widget_control,u(i+1),get_value=ech
			tabb[i]=strtrim(ech,2)
			; print, "tabb[",i,"]: ",tabb[i]
		endfor
		for i=501,500+n[0] do begin
			widget_control,u(i+1),get_value=ech
			if ech[0] eq 's' then begin
				tabb[i]=strtrim(ech,2)
			endif else if ech[0] eq 'n' then begin
				tabb[i]=strtrim(ech,2)
			endif else if ech[0] eq 'x' then begin
				tabb[i]=strtrim(ech,2)
				err2='*** warning: line '+strtrim(string(i),2) $
				+' cancelled ***'
				widget_control,error2,set_value=string(err2)
			endif else begin
				ok=0
;--------------------------------- ERROR ---------------------------------------
			err2='*** wrong value for the s/n field for position '
			widget_control,error2,set_value=string(err2) $
			+string(tabb[i-500])+string(' ***')
;-------------------------------------------------------------------------------
			endelse
			; print, "tabb[",i,"]: ",tabb[i]
		endfor
		for i=1,n[0] do begin
			widget_control,u(i+1),get_value=ech
			ind=fix(strtrim(ech[0],2))
			if ind le 100 and ind gt 0 then begin
				tabb[i]=ind
			endif else begin
				ok=0
;--------------------------------- ERROR ---------------------------------------
				err2='*** position'
				err3=' is out of range ***' 
				widget_control,error2,set_value=string(err2) $
				+string(ech[0])+string(err3)
;-------------------------------------------------------------------------------
			endelse
		endfor
		for i=1,n[0] do begin
			widget_control,u(i+1),get_value=ech
			tabb[i]=ech
			cpt=0
			for j=1,n[0] do begin
				a=strtrim(tabb[i],2)
				b=strtrim(tabb[j],2)
				aa=fix(a)
				bb=fix(b)
				if aa eq bb then begin
					cpt=cpt+1
				endif		
			endfor
			cpt=strtrim(cpt,2)
			if (cpt gt 1) and (ok eq 1) then begin
			;--------------------------------- ERROR ---------------
				err2='*** warning: position '
				widget_control,error2,set_value=string(err2) $
				+string(tabb[i])+string(' is repeated ***')
			;-------------------------------------------------------
				ok2=0
			endif
		endfor
		for i=101,100+n[0] do begin
			widget_control,u(i+1),get_value=ech
			tabb[i]=strtrim(ech,2)
			cpt=0
			for j=1,n[0] do begin
				a3=strtrim(tabb[i],2)
				b3=strtrim(tabb[j],2)
				if a3 eq b3 then begin
					cpt=cpt+1
				endif		
			endfor
			cpt=strtrim(cpt,2)
			if cpt ge 2 and ok eq 1 then begin
			;--------------------------------- ERROR ---------------
			err2='*** warning: position '
			widget_control,error2,set_value=string(err2) $
			+string(tabb[i])+string(' is repeated ***')
			;-------------------------------------------------------
				ok2=0
			endif
		endfor
;-------------------------------------------------------------------------------
		if ok eq 1 then begin
	
;------------------------- TOTAL TIME AND MONITOR (ADD EVERY TIME t AND m) -----
		time_display
;-------------------------------------------------------------------------------

			if ok2 eq 1 then begin			
;---------------------- ERROR --------------------------------------------------
				err2=' '
				widget_control,error2,set_value=string(err2)
				err3=' '
;-------------------------------------------------------------------------------
			endif


;----------------- VALUE OF DATE, TITLE, USER, PROP, LOG BOOK ------------------

			para =DialNewValue(TYPE='t_para',NAME='changer_file')

			user_n=string(para.c_user[0])
			for i=1,9 do begin
			  user_n=user_n+string(para.c_user[i])
			endfor

			prop_n=string(para.proposal_number[0])
			for i=1,7 do begin
			  prop_n=prop_n+string(para.proposal_number[i])
			endfor
	print,para.log_book_n[0], '   ',string(para.log_book_n[0])
;	print,para.log_book_n[1], '   ',string(para.log_book_n[1])

		book_n=string(para.log_book_n[0])+string(para.log_book_n[1])$
				+string(para.log_book_n[2])

			date_n=string(para.exp_start_time[0])
			for i=1,10 do begin
			  date_n=date_n+string(para.exp_start_time[i])
			endfor

			title_n=string(para.c_txt[0])
			for i=1,38 do begin
			title_n=title_n+string(para.c_txt[i])
			endfor
	
			tabb[1022]=user_n  
			tabb[1021]=date_n
			tabb[1023]=prop_n
			tabb[1026]=book_n
			tabb[1020]=title_n

;	print,user_n  
;	print,"date_n: [",date_n,"]"
;	print,prop_n
;	print,book_n
;	print,title_n
;-------------------------------------------------------------------------------


;----------------------------- CALL THE INFO PROCEDURE -------------------------
			info
;-------------------------------------------------------------------------------

		endif
	endif
endelse

end




;*******************************************************************************
;*******************************************************************************
pro grille
;*******************************************************************************
;*******************************************************************************
common nb,n
common tablo,tabb
common tabla,tabu
common nofile,exist_file,ex

common bases,pre,t,rep,sav,pos
common sa,sam

common erreur,error2,error3
common check,chec
common newl,newline
common newbut,base182
common total,ti,m,ti2,tx,txx,time_result_string
common totaltime,ttime			;to calculate total time
common top,changer_based0

pos=strarr(100)
sam=strarr(100)
pre=strarr(100)
t  =strarr(100)
rep=strarr(100)
sav=strarr(100)
;---------------------------------------- BASE CREATION  -----------------------

base0=widget_base(group_leader=changer_based0,title='Command File (4)',/column)

base01=widget_label(base0,frame='5',$
  value='enter all fields - carriage return after every modification')
;base02=widget_label(base0,value='**************')
base00=widget_base(base0,/column)
common errr,err2

;----------------- ERROR -------------------
	if exist_file eq 1 then begin
		err2=string(format='(A80)','')
	endif
	error2=widget_label(base00,value=err2)
	err3=string(format='(I3)','')
;-------------------------------------------


base1=widget_base(base00, /row)
base10=widget_base(base1,/column)
lab0=widget_label(base10,frame=4,value='nb')
base11=widget_base(base1,/column)
pos0=widget_label(base11,frame=4,value='pos')
base12=widget_base(base1,/column)
sam0=widget_label(base12,frame=4,value='samples')
base15=widget_base(base1,/column)
base151=widget_base(base15,/column)
base152=widget_base(base15,/align_center)
pre00=widget_label(base151,frame=4,value='preset')

base13=widget_base(base1,/column)
base131=widget_base(base13,/column)
base132=widget_base(base13,/align_center)

tm00=widget_label(base131,frame=4,value='t/m')

base16=widget_base(base1,/column)
base161=widget_base(base16,/column)
base162=widget_base(base16,/align_center)
rep00=widget_label(base161,frame=4,value='rep')

base17=widget_base(base1,/column)
base171=widget_base(base17,/column)
base172=widget_base(base17,/align_center)
sav00=widget_label(base171,frame=4,value='s/n/x')

base18=widget_base(base1,/column)
base181=widget_base(base18,/column)
base182=widget_base(base18,/column)

base19=widget_base(base0,/column)

n=tabb[1080]
for i=1,n[0] do begin
	txt1=string(format='(I2)',i)
	txt1=strtrim(txt1,2)
	indice=txt1
	txt=string(format='(I2)',tabb[i])
	txt=strtrim(txt,2)
	lab=widget_text(base10,value=indice,xsize=2)
	pos(i)=widget_text(base11,xsize=2,value=txt)
	widget_control,pos(i),/editable
	sam(i)=widget_text(base12,xsize=20,value=tabb(i+100))	
	widget_control,sam(i),/editable
	pre(i)=widget_text(base151,xsize=7)	
	widget_control,pre(i),/editable
	t(i)=widget_text(base131,xsize=1,value='t')
	widget_control,t(i),/editable
	rep(i)=widget_text(base161,xsize=3,value='1')	
	widget_control,rep(i),/editable
	sav(i)=widget_text(base171,xsize=3,value='s')	
	widget_control,sav(i),/editable
endfor

lab11=widget_label(base11,value='')
lab12=widget_label(base12,value='use 1st value for column:')

pre0=widget_button(base152,ysize=20,value='  ')
t0  =widget_button(base132,ysize=20,value='  ')
rep0=widget_button(base162,ysize=20,value='  ')
sav0=widget_button(base172,ysize=20,value='  ')


but14=widget_button(base18,value='add new line')
but15=widget_button(base18,value='delete last')
but13=widget_button(base18,frame=10,value='command file')
but12=widget_button(base18,value='new')
but11=widget_button(base18,value='close')


lambda=cw_field(base19, $
title='enter wavelength for estimation of time (in case of monitor preset): ', $
	xsize=4,value='8',/return_events)

tx=string(format='(I40)','')
tx=strtrim(tx,2)
ttime=widget_label(base19,xsize=800,value=tx)


widget_control,base0,/realize
	
	
;-------------------------------------------------------------------------------
;------------------------------------------ EVENTS -----------------------------


for i=1,n[0] do begin
	txt=string(format='(I2)',i)
	widget_control,pos(i),set_uvalue=[i,pos(i),ttime]
	widget_control,sam(i),set_uvalue=[i+100,sam(i),ttime]
	widget_control,pre(i),set_uvalue=[i+200,pre(i),ttime]
	widget_control,t(i)  ,set_uvalue=[i+300,t(i),ttime]
	widget_control,rep(i),set_uvalue=[i+400,rep(i),ttime]
	widget_control,sav(i),set_uvalue=[i+500,sav(i),ttime]
endfor

widget_control,pre0 ,set_uvalue=[1004,pre0,pre,t,rep,ttime]
widget_control,t0   ,set_uvalue=[1005,t0,t,ttime]
widget_control,rep0 ,set_uvalue=[1006,rep0,rep,ttime]
widget_control,sav0 ,set_uvalue=[1007,sav0,sav,ttime]

widget_control,but11,set_uvalue=[1000,but11,ttime]
widget_control,but12,set_uvalue=[1001,but12,ttime]

widget_control,error2,set_uvalue=[1002,error2]

;--------------------------  with a saved file ---------------
;print,'check',chec

if chec eq 1 then begin

	for i=1,n[0] do begin
		ind=fix(tabb[i])
		ech=tabb[ind+600]
		widget_control,sam(i),set_value=string(ech)
;		tabb[i+600]=strtrim(ech,2)
		widget_control,pos(i),set_value=string(tabb[i])
		widget_control,pre(i),set_value=string(tabb[i+200])
		widget_control,t(i)  ,set_value=string(tabb[i+300])
		widget_control,rep(i),set_value=string(tabb[i+400])
		widget_control,sav(i),set_value=string(tabb[i+500])
	endfor

;------------------- with a new file to create --------------
endif else begin
	print,'n= ',n
	for i=1,n[0] do begin
		;widget_control,pos(i),set_value=string(tabb[i])
		;print,'pos= ',pos(i)
		ind=tabb[i]
;		widget_control,sam(i),set_value=string(tabb[i+100])
		widget_control,sam(i),set_value=string(tabb[ind+600])
		if newline eq 1 then begin
			widget_control,pos(i),set_value=string(tabb[i])
			widget_control,pre(i),set_value=string(tabb[i+200])
			widget_control,t(i)  ,set_value=string(tabb[i+300])
			widget_control,rep(i),set_value=string(tabb[i+400])
			widget_control,sav(i),set_value=string(tabb[i+500])
		endif
	endfor
endelse


;------------- to add a line to the grid ---------------------
widget_control,but14,set_uvalue=[1008,pos,sam,pre,t,rep,sav,ttime]
;-------------------------------------------------------------

;------------- to delete last line of the grid ------------------
widget_control,but15,set_uvalue=[1009,pos,sam,pre,t,rep,sav,ttime]
;-------------------------------------------------------------

widget_control,but13,set_uvalue=[1003,pos,sam,pre,t,rep,sav,ttime]

;-------------------- wavelength -----------------------------
widget_control,lambda,set_uvalue=[1010,lambda,pre,t,rep,ttime]
widget_control,lambda,get_value=l
tabb[1099]=strtrim(l,2)
;-------------------------------------------------------------

;-------------------------- ERROR ------------------------------
if exist_file eq 1 then begin
	err2=' '
	widget_control,error2,set_value=string(err2)
	err3=' '
endif
;---------------------------------------------------------------

;--------------------- CALCULATE DURATION OF EXPERIMENT -------------
time_display
widget_control,ttime,set_value=time_result_string


;----------------------------------------------------------------

xmanager,'grille',base0,/just_reg,group_leader=changer_based0

end
