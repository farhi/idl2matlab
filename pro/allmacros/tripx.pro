;
;		********************
;		***  ROMS  ************
;		****************************************
;		an IDL routines library file
;		Written by JOUFFREY Romuald 1996
;
;
;
; nb2str			Convert a number to its string form
;
;+
; NAME:
;       WACCU
;
; PURPOSE:
;       This function add two spectra, using 
;       
;
; CATEGORY:
;       .
;
; CALLING SEQUENCE:
;       ??? waccu, xa, za, ea, na, OPER='+', xs, zs, es, ns
;
; INPUTS:
;       XA:      X data.
;       WA:      Y data.
;       EA:      Error data.
;       NA:      Normalisation vector.
;
;       WB:      An N-element vector of type: float, or double.
;
; KEYWORD PARAMETERS:
;       OPER: 	If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
;
; EXAMPLE:
;       Define an accumulator (a).
;         a = [ 2.0,  1.0,  1.0]
;
;       And operande (b).
;         b = [3.0, 10.0, -5.0]
;
;       Compute  a = a + b.
;
; PROCEDURE:
;       WACCU.PRO uses data and 
;       to calculate a new scan
;
; REFERENCE:
;       Filing (Alain Bouvet)
;
;
; MODIFICATION HISTORY:
;-


;**************************************************************************************
PRO combine_accu, 	x_a, w_a, e_a, n_a, $
			x_tolerance, file_factor, global_monitor, combine, $
			x_s, w_s, e_s, n_s ;***
;**************************************************************************************
; Apply file_factor on accu and data within the tolerance
; Thanks to Alain Bouvet

x_out = [0.0] ; first value will will be discarded before return
w_out = [0.0]
e_out = [0.0]
n_out = [0.0]
index_out = 0

nb_elem_a =  N_ELEMENTS(x_a)
nb_elem_s =  N_ELEMENTS(x_s)
index_a = 0
index_s = 0

WHILE (index_a LT nb_elem_a) OR (index_s LT nb_elem_s) DO BEGIN		
    IF (index_a GE nb_elem_a) THEN BEGIN
	x_diff = x_tolerance + 1	; force copy s in out
    ENDIF ELSE IF (index_s GE nb_elem_s) THEN BEGIN
	x_diff = x_tolerance - 1	; force copy a in out
    ENDIF ELSE $
	x_diff = x_a(index_a) - x_s(index_s)	; combine
    IF (x_diff LT -x_tolerance) THEN BEGIN
	IF NOT(combine) THEN BEGIN
	    ; point from a is included in the out data
	    x_out = [x_out, x_a(index_a)]
	    w_out = [w_out, w_a(index_a)]
	    e_out = [e_out, e_a(index_a)]
	    n_out = [n_out, n_a(index_a)]	
	    index_out = index_out + 1
	ENDIF ELSE BEGIN
	    ; point from a is discarded
	ENDELSE		
	index_a = index_a + 1			
    ENDIF ELSE IF (x_diff GT x_tolerance) THEN BEGIN		
	IF NOT(combine) THEN BEGIN
		; point from a is included in the out data
		x_out = [x_out, x_s(index_s)]
		w_out = [w_out, w_s(index_s)]
		e_out = [e_out, e_s(index_s)]
		n_out = [n_out, n_s(index_s)]
		index_out = index_out + 1
	END ELSE BEGIN
		; Do nothing (point from s is discarded)
	ENDELSE
	index_s=index_s+1
    ENDIF ELSE BEGIN
	; x_a and x_s current point are combined
	x_out = [x_out,(x_a(index_a)+x_s(index_s))/2]
	index_out = index_out + 1
	IF NOT(combine) THEN BEGIN	    ; ADD
	    ; -------------------- add wa and ws
	    new_w = w_a(index_a) +  w_s(index_s)
	    w_out = [w_out, new_w]
	    ; -------------------- compute new error
	    e_out = [e_out, SQRT(new_w)]
	    ; -------------------- compute new monitor
	    n_out = [n_out, n_a(index_a)+n_s(index_s)]
	END ELSE BEGIN		   ; COMBINE
	    ; -------------------- apply file_factor
	    new_w=((w_a(index_a)/n_a(index_a))+ $
		    (file_factor*w_s(index_s))/ $
		    n_s(index_s))*global_monitor
	    w_out=[w_out, new_w]
	    ; -------------------- compute new error
	    e_out = [e_out, SQRT( $
		     ((e_a(index_a)*e_a(index_a))/ $
		     (n_a(index_a)*n_a(index_a)))+ $
		     ABS(file_factor)* $
		     ((e_s(index_s)*e_s(index_s))/ $
		     (n_s(index_s)*n_s(index_s)))) $
		     *global_monitor]

	    ; -------------------- compute new monitor
	    n_out=[n_out, global_monitor]
	ENDELSE
	index_a=index_a+1
	index_s=index_s+1
    ENDELSE
ENDWHILE

; return results in accumulator
; We must discard the first elememt because of our trick on IDL array
IF index_out GE 1 THEN BEGIN
	x_a = x_out(1:*)
	w_a = w_out(1:*)
	e_a = e_out(1:*)
	n_a = n_out(1:*)
END ;
END ; do_cl_on_accu

;******************************************************************************
FUNCTION nb2str, number; *************************************************
;******************************************************************************
; This function return the string of a number expression
; Leading and/or trailing spaces are removed.

RETURN, STRTRIM(STRING(number),2)
END

;
;		********************
;		***  PickFile  ************
;		****************************************
;		a ...
;		Written by JOUFFREY Romuald and Yannick Raoul
;		January 1996
;		(Hoping this Help...)
;
; V1.0b 05/02/96

;****************************** Procedures
; pick_fil_event		Handles pickfile events
; pick_fil			Create the pickfile base

;******************************************************************************
FUNCTION pick_fil_event, event ;***********************************************
;******************************************************************************
; This procedure parses the pick_fil events
; IN : the event generated by pick_fil interface
; OUT : Event are redistributed to tripx event (Search next Event_Func)

WIDGET_CONTROL, bad_id=i, event.id, Get_Uvalue=uv
CASE uv(3) OF
    1 : BEGIN			    ; Update File List
	WIDGET_CONTROL, bad_id=i, uv(8), Get_UValue=file_list
	WIDGET_CONTROL, bad_id=i, uv(4), Get_Value=path
	WIDGET_CONTROL, bad_id=i, uv(5), Get_Value=filter, HourGlass=1
	WIDGET_CONTROL, bad_id=i, uv(6), Set_Value=['','',' Searching ...']
	WIDGET_CONTROL, bad_id=i, uv(7), Set_Value=''
	path=STRTRIM(path(0),2)
	cd, current=mee
	IF path EQ '' THEN path=mee
	WIDGET_CONTROL, bad_id=i, uv(4), Set_Value=path, Set_Text_Select=[STRLEN(path), 0]
	n_files=0
	stat=0 & catch,stat
	IF stat ne 0 THEN BEGIN catch,/cancel     & cd,mee
				print,!err_string & n_files=0
	ENDIF ELSE BEGIN
	  pp=strpos(path,'::')       & pth=''
	  if pp lt 0 then cd,path else pth=path
	  filter=STRTRIM(filter(0),2)
	  IF STRLEN(filter) gt 0 THEN file_string=filter ELSE file_string=''
	  file_list=FINDFILE(pth+file_string,Count=n_files)
	  if pp lt 0 then cd,mee,current=path else path=strupcase(path)
	 ;Put a slash to the end of the path if its not there
	  sep=sys_dep('DIVIDER')
	  IF STRPOS(path,sep,STRLEN(path)-1) lt 0 THEN path=path+sep
	  lenpath=STRLEN(path)
	ENDELSE
	IF n_files GT 0 THEN BEGIN
	    pos= STRPOS(file_list(0), path)
	    IF pos GE 0 THEN file_list=STRMID(file_list,lenpath+pos,50)
	    WIDGET_CONTROL, bad_id=i, uv(6), Set_Value=file_list
	    nn=N_ELEMENTS(file_list)-5 & IF nn LT 0 THEN nn=0
	    WIDGET_CONTROL, bad_id=i, uv(6), Set_List_Top=nn, Set_List_Select=nn
	    WIDGET_CONTROL, bad_id=i, uv(7), Set_Value=file_list(nn)
	ENDIF ELSE $
	WIDGET_CONTROL, bad_id=i, uv(6), Set_Value= $
	    ['Sorry : No file found', '   Change path','  and try again'] 	
	WIDGET_CONTROL, bad_id=i, uv(8), Set_UValue=file_list
	RETURN, {ID: event.id, TOP: event.top, HANDLER:0L, DATA: 'path', PFO: path}
	END
    2 : BEGIN			    ; Get files
    	WIDGET_CONTROL, bad_id=i, uv(5), Get_UValue=file_list
	filename=file_list(event.index)
	WIDGET_CONTROL, bad_id=i, uv(4), Set_Value=filename
	IF event.clicks eq 2 THEN ret_value=filename(0) ELSE ret_value=''
	RETURN, {ID: event.id, TOP: event.top, HANDLER:0L, DATA: ret_value,PFO: ''}	    
	END
    3 : BEGIN			    ; Get files
	WIDGET_CONTROL, bad_id=i, event.id,Get_Value=filename
	WIDGET_CONTROL, bad_id=i, uv(4),Get_Value=path
	path=STRTRIM(path(0),2)
	cd,current=mee
	IF path EQ '' THEN path=mee
        ret_value=''
	stat=0 & catch,stat
	IF stat ne 0 THEN BEGIN catch,/cancel     & cd,mee
				print,!err_string & nf=0
	ENDIF ELSE BEGIN
	  pp=strpos(path,'::')       & pth=''
	  if pp lt 0 then cd,path else pth=path
	  name=filename(0)
	  res=FINDFILE(pth+name, Count=nf)
	  if pp lt 0 then cd,mee,current=path else path=strupcase(path)
	  sep=sys_dep('DIVIDER')
	  IF STRPOS(path,sep,STRLEN(path)-1) lt 0 THEN path=path+sep
	  IF nf GT 0 THEN BEGIN
	    	pos= STRPOS(res(0), path)
	    	IF pos GE 0 THEN res(0)=STRMID(res(0),STRLEN(path)+pos,50)
		ret_value=res(0)
	  ENDIF
	ENDELSE
	RETURN, {ID: event.id, TOP: event.top, HANDLER:0L, DATA: ret_value,PFO: path} 
	END
    ELSE : see, 'Erreur dans le parsing de pick_fil_event'
    ENDCASE
END

;******************************************************************************
PRO pick_fil, pickbase, path, filter, file_list, file, owner, pfw_upd_but ;****
;******************************************************************************
; This procedure creates the pick_fil interface
; The parameters are defaults values,  owner is the WID of father interface
; Other variables are self_explanatory

@lamp.cbk
widebase    = WIDGET_BASE   (pickbase, /Column, /Frame) 
pfw_path    = WIDGET_TEXT   (widebase, Font=ft_propor, Value=path, /Edit, XSize=25) 
filtbase    = WIDGET_BASE   (pickbase, /Row) 
filtlbl	    = WIDGET_LABEL  (filtbase, Font=ft_propor, Value= "filter:") 
pfw_filter  = WIDGET_TEXT   (filtbase, Font=ft_propor, Value= filter, /edit, xs = 10) 
pfw_upd_but = WIDGET_BUTTON (filtbase, Font=ft_propor, Value= "Update")
selections  = WIDGET_BASE   (pickbase, /Row, /Frame) 
fls	    = WIDGET_BASE   (selections, /Column, /Frame)
pfw_list    = WIDGET_LIST   (fls     , Font=ft_propor, Value=file_list, XSize =20, YSize = 6)
pfw_txt	    = WIDGET_TEXT   (fls     , Font=ft_propor, Value=file, XSize=25,/Edit, $
			     UValue=[-88, owner, 2, 3]) 

WIDGET_CONTROL, bad_id=i, pfw_path,   SET_Uvalue=[-88, owner, 2, 1, $
				pfw_path, pfw_filter, pfw_list, pfw_txt, pickbase]
WIDGET_CONTROL, bad_id=i, pfw_filter, SET_Uvalue=[-88, owner, 2, 1, $
				pfw_path, pfw_filter, pfw_list, pfw_txt, pickbase]
WIDGET_CONTROL, bad_id=i, pfw_upd_but,SET_Uvalue=[-88, owner, 2, 1, $
				pfw_path, pfw_filter, pfw_list, pfw_txt, pickbase]
WIDGET_CONTROL, bad_id=i, pfw_list,   SET_Uvalue=[-88, owner, 2, 2, pfw_txt, pickbase, pfw_path]
WIDGET_CONTROL, bad_id=i, pfw_txt ,   SET_Uvalue=[-88, owner, 2, 3, pfw_path, pickbase], $
			    Set_Text_Select=[1,STRLEN(file)]
WIDGET_CONTROL, bad_id=i, pfw_path, Set_Text_Select=[STRLEN(path), 0]

	; The line below make pick_fil events handled locally
WIDGET_CONTROL, bad_id=i, pickbase, Event_Func='pick_fil_event'
	; The line below stores the file list into pickbase UValue
WIDGET_CONTROL, bad_id=i, pickbase, Set_UValue=file_list
RETURN
END

;******************************************************************************
PRO see, par1,par2,par3,par4,par5,par6,par7,par8,par9 ;************************
;******************************************************************************
;+
; NAME:
;       see
;
; PURPOSE:
;       To print parameters
;
; CATEGORY:
;       .
;
; CALLING SEQUENCE:
;	see, p1, p2, ....., p9
; INPUTS:
;	None
;
; KEYWORD PARAMETERS:
;	None
;
; EXAMPLE:
;	See, 'x=',x,'y=',y
;
; PROCEDURE:
;       None 
;
; REFERENCE:
;       None
;
; MODIFICATION HISTORY:
;-

i=N_PARAMS() & str=''
IF i le 0 or i gt 9 THEN BEGIN
    par1='Incorrect number of parameters in see'
    par2=STRING(7B) & i=2
ENDIF
FOR j=1,i DO BEGIN
    str=str+',par'+STRTRIM(STRING(j),2)
ENDFOR
str='print'+str
res=EXECUTE(str)
END

;******************************************************************************
PRO waccu, xa, wa, ea, na, oper, xs, ws, es, ns, x_tol, n_factor, monitor
;******************************************************************************
; The calculation factor used in tripx
; x|w|e|n followed by a are accumulator values 
; x|w|e|n followed by s are loaded spectrum values
; oper is operator code,  x_tol is the x_tolerance value
; n_factor is the factor value (used for combine operation only)
; monitor is the user defined value

;Check for consistencies
IF N_ELEMENTS(oper) LE 0 THEN RETURN
s=SIZE(xs)		    ; Eventualy make a x vector
IF s(0) EQ 0 THEN ns = MAKE_ARRAY(N_ELEMENTS(xs), Value=ns, /Float)

; xa and xs must have the same ordering
; We suppose xa (accumulator) already in ascending order and
; we do a sort on ws

sorted_xs = sort(xs)
xs = xs(sorted_xs)
ws = ws(sorted_xs)
es = es(sorted_xs)
ns = ns(sorted_xs)

IF oper EQ 's' THEN BEGIN	; If operator eq 'S' THEN store xs into xa
    xa  =	xs
    wa  =	ws
    ea  =	es
    na  =	ns
ENDIF ELSE BEGIN
    s=SIZE(na)		    ; Eventualy make a normalisation vector
    IF s(0) EQ 0 THEN na = MAKE_ARRAY(N_ELEMENTS(xa), Value=na, /Float)
    
    ; Operation
    CASE oper OF
	'+'	:   BEGIN
		    combine=0
		END
	'c'	:   BEGIN
		    combine=1
		END
	ELSE:   BEGIN
		END
    ENDCASE
    combine_accu,   xa, wa, ea, na, $
		    x_tol, n_factor, monitor, combine, $
		    xs, ws, es, ns
ENDELSE
END


;
;		********************
;		***  Tripx_face  ************
;		****************************************
;		a specifically designed tool for 3-axis group at ILL
;		Written by JOUFFREY Romuald and Yannick Raoul
;		Based upon tripx_face.pro 12/22/95 version
;		written by Don Kearley (ILL)
;		January 1996
;		(Hoping this Help...)
;
; V0.01b 05/02/96
;
; ************ Because Tripx use LAMP variables and functions ***********
; **************       It cannot be run alone		      ***********
;
;****************************** Tripx common definition procedure
; tx_commons			defines tripx common(s)
;****************************** Non tripx dependent procedure
; see				print procedure
;****************************** Functions
; tx_bufferize			To bufferize accumulators
; tx_calculate			perform calculation
; tx_extract			extract
; tx_get_buffer			To get a bufferized accu
; tx_read			gets tripple axis scan
;****************************** Procedures
; tx_accumulate			Called when an accu has to be bufferized
; tx_build_but			Buttons construction procedure
; tx_build_par			Parameters construction procedure
; tx_calculate_ranges		perform ranges calculation
; tx_data_process		preprocess data before plotting
; tx_end			called when quitting (save history)
; tx_export			Export procedure (to another w or a file)
; tx_grant_w			Fetch W values
; tx_help			define tripx help text
; tx_initialize			Variables initialisation
; tx_plot			Plot procedure
; tx_remove_points		Remove points from a W
; tx_reset_buffers		Called to initalize accumulator
; tx_save			Saving to a file routine
; tx_send_msg			Send a message in relevant widget label
; tx_send_err_msg		Beep + send_msg (Error)
; tx_store_parameters		Keep parameters for future saving
; tx_unscript			
; tx_update_w
; tx_update_ranges		Update values of ranges widgets
; tripx_event			Parsing procedure for tripx_face events
; ***************************** Widgets Creating Procedure
; tripx				Create tripx Interface
; ***************************** Little explanation on variables
; - Local variables are normaly named  			(ex. idx)
; - Each tripx shared variable begins with tx_ 		(ex. tx_parameters)
; - txw_ stands for tripx Widgets identifier variables	(ex. txw_base)
;
; Variables are ALWAYS lowercase, IDL Instructions are ALWAYS uppercase
; Keywords in IDL instructions are ALWAYS Capitalized.
; If you try this, you keep it ... very helpfull in reading ...
;
;******************************************************************************
PRO tx_commons ;***************************************************************
;******************************************************************************
; This procedure was created to define all common blocks for the
; tripx program.

COMMON txc_main,    txw_base	, $	; Widget id of tripx
		    txw_base1	, $	; Widget id of right tripx base
		    txw_but_0   , $	; Widget id of buttons interface top
		    txw_but_base, $	; Widget id of buttons interface base
		    txw_dp_txt	, $	; Widget id for general purpose text dump
		    txw_filttxt	, $	; Text widget id of file filter text
		    txw_pathtxt	, $	; Text widget id of file path text
		    txw_filelist, $	; Text widget id of filelist text
		    txw_xbut	, $	; Widget id array of X buttons
		    txw_wbut	, $	; Widget id array of Z buttons
		    txw_msg	, $	; Widget id of tripx message label
		    txw_pickbase, $	; Widget id of pickfile base
		    txw_ltslider, $	; Widget id of left slider
		    txw_rtslider, $	; Widget id of right slider
		    txw_slid_lab, $	; Widget id of slider label
		    txw_history	, $	; Widget id of history text
		    txw_degree	, $	; Widget id of temperature value
		    txw_unit_mev, $	; Widget id of mev unit button
		    txw_unit_thz, $	; Widget id of thz unit button
		    txw_rg_but	, $	; Widget id of range button
		    txw_basec	, $	; Widget id of calculation fcts
		    txw_xtol	, $	; Widget id of x tolerance field
		    txw_mon_txt	, $	; Widget id of monitor parameter
		    tx_rg_uv	, $	; UV of range button
		    tx_path	, $	; path to read files
		    tx_rwnb	, $	; number of workspace to read data
		    tx_awnb	, $	; number of accumulator workspace
		    tx_old_codes, $	; String for old data labels
		    tx_codes	, $	; String array for data labels
		    tx_old_eg_txt,$	; String for old eg labels
		    tx_eg_txt	, $	; String array for eg labels
		    tx_xsiz	, $	; Float array for x size saving
		    tx_ysiz	, $	; Float array for y size saving
		    tx_wsiz	, $	; Float array for w size saving
		    tx_nsiz	, $	; Float array for n size saving
		    tx_files	, $	; String array for filenames
		    tx_ytit	, $	; String y title
		    tx_wk_s	, $	; Workspace number in string format
		    tx_xsel	, $	; Number of selected for x axis
		    tx_ysel	, $	; Number of selected for y axis
		    tx_wsel	, $	; Flag set if a data is selected
		    tx_nsel	, $	; Flag set if amonitor is selected
		    tx_numor	, $	; String array for numor
		    tx_history	, $	; String array for history
		    tx_cur_buf	, $	; Number of current displayed buffer
		    tx_loaded	, $	; Boolean (correct data has been loaded)
		    tx_unit	, $	; String for current unit 
		    tx_xtol	, $	; X tolerance value (%)
		    tx_xrange	, $	; Xrange Array 
		    tx_yrange	, $	; YRange Array
		    tx_monitor	, $	; monitor value
		    tx_factor	, $	; factor value
		    tx_par_str	, $	; parameter structure
		    tx_par_txt	, $	; parameter text
		    tx_small_history	; compiled history
		    
COMMON txc_buffer,  tx_last_buffer, $	; Last bufferized accus
		    tx_buffers		; Array of Handle storing structures

COMMON txc_wd_id,   txw_d2	,$	; Widget id of plot left area
		    txw_d1		; Widget id of plot left area

COMMON txc_pb,	    tx_data	, $	; 
		    tx_old_data		;

RETURN
END

;******************************************************************************
FUNCTION tx_bufferize, position, x, y, w, n, e, p ;****************************
;******************************************************************************
COMMON txc_buffer			; Retrieve buffers common block 
; Store dataset (x, y, w, n, e, p) in buffer number 'position'
; Discard any data already stored after nb

CASE 1 OF
    (position LE 0)		    : RETURN, -1 ; Wrong position
    (position GT tx_last_buffer+1)  : RETURN, -2
ELSE				    : BEGIN & END
ENDCASE

IF (position LE tx_last_buffer) THEN BEGIN			
    FOR index=position, tx_last_buffer DO BEGIN	; Forget data stored after this position
	buffer_handle=tx_buffers(index)
	IF (buffer_handle NE 0) THEN $
	    Handle_Free, tx_buffers(index)
    ENDFOR
    tx_buffers=tx_buffers(0:position-1)
    tx_last_buffer=position-1
ENDIF

buffer = {X:x, Y:y, W:w, N:n, E:e, PAR:p}
buffer_handle=HANDLE_CREATE(VALUE=buffer, /NO_COPY)
tx_buffers = [tx_buffers, buffer_handle]
tx_last_buffer = position
RETURN, 0
END

;******************************************************************************
FUNCTION tx_calculate, accu_nb, oper, read_nb, $    ;**************************
	 x_tol, str, txw_basec, factor, monitor	    ;**************************
;******************************************************************************
; All purpose calculation function
; return 0 if no error occured
; return -1 if no data available

tx_grant_w, accu_nb, xa, ya, wa, na, ea, tit, xtit, ytit, SubTit
tx_grant_w, read_nb, xs, ys, ws, ns, es, tit, xtit, ytit, SubTit

IF N_ELEMENTS(xs) GE 2 THEN BEGIN
    steps=xs(1:*)-xs(0:(N_ELEMENTS(xs)-2))		    ; Differences x vector
    xtol=FLOAT(TOTAL(steps)/N_ELEMENTS(steps)*x_tol/100) ; Calculate diff average
    xtol=xtol(0)
ENDIF ELSE xtol=0.0
waccu, xa, wa, ea, na, oper, xs, ws, es, ns, xtol, factor, monitor
IF oper EQ 'c' THEN WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=0
tx_update_w, accu_nb, xa, ya, wa, na, ea
tx_accumulate, accu_nb, str
RETURN, 0
END

;******************************************************************************
FUNCTION tx_extract, partxt, line_tag, var_tag, value ;************************
;******************************************************************************
; partxt is an array of lines
;

result = -1 ; line_tag not found
nb_line = N_ELEMENTS(partxt)

line_tag = line_tag + ':'
current_line = 0
Done = 0
REPEAT BEGIN
    line = partxt(current_line)	
    line_tag_pos = STRPOS(line, line_tag)	
    IF (line_tag_pos GT -1) THEN BEGIN
	result = -2 ; line_tag found (looking for var_tag)
	IF (var_tag NE '') THEN BEGIN		
	    var_tag_pos = STRPOS(line, var_tag, line_tag_pos+6)			
	    IF (var_tag_pos GT -1) THEN BEGIN
		result = 0
		Done = 1
		value_begin = STRPOS(line, '=', var_tag_pos+1) + 1			
		value_end = STRPOS(line, ',', value_begin)
		IF (value_end EQ -1) THEN BEGIN
			value_end = strlen(line)
		ENDIF
		value = STRMID(line, value_begin, value_end-value_begin )
	    ENDIF	
	END ELSE BEGIN
		result = 0
		Done = 1
		value = STRMID(line, line_tag_pos+7, strlen(line)-line_tag_pos+7)
	ENDELSE
    ENDIF	
    
    current_line = current_line+1 ; look farther 
    IF (current_line EQ nb_line) THEN Done=1	
ENDREP UNTIL Done
; remove trailing = if exists
IF STRMID(value, STRLEN(value)-1, 1) EQ '=' THEN $
    value=STRMID(value, 0, STRLEN(value)-1)
RETURN, result
END

;******************************************************************************
FUNCTION tx_get_buffer, position, accu_nb ;************************************
;******************************************************************************
COMMON txc_buffer		    ; Retrieve buffers common block 

; Buffer are numbered from 1 to tx_last_buffer
IF (position LE tx_last_buffer) THEN $
    buffer_handle=tx_buffers(position)  ; get handle index nb
CASE 1 OF
(position GT tx_last_buffer)	: RETURN, -1
(buffer_handle EQ 0)		: RETURN, -2
ELSE				: BEGIN & END
ENDCASE
 
Handle_Value, buffer_handle, buffer ; Get data from handle
x=buffer.X			    ; Get data from buffer
y=buffer.Y
w=buffer.W
n=buffer.N
e=buffer.E
p=buffer.PAR
tx_update_w, accu_nb, x, y, w, n, e
RETURN, 0
END

;******************************************************************************
FUNCTION tx_get_step, partxt, Step_Name_Array, Step_Value_Array, Best_Step_Index
;******************************************************************************
; partxt is an array of line
;

result = -1 ; line_tag not found
Step_Name_Array = ['']
Step_Value_Array = [0.0]
Best_Step_Index = -1

nb_line = n_elements(partxt)

line_tag = 'STEPS:'
current_line = 0
Done = 0
REPEAT BEGIN
    line = partxt(current_line)
    line_tag_pos = STRPOS(line, line_tag)	
    IF (line_tag_pos GT -1) THEN BEGIN
	result = -2 ; line_tag found (looking for DXX variables)
	StepDone = 0
	StepIndex = 0
	var_tag_pos = line_tag_pos+6
	REPEAT BEGIN
	    var_tag_pos = STRPOS(line, ' D', var_tag_pos)			
	    IF (var_tag_pos EQ -1) THEN StepDone = 1 $
	    ELSE BEGIN
		result = 0	; found a DQx
		egal_pos = STRPOS(line, '=', var_tag_pos+1)
		VarName = STRTRIM(STRMID(line, var_tag_pos+1, egal_pos-var_tag_pos-1),2)					
		value_begin = egal_pos + 1
		value_end = STRPOS(line, ',', value_begin)
		IF (value_end EQ -1) THEN value_end = strlen(line)
		VarValue = Float(STRMID(line, value_begin, value_end-value_begin ))
		Step_Name_Array = [Step_Name_Array, VarName]
		Step_Value_Array = [Step_Value_Array, VarValue]
		StepIndex = StepIndex + 1
		var_tag_pos = value_end
	    ENDELSE
	ENDREP UNTIL StepDone
    ENDIF

    current_line = current_line+1 ; look further
    IF (current_line EQ nb_line) THEN Done=1
ENDREP UNTIL Done

; Strip first unused entry
IF (StepIndex GT 0) THEN BEGIN
    Step_Name_Array = Step_Name_Array(1:*)
    Step_Value_Array = Step_Value_Array(1:*)
    
    ; Find best guess for x-axis
    Best_Step_Index = WHERE(Step_Value_Array, count)
    IF count GT 0 THEN $
	Best_Step_Index = Best_Step_Index(0) $
    ELSE $
	result = -3 ; Error : All steps have null value
    
ENDIF
RETURN, result
END

;******************************************************************************
FUNCTION tx_monitor_changed, tx_nsel, monitor, tx_codes ;**********************
;******************************************************************************

IF tx_nsel GE  0 THEN label=STRTRIM(tx_codes(tx_nsel), 2)
IF tx_nsel EQ -1 THEN label='None'
IF tx_nsel EQ -2 THEN BEGIN
    label=nb2str(monitor)
ENDIF
IF tx_nsel EQ -3 THEN label='None'
RETURN, label(0)
END

;******************************************************************************
FUNCTION tx_read, run, typ ;***********************************************
;******************************************************************************
; gets tripple axis scan in order gjk

@lamp.cbk			    ; call lamp.cbk common block
COMMON txc_main			    ; call back tripx_face common

tx_wk_s	=nb2str(tx_rwnb)	    ; Construct w strings for reading
path_f_o_k  =path_for_online
path_for_online	=tx_path		    ; Assign correct values for rdrun
status		=0

IF typ EQ 0 THEN BEGIN
    str='w'+tx_wk_s+'=rdrun('+nb2str(run)+', status)'
    tx_send_msg, txw_msg, 'Reading '+nb2str(run)+'...'
    xicute,str			    ; Read Datas
ENDIF ELSE BEGIN
    one=tx_rwnb
    p_did_getfil, run, tx_rwnb, status
ENDELSE
path_for_online=path_f_o_k
tx_send_msg, txw_msg, 'Checking '+nb2str(run)+'...'
iii=EXECUTE('tx_pv=pv'+tx_wk_s)
my_check=SIZE(tx_pv)
IF my_check(0) lt 1 THEN BEGIN
    tx_send_err_msg, txw_msg, 'Bum data'
    RETURN,0
ENDIF
mycheck=0			    ; Decode data fields from par_txt
last_line=N_ELEMENTS(par_txt(one, *))-1
WHILE mycheck lt 2 DO BEGIN
    code_string=STRTRIM(STRCOMPRESS(par_txt(one,last_line)),2)+' '
    mycheck    =STRLEN(code_string)    ; check that there is something there
    last_line  =last_line-1
    IF last_line lt 1 THEN RETURN,0
ENDWHILE
IF STRPOS(code_string, '=') GE 0 THEN $
	code_string=STRMID(code_string, 0, STRPOS(code_string, '='))
code_string=code_string+' '
tx_old_codes=tx_codes
tx_codes=code_string
l=STRLEN(code_string) & j=0 & code_buf=''
FOR i=0,l-1 DO IF STRMID(code_string,i,1) eq ' ' THEN BEGIN
    code_buf=[code_buf,STRMID(code_string,j,i-j)]
    j=i+1
ENDIF
code_buf=code_buf(1:*)
;	code_buf=chop(code_string,' ')
tx_send_msg, txw_msg,"got"+" "+nb2str(run)
size_codes =SIZE(code_buf)
tx_old_eg_txt=tx_eg_txt
tx_eg_txt=STRARR(size_codes(1))
tx_codes=code_buf(0:(size_codes(1)-1))
pv_size=SIZE(tx_pv)
FOR icode=0,size_codes(1)-1 DO BEGIN
    tx_eg_txt(icode)=""
    IF my_check(2) gt 2 THEN BEGIN
	; Put the first few lines of data into ex_txt (a la Kaleida)
	lim=MIN([pv_size(2)-1,2])
	IF pv_size(0) EQ 1 THEN lim=0
	FOR ifld=0,lim DO BEGIN
	    tx_buf=nb2str(tx_pv(icode,ifld))
	    IF STRLEN(tx_buf) gt 5 THEN $
	    tx_buf=STRMID(tx_buf,0,5)
	    tx_eg_txt(icode)=tx_eg_txt(icode)+tx_buf+" "
	ENDFOR
	IF lim GT 0 THEN BEGIN
	    tx_buf=nb2str(tx_pv(icode,pv_size(2)-1))
	    tx_eg_txt(icode)=tx_eg_txt(icode)+' ...'+tx_buf
	ENDIF
    ENDIF
    space_nb=MAX([0, 4-STRLEN(tx_codes(icode))])
    IF space_nb GT 0 THEN space=STRING(REPLICATE(32B, space_nb)) else space=''
    tx_codes(icode)=tx_codes(icode)+space
ENDFOR
tx_update_w, tx_rwnb, [0], [0], [0], [0], [0]
tx_xsel=-1
tx_wsel=-1
tx_ysel=-1
tx_nsel=-1
RETURN, tx_pv
END

;******************************************************************************
FUNCTION tx_script, tx_numor, y_txt, x_txt, monitor_txt ;**********************
;******************************************************************************
; This Function return a compiled string which describe a tripx set of data

msg=tx_numor+' '+y_txt+'='+x_txt+'/'+monitor_txt
RETURN, msg
END

;******************************************************************************
;*************** END OF FUNCTION ********** BEGINNING OF PROCEDURE ************
;******************************************************************************

;******************************************************************************
PRO tx_accumulate, accu_nb, str ;**********************************************
;******************************************************************************
; Tx_Last_buffer is updated by bufferize function

COMMON txc_buffer			; Retrieve buffers common block 
COMMON txc_main				; call back tripx_face common

tx_grant_w, accu_nb, x, y, w, n, e, tit, xtit, ytit, SubTit
p=0 ; ???
IF tx_cur_buf EQ -1 THEN BEGIN
    WIDGET_CONTROL, bad_id=i, txw_rtslider, /Sensitive
    tx_cur_buf=0
ENDIF
y=0
res=tx_bufferize(tx_cur_buf+1, x, y, w, n, e, p)
IF res LT 0 THEN $
    tx_send_err_msg, txw_msg, 'Error '+nb2str(res)+' in bufferize' $
ELSE BEGIN
					; *** History stuff
    tx_history =    [tx_history(0:tx_last_buffer-1), str]
    tx_send_msg, txw_history, str
					; *** Slider stuff
    WIDGET_CONTROL, bad_id=i, txw_rtslider, Set_Slider_Max=tx_last_buffer
    WIDGET_CONTROL, bad_id=i, txw_rtslider, Set_Value=tx_last_buffer
    tx_cur_buf =    tx_last_buffer
					; *** Frame stuff
    WIDGET_CONTROL, bad_id=i, txw_slid_lab, Set_Value='Frame #'+nb2str(tx_cur_buf)
ENDELSE
RETURN
END

;******************************************************************************
PRO tx_build_but,   base, base_id, tx_codes, tx_eg_txt, $   ;******************
		    txw_xbut, txw_wbut			    ;******************
;******************************************************************************
; This procedure built the widget buttons to select y, x axis and monitor

@lamp.cbk				    ; call lamp.cbk common block
Display_Y=0	; set if you want that an Y axis column button be created

siz=size(tx_codes)
WIDGET_CONTROL, bad_id=i, base_id, /Destroy	; clear old base
base_id  = WIDGET_BASE(base)
WIDGET_CONTROL, bad_id=i, base_id, /HourGlass
map=sys_dep('MAP')
IF map EQ 1 THEN WIDGET_CONTROL, bad_id=i, base_id, Map=0
IF map EQ 2 THEN WIDGET_CONTROL, bad_id=i, base_id, Update=0
base_id1	=   WIDGET_BASE(base_id,  /Map, /Frame)
base_id11	=   WIDGET_BASE(base_id1, /Column, $
		    X_Scroll_Size=525, $
		    Y_Scroll_Size=280)
IF siz(0) EQ 1 THEN IF siz(1) GT 0 THEN BEGIN	; if number of codes gt 0
    baset	=   WIDGET_BASE(base_id11, /Row,resource_name='mic')
    base_id2	=   WIDGET_BASE(base_id11, /Row,resource_name='mic',Frame=2)
        ; ---------------------------------------- Make array for cw_bgroup
    txw_monitor=INTARR(1)
    xtxt=tx_codes & xtxt(*)='   '+xtxt & ztxt='  -> '+tx_eg_txt
    basex	=   WIDGET_BASE(base_id2,  /Row)
    txw_xbut	=   CW_BGROUP(basex, xtxt, /Column, /Exclusive,  $
				UValue=[-88, 357, 3, 1], Font=ft_propor) 
    IF Display_Y THEN BEGIN
	basey	=   WIDGET_BASE(base_id2,  /Column)
	txw_ybut=   CW_BGROUP(basey, xtxt, /Column, /Exclusive, $
			    Label_Top='Y Axis', UValue=[-88, 357, 3, 2]) 
    ENDIF
    basez	=   WIDGET_BASE(base_id2,  /Column)
    txw_wbut	=   CW_BGROUP(basez, ztxt, /Column, /Exclusive,  $
				UValue=[-88, 357, 3, 3], Font=ft_propor) 

    tit		=   WIDGET_LABEL(baset, Font=ft_b_bigger, $
		    Value='X axis      Y axis      Monitor:')
    txw_mon_but	=   WIDGET_BUTTON(baset, /Menu, Value='None')
    ii=0 & mn=-1
    txw_monitor(0)= WIDGET_BUTTON(txw_mon_but, Value='None')	
    WIDGET_CONTROL, bad_id=i, txw_monitor(0), $
		Set_UValue=[-88, 357, 3, 4, -2, txw_monitor(0),txw_mon_but]
    FOR i=1, N_ELEMENTS(tx_codes)-1 DO BEGIN
	IF (tx_codes(i-1) EQ 'M1  ' OR $
	    tx_codes(i-1) EQ 'TIME') THEN BEGIN
	    ii=ii+1
	    IF tx_codes(i-1) EQ 'M1  ' THEN mn=i-1
	    txw_monitor=[txw_monitor,0]
	    txw_monitor(ii)=WIDGET_BUTTON(txw_mon_but, Value=tx_codes(i-1))
	    ; Set correct UValues for those widget Identifiers
	    WIDGET_CONTROL, bad_id=iii, txw_monitor(ii), Set_UValue= $
			    [-88, 357, 3, 4, i, txw_monitor(ii), txw_mon_but]
	ENDIF
    ENDFOR
    IF mn NE -1 THEN $
    WIDGET_CONTROL, bad_id=i, txw_mon_but, Set_Value=tx_codes(mn)
ENDIF
tx_build_par, base_id11
bid=sys_dep('DYNLAB',base_id,1)
IF map EQ 1 THEN WIDGET_CONTROL, bad_id=i, base_id, Map=1
IF map EQ 2 THEN WIDGET_CONTROL, bad_id=i, base_id, Update=1
END

;******************************************************************************
PRO tx_build_par, base ;*******************************************************
;******************************************************************************
; This procedure built the parameters widget

@lamp.cbk				    ; call lamp.cbk common block
COMMON txc_main				    ; call back tripx_face common

base_id	    =	WIDGET_BASE (base, /Column, Frame=2,resource_name='did')
label_base  =	WIDGET_BASE (base_id, /Row)
label	    =	WIDGET_LABEL(label_base, Font=ft_b_bigger, $
		Value='Tolerance Values')
label	    =	WIDGET_LABEL(label_base, Font=ft_smaller, $
		Value='In percentage of the corresponding value.')

base_row1   =	WIDGET_BASE (base, /Row, /Frame,resource_name='don')
unit_l	    =	WIDGET_LABEL(base_row1, Font=ft_propor, Value='Unit :')
unit_base   =	WIDGET_BASE (base_row1, /Exclusive, /Row)
txw_unit_mev=	WIDGET_BUTTON(unit_base, Value='MeV', Font=ft_propor, $
		UValue=[-88, 357, 6, 4])
txw_unit_thz=	WIDGET_BUTTON(unit_base, Value='THz', Font=ft_propor, $
		UValue=[-88, 357, 6, 5])
WIDGET_CONTROL, bad_id=i, txw_unit_mev, /Set_Button
label	    =	WIDGET_LABEL(base_row1, Font=ft_propor, Value='Monitor ref:')
txw_mon_txt =   WIDGET_TEXT (base_row1, Value='?????', XSize=7, $
			     /Edit, UValue=[-88, 357, 6, 7])

base_row2   =	WIDGET_BASE (base_id, /Row)
label	    =	WIDGET_LABEL(base_row2, Font=ft_propor, Value='Abscissa:')
txw_xtol    =	WIDGET_TEXT (base_row2, Font=ft_normal, /Edit, Value='', $
			     UValue=[-88, 357, 6, 1], XSize=5)
;label	    =	WIDGET_LABEL(base_row2, Font=ft_propor, Value='Temp. :')
;txw_degree  =	WIDGET_TEXT (base_row2, Font=ft_normal, /Edit, Value='10.0', $
;			     UValue=[-88, 357, 6, 2], XSize=5)


base_row3   =	WIDGET_BASE (base, /Row, /Frame,resource_name='don')
base_row31  =	WIDGET_BASE (base_row3, /NonExclusive)
label	    =	WIDGET_LABEL(base_row3, Font=ft_normal, $
		Value='X Range')
txw_xmin    =	WIDGET_TEXT (base_row3, Font=ft_propor, /Edit, $
			     Value='', XSize=7, UValue=[-88, 357, 6, 6])
txw_xmax    =	WIDGET_TEXT (base_row3, Font=ft_propor, /Edit, $
			     Value='', XSize=7, UValue=[-88, 357, 6, 6])
label	    =	WIDGET_LABEL(base_row3, Font=ft_normal, $
		Value='Y Range')
txw_ymin    =	WIDGET_TEXT (base_row3, Font=ft_propor, /Edit, $
			     Value='', XSize=7, UValue=[-88, 357, 6, 6])
txw_ymax    =	WIDGET_TEXT (base_row3, Font=ft_propor, /Edit, $
			     Value='', XSize=7, UValue=[-88, 357, 6, 6])	
tx_rg_uv=[-88, 357, 6, 3, txw_xmin, txw_xmax, txw_ymin, txw_ymax]
WIDGET_CONTROL, bad_id=i, txw_xmin, Set_UValue=[-88, 357, 6, 6, $
				    txw_xmin, txw_xmax, txw_ymin, txw_ymax]
WIDGET_CONTROL, bad_id=i, txw_xmax, Set_UValue=[-88, 357, 6, 6, $
				    txw_xmin, txw_xmax, txw_ymin, txw_ymax]
WIDGET_CONTROL, bad_id=i, txw_ymin, Set_UValue=[-88, 357, 6, 6, $
				    txw_xmin, txw_xmax, txw_ymin, txw_ymax]
WIDGET_CONTROL, bad_id=i, txw_ymax, Set_UValue=[-88, 357, 6, 6, $
				    txw_xmin, txw_xmax, txw_ymin, txw_ymax]

base_row4   =	WIDGET_BASE (base, /Row, /Frame,resource_name='ben')
txw_vp_but  =	WIDGET_BUTTON(base_row4, Value='View parameters', $
			      UValue=[-88, 357, 6, 8])

RETURN
END

;******************************************************************************
PRO tx_calculate_ranges, which ;***********************************************
;******************************************************************************
; Which is optionnal
; it can be 0, 1 or 2
;	0 : ranges are calculated using read and accu spectra
;	1 : ranges are calculated using read spectra only
;	2 : ranges are calculated using accu spectra only
;			  

COMMON txc_main
IF N_PARAMS() EQ 0 THEN which=0

CASE which OF
    0 : BEGIN
	    tx_grant_w, tx_rwnb, x, y, w, n, e, tit, xtit, ytit, SubTit
	    tx_grant_w, tx_awnb, xa, ya, wa, na, ea, tit, xtit, ytit, SubTit
	END
    1 : BEGIN
	    tx_grant_w, tx_rwnb, x, y, w, n, e, tit, xtit, ytit, SubTit
	    tx_grant_w, tx_rwnb, xa, ya, wa, na, ea, tit, xtit, ytit, SubTit
	END
    2 : BEGIN
	    tx_grant_w, tx_awnb, x, y, w, n, e, tit, xtit, ytit, SubTit
	    tx_grant_w, tx_awnb, xa, ya, wa, na, ea, tit, xtit, ytit, SubTit
	END
    ELSE :
ENDCASE
tx_xrange=[MIN([x, xa]), MAX([x, xa])]
tx_yrange=[MIN([w, wa]), MAX([w, wa])]
RETURN
END

;******************************************************************************
PRO tx_data_process ;**********************************************************
;******************************************************************************
; Process data to be plotted

@lamp.cbk				    ; call lamp.cbk common block
COMMON txc_main				    ; call back tripx_face common
COMMON txc_wd_id			    ; call back plot area common block
COMMON txc_pb				    ; call back problem common block

flag=0
IF N_ELEMENTS(tx_old_eg_txt) NE N_ELEMENTS(tx_eg_txt) THEN flag=1 $
ELSE BEGIN
    FOR iii=0, N_ELEMENTS(tx_eg_txt)-1 DO $
	IF tx_old_eg_txt(iii) NE tx_eg_txt(iii) THEN flag=1
ENDELSE
IF flag EQ 1 THEN BEGIN
    tx_old_eg_txt=tx_eg_txt
    tx_send_msg, txw_msg, 'Creating Axis buttons'
    tx_build_but, txw_but_0, txw_but_base, $    ; rebuilt ...
	tx_codes ,tx_eg_txt, txw_xbut, txw_wbut
    WIDGET_CONTROL, bad_id=i, txw_xtol, Set_Value=nb2str(tx_xtol)
    WIDGET_CONTROL, bad_id=i, txw_mon_txt, Set_Value=nb2str(tx_monitor)
    WIDGET_CONTROL, bad_id=i, txw_rg_but, Set_UValue=tx_rg_uv
ENDIF
siz_codes=SIZE(tx_codes)
IF siz_codes(0) EQ 0 THEN flag=2 ELSE BEGIN
    pidx=WHERE(tx_codes EQ 'PNT ')	    ; Try to find points (PNT)
    xidx=WHERE(tx_codes EQ 'EN  ')	    ; Try to find default X (EN)
    yidx=WHERE(tx_codes EQ 'XXXX')	    ; Try to find nothing
    widx=WHERE(tx_codes EQ 'CNTS')	    ; Try to find counts (CNTS)
    nidx=WHERE(tx_codes EQ 'M1  ')	    ; Try to find monitor (M1)
    eidx=WHERE(tx_codes EQ 'DELT')	    ; Try to find error (DELT)
    qhidx=WHERE(tx_codes EQ 'QH  ')	    ; Try to find points (QH)
    qkidx=WHERE(tx_codes EQ 'QK  ')	    ; Try to find points (QK)
    qlidx=WHERE(tx_codes EQ 'QL  ')	    ; Try to find points (QL)
    
					; Get X array number
    res = tx_get_step(par_txt(tx_rwnb,*), Step_Name_Array, Step_Value_Array, Best_Step_Index)
    IF (res EQ 0) THEN BEGIN
	best_X_axis=Step_Name_Array(Best_Step_Index)
	best_X_axis=STRMID(best_X_axis, 1, STRLEN(best_X_axis))
	xidx=WHERE(STRTRIM(tx_codes,2) EQ best_X_axis)
    ENDIF
    res=tx_extract(par_txt(tx_rwnb,*), 'FILE_', '', tx_numor)
    IF res LT 0 THEN tx_numor=''
    tx_numor=STRTRIM(tx_numor, 2)	    ; removed extra spaces
    res=tx_extract(par_txt(tx_rwnb,*), 'POSQE', 'UN', tx_unit)
    IF res LT 0 THEN tx_unit=''
    
    IF STRUPCASE(tx_unit) EQ 'MEV' THEN $
	WIDGET_CONTROL, bad_id=i, txw_unit_mev, /Set_Button
    IF STRUPCASE(tx_unit) EQ 'THZ' THEN $
	WIDGET_CONTROL, bad_id=i, txw_unit_thz, /Set_Button
    
    IF (pidx(0) NE -1) THEN tx_psel=pidx(0) ; store selected points
    IF (xidx(0) NE -1) AND (tx_xsel EQ -1) THEN tx_xsel=xidx(0) ; store selected x
    IF (yidx(0) NE -1) AND (tx_ysel EQ -1) THEN tx_ysel=yidx(0) ; store selected y
    IF (widx(0) NE -1) AND (tx_wsel EQ -1) THEN tx_wsel=widx(0) ; store selected w
    IF (nidx(0) NE -1) AND (tx_nsel EQ -1) THEN tx_nsel=nidx(0) ; store selected n
    tx_esel=-1
    tx_qhsel=-1
    tx_qksel=-1
    tx_qlsel=-1
    IF (eidx(0) NE -1)  THEN tx_esel =eidx(0)  ; store selected qh
    IF (qhidx(0) NE -1) THEN tx_qhsel=qhidx(0) ; store selected qh
    IF (qkidx(0) NE -1) THEN tx_qksel=qkidx(0) ; store selected qk
    IF (qlidx(0) NE -1) THEN tx_qlsel=qlidx(0) ; store selected ql
    IF tx_qksel NE -1 THEN tx_qk=tx_data(tx_qksel,*)
    IF tx_qhsel NE -1 THEN tx_qh=tx_data(tx_qhsel,*)
    IF tx_qlsel NE -1 THEN tx_ql=tx_data(tx_qlsel,*)
    IF tx_xsel GE 0 THEN $
    WIDGET_CONTROL, bad_id=i, txw_xbut, Set_Value=tx_xsel	    ; Refresh CW_Bgroup
    IF tx_wsel NE -1 THEN $
    WIDGET_CONTROL, bad_id=i, txw_wbut, Set_Value=tx_wsel	    ; Refresh CW_Bgroup
    IF tx_xsel NE -1 THEN x =tx_data(tx_xsel,*)	; Retrieve x data
    IF tx_ysel NE -1 THEN y =tx_data(tx_ysel,*)	; Retrieve y data
    IF tx_wsel NE -1 THEN w =tx_data(tx_wsel,*)	; Retrieve w data
    
    IF tx_nsel GE  0 THEN n=tx_data(tx_nsel,*)
    IF tx_nsel EQ -1 THEN n=1 ELSE $
    IF tx_nsel EQ -2 THEN n=FLOAT(tx_monitor)
    IF tx_nsel EQ -3 THEN n=1
    tx_xsiz=fix(N_ELEMENTS(x)) & IF tx_xsiz gt 1 THEN x=REFORM(x,tx_xsiz)
    tx_ysiz=fix(N_ELEMENTS(y)) & IF tx_ysiz gt 1 THEN y=REFORM(y,tx_ysiz)
    tx_wsiz=fix(N_ELEMENTS(w)) & IF tx_wsiz gt 1 THEN w=REFORM(w,tx_wsiz)
    tx_nsiz=fix(N_ELEMENTS(n)) & IF tx_nsiz gt 1 THEN n=REFORM(n,tx_nsiz)
    IF tx_esel NE -1 THEN e=tx_data(tx_esel,*) ELSE e=SQRT(w)   ; Calculate error vector
    IF tx_psel  NE -1 THEN BEGIN
	tx_pnt=tx_data(tx_psel,*)
	IF FLOOR(tx_pnt(0)) NE tx_pnt(0) THEN BEGIN
	    str_pnt=nb2str(tx_pnt)
	    FOR i=0, N_ELEMENTS(str_pnt)-1 DO BEGIN
		str_pnt(i)=STRMID(str_pnt(i), STRPOS(str_pnt(i), '.')+1, 10)
		j=STRLEN(str_pnt(i))-1
		WHILE (STRMID(str_pnt(i),j, 1) EQ '0') DO BEGIN
		 j=j-1
		 str_pnt(i)=STRMID(str_pnt(i),0, j+1)
		ENDWHILE
	    ENDFOR
	    nb_pol=MAX(FIX(str_pnt))
	    tx_send_msg, txw_msg, nb2str(nb_pol)+' Polarizations file'
	    WIDGET_CONTROL, bad_id=i,txw_ltslider, Sensitive=1
	    WIDGET_CONTROL, bad_id=i,txw_ltslider, Set_Slider_Max=nb_pol
	    WIDGET_CONTROL, bad_id=i,txw_ltslider, Set_Value=1
	ENDIF ELSE BEGIN
	    WIDGET_CONTROL, bad_id=i, txw_ltslider, Sensitive=0
	ENDELSE
    ENDIF
ENDELSE
IF N_ELEMENTS(n) LE 0 THEN n=1
IF (N_ELEMENTS(w) LE 0) OR (N_ELEMENTS(x) LE 0) THEN BEGIN
    tx_send_err_msg, txw_msg, 'No valid data to plot'
    tx_loaded = 0		    ; no data available
ENDIF ELSE BEGIN
    WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=1
    y=[0]			    ; Y is not yet used
    tx_update_w, tx_rwnb, x, y, w, n, e
				    ; Titles Stuff
    head_tit(tx_rwnb,0)=tx_numor
    x_tit(tx_rwnb)	=STRTRIM(tx_codes(tx_xsel), 2)
    IF x_tit(tx_rwnb) EQ 'EN' THEN x_tit(tx_rwnb)=x_tit(tx_rwnb)+'('+tx_unit+')'
    y_tit(tx_rwnb)	=STRTRIM(tx_codes(tx_wsel), 2)
    mon_txt=tx_monitor_changed(tx_nsel, tx_monitor, tx_codes)
    msg='Plot of '+tx_script(tx_numor, y_tit(tx_rwnb), x_tit(tx_rwnb), mon_txt)
    tx_send_msg, txw_msg, msg
    IF tx_cur_buf NE -1 THEN BEGIN
	IF tx_xrange(0) EQ -1 AND tx_yrange(0) EQ -1 THEN $
	    tx_calculate_ranges ELSE WIDGET_CONTROL, bad_id=i, txw_rg_but, /Set_button
	IF tx_loaded THEN tx_update_ranges
    ENDIF ELSE BEGIN
        tx_xrange=[0]
        tx_yrange=[0]
    ENDELSE
    tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
    IF tx_loaded THEN tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
    tx_loaded = 1		    ; data are now available
ENDELSE
RETURN
END

;******************************************************************************
PRO tx_end, wid ;**************************************************************
;******************************************************************************
; tx_end is invoked when tripx is interrupted
@lamp.cbk				    ; call lamp.cbk common block
COMMON txc_main				    ; call back tripx_face common

DON_WRITE_PROG_MAC ,1
;DID_WRITE_JOURNAL
	
IF tx_cur_buf NE -1 THEN BEGIN
    n=N_ELEMENTS(tx_history)
    lun=0 & on_ioerror,mis_his
    OPENW, lun, 'tripx.his', /Get_Lun,/APPEND
    PRINTF, lun, n, ' ***** ',!stime
    FOR i=0, n-1 DO $
    PRINTF, lun, tx_history(i)
    mis_his:FREE_LUN, lun
ENDIF
RETURN
END

;******************************************************************************
PRO tx_export, workspace_number, OWN=own ;*************************
;******************************************************************************
; Export to Workspace defined into text widget the workspace "workspace_number"

@lamp.cbk				    ; call lamp.cbk common block
COMMON txc_main				    ; call back tripx_face common

i_will=0				    ; Start tests for valid workspace
IF KEYWORD_SET(OWN) THEN export_str='W'+nb2str(own) ELSE BEGIN
    WIDGET_CONTROL, bad_id=i, txw_dp_txt, Get_Value=export_str
    export_str=STRTRIM(export_str(0),2)
ENDELSE
expstrln=STRLEN(export_str)
pos_w=STRPOS(STRUPCASE(export_str),'W')        
IF (pos_w eq 0) and (expstrln le 3) and (expstrln gt 1)  THEN BEGIN
    IF expstrln eq 3 THEN fig=STRMID(export_str,1,2) $
	    ELSE fig=STRMID(export_str,1,1)
    ON_IOERROR, no_fix1
    owk=fix(fig)
    IF owk LT 1 OR owk GT 23 THEN i_will=2 $
    ELSE BEGIN
	wnstr=nb2str(workspace_number)
	out_txt='w'+nb2str(owk)+'=w'+wnstr+'/n'+wnstr+$
		    '*total(n'+wnstr+')/N_ELEMENTS(n'+wnstr+')'
	xicute,out_txt
	i_will=1
    ENDELSE
ENDIF
no_fix1:
    IF i_will eq 0 THEN tx_save, export_str	; Save accumulator
    if i_will EQ 1 THEN BEGIN
	IF workspace_number EQ tx_awnb THEN str='accumulator' $
	ELSE str=tx_numor
	tx_send_msg, txw_msg,'Wrote '+str+' to w'+nb2str(owk)       
    ENDIF
    if i_will EQ 2 THEN $
    	tx_send_err_msg, txw_msg, 'W must be within 1-20'
END

;******************************************************************************
PRO tx_grant_w, wnb, x, y, w, n, e, tit, xtit, ytit, SubTit ;******************
;******************************************************************************
@lamp.cbk			    ; call lamp.cbk common block

iii=EXECUTE('x=x'+nb2str(wnb))  ; Fetch x data
iii=EXECUTE('y=y'+nb2str(wnb))  ; Fetch y data
iii=EXECUTE('w=w'+nb2str(wnb))  ; Fetch w data
iii=EXECUTE('n=n'+nb2str(wnb))  ; Fetch n data
iii=EXECUTE('e=e'+nb2str(wnb))  ; Fetch e data

iii=EXECUTE('tit=head_tit('+nb2str(wnb)+', 0)')	    ; Fetch Title
iii=EXECUTE('xtit=x_tit('+nb2str(wnb)+')')	    ; Fetch Xtitle
iii=EXECUTE('ytit=y_tit('+nb2str(wnb)+')')	    ; Fetch Ytitle
iii=EXECUTE('SubTit=other_tit('+nb2str(wnb)+', 0)') ; Fetch SubTitle

RETURN
END

;===============================================================================
PRO tx_help	, dummy, formu, formt
;===============================================================================

formu=''
formt='                                      By Yannick Raoul and Romuald Jouffrey.'
formu=[formu,'']
formt=[formt,'']
formu=[formu,'General principles :']
formt=[formt,'']
formu=[formu,'']
formt=[formt,'Left Draw is used to display loaded dataset, right one displays current accu']
formu=[formu,'']
formt=[formt,'Right/up : The axis selector and general parameters(move vertical slider)']
formu=[formu,'']
formt=[formt,'Bottom/center : calculation facilities']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'File selector :']
formt=[formt,'Select the correct path and choose the file you want to load (double click or return)']
formu=[formu,'']
formt=[formt,'Data are loaded and X and Y scales are detected (Counts versus Energy)']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Zoom :']
formt=[formt,'Enter zoom ranges or use left mouse button to select zoom area']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Remove point :']
formt=[formt,'Middle mouse button to select point(s) to be removed']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Add :']
formt=[formt,'Create a new accu with old accu and current loaded dataset (copy if accu was empty)']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Combine :']
formt=[formt,'Use factor to perform calculation : new_accu = old_accu+factor*dataset']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Reset and New Button :']
formt=[formt,'Reset erase all accumulators, new create a new empty accumulator']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'FIT :']
formt=[formt,'Run fit interface with current data or accumulator (left or right)']
formu=[formu,'']
formt=[formt,'']
formu=[formu,'Right slider :']
formt=[formt,'allows to go back to older accumulator']
RETURN
END

;******************************************************************************
PRO tx_initialize ;************************************************************
;******************************************************************************

COMMON txc_main			    ; call back tripx common block
COMMON txc_wd_id	    	    ; call back plot area common block
COMMON txc_buffer		    ; call back buffers common block

tx_cur_buf   =-1		    ; No buffer has been stored
tx_history   =['Empty accumulator'] ; 
tx_rwnb	    =21			    ; number of workspace to read data (21)
tx_awnb	    =22			    ; number of accumulator workspace (22)
tx_data	    =0			    ; Work to store tripx datas
tx_update_w, tx_awnb, [0], [0], [0], [0], [0] ; Clear accumulator workspace
tx_old_data =0			    ; ??? temporary buffer for w
tx_small_history=''		    ; Clear compiled history
tx_reset_buffers		    ; Clear & Initialize buffers
RETURN
END

;******************************************************************************
PRO tx_save, export_str ;******************************************************
;******************************************************************************


@lamp.cbk				    ; call lamp.cbk common block
COMMON txc_main				    ; call back tripx_face common

IF export_str NE '' AND tx_cur_buf NE -1 THEN BEGIN
tx_grant_w, tx_awnb, x, y, w, n, e, tit, xtit, ytit, SubTit
    lun=0 & on_ioerror,mis_sav
    OPENW,  lun, export_str, /Get_Lun
    PRINTF, lun, STRING(REPLICATE(82B, 80))
    PRINTF, lun, tx_small_history+'      1      0'
    PRINTF, lun, 'Tripx exported file : '+export_str
    PRINTF, lun, STRING(REPLICATE(65B, 80))    
    PRINTF, lun, '      80      0'
    PRINTF, lun, ''
    PRINTF, lun, STRING(REPLICATE(86B, 80))
    PRINTF, lun, 'INSTR: '+tx_par_str.instr
    PRINTF, lun, 'EXPNO: '+tx_par_str.expno
    PRINTF, lun, 'USER_: '+tx_par_str.user
    PRINTF, lun, 'LOCAL: '+tx_par_str.local
    PRINTF, lun, 'FILE_: '+export_str                                                                                                                                           
    PRINTF, lun, 'DATE_: '
    PRINTF, lun, 'TITLE: '+other_tit(tx_awnb, 0)
    PRINTF, lun, 'COMND: '+tx_par_str.comnd
    PRINTF, lun, 'COMM_: '
    PRINTF, lun, 'POLAN: '
    PRINTF, lun, 'POSQE: UN='+tx_unit
    PRINTF, lun, 'STEPS: '+tx_par_str.steps
    PRINTF, lun, 'PARAM:'                                                                                                                                                                                           
    PRINTF, lun, 'FORMT: (F8.3,2X,F8.3,2X,F8.3,2X,F8.3,2X,G10.3)'
    PRINTF, lun, 'DATA_:'
    str='PNT       '+tx_codes(tx_xsel)+'     '+tx_codes(tx_wsel)+$
	'         DELT       '+tx_codes(tx_nsel)
    PRINTF, lun, str
    FOR i=0, N_ELEMENTS(x)-1 do BEGIN
    PRINTF, lun,      STRING(   i+1, FORMAT='(I3)'), $
		 ' ', STRING(x(i), FORMAT='(TR2, F8.3)'), $
		 ' ', STRING(w(i), FORMAT='(TR2, F8.3)'), $
		 ' ', STRING(e(i), FORMAT='(TR2, F8.3)'), $
		 ' ', STRING(n(i), FORMAT='(TR2, G10.3)')
    ENDFOR
    tx_send_msg, txw_msg, 'Accu has been saved in '+export_str
    mis_sav:FREE_LUN, lun
ENDIF
RETURN
END

;******************************************************************************
PRO tx_see_buffers ;***********************************************************
;******************************************************************************
; To view buffer content

COMMON txc_buffer			; Retrieve buffers common block 

buffer_in_stock = N_ELEMENTS(tx_buffers)
FOR index=0, buffer_in_stock-1 DO BEGIN
	Print, 'Value of buffer ', index
	buffer_handle=tx_buffers(index)	; get handle
	IF buffer_handle NE 0 THEN BEGIN	
		Handle_Value, buffer_handle, buffer ; Get data from handle
	END
ENDFOR
RETURN
END

;******************************************************************************
PRO tx_plot, wid, wnb, x_range, y_range, type, PS=ps ;*************************
;******************************************************************************
; The unique plotting procedure for the whole tripx project (by JR)
; x_tit , y_tit , z_tit , w_tit , other_tit , head_tit

IF N_PARAMS() EQ 4 THEN type=0
tx_grant_w, wnb, x, y, w, n, e, tit, xtit, ytit, SubTit
IF  (N_ELEMENTS(x_range) EQ 1) AND (N_ELEMENTS(y_range) EQ 1) THEN norange=1 $
ELSE norange=0
avg=TOTAL(n)/(N_ELEMENTS(n))>1
m  =avg/(n>1)
IF NOT(KEYWORD_SET(PS)) THEN WSET,wid
yp=w*m
IF N_ELEMENTS(yp) GT 1 THEN BEGIN
    IF type EQ 0 THEN BEGIN
	IF norange THEN $
	PLOT, x, yp, $
	      Title=Tit, XTitle=XTit, YTitle=YTit, SubTitle=SubTit, LineStyle=0 $
	ELSE $
	PLOT, x, yp, XRange=x_range, YRange=y_range, $
	      Title=Tit, XTitle=XTit, YTitle=YTit, SubTitle=SubTit, LineStyle=0
	IF (N_ELEMENTS(x) GT 0) THEN OPLOTERR,x,yp,e*m
    ENDIF ELSE BEGIN
	OPLOT, x, yp, LineStyle=type
    ENDELSE
ENDIF
RETURN
END

;******************************************************************************
PRO tx_remove_points, wnb, removing_window		; *********************
;******************************************************************************

xx=[0.0] & ww=[0.0] & yy=[0.0] & ee=[0.0] & nn=[0.0]
tx_grant_w, wnb, x, y, w, n, e, tit, xtit, ytit, SubTit
IF N_ELEMENTS(x) GT 1 THEN BEGIN
    pnt_removed=0
    FOR i=0, N_ELEMENTS(x)-1 DO BEGIN
	IF  (x(i) GE removing_window(0) AND $
	     x(i) LE removing_window(1)) AND $
	    (w(i) GE removing_window(2) AND $
	     w(i) LE removing_window(3)) THEN BEGIN
	    pnt_removed=1
	ENDIF ELSE BEGIN
	    xx=[xx, x(i)]
	    ww=[ww, w(i)]
	    ;yy=[yy, y(i)]
	    nn=[nn, n(i)]
	    ee=[ee, e(i)]
	ENDELSE
    ENDFOR
    IF pnt_removed THEN BEGIN
	x=xx(1:*) & w=ww(1:*) & e=ee(1:*) & n=nn(1:*)
	tx_update_w, wnb, x, y, w, n, e
    ENDIF
ENDIF		
RETURN
END

;******************************************************************************
PRO tx_reset_buffers ;*********************************************************
;******************************************************************************
COMMON txc_buffer		; Retrieve buffers common block 

buffer_in_stock = N_ELEMENTS(tx_buffers)
FOR index=0,buffer_in_stock-1 DO BEGIN
	buffer_handle = tx_buffers(index)
	IF buffer_handle NE 0 THEN $
		Handle_Free, buffer_handle
END

tx_buffers = [0L]
tx_last_buffer = 0
END ; tx_reset_buffers

;******************************************************************************
PRO tx_send_msg, wid, txt_msg ;************************************************
;******************************************************************************
; Send a message into the relevant widget label

space_nb=MAX([0, (35-STRLEN(txt_msg))/2])
IF space_nb GT 0 THEN space=STRING(REPLICATE(32B, space_nb)) else space=''
txt_msg=space+txt_msg
WIDGET_CONTROL, bad_id=i, wid, Set_Value=txt_msg
RETURN
END

;******************************************************************************
PRO tx_send_err_msg, wid, txt_msg ;********************************************
;******************************************************************************
; beep and send a message into the relevant widget label (using tx_send_msg)

print, STRING(7B)
tx_send_msg, wid, txt_msg
RETURN
END

;******************************************************************************
PRO tx_store_parameters ;******************************************************
;******************************************************************************
@lamp.cbk			    ; call lamp.cbk common block
COMMON txc_main			    ; call back tripx_face common

; DATE
; TITRE
; COMND
; POSQE
; Nombre de donnees
; PARAMS et VARIABLES

res=tx_extract(par_txt(tx_rwnb,*), 'INSTR', '', instr)
IF res LT 0 THEN instr='error'
res=tx_extract(par_txt(tx_rwnb,*), 'EXPNO', '', expno)
IF res LT 0 THEN expno='error'
res=tx_extract(par_txt(tx_rwnb,*), 'USER_', '', user)
IF res LT 0 THEN user='error'
res=tx_extract(par_txt(tx_rwnb,*), 'LOCAL', '', local)
IF res LT 0 THEN local='error'
res=tx_extract(par_txt(tx_rwnb,*), 'FILE_', '', file)
IF res LT 0 THEN file='error'
res=tx_extract(par_txt(tx_rwnb,*), 'DATE_', '', date)
IF res LT 0 THEN date='error'
res=tx_extract(par_txt(tx_rwnb,*), 'TITLE', '', title)
IF res LT 0 THEN title='error'
res=tx_extract(par_txt(tx_rwnb,*), 'COMND', '', comnd)
IF res LT 0 THEN title='error'
res=tx_extract(par_txt(tx_rwnb,*), 'POSQE', '', posqe)
IF res LT 0 THEN posqe='error'
res=tx_extract(par_txt(tx_rwnb,*), 'STEPS', '', steps)
IF res LT 0 THEN steps='error'

tx_par_str={	instr:	instr	, expno:expno	, $
		user:	user	, local:local	, $
		file:	file	, date:date	, $
		title:	title	, comnd:comnd	, $
		posqe:	posqe	, steps:steps}

tx_par_txt=[			      $
	'Description	  :'	    , $
	'  Instrument	  : '+instr , $
	'  Exp. number	  : '+expno , $
	'  User		  : '+user  , $
	'  Local	  : '+local , $
	'  File           : '+file  , $
	'  Date           : '+date  , $
	'  Title	  : '+title , $
	'  Command	  : '+comnd , $
	'  PosQE	  : '+posqe , $
	'  steps	  : '+steps , $
	''	    , $
	'Historique       : '	    , $
	'Original dataset : '+file  , $
	''			    , $
	'Number of data   :'	    , $
	''			    , $
	'Header Info :']
v=par_txt(tx_rwnb,*)
FOR i=0, N_ELEMENTS(v)-1 DO BEGIN
    IF STRMID(v(i), 0, 5) EQ 'VARIA' OR STRMID(v(i), 0, 5) EQ 'PARAM' THEN BEGIN
	tx_par_txt=[tx_par_txt, v(i)]
    ENDIF
ENDFOR
RETURN
END

;******************************************************************************
PRO tx_unscript, txt, tx_numor, y_txt, x_txt, monitor_txt ;********************
;******************************************************************************
w_txt=txt

space=STRPOS(w_txt, ' ')
tx_numor=STRMID(w_txt, 0, space)
w_txt=STRMID(w_txt, space+1, STRLEN(w_txt)-space+1)

equal=STRPOS(w_txt, '=')
y_txt=STRMID(w_txt, 0, equal)
w_txt=STRMID(w_txt, equal+3, STRLEN(w_txt)-equal+1)

parenthesis=STRPOS(w_txt, '/')
x_txt=STRMID(w_txt, 0, parenthesis)
w_txt=STRMID(w_txt, parenthesis+2, STRLEN(w_txt)-parenthesis+1)

space=STRPOS(w_txt, ' ')
monitor_txt=w_txt
RETURN
END

;******************************************************************************
PRO tx_update_ranges ;*********************************************************
;******************************************************************************
COMMON txc_main

WIDGET_CONTROL, bad_id=i, txw_rg_but, Get_UValue=UV
IF N_ELEMENTS(tx_xrange) EQ 0 AND  N_ELEMENTS(tx_yrange) THEN BEGIN
    WIDGET_CONTROL, bad_id=i, UV(4), Set_Value=''
    WIDGET_CONTROL, bad_id=i, UV(5), Set_Value=''
    WIDGET_CONTROL, bad_id=i, UV(6), Set_Value=''
    WIDGET_CONTROL, bad_id=i, UV(7), Set_Value=''
ENDIF ELSE BEGIN
    WIDGET_CONTROL, bad_id=i, UV(4), Set_Value=nb2str(tx_xrange(0))
    WIDGET_CONTROL, bad_id=i, UV(5), Set_Value=nb2str(tx_xrange(1))
    WIDGET_CONTROL, bad_id=i, UV(6), Set_Value=nb2str(tx_yrange(0))
    WIDGET_CONTROL, bad_id=i, UV(7), Set_Value=nb2str(tx_yrange(1))
ENDELSE
RETURN
END

;******************************************************************************
PRO tx_update_w, wnb, x, y, w, n, e ;******************************************
;******************************************************************************
@lamp.cbk			    ; call lamp.cbk common block

iii=EXECUTE('x'+nb2str(wnb)+'=x')	; Update x data
iii=EXECUTE('y'+nb2str(wnb)+'=y')	; Update y data
iii=EXECUTE('w'+nb2str(wnb)+'=w')	; Update w data
iii=EXECUTE('n'+nb2str(wnb)+'=n')	; Update n data
iii=EXECUTE('e'+nb2str(wnb)+'=e')	; Update e data
RETURN
END

;******************************************************************************
PRO tripx_event, Event ;*******************************************************
;******************************************************************************
; ???

@lamp.cbk			    ; call lamp.cbk common block
COMMON txc_main			    ; call back tripx_face common
COMMON txc_wd_id		    ; call back plot area common block
COMMON txc_pb			    ; call back problem common block

wplot=!D.NAME
stat=0 & catch,stat
IF stat ne 0 THEN BEGIN catch,/cancel & print,string(7b),!err_string & SET_PLOT,wplot & return & ENDIF

WIDGET_CONTROL, bad_id=i,Event.Id,Get_UValue=uv,/Hourglass

uv1=uv(2)				; Get UValue code
IF N_ELEMENTS(uv) GT 3 then uv2=uv(3)	; and other optionnal code
IF N_ELEMENTS(uv) GT 4 then uv3=uv(4)
IF N_ELEMENTS(uv) GT 5 then uv4=uv(5)
IF N_ELEMENTS(uv) GT 6 then uv5=uv(6)
IF N_ELEMENTS(uv) GT 7 then uv6=uv(7)
IF N_ELEMENTS(uv) GT 8 then uv7=uv(8)
CASE uv1 OF
    -1	:  BEGIN & END				; *********** No event generated
     1	:  BEGIN				; *********** Tripx general events
	    CASE uv2 OF
	0 : BEGIN				; Print Button Event
		SET_PLOT, 'PS'
		tx_grant_w, tx_awnb, xa, ya, wa, na, ea, tit, xtit, ytit, SubTit 
		short_side = 18.0
		small_offset=0.5
		long_side = 13.0
		big_offset=14.0
		fn = 'idl.ps'
		DEVICE, bits=8, /color, xsize=short_side, ysize=long_side, $
		xoffset=small_offset, yoffset=big_offset, $
		/portrait, /isolatin1, filename=fn
		tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange, /PS
		FOR i=0, N_ELEMENTS(tx_par_txt)-1 DO BEGIN
		    IF tx_par_txt(i) EQ 'Number of data   :' THEN $
			tx_par_txt(i)=tx_par_txt(i)+' '+NB2STR(N_ELEMENTS(xa))
		    XYOUTS, 100, -360.-(i-1)*300., tx_par_txt(i), Size=0.8, /Device
		ENDFOR
		DEVICE, /close_file
		SET_PLOT,wplot
		tx_send_msg, txw_msg, 'Accu printed in idl.ps'
	    END
	1 : BEGIN				; Done Button Event
	    tx_end, txw_base			; Save tripx history
	    WIDGET_CONTROL, bad_id=i, txw_base, /Destroy
	    IF txw_base EQ lamp_b1 THEN WIDGET_CONTROL,/reset
	    END
	2 : BEGIN				; TOUCH BASE Button Event
	    TOUCH_B, 331, inst_value
	    END
	3 : BEGIN				; Help Button Event
	    show_helps, [-88, 593]
	    END
	4 : BEGIN				; formula DO event
		WIDGET_CONTROL, bad_id=i, uv3, Get_Value=str
		str=str(0)
		tx_send_msg, txw_msg, 'Executing '+str
		xicute, str
		tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange		
		tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
	    END
	5 : BEGIN				; export event
		IF tx_cur_buf GT -1 THEN BEGIN
		    IF tx_history(tx_cur_buf) EQ 'Empty accumulator' THEN $
			tx_export, tx_rwnb ELSE tx_export, tx_awnb
		ENDIF ELSE tx_export, tx_rwnb
	    END
	6 : BEGIN				; Left fit button
		IF tx_loaded THEN BEGIN
		    tx_export, tx_rwnb, OWN=23
		    gfit, Group=txw_base, /Tripx, tx_par_txt
		ENDIF ELSE tx_send_err_msg, txw_msg, 'Load data first'
	    END	    
	7 : BEGIN				; Right fit button
		IF tx_cur_buf GT -1 THEN BEGIN
		    IF tx_history(tx_cur_buf) EQ 'Empty accumulator' THEN BEGIN
			tx_send_err_msg, txw_msg, 'Accumulator is empty'
		    ENDIF ELSE BEGIN
			tx_export, tx_awnb, OWN=23
			gfit, Group=txw_base, /Tripx, tx_par_txt
		    ENDELSE
		ENDIF ELSE tx_send_err_msg, txw_msg, 'Accumulator is empty'
	    END
	8 : BEGIN				; Free event 
		IF txw_rg_but LT 0 THEN $
		    tx_send_err_msg, txw_msg, 'Load Data first ...' $
		ELSE BEGIN
		    
		ENDELSE	    
	    END
	ELSE: tx_send_err_msg, txw_msg, 'erreur dans le parsing de 1, x !'
	ENDCASE
	END
     2	: BEGIN				; *********** Parse pickfile events  
	    result=event.data		; filename from pickfile
	    flag=-1
	    IF result EQ 'path' THEN tx_path=event.PFO ELSE BEGIN
		IF result NE '' THEN BEGIN	; A valid filename has been given
		    flag=1
		    IF event.PFO ne '' then tx_path=event.PFO
		    ON_IOERROR, nofix2
		    r_number=FLOAT(result)
		    r_number=FIX(r_number)
		    tx_data=tx_read(r_number, 0)
		    IF N_ELEMENTS(tx_data) GT 1 THEN tx_data_process ELSE $
		    tx_send_err_msg, txw_msg, 'File input Error'
		    flag=0
		ENDIF
		nofix2 : IF flag EQ 1 THEN BEGIN
		    tx_data=tx_read(result, 1)
		    ;IF N_ELEMENTS(tx_data) GT 1 THEN tx_data_process ELSE $
		    ;tx_send_err_msg, txw_msg, 'File input Error'
		    tx_data_process
		ENDIF
	    ENDELSE
	  END
     3	:   BEGIN				; *********** Axis & monitor selection
	    WIDGET_CONTROL, bad_id=i, Event.id, Get_Value=selected
	    CASE uv2 OF
	    1 : BEGIN			    ; X axis selected
		    tx_xsel=selected
		    msg='X-axis now '+tx_codes(tx_xsel)
		    tx_send_msg, txw_msg, msg
		END
	    2 : BEGIN			    ; Y axis selected
		    tx_ysel=selected
		END
	    3 : BEGIN			    ; W axis selected
		    tx_wsel=selected
		    tx_send_msg, txw_msg, 'Intensity-axis now '+tx_codes(tx_wsel)
		END
	    4 : BEGIN			    ; N axis selected
		    WIDGET_CONTROL, bad_id=i, uv4, Get_Value=v
		    tx_nsel=uv3-1
		    mon_txt=tx_monitor_changed(tx_nsel, tx_monitor, tx_codes)
		    WIDGET_CONTROL, bad_id=i, uv5, Set_Value=mon_txt
		    tx_send_msg, txw_msg, 'Monitor now '+mon_txt
		END
	    ELSE: tx_send_err_msg, txw_msg, 'erreur dans le parsing de 2, x !'
	    ENDCASE
	    tx_data_process			; do data process
	    END
     4	:   BEGIN				; *********** Calculation events
	    CASE uv2 OF
	    1 : BEGIN				; combine event
		IF NOT(tx_loaded) THEN BEGIN
		    tx_send_err_msg, txw_history, 'No Data to combine !!!'
		ENDIF ELSE BEGIN
		    oper='c'
		    IF tx_cur_buf EQ -1 THEN oper='s' ELSE $
		    IF tx_history(tx_cur_buf) EQ 'Empty accumulator' THEN oper='s'
		    mon_txt=tx_monitor_changed(tx_nsel, tx_monitor, tx_codes)
		    msg=tx_script(tx_numor, y_tit(tx_rwnb), x_tit(tx_rwnb), mon_txt)
		    IF oper EQ 's' THEN BEGIN
			tx_store_parameters
			msg=msg+' stored'
		    ENDIF ELSE msg=msg+' combined'
		    res=tx_calculate(tx_awnb, oper, tx_rwnb, tx_xtol, $
				     msg, txw_basec, tx_factor, tx_monitor)
		    IF res GE 0 THEN BEGIN
			IF oper EQ 's' THEN tx_small_history=head_tit(tx_rwnb, 0) ELSE $
			tx_small_history=tx_small_history+oper+head_tit(tx_rwnb, 0)
			other_tit(tx_awnb)=tx_small_history
			strl=STRLEN(tx_small_history)
			IF strl GT 60 THEN $
			    other_tit(tx_awnb)=STRMID(tx_small_history, 0, 30)+'...'+$
					       STRMID(tx_small_history, strl-30, strl)
			tx_calculate_ranges
			tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
			tx_calculate_ranges, 2
			tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
		    ENDIF
		ENDELSE
	    END
	    2 : BEGIN				; Add event
		IF NOT(tx_loaded) THEN BEGIN
		    tx_send_err_msg, txw_history, 'No Data to add !!!'
		ENDIF ELSE BEGIN
		    oper='+'
		    IF tx_cur_buf EQ -1 THEN oper='s' ELSE $
		    IF tx_history(tx_cur_buf) EQ 'Empty accumulator' THEN oper='s'
		    mon_txt=tx_monitor_changed(tx_nsel, tx_monitor, tx_codes)
		    msg=tx_script(tx_numor, y_tit(tx_rwnb), x_tit(tx_rwnb), mon_txt)
		    IF oper EQ 's' THEN BEGIN
			tx_store_parameters
			msg=msg+' stored'
		    ENDIF ELSE msg=msg+' added'
		    res=tx_calculate(tx_awnb, oper, tx_rwnb, tx_xtol, $
				     msg, txw_basec, tx_factor, tx_monitor)
		    IF res GE 0 THEN BEGIN
			IF oper EQ 's' THEN tx_small_history=head_tit(tx_rwnb, 0) ELSE $
			tx_small_history=tx_small_history+oper+head_tit(tx_rwnb, 0)
			other_tit(tx_awnb)=tx_small_history
			strl=STRLEN(tx_small_history)
			IF strl GT 60 THEN $
			    other_tit(tx_awnb)=STRMID(tx_small_history, 0, 30)+'...'+$
					       STRMID(tx_small_history, strl-30, strl)
			tx_calculate_ranges
			tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
			tx_calculate_ranges, 2
			tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
		    ENDIF
		ENDELSE
	    END
	    3 : BEGIN			; calculation factor event
		    WIDGET_CONTROL, bad_id=i, event.id, Get_Value=v
		    ON_IOERROR, no_fix4
		    tx_factor=FLOAT(v)
		    tx_send_msg, txw_msg, 'Factor is '+v+' now'
		    no_fix4 : BEGIN & END
		END
	    4 : BEGIN				; Reset Button event
		tx_initialize
		tx_send_msg, txw_history, tx_history(0)
		WIDGET_CONTROL, bad_id=i, txw_rtslider, Set_Value=0
		WIDGET_CONTROL, bad_id=i, txw_slid_lab, Set_Value='Frame #0'
		WIDGET_CONTROL, bad_id=i, txw_rtslider, Sensitive=0
		WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=1
		WSET, txw_d2 & ERASE
		XYOUTS, 110, 100, 'Empty accumulator', /Device
	    END
	    5 : BEGIN				; New Button event
	    IF tx_cur_buf NE -1 THEN $		; only if current isn't empty
		IF tx_history(tx_cur_buf) NE 'Empty accumulator' THEN BEGIN
		tx_update_w, tx_awnb, [0], [0], [0], [0], [0]
		WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=1
		WSET, txw_d2 & ERASE
		tx_small_history=''
		XYOUTS, 110, 100, 'Empty accumulator', /Device
		tx_accumulate, tx_awnb, 'Empty accumulator'
	    ENDIF
	    END
	ELSE : see, 'error de uvalue 4, x'   
	ENDCASE
	END
     5	:  BEGIN				; *********** Draw & slider events
	    CASE uv2 OF
	1 : BEGIN			; Widget Plot Area 1 Event
	    IF tx_loaded EQ 1 AND event.type EQ 0 THEN BEGIN
		    wset,txw_d1
		    CURSOR,xg,yg, /NoWait, /Data		    
		    res=sl_zoom(event.x,event.y,xd,yd)
		    CURSOR,xh,yh, /NoWait, /Data
		    IF res EQ 0 THEN BEGIN ; No zoom range, write coodinates
			txstr=nb2str(xh)+','+nb2str(yh)
			XYOUTS,xh,yh,txstr,/Noclip
		    ENDIF
		    IF res EQ 1 THEN BEGIN
			tx_xrange=[MIN([xg, xh]), MAX([xg, xh])]
			tx_yrange=[MIN([yg, yh]), MAX([yg, yh])]
			tx_update_ranges
			WIDGET_CONTROL, bad_id=i, txw_rg_but, /Set_button
			tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
		    ENDIF
		    IF res EQ -2 THEN BEGIN	; Remove point(s)
			removing_window=[MIN([xg, xh]), MAX([xg, xh]), $
					 MIN([yg, yh]), MAX([yg, yh])]
			tx_remove_points, tx_rwnb, removing_window
			tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
		    ENDIF
		    IF res EQ -3 THEN BEGIN	; Free event
		    ENDIF
	    ENDIF ELSE tx_send_msg, txw_msg, 'You must load data first'
	    END
	2 : BEGIN			; Widget Plot Area 2 Event
	    IF tx_cur_buf EQ -1 THEN $
		tx_send_msg, txw_msg, 'Accu must not be empty' $
	    ELSE IF tx_history(tx_cur_buf) EQ 'Empty accumulator' THEN $
		tx_send_msg, txw_msg, 'Accu must not be empty'	    
	    IF tx_loaded EQ 0 THEN $
		tx_send_msg, txw_msg, 'You must load data first'
	    IF tx_loaded EQ 1 AND event.type EQ 0 AND tx_cur_buf NE -1 THEN $
		 IF tx_history(tx_cur_buf) NE 'Empty accumulator' THEN BEGIN
		    wset,txw_d2
		    CURSOR,xg,yg, /NoWait, /Data		    
		    res=sl_zoom(event.x,event.y,xd,yd)
		    CURSOR,xh,yh, /NoWait, /Data
		    IF res EQ 0 THEN BEGIN ; No zoom range, write coodinates
			txstr=nb2str(xh)+','+nb2str(yh)
			XYOUTS,xh,yh,txstr,/Noclip
		    ENDIF
		    IF res EQ 1 THEN BEGIN
			tx_xrange=[MIN([xg, xh]), MAX([xg, xh])]
			tx_yrange=[MIN([yg, yh]), MAX([yg, yh])]
			tx_update_ranges
			WIDGET_CONTROL, bad_id=i, txw_rg_but, /Set_button
			tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
		    ENDIF
		    IF res EQ -2 THEN BEGIN	; Remove point(s)
			removing_window=[MIN([xg, xh]), MAX([xg, xh]), $
					 MIN([yg, yh]), MAX([yg, yh])]
			tx_remove_points, tx_awnb, removing_window
			tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
			tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
		    ENDIF
		    IF res EQ -3 THEN BEGIN	; free event
		    ENDIF
	    ENDIF
	    END
	3 : BEGIN				; Left Slider event 
		WIDGET_CONTROL, bad_id=i, event.id, Get_Value=V
		tx_send_msg, txw_msg, 'Polarization #'+nb2str(V)+' selected'
	    END
	4 : BEGIN				; Right slider event
	    WIDGET_CONTROL, bad_id=i, event.id, Get_Value=nb		
	    IF (nb GT 0) AND (tx_cur_buf EQ -1) THEN BEGIN
		nb=0 & WIDGET_CONTROL, bad_id=i, event.id, Set_Value=0	    
	    ENDIF
	    IF (tx_cur_buf NE nb) THEN BEGIN
		tx_cur_buf=nb
		tx_send_msg, txw_history, tx_history(nb)
		WIDGET_CONTROL, bad_id=i, txw_slid_lab, Set_Value='Frame #'+nb2str(nb)
		IF (nb EQ 0) OR (tx_history(nb) EQ 'Empty accumulator') THEN BEGIN
		    WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=1
		    WSET, txw_d2 & ERASE
		    XYOUTS, 110, 100, 'Empty accumulator', /Device
		ENDIF ELSE BEGIN
		    res=tx_get_buffer(nb, tx_awnb)
		    IF STRPOS(tx_history(nb), 'combined') GE 0 THEN BEGIN
			WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=0
		    ENDIF ELSE BEGIN
			WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=1
		    ENDELSE
		    iii=nb
		    WHILE (tx_history(iii) NE 'Empty accumulator') DO iii=iii-1
		    iii=iii+1
		    WHILE  (iii NE nb+1) DO BEGIN
			IF (tx_history(iii) NE 'Empty accumulator') THEN BEGIN
			IF (STRPOS(tx_history(iii), 'stored')) GE 0 THEN oper='s'
			IF (STRPOS(tx_history(iii), 'added')) GE 0 THEN oper='+'
			IF (STRPOS(tx_history(iii), 'combined')) GE 0 THEN oper='c'
			tx_unscript, tx_history(iii), name, p2, p3, p4
			IF oper EQ 's' THEN tx_small_history=name ELSE $
			tx_small_history=tx_small_history+oper+name
			iii=iii+1
			ENDIF
		    ENDWHILE
		    other_tit(tx_awnb)=tx_small_history
		    strl=STRLEN(tx_small_history)
		    IF strl GT 60 THEN $
		    other_tit(tx_awnb)=STRMID(tx_small_history, 0, 30)+'...'+$
				       STRMID(tx_small_history, strl-30, strl)
		    tx_calculate_ranges, 2
		    tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
		ENDELSE
	    ENDIF
	    END
	5 : BEGIN				; ... event 
		see, 'free event 5, 5'
	    END
	6 : BEGIN				; ... event 
		see, 'free event 5, 6'
	    END
	ELSE : see, 'error de uvalue 5, x'
	ENDCASE
	END
     6	:  BEGIN				; *********** Parameters events
	   CASE uv2 OF
	1 : BEGIN			; Tolerance value modif.
		flag=0
		WIDGET_CONTROL, bad_id=i, event.id, Get_Value=v
		ON_IOERROR, nofix1
		tx_xtol=FLOAT(v)
		flag=1
		tx_send_msg, txw_msg, 'New X tolerance : '+v+'%'
	    	nofix1 : BEGIN
		IF flag EQ 0 THEN $
		    tx_send_err_msg, txw_msg, 'Cannot convert to float'
		END
	    END
	2 : BEGIN			; Temperature value modif.
	    WIDGET_CONTROL, bad_id=i, event.id, Get_Value=v
	    tx_send_msg, tx_msg, v+' is new Temperature'
	    END
	3 : BEGIN			; Toggle for Range
	    flag=0
	    IF event.Select EQ 1 THEN BEGIN
		WIDGET_CONTROL, bad_id=i, uv3, Get_Value=xminstr
		WIDGET_CONTROL, bad_id=i, uv4, Get_Value=xmaxstr
		WIDGET_CONTROL, bad_id=i, uv5, Get_Value=yminstr
		WIDGET_CONTROL, bad_id=i, uv6, Get_Value=ymaxstr
		tx_grant_w, tx_rwnb, x, y, w, n, e, tit, xtit, ytit, SubTit
		tx_grant_w, tx_awnb, xa, ya, wa, na, ea, tit, xtit, ytit, SubTit
		ON_IOERROR, nofix3
		IF xminstr(0) EQ '' THEN xmin=MIN([x, xa]) ELSE xmin=FLOAT(xminstr(0))
		IF xmaxstr(0) EQ '' THEN xmax=MAX([x, xa]) ELSE xmax=FLOAT(xmaxstr(0))
		IF yminstr(0) EQ '' THEN ymin=MIN([w, wa]) ELSE ymin=FLOAT(yminstr(0))
		IF ymaxstr(0) EQ '' THEN ymax=MAX([w, wa]) ELSE ymax=FLOAT(ymaxstr(0))
		flag=1
		tx_xrange=[xmin, xmax]
		tx_yrange=[ymin, ymax]
		tx_send_msg, txw_msg, 'Using selected ranges'
	    ENDIF ELSE BEGIN
		flag=1
		tx_calculate_ranges
		tx_send_msg, txw_msg, 'No range'
	    ENDELSE
	    nofix3 : BEGIN
		IF flag EQ 0 THEN $
		    tx_send_err_msg, txw_msg, 'Cannot convert to float'
		END
		tx_plot, txw_d2, tx_awnb, tx_xrange, tx_yrange
		tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
		tx_plot, txw_d1, tx_awnb, tx_xrange, tx_yrange, 1
	    END
	4 : BEGIN			; Unit MeV button event
	    IF tx_codes(tx_xsel) EQ 'EN  ' THEN BEGIN
		IF STRUPCASE(tx_unit) EQ 'THZ' THEN BEGIN
		    tx_grant_w, tx_rwnb, x, y, w, n, e, tit, xtit, ytit, SubTit
		    x=x*4.13547
		    x_tit(tx_rwnb)=tx_numor+' (meV)'
		    tx_update_w, tx_rwnb, x, y, w, n, e
		    tx_send_msg, txw_msg, 'Unit changed to meV'
		    tx_unit='meV'
		    tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
		ENDIF ELSE tx_send_msg, txw_msg, "Current unit isn't THz"
	    ENDIF
	    END
	5 : BEGIN			; Unit Thz button event
	    IF tx_codes(tx_xsel) EQ 'EN  ' THEN BEGIN
		IF STRUPCASE(tx_unit) EQ 'MEV' THEN BEGIN
		    tx_grant_w, tx_rwnb, x, y, w, n, e, tit, xtit, ytit, SubTit
		    x=x/4.13547
		    x_tit(tx_rwnb)=tx_numor+' (THz)'
		    tx_update_w, tx_rwnb, x, y, w, n, e
		    tx_send_msg, txw_msg, 'Unit changed to THz'
		    tx_unit='THz'
		    tx_plot, txw_d1, tx_rwnb, tx_xrange, tx_yrange
		ENDIF ELSE tx_send_msg, txw_msg, "Current unit isn't meV"
	    ENDIF
	    END
	6 : BEGIN			; Range is on
	    WIDGET_CONTROL, bad_id=i, txw_rg_but, /Set_button
	    ; Simulate range button set
	    tripx_event, {ID: txw_rg_but, TOP : txw_base, Handle: 0L, Select : 1}
	    END
	7 : BEGIN			; Unit field event
		ON_IOERROR, nofix4
		WIDGET_CONTROL, bad_id=i, event.id, Get_Value=v
		tx_monitor=FLOAT(v)
		nofix4 : BEGIN & END
	    END
	8 : BEGIN			; view parameters event
		par_base    =WIDGET_BASE(Title=tx_numor+' parameters')
		txt=par_txt(tx_rwnb, *)
		FOR i=0, N_ELEMENTS(txt)-1 DO BEGIN
		    IF STRMID(txt(i), STRLEN(txt(i))-1, 1) EQ '=' THEN $
			txt(i)=STRMID(txt(i), 0, STRLEN(txt(i))-1)
		ENDFOR
		par_text    =WIDGET_TEXT(par_base, Xsize=50, YSize=15, $
					 Value=txt, Font=ft_propor, $
					 /Scroll, Editable=0)
		WIDGET_CONTROL, bad_id=i, par_base, Group_Leader=txw_base, /Realize
	    END
	ELSE : see, 'error de uvalue 6, x'
	ENDCASE
	END
    ELSE : see, 'error de uvalue x, x'
    ENDCASE	
END

;******************************************************************************
PRO tripx, Three_Axis=three_axis ;*********************************************
;******************************************************************************
; Create the main Tripx Interface,  do initialisations and register tripx
; to Xmanager. This procedure generated widgets, their events are handled by
; the tripx_event procedure,  just above (I presume ;-)

@lamp.cbk			    ; call lamp.cbk common block
COMMON txc_main			    ; call back tripx common block
COMMON txc_wd_id		    ; call back plot area common block
COMMON txc_pb			    ; call back problem common block

IF XREGISTERED('tripx') le 0 THEN BEGIN	; if tripx isn't registered yet
    stradd=0				; Screen size dependent adjustement
    IF lamp_siz GT 900 THEN BEGIN
	xs=450
	ys=300
	strs=34
    ENDIF ELSE BEGIN
	IF lamp_siz GE 800 THEN BEGIN
	    xs=410
	    ys=280
	    strs=27
	ENDIF ELSE BEGIN
	    xs=350
	    ys=250    
	    strs=30
	    stradd=8
	ENDELSE
    ENDELSE
    tx_initialize			    ; Do initialisations
    tx_xsel	    =-1			    ; No x data axis is selected
    tx_ysel	    =-1			    ; No y data axis is selected
    tx_wsel	    =-1			    ; No w data axis is selected
    tx_nsel	    =-1	   		    ; No monitor axis is selected
    tx_xrange	    =[-1]		    ; Xrange is undefined
    tx_yrange	    =[-1]		    ; Yrange is undefined
    tx_update_w, tx_rwnb, [0], [0], [0], [0], [0]  ; Clear current workspace
    tx_old_codes=''
    tx_codes    =' '
    tx_eg_txt   =[-1]
    tx_old_eg_txt=[0]			    ;
    tx_monitor	=10000.0		    ; Default monitor value
    tx_numor    =''			    ; No file has been read
    tx_loaded   =0			    ; No file has been correctly loaded
    tx_factor	=1.0
    tx_xtol	=10.0
    txw_rg_but	=-1			    ; build buttons has not been done
    x_tit(tx_rwnb)=''			    ; Title initialize
    x_tit(tx_awnb)=''
    y_tit(tx_rwnb)=''	
    y_tit(tx_awnb)=''
    head_tit(tx_rwnb,0)=''
    head_tit(tx_awnb,0)='ACCUMULATOR'

    junk   = { CW_PDMENU_S, flags:0, name:'' }
    txw_base	=   WIDGET_BASE(/Column, $
		Title='Tripx_Face : The 3-axes LAMP interface 05/02/96', /Frame, $
		Resource_Name='lamp', /Map)
		
    filter =   ''
    IF KEYWORD_SET(Three_Axis) THEN BEGIN
	lamp_b1=txw_base
        tx_path=lamp_dvd+'hosts'+lamp_dvd+'serdon'+lamp_dvd+'illdata/data'+lamp_dvd
	path_for_online=tx_path
        IF lamp_dvd EQ ''  THEN begin tx_path=getenv('DI1')
    				      filter = '*.SCN;0'
				      ENDIF
    ENDIF ELSE BEGIN
        tx_path=path_for_online
    ENDELSE

;***    
    txw_base1	=   WIDGET_BASE(txw_base, /Row)
    base3x_0	=   WIDGET_BASE	    (txw_base1, /Column, /Frame,Resource_Name='mic')
    base3x_00	=   WIDGET_BASE	    (base3x_0, /Row, /Map)
    txw_done_but=   WIDGET_BUTTON   (base3x_00, Value='DONE', $
				     Font=ft_b_bigger, UValue=[-88, 357, 1, 1])   
    txw_tb_but	=   WIDGET_BUTTON   (base3x_00, Value='Touch', $
				     Font=ft_b_bigger,UValue=[-88, 357, 1, 2])
    txw_prt_but	=   WIDGET_BUTTON   (base3x_00, Value='Print', $
				     Font=ft_b_bigger,UValue=[-88, 357, 1, 0])
    txw_help_but=   WIDGET_BUTTON   (base3x_00, Value='HELP', $
				     Font=ft_b_bigger,UValue=[-88, 357, 1, 3])

    txw_pickbase=   WIDGET_BASE	    (base3x_0, /Column)
    ; ****************************** Widgets for file selection
    tx_files	=   ['No File']
    file	=   ''
    pick_fil, txw_pickbase, tx_path, filter, tx_files, file, 357, pfw_upd_but
;***
    ; ****************************** Widget base for axis selections
    bidon	=   WIDGET_LABEL    (txw_base1,Font=ft_propor,value='          ')
    txw_but_b   =   WIDGET_BASE	    (txw_base1,/column)
    txw_but_0   =   WIDGET_BASE	    (txw_but_b)
    txw_but_base=   WIDGET_DRAW	    (txw_but_0,xsize=525,ysize=280)
;   txw_but_base=   WIDGET_BASE	    (txw_but_0, /Map)
    IF stradd eq 0  THEN $
    bidon	=   WIDGET_LABEL    (txw_but_b,value='... 3-AXES GROUP ...',Font=ft_biggest)
    ; Set up a base for each row of buttons with data description and example
;***
    ; ****************************** The Label Widget to display messages
    midbase	=   WIDGET_BASE	    (txw_base, /Row,Resource_Name='did')
    txw_msg	=   WIDGET_TEXT	    (midbase, Frame=1, Font=ft_propor, $
				     Scroll=0, XSize=35+stradd, YSize=1)
    formula_base=   WIDGET_BASE	    (midbase, /Row, /Frame)
    txw_form_txt=   WIDGET_TEXT	    (formula_base, Font=ft_propor, /Edit, $
				     Value='Formula Entry field', XSize=strs+stradd)
    txw_do_form =   WIDGET_BUTTON   (formula_base, Font=ft_propor, Value='DO')
    WIDGET_CONTROL, bad_id=i, txw_form_txt, Set_UValue=[-88, 357, 1, 4, txw_form_txt]
    WIDGET_CONTROL, bad_id=i, txw_do_form,  Set_UValue=[-88, 357, 1, 4, txw_form_txt]
    txw_history	=   WIDGET_TEXT	    (midbase, Frame=1, Font=ft_propor, $
				     Value='', XSize=35+stradd, YSize=1)

    ; ****************************** The base for the two plotting fields
    base_3x_draw=   WIDGET_BASE	    (txw_base, /Row, /Map, UValue='',Resource_Name='don')

    ; ****************************** The first Widget Draw ...
    tx_d1_base	=   WIDGET_BASE	    (base_3x_draw, /Column,  Frame=2)
    tripx_draw1	=   WIDGET_DRAW	    (tx_d1_base, XSize=xs, YSize=ys, $
				     Button_Events=1,Retain=2,Frame=1, $
				     UValue=[-88, 357, 5, 1])
    txw_ltslider=   WIDGET_SLIDER   (tx_d1_base, XSize=xs, YSize=20, /Suppress_value, $
				     Value=1, Min=1, Max=2, UValue=[-88, 357, 5, 3])

    ; ****************************** Widgets for calculations
    control_base=   WIDGET_BASE	    (base_3x_draw, /Column)
    frame_base	=   WIDGET_BASE	    (control_base, /Column, /Frame)
    frame_bfct	=   WIDGET_BASE     (frame_base,   /row)
    txw_a_lab   =   WIDGET_LABEL    (frame_bfct, Font=ft_b_normal, Value='Factor')
    txw_afct_txt=   WIDGET_TEXT	    (frame_bfct, Font=ft_propor, /Edit, XSize=4, $
				     Value='1.0', UValue=[-88, 357, 4, 3])
    comb	=   WIDGET_BUTTON   (frame_base,Font=ft_b_normal, $
				     UValue=[-88, 357, 4, 1],Value = " Combine ")
    add		=   WIDGET_BUTTON   (control_base,Font=ft_b_normal, $
				     UValue=[-88, 357, 4, 2],Value = " add ")
    frame_base	=   WIDGET_BASE	    (control_base, /Column, /Frame)
    c_base	=   WIDGET_BASE	    (frame_base, /Row)
    txw_call_but=   WIDGET_BUTTON   (c_base, Font=ft_b_normal, Value='Reset', $
				     UValue=[-88, 357, 4, 4])   
    clear	=   WIDGET_BUTTON   (c_base, Font=ft_b_normal, Value=' New ', $
				     UValue=[-88, 357, 4, 5]) 
    frame_base	=   WIDGET_BASE	    (control_base, /Column, /Frame)
    txw_gf_base =   WIDGET_BASE	    (frame_base, /Row)
    txw_left_gk =   WIDGET_BUTTON   (txw_gf_base, Font=ft_b_normal, Value=' <-Fit',$
				     UValue=[-88, 357, 1, 6])
    txw_right_gk=   WIDGET_BUTTON   (txw_gf_base, Font=ft_b_normal, Value='Fit-> ',$
				     UValue=[-88, 357, 1, 7])
    dump_lab	=   WIDGET_LABEL    (control_base,Value="Save Accu in", Font=ft_b_normal)
    txw_dp_txt	=   WIDGET_TEXT	    (control_base,UValue=[-88, 357, 1, 5], $
				     Value='w1', /Edit, xs = 10)
    txw_slid_lab=   WIDGET_LABEL    (control_base, Font=ft_propor, Value="Frame #0")
    ne_base	=   WIDGET_BASE	    (control_base, /NonExclusive)
    txw_rg_but	=   WIDGET_BUTTON   (ne_base, Font=ft_b_normal, Value='Zoom')
    ; ****************************** End of widgets for calculations

    ; ****************************** ... The second Widget Draw.
    tx_d2_base	=   WIDGET_BASE	    (base_3x_draw, /Column,  Frame=2)    
    tripx_draw2	=   WIDGET_DRAW	    (tx_d2_base,XSize=xs,YSize=ys, $
				     Button_Events=1,Retain=2,Frame=1, $
				     UValue=[-88, 357, 5, 2])
    txw_rtslider=   WIDGET_SLIDER   (tx_d2_base, XSize=xs, YSize=20, /Drag, /Suppress_value, $
				     Value=0, Max=1, UValue=[-88, 357, 5, 4])
    bid=sys_dep('DYNLAB',txw_base,1)

    ; ****************************** Create tripx interface ...
    WIDGET_CONTROL, bad_id=i, txw_base, Group_Leader=lamp_b1, /Realize
    ; ****************************** ... and make post initialisations

    p_after_realize_did, 0, 0, 0

	   pixmap=0 & LOGO,pixmap
	   WIDGET_CONTROL, bad_id=i,txw_but_base,get_value=idw
	   !order=1 & wset,idw
	   if n_elements(pixmap) eq 1 then $
	   if lamp_siz  ge 800 then device,copy=[0,0,512,256,    0  ,  0  ,pixmap] $
		else device,copy=[0,0,300,150,(525-300)/2,(280-150)/2,pixmap]
	   !order=0
;
    IF KEYWORD_SET(Three_Axis) THEN IF inst_group NE '3Axes' THEN BEGIN
        inst_group='3Axes'
        inst_value='IN8'
    ENDIF
    tx_send_msg, txw_history, tx_history(0)
    
   ;WIDGET_CONTROL, bad_id=i, control_base, Sensitive=0
    WIDGET_CONTROL, bad_id=i, txw_ltslider, Sensitive=0
    WIDGET_CONTROL, bad_id=i, txw_rtslider, Sensitive=0
    txw_basec=comb
    WIDGET_CONTROL, bad_id=i, txw_basec, Sensitive=0
    ; Get drawable window indexes
    WIDGET_CONTROL, bad_id=i, tripx_draw1, GET_VALUE=txw_d1
    WIDGET_CONTROL, bad_id=i, tripx_draw2, GET_VALUE=txw_d2
    WSET, txw_d1 & ERASE
    XYOUTS, 140, 110, 'No data ...', /Device
    XYOUTS, 10,  40, 'W'+nb2str(tx_rwnb)+ $
	    ' is used here.', /Device
    XYOUTS, 10,  20, 'Use formula entry to store it into a LAMP W. ie : w1=w'+nb2str(tx_rwnb), /Device
    WSET, txw_d2 & ERASE
    XYOUTS, 110, 100, 'Empty accumulator', /Device
    XYOUTS, 10,  40, 'W'+nb2str(tx_awnb)+ $
	    ' is used here.', /Device
    XYOUTS, 10,  20, 'It can be automatically saved as W# set in "save as"',  /Device
    dummy=pick_fil_event({ID: pfw_upd_but, TOP : txw_base, Handle: 0L,  Select : 1}) 
    
    XMANAGER, 'TRIPX', txw_base, /Just_Reg, Cleanup='tx_end'
ENDIF ELSE BEGIN
    WIDGET_CONTROL, bad_id=i, txw_base, /Map    ; Show existing Tripx	
ENDELSE
END
