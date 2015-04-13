;-------------------------------------------------------------------------------
;*******************************************************************************

	PRO read_parameters, s

	V = DIAL_MAD_READ('t_para')
;------------------------------------------------------------------------------
; Setup parameter strings

	nparams = 52
	s = STRARR(nparams)
	s[0]     = STRTRIM(STRING(V.c_user),2)
	s[1]     = STRTRIM(STRING(V.c_lc),2)
	s[2]     = STRTRIM(STRING(V.c_txt),2)		
	s[3]     = STRTRIM(STRING(V.tof_cha_resol),2)
	s[4]     = STRTRIM(STRING(V.cha_width),2)
	s[5]     = STRTRIM(STRING(V.tof_delay),2)
	s[6]     = STRTRIM(STRING(V.wave),2)
	s[7]     = STRTRIM(STRING(V.icry),2)
	s[8]     = STRTRIM(STRING(V.preset_base),2)
	s[9]     = STRTRIM(STRING(V.preset_coef_z),2)
	s[10]    = STRTRIM(STRING(V.preset_coef_xyz),2)
	s[11:15] = STRTRIM(STRING(V.zcurrent[*]),2)
	s[16:20] = STRTRIM(STRING(V.xcurrent[*]),2)
	s[21:25] = STRTRIM(STRING(V.ycurrent[*]),2)
	s[26]    = STRTRIM(STRING(V.zpo_fli),2)
	s[27]    = STRTRIM(STRING(v.zpo_cor),2)
	s[28]    = STRTRIM(STRING(V.the_bob_dep),2)
	s[29]    = STRTRIM(STRING(v.the_bob_last),2)
	s[30]    = STRTRIM(STRING(V.the_bob_depx),2)
	s[31]    = STRTRIM(STRING(v.the_bob_lastx),2)
	s[32]    = STRTRIM(STRING(V.the_bob_depy),2)
	s[33]    = STRTRIM(STRING(v.the_bob_lasty),2)
	s[34]    = STRTRIM(STRING(V.the_bob2_dep),2)
	s[35]    = STRTRIM(STRING(v.the_bob2_last),2)
	s[36]    = STRTRIM(STRING(V.the_bob2_depx),2)
	s[37]    = STRTRIM(STRING(v.the_bob2_lastx),2)
	s[38]    = STRTRIM(STRING(V.the_bob2_depy),2)
	s[39]    = STRTRIM(STRING(v.the_bob2_lasty),2)
	s[40:42] = STRTRIM(STRING(V.bank1[*]),2)
	s[43:45] = STRTRIM(STRING(V.bank2[*]),2)
	s[46:48] = STRTRIM(STRING(V.bank3[*]),2)
	s[49:51] = STRTRIM(STRING(V.bank4[*]),2)

	FOR i = 0, nparams-1 DO BEGIN
		posdot = STRPOS(s[i],'.')
		pos00  = STRPOS(STRMID(s[i],posdot),'00')
		IF (pos00 NE -1) THEN s[i] = STRMID(s[i],0,pos00+posdot)
		last = STRLEN(s[i])
		IF STRPOS(s[i],'.') EQ last - 1 THEN s[i] = s[i]+'0'
		IF RSTRPOS(s[i],'0') EQ last - 1 THEN $
		IF STRPOS(s[i],'.') NE -1  THEN s[i] = STRMID(s[i],0,last-1)
	ENDFOR

	END

;-----------------------------------------------------------------------------
;*****************************************************************************

	PRO Set_Param_D7_event, ev

;-----------------------------------------------------------------------------

	WIDGET_CONTROL, ev.top, GET_UVALUE = info

	CASE ev.id OF

	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_2t0'): BEGIN
		a = STRARR(6)	
		FOR i = 0,5 DO BEGIN
			WIDGET_CONTROL, info.theta[0,i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par 2theta fli '+a[0]+a[1]+a[2]+a[3]+a[4]+a[5]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_2t1'): BEGIN
		a = STRARR(6)	
		FOR i = 0,5 DO BEGIN
			WIDGET_CONTROL, info.theta[1,i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par 2theta cor '+a[0]+a[1]+a[2]+a[3]+a[4]+a[5]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_2t2'): BEGIN
		a = STRARR(3)	
		FOR i = 0,2 DO BEGIN
			WIDGET_CONTROL, info.theta[2,i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par 2theta th1 '+a[0]+a[1]+a[2]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_2t3'): BEGIN
		a = STRARR(3)	
		FOR i = 0,2 DO BEGIN
			WIDGET_CONTROL, info.theta[3,i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par 2theta th2 '+a[0]+a[1]+a[2]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_2t4'): BEGIN
		a = STRARR(3)	
		FOR i = 0,2 DO BEGIN
			WIDGET_CONTROL, info.theta[4,i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par 2theta th3 '+a[0]+a[1]+a[2]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_2t5'): BEGIN
		a = STRARR(3)	
		FOR i = 0,2 DO BEGIN
			WIDGET_CONTROL, info.theta[5,i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par 2theta th4 '+a[0]+a[1]+a[2]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_s0'): BEGIN
		a = STRARR(2)	
		FOR i = 0,1 DO BEGIN
			WIDGET_CONTROL, info.setup[i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par name '+a[0]+a[1]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_s1'): BEGIN
		a = STRARR(2)	
		FOR i = 0,1 DO BEGIN
			WIDGET_CONTROL, info.setup[i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par name '+a[0]+a[1]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_s2'): BEGIN
		WIDGET_CONTROL, info.setup[2], GET_VALUE = dummy
		a = dummy+' '
		command = 'par mco '+a
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_c0'): BEGIN
		WIDGET_CONTROL, info.count[0], GET_VALUE = dummy
		a = dummy+' '
		command = 'par preset_b '+a
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_c1'): BEGIN
		WIDGET_CONTROL, info.count[1], GET_VALUE = dummy
		a = dummy+' '
		command = 'par preset_z '+a
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_c2'): BEGIN
		WIDGET_CONTROL, info.count[2], GET_VALUE = dummy
		a = dummy+' '
		command = 'par preset_xyz '+a
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_i0'): BEGIN
		WIDGET_CONTROL, info.inst[0], GET_VALUE = dummy
		a = FIX(dummy[0])
		command = 'par tof '+STRING(a)
		WIDGET_CONTROL, info.inst[0], SET_VALUE = STRTRIM(STRING(a),2)
		IF FIX(dummy[0]) EQ 1 THEN $
			FOR i = 1,2 DO WIDGET_CONTROL, info.inst[i], SENSITIVE = 0 $
		ELSE $
			FOR i = 1,2 DO WIDGET_CONTROL, info.inst[i], SENSITIVE = 1
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_i1'): BEGIN
		a = STRARR(3)	
		FOR i = 0,2 DO BEGIN
			WIDGET_CONTROL, info.inst[i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par tof '+a[0]+a[1]+a[2]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_i2'): BEGIN
		a = STRARR(3)	
		FOR i = 0,2 DO BEGIN
			WIDGET_CONTROL, info.inst[i], GET_VALUE = dummy
			a[i] = dummy+' '
		ENDFOR
		command = 'par tof '+a[0]+a[1]+a[2]
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_i3'): BEGIN
		WIDGET_CONTROL, info.inst[3], GET_VALUE = dummy
		a = dummy+' '
		command = 'par wav '+a
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_i4'): BEGIN
		WIDGET_CONTROL, info.inst[4], GET_VALUE = dummy
		a = FIX(dummy[0])
		command = 'par cryo '+STRING(a)
		WIDGET_CONTROL, info.inst[4], SET_VALUE = STRTRIM(STRING(a),2)
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_refresh'): BEGIN
		read_parameters, s
		FOR i = 0,2 DO WIDGET_CONTROL, info.setup[i], SET_VALUE = s[i]
		FOR i = 3,7 DO WIDGET_CONTROL, info.inst[i - 3], SET_VALUE = s[i]
		FOR i = 8,10 DO WIDGET_CONTROL, info.count[i - 8], SET_VALUE = s[i]
		null = ['','','']
		cpar = REFORM([s[11:27],null],5,4)
		FOR i = 0,4 DO FOR j = 0,3 DO IF (j LT 3) OR ((j EQ 3) AND (i LT 2)) THEN $
			WIDGET_CONTROL, info.bobs[j,i], SET_VALUE = cpar[i,j]
		tpar = REFORM([s[28:42],null,s[43:45],null,s[46:48],null,s[49:51],null], 6, 6)
		FOR i = 0,5 DO FOR j = 0,5 DO IF (j LT 2) OR ((j GE 2) AND (i LT 3)) THEN $
			WIDGET_CONTROL, info.theta[j,i], SET_VALUE = tpar[i,j]
		GOTO, finished
	    END
	    WIDGET_INFO(ev.top, FIND_BY_UNAME='d7p_exit'): BEGIN
		WIDGET_CONTROL, ev.top, /DESTROY
		GOTO, finished
	    END	

	    ELSE: GOTO, finished
	ENDCASE

; send command to MAD

	PRINT, 'Command sent to MAD: '+command
	dummy = DIAL_MAD_SEND('',0,command,'')
	
finished:

	END


;------------------------------------------------------------------------------
;******************************************************************************

	PRO Set_Param_D7

; Interactively reads and sets instrument parameters
; (perhaps to be bolted on to d7_status)
;
;						JRS 22/5/02
;------------------------------------------------------------------------------
;******************************************************************************

	COMMON c_lamp_font

	iprint = 0

	read_parameters, s

;-------------------------------------------------------------------------------
; Create Widgets

	d7p_base  = WIDGET_BASE(TITLE = 'D7 Parameters', UNAME = 'D7P_BASE', $
		   /COLUMN)
	d7p_row1  = WIDGET_BASE(d7p_base, /COLUMN, /ALIGN_CENTER)
	d7p_row11 = WIDGET_BASE(d7p_row1, /ROW, /ALIGN_CENTER)
	d7p_row12 = WIDGET_BASE(d7p_row1, /ROW, /ALIGN_CENTER)
	d7p_row2  = WIDGET_BASE(d7p_base, /ROW, /ALIGN_CENTER)
	d7p_setup = WIDGET_BASE(d7p_row11, FRAME = 1, /ROW)
	d7p_count = WIDGET_BASE(d7p_row11, FRAME = 1, /ROW)
	d7p_inst  = WIDGET_BASE(d7p_row12, FRAME = 1, /ROW)
	d7p_bobs  = WIDGET_BASE(d7p_row12, FRAME = 1, /ROW)
	d7p_2t    = WIDGET_BASE(d7p_row2,  FRAME = 1, /ROW)

;-------------------------------------------------------------------------------
; Setup base

	d7p_setup_l = INTARR(3)
	d7p_setup_b = INTARR(3)
	d7p_setup_d = INTARR(3)
	d7p_txt = ['User: ','Local Contact: ','Expt. Title: ']
	d7p_setup_label = WIDGET_LABEL(d7p_setup,VALUE = 'Setup: ', $
			  FONT = ft_b_bigger)
	d7p_setup_labs  = WIDGET_BASE(d7p_setup, /COLUMN)
	d7p_setup_boxes = WIDGET_BASE(d7p_setup, /COLUMN)
	d7p_setup_butts = WIDGET_BASE(d7p_setup, /COLUMN)
	FOR i = 0, 2 DO BEGIN
		d7p_setup_l[i]  = WIDGET_LABEL(d7p_setup_labs, VALUE = d7p_txt[i], $
				  XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, FONT = ft_normal, $
				  /ALIGN_RIGHT)
		d7p_setup_b[i]	= WIDGET_TEXT(d7p_setup_boxes, SCR_XSIZE = 200, SCR_YSIZE = 30, $
				  /EDITABLE, FONT = ft_propor, VALUE = s[i])
		d7p_setup_d[i]  = WIDGET_BUTTON(d7p_setup_butts, SCR_XSIZE = 30, SCR_YSIZE = 26, $
				  VALUE = 'Do',UNAME = 'd7p_s' + STRTRIM(STRING(i),2), $
				  FONT = ft_b_bigger, FRAME = 1)
	ENDFOR

;-------------------------------------------------------------------------------
; Instrument base

	d7p_inst_l = INTARR(5)
	d7p_inst_b = INTARR(5)
	d7p_inst_d = INTARR(5)
	d7p_txt = ['Time channels: ','Channel width: ','Delay: ',$
		   'Wavelength: ','Cryostat (0/1): ']
	d7p_inst_label = WIDGET_LABEL(d7p_inst, VALUE = 'Inst.: ', FONT = ft_b_bigger)
	d7p_inst_labs  = WIDGET_BASE(d7p_inst, /COLUMN)
	d7p_inst_boxes = WIDGET_BASE(d7p_inst, /COLUMN)
	d7p_inst_butts = WIDGET_BASE(d7p_inst, /COLUMN)
	FOR i = 0, 4 DO BEGIN
		d7p_inst_l[i]  = WIDGET_LABEL(d7p_inst_labs, VALUE = d7p_txt[i],$
				 XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, FONT = ft_normal, $
				 /ALIGN_RIGHT)
		d7p_inst_b[i]  = WIDGET_TEXT(d7p_inst_boxes, SCR_XSIZE = 100, SCR_YSIZE = 30, $
				 /EDITABLE, FONT = ft_propor, VALUE = s(i+3))
		d7p_inst_d[i]  = WIDGET_BUTTON(d7p_inst_butts, SCR_XSIZE = 30, SCR_YSIZE = 26, $
				 VALUE = 'Do', UNAME = 'd7p_i' + STRTRIM(STRING(i),2), $
				 FONT = ft_b_bigger, FRAME = 1)
	ENDFOR

;-------------------------------------------------------------------------------
; Count base

	d7p_count_l = INTARR(3)
	d7p_count_b = INTARR(3)
	d7p_count_d = INTARR(3)
	d7p_txt = ['preset_b: ','preset_z: ','preset_xyz: ']
	d7p_count_label = WIDGET_LABEL(d7p_count, VALUE = 'Count: ', FONT = ft_b_bigger)
	d7p_count_labs  = WIDGET_BASE(d7p_count, /COLUMN)
	d7p_count_boxes = WIDGET_BASE(d7p_count, /COLUMN)
	d7p_count_butts = WIDGET_BASE(d7p_count, /COLUMN)
	FOR i = 0, 2 DO BEGIN
		d7p_count_l[i]  = WIDGET_LABEL(d7p_count_labs, VALUE = d7p_txt[i], $
				  XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, FONT = ft_normal, $
				  /ALIGN_RIGHT)
		d7p_count_b[i]	= WIDGET_TEXT(d7p_count_boxes, SCR_XSIZE = 100, SCR_YSIZE = 30, $
				  /EDITABLE, FONT = ft_propor, VALUE = s(i+8))
		d7p_count_d[i]  = WIDGET_BUTTON(d7p_count_butts,SCR_XSIZE = 30,SCR_YSIZE = 26, $
				  VALUE = 'Do', UNAME = 'd7p_c' + STRTRIM(STRING(i),2), $
				  FONT = ft_b_bigger, FRAME = 1)
	ENDFOR

;-------------------------------------------------------------------------------
; Currents base

	d7p_bobs_l     = INTARR(5)		;no.of label lines
	d7p_bobs_l2    = INTARR(5)		;no of columns
	d7p_bobs_b     = INTARR(4,5)		;boxes table
	d7p_bobs_d     = INTARR(4)		;no. of do buttons
	d7p_bobs_boxes = INTARR(5)
	bpars	       = REFORM([s[11:27],'','',''],5,4)	;currents
	d7p_txt  = [' ','pcz: ','pcx: ','pcy: ','pczp: ']
	d7p_txt2 = ['b1','b2','b3','b4','b5']

	d7p_bobs_label    = WIDGET_LABEL(d7p_bobs, VALUE = 'Currents: ', FONT = ft_b_bigger)
	d7p_bobs_labs     = WIDGET_BASE (d7p_bobs, /COLUMN)
	FOR i=0,4 DO $
	d7p_bobs_boxes(i) = WIDGET_BASE (d7p_bobs, /COLUMN)
	d7p_bobs_butts    = WIDGET_BASE (d7p_bobs, /COLUMN)
	d7p_blank         = WIDGET_LABEL(d7p_bobs_butts, VALUE = ' ',XOFFSET = 1, $
			    YOFFSET = 3, SCR_YSIZE = 30)

	FOR i = 0, 4 DO BEGIN			;loop around rows

		d7p_bobs_l[i] = WIDGET_LABEL(d7p_bobs_labs, VALUE = d7p_txt[i], $
			        XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, $
			        FONT = ft_normal, /ALIGN_RIGHT)
		IF  i LT 4 THEN $
		d7p_bobs_d[i] = WIDGET_BUTTON(d7p_bobs_butts, $
		   		SCR_XSIZE = 30, SCR_YSIZE = 26, $
		   		VALUE = 'Do', UNAME = 'd7p_b' + STRTRIM(STRING(i),2), $
		   		FONT = ft_b_bigger, FRAME = 1)
				   
		FOR j = 0, 4 DO BEGIN		;loop around columns

			IF  i EQ 0 THEN $
			d7p_bobs_l2[j] = WIDGET_LABEL(d7p_bobs_boxes[j], VALUE = d7p_txt2[j], $
					 XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, $
					 FONT = ft_normal) $
			ELSE IF (i LT 4) OR ((i EQ 4) AND (j LT 2)) THEN $
			d7p_bobs_b[i-1,j] = WIDGET_TEXT(d7p_bobs_boxes[j], SCR_XSIZE = 50, $
					    SCR_YSIZE = 30, /EDITABLE, FONT = ft_propor, $
					    VALUE = bpars[j,i-1])
		ENDFOR
	ENDFOR

;-------------------------------------------------------------------------------
; 2theta base

	d7p_2t_l     = INTARR(8)		;no of label lines
	d7p_2t_l2    = INTARR(6)		;no of columns
	d7p_2t_l3    = INTARR(6)		;no of columns
	d7p_2t_b     = INTARR(6,6)		;boxes table
	d7p_2t_d     = INTARR(6)		;no.of do buttons
	d7p_2t_boxes = INTARR(6)
	null = ['','','']
	tpars = REFORM([s[28:42],null,s[43:45],null,s[46:48],null,s[49:51],null], 6, 6)
	d7p_txt  = [' ','Flipper: ','Corr.: ',' ','Bank1: ','Bank2: ','Bank3: ',$
		    'Bank4: ']
	d7p_txt2 = ['First Z','Last Z','First X','Last X','First Y','Last Y']
	d7p_txt3 = ['Start','End','Step',null]

	d7p_2t_label    = WIDGET_LABEL(d7p_2t, VALUE = '2Theta: ',FONT = ft_b_bigger)
	d7p_2t_labs     = WIDGET_BASE (d7p_2t, /COLUMN)
	FOR i=0,5 DO $
	d7p_2t_boxes(i) = WIDGET_BASE (d7p_2t, /COLUMN)
	d7p_2t_butts    = WIDGET_BASE (d7p_2t, /COLUMN)
	d7p_blank       = WIDGET_LABEL(d7p_2t_butts, VALUE = ' ', XOFFSET = 1, $
		          YOFFSET = 3, SCR_YSIZE = 30)

	FOR i = 0, 7 DO BEGIN			;loop around rows

		d7p_2t_l[i] = WIDGET_LABEL(d7p_2t_labs, VALUE = d7p_txt[i], $
			      XOFFSET = 1, YOFFSET = 3, SCR_YSIZE = 30, $
			      FONT = ft_normal, /ALIGN_RIGHT)
		IF i EQ 2 THEN $
		d7p_blank   = WIDGET_LABEL(d7p_2t_butts, VALUE = ' ', XOFFSET = 1, $
		    	      YOFFSET = 3, SCR_YSIZE = 30)
		IF (i LT 7) AND (i NE 2) THEN BEGIN
			IF i GT 2 THEN ii = i-1 ELSE ii = i
			d7p_2t_d(ii) = WIDGET_BUTTON(d7p_2t_butts, $
		   		       SCR_XSIZE = 30,SCR_YSIZE = 26, VALUE = 'Do', $
				       UNAME = 'd7p_2t' + STRTRIM(STRING(ii),2), $
		   		       FONT = ft_b_bigger, FRAME = 1)
		ENDIF

		FOR j = 0, 5 DO BEGIN		;loop around columns
			IF i EQ 0 THEN $
			d7p_2t_l2[j] = WIDGET_LABEL(d7p_2t_boxes[j], $
			   	       VALUE = d7p_txt2[j], XOFFSET = 1, YOFFSET = 3, $
				       SCR_YSIZE = 30,FONT = ft_normal) $
			ELSE IF i EQ 3 THEN $
			d7p_2t_l3[j] = WIDGET_LABEL(d7p_2t_boxes[j], $
				       VALUE = d7p_txt3[j],XOFFSET = 1, YOFFSET = 3, $
				       SCR_YSIZE = 30,FONT = ft_normal) $			
			ELSE IF (i LT 3) OR ((i GT 3) AND (j LT 3)) THEN BEGIN
			        IF i GT 3 THEN ii = i -1 ELSE ii = i
			        d7p_2t_b[ii-1,j] = WIDGET_TEXT(d7p_2t_boxes[j], $
						   SCR_XSIZE = 100, SCR_YSIZE = 30, /EDITABLE, $
						   FONT = ft_propor, VALUE = tpars[j,ii-1])
			ENDIF
		ENDFOR
	ENDFOR

;------------------------------------------------------------------------------
; Buttons
	
	d7p_refresh = WIDGET_BUTTON(d7p_row11, SCR_XSIZE = 80, $
		      VALUE = 'Update', FRAME = 1, FONT = ft_b_bigger, $
		      UNAME = 'd7p_refresh')
	d7p_exit    = WIDGET_BUTTON(d7p_base, SCR_XSIZE = 40, $
		      VALUE = 'Exit', FRAME = 1, FONT = ft_b_bigger, $
		      UNAME = 'd7p_exit')

;------------------------------------------------------------------------------
; Lauch interface and prepare info block

	WIDGET_CONTROL, d7p_base, /REALIZE
	info = {setup:d7p_setup_b, count:d7p_count_b, inst:d7p_inst_b, $
		theta:d7p_2t_b, bobs:d7p_bobs_b}
	WIDGET_CONTROL, d7p_base, SET_UVALUE = info
	IF FIX(s[3]) EQ 1 THEN BEGIN
		 WIDGET_CONTROL, d7p_inst_b(1), SENSITIVE = 0
		WIDGET_CONTROL, d7p_inst_b(2), SENSITIVE = 0
	ENDIF
	XMANAGER, 'Set_Param_D7', d7p_base, /JUST_REG

	END
