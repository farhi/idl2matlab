;==========================================================================================
;==========================================================================================
;                                 CREATING MIC-INTERFACE
;==========================================================================================
;==========================================================================================

PRO P_MIC_CREATE, base_mic , just

@lamp.cbk
@mics.cbk
    
;========================================================================================== 
;     INITIALISATION
;========================================================================================== 
           
      command            = STRARR(1)     & command(0) = ''
      norm_value         = 'No Normalization'
      choice_access_data = 1
      flag_acces         = -1
      cycle              = 'On_Line'
      
      aline = STRARR(1) & iline = STRARR(1) & fline = STRARR(1) & sline = STRARR(1)
      aline(0)      = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' + $
                      'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
      fline(0)      = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' + $
                      'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
      iline(0)      = 'IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII' + $
                      'IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII'
      sline(0)      = 'SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS' + $
                      'SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS'
                      
      nx_global    = 0
      ny_global    = 0
      nn_global    = 0
      
      index2       = LONG(0)
      n_wk_max     = LONG(5)
      n_rk_max     = LONG(5)
      nwk_select   = LONG(n_wk_max+1)
      swk_select   = STRTRIM(STRING(nwk_select),2)
      n_operator   = n_wk_max * (n_rk_max-1)
      n_numor      = n_wk_max *  n_rk_max
      nmax_num     = LONG(100)
      nmax_num_wk  = LONG(100)
      nerr_max     = LONG(100)
      npar_sas     = LONG(17)
      n_wk_tot     = LONG(23)

      flag_norm    = INTARR(n_wk_tot)             & flag_norm(*)     =  0
      par_norm     = LONG(64)
      f_norm_tof   = 1.0E5
      sf_norm_tof  = STRTRIM(STRING(f_norm_tof),2)
      f_norm_bck   = 1.0E4
      sf_norm_bck  = STRTRIM(STRING(f_norm_bck),2)
                  
      lab_wkp_8    = LONARR(n_wk_max)
      keep_id      = LONARR(n_wk_max,2*n_rk_max-1)

      list_numor   = STRARR(n_wk_max,n_rk_max)    & list_numor(*,*)  = 'No numor'
      list_numint  = INTARR(n_wk_max,n_rk_max)    & list_numint(*,*) =  0
      list_oper    = STRARR(n_wk_max,n_rk_max-1)  & list_oper(*,*)   = ''
      code_oper    = INTARR(n_wk_max,n_rk_max-1)  & code_oper(*,*)   = -1
      code_op_op   = INTARR(n_wk_max,n_rk_max-1)  & code_op_op(*,*)  = -1
      code_numor   = INTARR(n_wk_max,n_rk_max)    & code_numor(*,*)  =  0
      list_path    = STRARR(n_wk_max,n_rk_max)    & list_path(*,*)   = 'Not used'

      nw_op             = INTARR(5,n_operator)    & nw_op(*,*)   = -1
      npos_op           = INTARR(5,n_operator)    & npos_op(*,*) = -1
      nw_oper           = INTARR(n_operator)      & nw_oper(*)   = -1
      npos_oper         = INTARR(n_operator)      & npos_oper(*) = -1

      nw_num            = INTARR(n_numor)         & nw_num(*)    = -1    
      npos_num          = INTARR(n_numor)         & npos_num(*)  = -1
      nb_num_wk         = INTARR(n_wk_max)        & nb_num_wk(*) =  0
      nb_op_wk          = INTARR(n_wk_max)        & nb_op_wk(*)  =  0

      nw_op_wrong       = INTARR(n_operator)      & nw_op_wrong(*)       = -1
      nw_op_op_wrong    = INTARR(n_operator)      & nw_op_op_wrong(*)    = -1
      nw_num_id         = INTARR(n_numor)         & nw_num_id(*)         = -1
      npos_op_wrong     = INTARR(n_operator)      & npos_op_wrong(*)     = -1
      npos_op_op_wrong  = INTARR(n_operator)      & npos_op_op_wrong(*)  = -1
      npos_num_id       = INTARR(n_numor)         & npos_num_id(*)       = -1
      flag_bef_op       = INTARR(n_operator)      & flag_bef_op(*)       =  0
      flag_aft_op       = INTARR(n_operator)      & flag_aft_op(*)       =  0
      flag_both_op      = INTARR(n_operator)      & flag_both_op(*)      =  0
      nb_num_wk         = INTARR(n_wk_max)        & nb_num_wk(*)         =  0
      where_multi       = LONARR(n_wk_max)        & where_multi(*)       =  LONG(-1)
      numint_single     = INTARR(n_wk_max)        & numint_single(*)     =  0
      num_single        = STRARR(n_wk_max)        & num_single(*)        = 'No numor' 
      list_err_opnum    = STRARR(n_operator)      & list_err_opnum(*)    = 'No error detected'
      list_err_opop     = STRARR(n_operator)      & list_err_opop(*)     = 'No error detected'
      list_err_numid    = STRARR(n_operator)      & list_err_numid(*)    = 'No error detected'
      list_err_mult     = STRARR(n_wk_max)        & list_err_mult(*)     = 'No error detected'
      
      err1_wk           = INTARR(n_wk_max)             & err1_wk(*)           =  0
      err2_wk           = INTARR(n_wk_max)             & err2_wk(*)           =  0
      err3_wk           = INTARR(n_wk_max)             & err3_wk(*)           =  0
      err4_wk           = INTARR(n_wk_max)             & err4_wk(*)           =  0
      nb_num_tot        = INTARR(n_wk_max)             & nb_num_tot(*)        = -1
      list_numor_fin    = STRARR(nmax_num)             & list_numor_fin(*)    = 'No numor'
      list_numor_wk     = STRARR(n_wk_max,nmax_num_wk) & list_numor_wk(*,*)   = 'No numor'
      flag_list_fin     = INTARR(nmax_num)             & flag_list_fin(*)     =  0
      nb_andto          = INTARR(n_wk_max)             & nb_andto(*)          = -1 
      nb_and            = INTARR(n_wk_max)             & nb_and(*)            = -1
      nb_sumto          = INTARR(n_wk_max)             & nb_sumto(*)          = -1 
      nb_plus           = INTARR(n_wk_max)             & nb_plus(*)           = -1 
      nb_minus          = INTARR(n_wk_max)             & nb_minus(*)          = -1
      num_bef_andto     = INTARR(n_wk_max,n_rk_max-1)  & num_bef_andto(*,*)   =  0
      num_bef_and       = INTARR(n_wk_max,n_rk_max-1)  & num_bef_and(*,*)     =  0
      num_bef_sumto     = INTARR(n_wk_max,n_rk_max-1)  & num_bef_sumto(*,*)   =  0
      num_bef_plus      = INTARR(n_wk_max,n_rk_max-1)  & num_bef_plus(*,*)    =  0
      num_bef_minus     = INTARR(n_wk_max,n_rk_max-1)  & num_bef_minus(*,*)   =  0
      num_aft_andto     = INTARR(n_wk_max,n_rk_max-1)  & num_aft_andto(*,*)   =  0
      num_aft_and       = INTARR(n_wk_max,n_rk_max-1)  & num_aft_and(*,*)     =  0
      num_aft_sumto     = INTARR(n_wk_max,n_rk_max-1)  & num_aft_sumto(*,*)   =  0
      num_aft_plus      = INTARR(n_wk_max,n_rk_max-1)  & num_aft_plus(*,*)    =  0
      num_aft_minus     = INTARR(n_wk_max,n_rk_max-1)  & num_aft_minus(*,*)   =  0
      
      list_err_elmt     = STRARR(nerr_max)             & list_err_elmt(*)     = ''
      list_err_lambda   = STRARR(nerr_max)             & list_err_lambda(*)   = ''
      list_err_deltae   = STRARR(nerr_max)             & list_err_deltae(*)   = ''
      list_err_elpp     = STRARR(nerr_max)             & list_err_elpp(*)     = ''
      list_err_par      = STRARR(nerr_max)             & list_err_par(*)      = ''
      list_err_scan     = STRARR(nerr_max)             & list_err_scan(*)     = ''
      list_err_dopmin   = STRARR(nerr_max)             & list_err_dopmin(*)   = ''
      list_err_dopmax   = STRARR(nerr_max)             & list_err_dopmax(*)   = ''
      list_err_mono     = STRARR(nerr_max)             & list_err_mono(*)     = ''
      list_err_anal     = STRARR(nerr_max)             & list_err_anal(*)     = ''
      list_err_defl     = STRARR(nerr_max)             & list_err_defl(*)     = ''
      list_err_coefmono = STRARR(nerr_max)             & list_err_coefmono(*) = ''
      list_err_xval     = STRARR(nerr_max)             & list_err_xval(*)     = ''
      pc1               = FLTARR(npar_sas)             & pc1(*)               = 0.0
      pc2               = FLTARR(npar_sas)             & pc2(*)               = 0.0
      
      string_norm       = STRARR(n_wk_tot)             & string_norm(*)          = 'No norm.'
      string_hist_num   = STRARR(n_wk_max,n_rk_max)    & string_hist_num(*,*)    = ''
      string_hist_oper  = STRARR(n_wk_max,n_rk_max-1)  & string_hist_oper(*,*)   = ''
      string_hist       = STRARR(n_wk_max)             & string_hist(*)          = ''
      nstr_num          = INTARR(n_wk_max)             & nstr_num(*)             = -1
      nstr_oper         = INTARR(n_wk_max)             & nstr_oper(*)            = -1
      
      snumor            = ''
      substr1           = ''
      substr2           = ''
      nerr_elements     = -1      
      nerr_lambda       = -1      
      nerr_deltae       = -1      
      nerr_elpp         = -1
      nerr_par          = -1      
      nerr_scan         = -1      
      nerr_dopmin       = -1      
      nerr_dopmax       = -1      
      nerr_mono         = -1      
      nerr_anal         = -1      
      nerr_defl         = -1      
      nerr_coefmono     = -1      
      nerr_xval         = -1      
      ntot_num          = LONG(-1)
      
      tol_lambda        = 0.01
      tol_deltae        = 0.01
      tol_elpp          = 5
      tol_par           = 0.01      
      tol_doppler       = 0.01      
      tol_mono          = 0.0001      
      tol_anal          = 0.0001      
      tol_defl          = 0.0001      
      tol_coefmono      = 0.0001      
      tol_xval          = 0.0001      
      tol_pc            = FLTARR(npar_sas)
      tol_pc(0)         = 0.01
      tol_pc(1)         = 0.005
      tol_pc(2)         = 0.005
      tol_pc(3)         = 0.005
      tol_pc(4)         = 0.005
      tol_pc(5)         = 0.005
      tol_pc(6)         = 0.005
      tol_pc(7)         = 0.005
      tol_pc(8)         = 0.005
      tol_pc(9)         = 0.01
      tol_pc(10)        = 0.01
      tol_pc(11)        = 0.01
      tol_pc(12)        = 0.01
      tol_pc(13)        = 0.01
      tol_pc(14)        = 0.01
      tol_pc(15)        = 0.01
      tol_pc(16)        = 0.01
      
      w1_0 = 0    &  w1_1 = 0    &  w1_2 = 0    &  w1_3 = 0    &  w1_4 = 0
      w2_0 = 0    &  w2_1 = 0    &  w2_2 = 0    &  w2_3 = 0    &  w2_4 = 0
      w3_0 = 0    &  w3_1 = 0    &  w3_2 = 0    &  w3_3 = 0    &  w3_4 = 0
      w4_0 = 0    &  w4_1 = 0    &  w4_2 = 0    &  w4_3 = 0    &  w4_4 = 0
      w5_0 = 0    &  w5_1 = 0    &  w5_2 = 0    &  w5_3 = 0    &  w5_4 = 0
      
      x1_0 = 0    &  x1_1 = 0    &  x1_2 = 0    &  x1_3 = 0    &  x1_4 = 0
      x2_0 = 0    &  x2_1 = 0    &  x2_2 = 0    &  x2_3 = 0    &  x2_4 = 0
      x3_0 = 0    &  x3_1 = 0    &  x3_2 = 0    &  x3_3 = 0    &  x3_4 = 0
      x4_0 = 0    &  x4_1 = 0    &  x4_2 = 0    &  x4_3 = 0    &  x4_4 = 0
      x5_0 = 0    &  x5_1 = 0    &  x5_2 = 0    &  x5_3 = 0    &  x5_4 = 0

      y1_0 = 0    &  y1_1 = 0    &  y1_2 = 0    &  y1_3 = 0    &  y1_4 = 0
      y2_0 = 0    &  y2_1 = 0    &  y2_2 = 0    &  y2_3 = 0    &  y2_4 = 0
      y3_0 = 0    &  y3_1 = 0    &  y3_2 = 0    &  y3_3 = 0    &  y3_4 = 0
      y4_0 = 0    &  y4_1 = 0    &  y4_2 = 0    &  y4_3 = 0    &  y4_4 = 0
      y5_0 = 0    &  y5_1 = 0    &  y5_2 = 0    &  y5_3 = 0    &  y5_4 = 0
      
      p1_0 = 0.0  &  p1_1 = 0.0  &  p1_2 = 0.0  &  p1_3 = 0.0  &  p1_4 = 0.0
      p2_0 = 0.0  &  p2_1 = 0.0  &  p2_2 = 0.0  &  p2_3 = 0.0  &  p2_4 = 0.0
      p3_0 = 0.0  &  p3_1 = 0.0  &  p3_2 = 0.0  &  p3_3 = 0.0  &  p3_4 = 0.0
      p4_0 = 0.0  &  p4_1 = 0.0  &  y4_2 = 0.0  &  p4_3 = 0.0  &  p4_4 = 0.0
      p5_0 = 0.0  &  p5_1 = 0.0  &  p5_2 = 0.0  &  p5_3 = 0.0  &  p5_4 = 0.0
            
      where_oper        = LONARR(n_operator)      & where_oper     = LONG(-1)
      where_andto       = LONARR(n_operator)      & where_andto    = LONG(-1)
      where_and         = LONARR(n_operator)      & where_and      = LONG(-1)
      where_sumto       = LONARR(n_operator)      & where_sumto    = LONG(-1)
      where_plus        = LONARR(n_operator)      & where_plus     = LONG(-1)
      where_minus       = LONARR(n_operator)      & where_minus    = LONG(-1)
      where_null        = LONARR(n_operator)      & where_null     = LONG(-1)
      where_num         = LONARR(n_numor)         & where_num      = LONG(-1)
      
      cnt_oper          = LONG(0)
      cnt_numor         = LONG(0)
      cnt_andto         = LONG(0)
      cnt_and           = LONG(0)
      cnt_sumto         = LONG(0)
      cnt_plus          = LONG(0)
      cnt_minus         = LONG(0)
      cnt_null          = LONG(0)
      cnt_num_multi     = LONG(0)
            
      n_op_wrong        = 0
      n_op_op_wrong     = 0
      n_num_id          = 0
      n_err_mult        = 0
        
      text_help_op      = STRARR(6)
      text_help_op(0)   = 'To perform operations'
      text_help_op(1)   = 'between workspaces'
      text_help_op(2)   = 'please pull down the'
      text_help_op(3)   = '"OPR" button'
      text_help_op(4)   = '------------------'
      text_help_op(5)   = '------------------'
                
      oper_pdm   = ['"OPR ?"{', $
                    '"PLUS"', '"MINUS"', '"AND"', '"ANDTO"', '"SUMTO"', '"CANCEL"', '}']
                                      
;	------------------------------
;	Setting Default for Instrument
;	------------------------------
	user = '' & host = ''
	user = sys_dep      ('GETENV','USER')
        host = sys_dep      ('GETENV','HOST')
        if host eq '' then host= GETENV('SYS$NODE')
	
	inst_value ='?Inst?'
	IF (host EQ 'in5sgi')    THEN inst_value = 'IN5'
	IF (host EQ 'in6sgi')    THEN inst_value = 'IN6'
	IF (host EQ 'in10sgi')   THEN inst_value = 'IN10'
	IF (host EQ 'd7sgi')     THEN inst_value = 'D7'
	IF (host EQ 'd19sgi')    THEN inst_value = 'D19'
	IF (host EQ 'db21sgi')   THEN inst_value = 'DB21'
	IF (host EQ 'd16sgi')    THEN inst_value = 'D16'
	
	IF (host EQ 'qaz') or (host EQ 'lamp') $
			   or (host EQ 'tofi') THEN BEGIN inst_value = 'IN5'
	 IF (user EQ 'dianoux')  THEN inst_value = 'IN6'
	 IF (user EQ 'bee')      THEN inst_value = 'IN6'
	 IF (user EQ 'girard')   THEN inst_value = 'IN6'
	 IF (user EQ 'kearley')  THEN inst_value = 'IN5'
	 IF (user EQ 'muttka')   THEN inst_value = 'IN4'
	 IF (user EQ 'ferrand')  THEN inst_value = 'IN5'
	 IF (user EQ 'frick')    THEN inst_value = 'IN16'
	 IF (user EQ 'cook')     THEN inst_value = 'IN5'
	 IF (user EQ 'richard')  THEN inst_value = 'IN5'
	 IF (user EQ 'lartigue') THEN inst_value = 'IN15'
	 IF (user EQ 'in15') 	 THEN inst_value = 'IN15'
	 IF (user EQ 'andersen') THEN inst_value = 'D7'
	 IF (user EQ 'murani')   THEN inst_value = 'D7'
	 IF (user EQ 'scharpf')  THEN inst_value = 'D7'
	ENDIF
	IF (host EQ 'lass1') or (host EQ 'lass2') THEN BEGIN inst_value = 'D22'
	 IF (user EQ 'may')      THEN inst_value = 'D22'
	 IF (user EQ 'lindner')  THEN inst_value = 'D11'
	 IF (user EQ 'cubitt')   THEN inst_value = 'D22'
	 IF (user EQ 'timmins')  THEN inst_value = 'D11'
	 IF (user EQ 'zaccai')   THEN inst_value = 'D16'	 
        ENDIF 

;	-----------------
;	Setting Pathnames
;	-----------------
	cd,current=mee
        path_for_spectra = ''
        path_for_current = ''
        path_for_test    = ''


       dat1_pdm   = ['"Instrument" {', $
                     '"D7" ',  $
                     '"D11" ', $
                     '"D16" ', $                     
                     '"D17" ', $
                     '"D22" ', $
                     '"IN4" ', $                   
                     '"IN5" ', $
                     '"IN6" ', $
                     '"IN10" ', $                     
                     '"IN13" ', $                     
                     '"IN16" ', '}']
                           
      dat2_pdm    = ['" Database" {', $
                     '"D7" ',  $
                     '"D11" ', $
                     '"D16" ', $                     
                     '"D17" ', $
                     '"D22" ', $
                     '"IN4" ', $
                     '"IN5" ', $
                     '"IN6" ', $                     
                     '"IN10" ',$                     
                     '"IN13" ',$                     
                     '"IN16" ', '}']
                                        
      dat3_pdm    = ['" Spectra "{', $
                     '"D7" ',  $
                     '"D11" ', $
                     '"D16" ', $                     
                     '"D17" ', $
                     '"D22" ', $
                     '"IN4" ', $                   
                     '"IN5" ', $
                     '"IN6" ', $
                     '"IN10" ',$                     
                     '"IN13" ',$                     
                     '"IN16" ', '}']
                    
      dat4_pdm   = ['" INX "{', $
                    '"IN4" ', $                   
                    '"IN5" ', $
                    '"IN6" ', '}']
                    
      dat5_pdm   = ['"Tests"{', $
                    '"IN15" ',  $
                    '"D19" ', $                   
                    '"DB21" ', $
                    '"D11-TOF" ', '}']

      dat6_pdm   = ['" Database " {', $
                    '"D7" ',  $
                    '"D11" ', $
                    '"D16" ', $                     
                    '"D17" ', $
                    '"D22" ', $
                    '"IN4" ', $
                    '"IN5" ', $
                    '"IN6" ', $                     
                    '"IN10" ', $                     
                    '"IN13" ', $                     
                    '"IN16" ', '}']
                    
      text_help_data    = STRARR(6)
      text_help_data(0) = '"Current Cycle" is present on '+host+' and'
      text_help_data(1) = 'on the central database serdon:/data/'+strlowcase(inst_value)
      text_help_data(2) = '"Previous Cycle" can be linked to'
      text_help_data(3) = 'serdon:/data-1/'+strlowcase(inst_value)
      text_help_data(4) = 'Use "SPECTRA" program for older data.'
      text_help_data(5) = '--------------------'
		                                              
;==========================================================================================
;==========================================================================================
ok=1
IF n_elements(just) GT 0 THEN IF just EQ 'just' THEN ok=0
IF ok EQ 1 then begin

    base_main    = WIDGET_BASE  (base_mic,     /COLUMN,SPACE = 10)    
    label_main   = WIDGET_LABEL (base_main,     VALUE = 'DATA  ACCESS', FONT = ft_biggest)
    base_fmt_1   = WIDGET_BASE  (base_main,    /ROW   ,SPACE = 20)
    
    base_fmt_1_1 = WIDGET_BASE  (base_fmt_1,   /COLUMN)
    base_fmt_2_0 = WIDGET_BASE  (base_fmt_1,   /COLUMN)
    base_fmt_2_0 = WIDGET_BASE  (base_fmt_2_0, /COLUMN)
    base_fmt_2_5 = WIDGET_BASE  (base_fmt_1,   /COLUMN)
    base_fmt_2_5 = WIDGET_BASE  (base_fmt_2_5, /COLUMN)
    
    if LAMP_SIZ ge 1000 then begin
     base_fmt_1_4= WIDGET_BASE  (base_fmt_1,   /ROW, /FRAME)
     base_fmt_2_6= WIDGET_BASE  (base_fmt_1_4, /COLUMN)
     base_fmt_1_6= WIDGET_BASE  (base_fmt_1_4, /COLUMN)
    endif
    
    base_wkp_1   = WIDGET_BASE  (base_main,    /FRAME , /COLUMN)    
    label_wkp_1  = WIDGET_LABEL (base_wkp_1, VALUE = 'INSTRUMENT  : '         ,FONT = ft_b_bigger)
    
    label_fmt_3  = WIDGET_LABEL (base_fmt_1_1, VALUE = text_help_data(0)      ,FONT = ft_normal)
    label_fmt_4  = WIDGET_LABEL (base_fmt_1_1, VALUE = text_help_data(1)      ,FONT = ft_normal)
    label_fmt_5  = WIDGET_LABEL (base_fmt_1_1, VALUE = text_help_data(2)      ,FONT = ft_normal)
    label_fmt_6  = WIDGET_LABEL (base_fmt_1_1, VALUE = text_help_data(3)      ,FONT = ft_normal)
;   label_fmt_8  = WIDGET_LABEL (base_fmt_1_1, VALUE = text_help_data(4)      ,FONT = ft_normal)
    label_fmt_9  = WIDGET_LABEL (base_fmt_1_1, VALUE = text_help_data(5)      ,FONT = ft_normal)
    label_fmt_7  = WIDGET_LABEL (base_fmt_1_1, VALUE = 'Access Path to Data ?',FONT = ft_b_normal)
    text_fmt_1   = WIDGET_TEXT  (base_fmt_1_1, /EDITABLE, XSIZE=25, YSIZE=1   ,FONT = ft_normal)
    WIDGET_CONTROL, text_fmt_1,  BAD_ID = I, SET_UVALUE = [-88,102,0,0,0,0,0,0,0]
    
    uv_pd_dat1   = [-88,150,0,0,0,label_fmt_7,label_wkp_1,text_fmt_1,0]                
    uv_pd_dat2   = [-88,151,0,0,0,label_fmt_7,label_wkp_1,text_fmt_1,0]
    uv_pd_dat3   = [-88,152,0,0,0,label_fmt_7,label_wkp_1,text_fmt_1,0]
    uv_pd_dat4   = [-88,153,0,0,0,label_fmt_7,label_wkp_1,text_fmt_1,0]
    uv_pd_dat5   = [-88,154,0,0,0,label_fmt_7,label_wkp_1,text_fmt_1,0]
    uv_pd_dat6   = [-88,155,0,0,0,label_fmt_7,label_wkp_1,text_fmt_1,0]
    
    label_2_0    = WIDGET_LABEL (base_fmt_2_0, VALUE = 'Current Cycle'        ,FONT = ft_b_normal) 
    label_trait  = WIDGET_LABEL (base_fmt_2_0, VALUE = '-------------' ,FONT = ft_normal) 
    LAMP_PDM, dat1_pdm, FONT = ft_propor, base_fmt_2_0, LAMP_PDM_UVALUE = uv_pd_dat1
    LAMP_PDM, dat6_pdm, FONT = ft_propor, base_fmt_2_0, LAMP_PDM_UVALUE = uv_pd_dat6        
    label_2_5    = WIDGET_LABEL (base_fmt_2_5, VALUE = 'Previous Cycle'       ,FONT = ft_b_normal) 
    label_trait  = WIDGET_LABEL (base_fmt_2_5, VALUE = '--------------'   ,FONT = ft_normal) 
    LAMP_PDM, dat2_pdm, FONT = ft_propor, base_fmt_2_5, LAMP_PDM_UVALUE = uv_pd_dat2 
    LAMP_PDM, dat3_pdm, FONT = ft_propor, base_fmt_2_5, LAMP_PDM_UVALUE = uv_pd_dat3
    
    if LAMP_SIZ ge 1000 then begin
     label_2_6   = WIDGET_LABEL (base_fmt_2_6, VALUE = 'Other Data'           ,FONT = ft_b_normal) 
     label_trait = WIDGET_LABEL (base_fmt_2_6, VALUE = '----------'   ,FONT = ft_normal) 
     LAMP_PDM, dat4_pdm, FONT = ft_propor, base_fmt_2_6, LAMP_PDM_UVALUE = uv_pd_dat4 
     LAMP_PDM, dat5_pdm, FONT = ft_propor, base_fmt_2_6, LAMP_PDM_UVALUE = uv_pd_dat5 

     label_1_6_0 = WIDGET_LABEL (base_fmt_1_6, VALUE = 'WK_Selection &'       ,FONT = ft_b_normal) 
     label_1_6_1 = WIDGET_LABEL (base_fmt_1_6, VALUE = 'Normalization'        ,FONT = ft_b_normal) 
     slid_fmt_1_6= WIDGET_SLIDER(base_fmt_1_6, TITLE = 'WK_SPACE n'           ,FONT = ft_normal, $
                                 MAXIMUM= 20 , MINIMUM = n_wk_max+1, VALUE = n_wk_max+1)
     base_fmt_1_7= WIDGET_BASE  (base_fmt_1_6,/NONEXCLUSIVE)
     but_fmt_17  = WIDGET_BUTTON(base_fmt_1_7, VALUE = "Normalization"        ,FONT = ft_normal)
     WIDGET_CONTROL, slid_fmt_1_6,BAD_ID = I, SET_UVALUE = [-88,106,0,0,0,0,0,0,0]                           
     WIDGET_CONTROL, but_fmt_17,  BAD_ID = I, SET_UVALUE = [-88,194,0,slid_fmt_1_6,0,0,0,0,0]
    endif
                                     
;==========================================================================================
;==========================================================================================

    if LAMP_SIZ ge 1000 then base_width = 830 else base_width=550

    base_wkp_2     = WIDGET_BASE  (base_wkp_1,   /COLUMN, SPACE = 10)         
    base_wkp_3     = WIDGET_BASE  (base_wkp_2,   /FRAME, /SCROLL, /ROW, $
                                   X_SCROLL_SIZE=base_width, Y_SCROLL_SIZE=250)
    base_wkp_3_1   = WIDGET_BASE  (base_wkp_2,   /ROW   , SPACE=((base_width-610)/2)>5, /FRAME)                                                                
    base_wkp_4     = WIDGET_BASE  (base_wkp_3_1, /COLUMN, SPACE=5)
    base_wkp_5     = WIDGET_BASE  (base_wkp_3_1, /COLUMN)
    base_wkp_6     = WIDGET_BASE  (base_wkp_3_1, /COLUMN)
    
    lab_wkp_4      = WIDGET_LABEL (base_wkp_4, FONT  = ft_b_bigger,VALUE = 'About Parameters')     
    list_wkp_4     = WIDGET_LIST  (base_wkp_4, VALUE = list_numor(0:n_wk_max-1, 0:n_rk_max-1), $
                                   YSIZE = 3 , FONT  = ft_normal)
                                 
    FOR nw = 0, n_wk_max-1 DO BEGIN
     base_wkp_7    = WIDGET_BASE  (base_wkp_3, /COLUMN)
     base_wkp_8    = WIDGET_BASE  (base_wkp_7, /COLUMN)
     lab_wkp_8(nw) = WIDGET_LABEL (base_wkp_8, FONT = ft_b_normal, $
                                   VALUE = 'Workspace ' + STRTRIM(STRING(nw+1),2))     
     base_wkp_9    = WIDGET_BASE  (base_wkp_7, /COLUMN, /FRAME)
                                          
     FOR npos = 0, n_rk_max-2 DO BEGIN
      base_wkp_10  = WIDGET_BASE  (base_wkp_9,  /ROW)
      label_wkp_10 = WIDGET_LABEL (base_wkp_10, VALUE = 'Run  =', FONT = ft_b_normal)
      text_wkp_10  = WIDGET_TEXT  (base_wkp_10, /EDITABLE, XSIZE = 6, YSIZE = 1, FONT = ft_propor)
      WIDGET_CONTROL, text_wkp_10, BAD_ID = I,  SET_UVALUE = [-88,120,0,nw,npos,list_wkp_4,0,0,0]
      base_wkp_11  = WIDGET_BASE  (base_wkp_9,  /ROW)
      base_wkp_12  = WIDGET_BASE  (base_wkp_11)
      label_wkp_11 = WIDGET_LABEL (base_wkp_11, FONT = ft_normal, VALUE ='')
      uv_pd_wkp_12 = [-88,180,0,nw,npos,label_wkp_11,0,0,0]
      LAMP_PDM, oper_pdm, base_wkp_12, FONT = ft_smaller, LAMP_PDM_UVALUE = uv_pd_wkp_12
      keep_id(nw,2*npos)   = text_wkp_10
      keep_id(nw,2*npos+1) = label_wkp_11
     ENDFOR
     
     npos = n_rk_max-1
     base_wkp_13   = WIDGET_BASE  (base_wkp_9,  /ROW)
     label_wkp_13  = WIDGET_LABEL (base_wkp_13, VALUE = 'Run  =', FONT = ft_b_normal)
     text_wkp_13   = WIDGET_TEXT  (base_wkp_13, /EDITABLE, XSIZE = 6, YSIZE = 1, FONT = ft_propor)                  
     WIDGET_CONTROL, text_wkp_13, BAD_ID = I,   SET_UVALUE = [-88,120,0,nw,npos,list_wkp_4,0,0,0]
     keep_id(nw,2*n_rk_max-2) = text_wkp_13
    ENDFOR
    
    label_wkp_5    = WIDGET_LABEL (base_wkp_5  , VALUE = "  Normalization  ", FONT = ft_b_bigger)    
    base_wkp_5_2   = WIDGET_BASE  (base_wkp_5,   /ROW, /NONEXCLUSIVE)
    FOR nw = 0, n_wk_max-1 DO BEGIN
     butt_wkp_5    = WIDGET_BUTTON(base_wkp_5_2, VALUE = "W"+STRTRIM(STRING(nw+1),2), FONT = ft_normal)
     WIDGET_CONTROL, butt_wkp_5, BAD_ID = I,SET_UVALUE = [-88,197,0,nw,0,0,0,0,0]
    ENDFOR
    
    but_exit       = WIDGET_BUTTON(base_wkp_5  , VALUE = 'EXIT', FONT = ft_b_bigger,UVALUE=[-88,190,0])
    
    if sys_dep('MAP') ne -1 then $
    butt1_wkp_6    = WIDGET_BUTTON(base_wkp_6, VALUE = "UPDATE WKSP", FONT = ft_normal,$
    				               RESOURCE_NAME='red') else $
    butt1_wkp_6    = WIDGET_BUTTON(base_wkp_6, VALUE = "UPDATE WKSP", FONT = ft_normal)
    
    WIDGET_CONTROL, butt1_wkp_6, BAD_ID = I, SET_UVALUE = [-88,195,0,0,0,0,0,0,0]
    butt2_wkp_6    = WIDGET_BUTTON(base_wkp_6, VALUE = "RESET  WKSP", FONT = ft_normal)
    WIDGET_CONTROL, butt2_wkp_6, BAD_ID = I, SET_UVALUE = [-88,196,0,0,0,0,0,0,0]

    button_wkp_6   = WIDGET_BUTTON(base_wkp_6, VALUE = "About Operators", $ 
                                  FONT = ft_bigger)
    WIDGET_CONTROL, button_wkp_6, BAD_ID = I,  SET_UVALUE = [-88,101,0,0,0,0,0,0,0]
    
    SET_LAB_W1_BIS
    SET_LAB_WKP_8
    SET_LAB_WKP_5
    SET_TXT_FMT_1
ENDIF
            
RETURN
END

;==========================================================================================
;==========================================================================================
;                                 PROCEDURE P_MIC_EVENT
;==========================================================================================
;==========================================================================================

PRO  P_MIC_EVENT , event, uv

@lamp.cbk
@mics.cbk

;------------------------------------------------------------------------------------------
;    101  Help on Operators
;    **********************
        IF uv(1) eq 101 THEN HELP_OPERATOR, event
;------------------------------------------------------------------------------------------
;    102  Getting File_Pathname for Data read-in
;    *******************************************
        IF uv(1) eq 102 THEN BEGIN
         ENTRY_FMT_1, event
        ENDIF             
;------------------------------------------------------------------------------------------
;    106 Slider Control
;    ******************
        IF uv(1) eq 106 THEN GET_WK_VAL, event                
;------------------------------------------------------------------------------------------
;    120  Storing Datafiles in corresponding Workspaces
;    **************************************************
        IF uv(1) eq 120 THEN NUMOR_ENTRY, event , uv
;------------------------------------------------------------------------------------------
;    150/151/152/153/154/155  Changing label according to data format
;                             Getting  File_Pathname for Data read-in
;    ****************************************************************
        IF uv(1) eq 150 THEN BEGIN
         flag_acces = 0
         w_numor(nwk_select) = 'Current_Run'
         SET_LAB_FMT_7_A, event, uv
         SET_LAB_WKP_1,   event, uv
         INIT_FMT_1_A,    event, uv          
         SET_LAB_WKP_8
         TEST_FMT_1,      event, uv
         cycle = 'On_Line'
         P_MAC_LABINS
;        WIDGET_CONTROL, BAD_ID=I, event.top, MAP=0
         ENDIF
        
        IF uv(1) eq 151 THEN BEGIN
         flag_acces = 1
         SET_LAB_FMT_7_B, event, uv
         SET_LAB_WKP_1,   event, uv
         INIT_FMT_1_B,    event, uv 
         SET_LAB_WKP_8      
         P_MAC_LABINS
        ENDIF
                 
        IF uv(1) eq 152 THEN BEGIN
         flag_acces = 2
         SET_LAB_FMT_7_B, event, uv
         SET_LAB_WKP_1,   event, uv
         INIT_FMT_1_C,    event, uv         
         SET_LAB_WKP_8   
         TEST_FMT_1,      event, uv        
         P_MAC_LABINS        
        ENDIF
                 
        IF uv(1) eq 153 THEN BEGIN
         flag_acces = 3
         SET_LAB_FMT_7_C, event, uv
         SET_LAB_WKP_1,   event, uv
         INIT_FMT_1_D,    event, uv         
         SET_LAB_WKP_8   
         TEST_FMT_1,      event, uv        
         P_MAC_LABINS        
        ENDIF

        IF uv(1) eq 154 THEN BEGIN
         flag_acces = 4
         SET_LAB_FMT_7_C, event, uv
         SET_LAB_WKP_1,   event, uv
         INIT_FMT_1_D,    event, uv         
         SET_LAB_WKP_8        
         P_MAC_LABINS        
        ENDIF

        IF uv(1) eq 155 THEN BEGIN
         flag_acces = 5
         SET_LAB_FMT_7_B, event, uv
         SET_LAB_WKP_1,   event, uv
         INIT_FMT_1_B,    event, uv 
         SET_LAB_WKP_8      
         cycle = 'On_Line'
         P_MAC_LABINS
        ENDIF
;------------------------------------------------------------------------------------------
;    180  Echo of OPERATOR PDM
;    *************************
        IF uv(1) eq 180 THEN ECHO_PDM_OPER, event , uv
;------------------------------------------------------------------------------------------
;    190  Un_map  window
;    *******************
        IF uv(1) eq 190 THEN widget_control, event.top, map=0
;------------------------------------------------------------------------------------------
;    195  Updating Workspaces
;    ************************
        IF uv(1) eq 195 THEN UPDATE_OPR_NUM, event
;------------------------------------------------------------------------------------------
;    196  Reset on Workspaces
;    ************************
        IF uv(1) eq 196 THEN RESET_OPR_NUM, event, uv
;------------------------------------------------------------------------------------------
;    197  Normalisation of W1 to W(n_wk_max) (Database or Spectra)
;    *************************************************************
        IF uv(1) eq 197 THEN NORM, event, uv
;------------------------------------------------------------------------------------------
;    194  Normalisation of W(n_wk_max+1) to W20  (Test & Local Current Cycle)
;    ************************************************************************
        IF uv(1) eq 194 THEN NORM_BIS, event, uv
;------------------------------------------------------------------------------------------
;    198  Help Mounting
;    ******************
        IF uv(1) eq 198 THEN HELP_MOUNT, event
;------------------------------------------------------------------------------------------
;    199  Destroy window
;    *******************
        IF uv(1) eq 199 THEN widget_control, event.top, /destroy
;------------------------------------------------------------------------------------------
                	                                                               
        return
        end


;==========================================================================================
;==========================================================================================
;                                 PROCEDURE P_MIC_ACTIONS
;==========================================================================================
;==========================================================================================

PRO   HELP_OPERATOR, event

@lamp.cbk

;     ABOUT OPERATORS BUTTON
;     ----------------------

      help_wind_text = STRARR(37)

help_wind_text(0)  = '                                          HELP ON OPERATORS'
help_wind_text(1)  = '                                          ----------------------------------'
help_wind_text(2)  = ''
help_wind_text(3)  = '  To perform addition of numors 3321 and 3324, type the following sequence:'
help_wind_text(4)  = '     Data       =       3321     ...    in the desired workspace''s box'
help_wind_text(5)  = '     Operator =          +      ...    by simple click on the OPERATOR button'
help_wind_text(6)  = '     Data       =       3324     ...    in the desired workspace''s box'
help_wind_text(7)  = ''
help_wind_text(8)  = ''
help_wind_text(9)  = '  To substract numor 3321 to numor 3324, type the following sequence:'
help_wind_text(10) = '     Data       =       3324     ...    in the desired workspace''s box'
help_wind_text(11) = '     Operator =          -        ...    by simple click on the OPERATOR button'
help_wind_text(12) = '     Data       =       3321     ...    in the desired workspace''s box'
help_wind_text(13) = ''
help_wind_text(14) = ''
help_wind_text(15) = '  To perform summation from numor 3321 to numor 3324, type the following sequence:'
help_wind_text(16) = '     Data       =       3321     ...    in the desired workspace''s box'
help_wind_text(17) = '     Operator =     SUMTO   ...    by simple click on the OPERATOR button'
help_wind_text(18) = '     Data       =       3324     ...    in the desired workspace''s box'
help_wind_text(19) = ''
help_wind_text(20) = ''
help_wind_text(21) = '  To obtain a display of numor 3321 and numor 3324, type the following sequence:'
help_wind_text(22) = '     Data       =       3321     ...    in the desired workspace''s box'
help_wind_text(23) = '     Operator =       AND     ...    by simple click on the OPERATOR button'
help_wind_text(24) = '     Data       =       3324     ...    in the desired workspace''s box'
help_wind_text(25) = ''
help_wind_text(26) = ''
help_wind_text(27) = '  To obtain a display from numor 3321 to numor 3324, type the following sequence:'
help_wind_text(28) = '     Data       =       3321     ...    in the desired workspace''s box'
help_wind_text(29) = '     Operator =     ANDTO   ...    by simple click on the OPERATOR button'
help_wind_text(30) = '     Data       =       3324     ...    in the desired workspace''s box'
help_wind_text(31) = ''
help_wind_text(32) = ''
help_wind_text(33) = '  After typing the sequence of operations, click on:'
help_wind_text(34) = '     READ button           = Read numors/files - Storage of result'
help_wind_text(35) = '     READ & DISPLAY button = Read numors/files - Storage of result - Display'
help_wind_text(36) = ''


      help_lamp = WIDGET_BASE(TITLE='More Details About Operators',$
                              /COLUMN, RESOURCE_NAME='lampmic')
      commands  = WIDGET_BASE(help_lamp, /ROW)
   
      butt_exit = WIDGET_BUTTON(commands, VALUE='QUIT HELP', FONT = ft_bigger)
      
      help_wind = WIDGET_TEXT(help_lamp, XSIZE=80, YSIZE=20, /SCROLL, FONT = ft_bigger)
      
      WIDGET_CONTROL, help_lamp, GROUP_LEADER=lamp_mic, /REALIZE
      WIDGET_CONTROL, help_wind, SET_VALUE=help_wind_text
      WIDGET_CONTROL, butt_exit, BAD_ID=I, SET_UVALUE=[-88,199,0,0,0,0,0,0,0]
            
      XMANAGER, 'HELP_LAMP', help_lamp, EVENT_HANDLER='LAMP_EVENT_PARSER', /JUST_REG
      
      return
      end


;------------------------------------------------------------------------------------------
PRO   HELP_MOUNT, event

@lamp.cbk

;     ABOUT MOUNTING OPERATIONS
;     -------------------------

      help_mnt_text = STRARR(64)

help_mnt_text(0)  = '                                          ABOUT MOUNTING OPERATIONS'
help_mnt_text(1)  = '                                          ---------------------------' + $
                    '------------------------'
help_mnt_text(2)  = ''
help_mnt_text(3)  = '  To mount Remote Disk Instrument (RDI), proceed as follows:'
help_mnt_text(4)  = ''
help_mnt_text(5)  = '   1- Connect via Telnet to the Instrument Remote Host.'
help_mnt_text(6)  = '      COMMAND: telnet IP_number [ex: IP_IN6 = 192.93.249.75]'
help_mnt_text(7)  = ''
help_mnt_text(8)  = ''
help_mnt_text(9)  = '   2- Use the System Account to login.'
help_mnt_text(10) = '      Username: SYSTEM     Password:TOUAREGS'
help_mnt_text(11) = ''
help_mnt_text(12) = ''
help_mnt_text(13) = '   3- Get information on device name.'
help_mnt_text(14) = '      COMMAND: show dev d'
help_mnt_text(15) = ''
help_mnt_text(16) = ''
help_mnt_text(17) = '   4- Get information on file/directory protection.'
help_mnt_text(18) = '      COMMAND: ex: dir/prot DUB0:[0,0]'
help_mnt_text(19) = ''
help_mnt_text(20) = ''
help_mnt_text(21) = '   5- Verify that .DON files are located in DUB0:[MAD.DATA0 or DATA1 or DATA2]'
help_mnt_text(22) = '      with the correct protections, i.e. World:RE. Subdirectories must be'
help_mnt_text(23) = '      configured in the same way.'
help_mnt_text(24) = ''
help_mnt_text(25) = ''
help_mnt_text(26) = '   6- If not change protections.'
help_mnt_text(27) = '      COMMAND: ex: set/prot=(W:RE) dub0:[MAD.DATA0/1/2]'
help_mnt_text(28) = ''
help_mnt_text(29) = ''
help_mnt_text(30) = '   7- Type UCX SHOW BIND'
help_mnt_text(31) = ''
help_mnt_text(32) = ''
help_mnt_text(33) = '   8- If the concerned device (ex: DUB0) is not binded, bind it.'
help_mnt_text(34) = '      COMMAND: ex: UCX BIND DUB0: "/dub0"'
help_mnt_text(35) = ''
help_mnt_text(36) = ''
help_mnt_text(36) = '   9- Type UCX SHOW EXPORT'
help_mnt_text(37) = ''
help_mnt_text(38) = ''
help_mnt_text(39) = '  10- If the Filesystem (ex: DUB0:[MAD.DATA0/1/2]) is not exported, export it.'
help_mnt_text(40) = '      COMMAND: ex: UCX ADD EXPORT "/dub0/mad/data0" /host="hostname" or *'
help_mnt_text(41) = ''
help_mnt_text(42) = ''
help_mnt_text(43) = '  11- Type UCX SHOW PROXY'
help_mnt_text(44) = ''
help_mnt_text(45) = ''
help_mnt_text(46) = '  12- If your are not allowed to read the exported filesystem, create a new proxy.'
help_mnt_text(47) = '      COMMAND: ex: UCX ADD PROXY SYSTEM/UID=n1/GID=n2/host="hostname" or *'
help_mnt_text(48) = ''
help_mnt_text(49) = ''
help_mnt_text(50) = '  13- Type UCX SHOW HOST'
help_mnt_text(51) = ''
help_mnt_text(52) = ''
help_mnt_text(53) = '  14- If "hostname" address is not defined, extend the host_base.'
help_mnt_text(54) = '      COMMAND: ex: UCX SET HOST "hostname"/add=IP_address'
help_mnt_text(55) = ''
help_mnt_text(56) = ''
help_mnt_text(57) = '  15- Exit.'
help_mnt_text(58) = ''
help_mnt_text(59) = ''
help_mnt_text(60) = '  16- Connect as Super-User on the Workstation on the directory /usr/instruments'
help_mnt_text(61) = '      Create relevant directory - COMMAND: ex: mkdir IN6'
help_mnt_text(62) = '      Mount the RDI. COMMAND: ex: mount 192.93.249.75:/dub0/mad/data0 /usr/instruments/in6'
help_mnt_text(63) = '      Verify that you can touch .DON files. COMMAND: ex: ls /in6'


      help_mnt  = WIDGET_BASE(TITLE='Remote Disk Instrument - Mount Protocol - HELP',$
                              /COLUMN, RESOURCE_NAME='lampmic')
      commands  = WIDGET_BASE(help_mnt, /ROW)
   
      butt_exit = WIDGET_BUTTON(commands, VALUE='QUIT HELP', FONT = ft_bigger)
      
      help_text = WIDGET_TEXT(help_mnt, XSIZE=80, YSIZE=20, /SCROLL, FONT = ft_bigger)
      
      WIDGET_CONTROL, help_mnt, GROUP_LEADER=lamp_mic, /REALIZE
      WIDGET_CONTROL, help_text, SET_VALUE=help_mnt_text
      WIDGET_CONTROL, butt_exit, BAD_ID=I, SET_UVALUE=[-88,199,0,0,0,0,0,0,0]
            
      XMANAGER, 'HELP_LAMP', help_mnt, EVENT_HANDLER='LAMP_EVENT_PARSER', /JUST_REG
      
      return
      end


;------------------------------------------------------------------------------------------
;     Here, operator values are obtained through the LAMP_PDM procedure (included in lamp_pdm.pro)
;     via the user_value defined as LAMP_PDM_UVALUE(2), i.e. in the third field of the user_value.
;     The widget_control of the pulldown menu is performed in the LAMP_PDM procedure. The corresponding
;     sequence [uv(1)] for any action on the "Operator PDM" is 180 which is recognized by the
;     LAMP_EVENT_PARSER. The current PDM_OPER procedure is then EXECUTEd according to the LAMP_MIC_EVENT
;     procedure. This procedure get the label of the activated PDM, defined as the VALUE of this PDM.
;     This label is then set up to the corresponding WIDGET.LABEL {equivalent to [uv(5)]} 
;     located in the interface according to two parameters:
;           1- nw:   Workspace number
;           2- npos: Position in the workspace box.

PRO   ECHO_PDM_OPER, event, uv

@mics.cbk

      WIDGET_CONTROL, event.id, GET_VALUE = oper_value
      IF (oper_value NE 'CANCEL') THEN BEGIN     
       WIDGET_CONTROL, BAD_ID = I, uv(5), SET_VALUE= oper_value
       list_oper(uv(3),uv(4)) = oper_value
       IF (oper_value EQ 'ANDTO')  THEN BEGIN
        code_oper(uv(3),uv(4)) = 0
        code_op_op(uv(3),uv(4)) = 1
       ENDIF
       IF (oper_value EQ 'AND')     THEN BEGIN
        code_oper(uv(3),uv(4)) = 1
        code_op_op(uv(3),uv(4)) = 3
       ENDIF
       IF (oper_value EQ 'SUMTO')  THEN BEGIN
        code_oper(uv(3),uv(4)) = 2
        code_op_op(uv(3),uv(4)) = 6
       ENDIF
       IF (oper_value EQ 'PLUS')    THEN BEGIN
        code_oper(uv(3),uv(4)) = 3
        code_op_op(uv(3),uv(4)) = 13
       ENDIF
       IF (oper_value EQ 'MINUS')   THEN BEGIN
        code_oper(uv(3),uv(4)) = 4
        code_op_op(uv(3),uv(4)) = 21
       ENDIF
      ENDIF ELSE BEGIN
       WIDGET_CONTROL, BAD_ID = I, uv(5), SET_VALUE= ''
       list_oper(uv(3),uv(4)) = oper_value
       code_oper(uv(3),uv(4)) = 5
      ENDELSE
      
      err3_wk(uv(3)) = 1
            
      return
      end
;------------------------------------------------------------------------------------------

PRO   GET_WK_VAL, event

@lamp.cbk
@mics.cbk

      WIDGET_CONTROL, event.id, GET_VALUE = nwk_select
      swk_select = STRTRIM(STRING(nwk_select),2)
      IF (flag_acces EQ 0) THEN w_numor(nwk_select) = 'Current Run'
      IF (flag_acces EQ 3) THEN w_numor(nwk_select) = 'INX File'
      IF (flag_acces EQ 4) THEN BEGIN
       IF (inst_value EQ 'D11-TOF') THEN w_numor(nwk_select) = 'D11-TOF'
       IF (inst_value NE 'D11-TOF') THEN w_numor(nwk_select) = 'Test File'
      ENDIF
            
      return
      end
;------------------------------------------------------------------------------------------

PRO   NORM, event, uv

@mics.cbk
           
      WIDGET_CONTROL, BAD_ID = I
      flag_norm(uv(3))  = event.select
      err4_wk(uv(3))    = 1
      IF (flag_norm(uv(3)) EQ 0) THEN string_norm(uv(3)) = 'No norm.'
      IF (flag_norm(uv(3)) EQ 1) THEN string_norm(uv(3)) = 'Norm.'
      return
      end
;------------------------------------------------------------------------------------------

PRO   NORM_BIS, event, uv

@mics.cbk
           
      WIDGET_CONTROL, BAD_ID = I, uv(3), GET_VALUE = nw_slid
      flag_norm(nw_slid-1)           = event.select
      flag_norm(n_wk_max:n_wk_tot-1) = event.select
      IF (flag_norm(nw_slid-1) EQ 0) THEN string_norm(n_wk_max:n_wk_tot-1) = 'No norm.'
      IF (flag_norm(nw_slid-1) EQ 1) THEN string_norm(n_wk_max:n_wk_tot-1) = 'Norm.'
      return
      end
;------------------------------------------------------------------------------------------

PRO   SET_LAB_FMT_7_B, event, uv

@mics.cbk
           
      WIDGET_CONTROL, BAD_ID = I, uv(5), SET_VALUE= 'Path for "Database or Spectra" :'
      choice_access_data = 2
      return
      end      
;------------------------------------------------------------------------------------------

PRO   SET_LAB_FMT_7_A, event, uv
     
@mics.cbk
      
      WIDGET_CONTROL, BAD_ID = I, uv(5), SET_VALUE= 'File Path for "Current Run" :'
      choice_access_data = 1
      return
      end      
;------------------------------------------------------------------------------------------

PRO   SET_LAB_FMT_7_C, event, uv
     
@mics.cbk
      
      WIDGET_CONTROL, BAD_ID = I, uv(5), SET_VALUE= 'Enter File Path for "INX/Test Files" ?'
      choice_access_data = 3      
      return
      end
;------------------------------------------------------------------------------------------

PRO   SET_TXT_FMT_1
     
@lamp.cbk
@mics.cbk

      cd,current=mee
      current_directory=''
      
      WIDGET_CONTROL, BAD_ID = I, text_fmt_1, SET_VALUE= mee
      return
      end
;------------------------------------------------------------------------------------------

PRO   SET_LAB_WKP_1, event, uv

@lamp.cbk
@mics.cbk

      WIDGET_CONTROL, event.id, GET_VALUE = inst_value
      str_wkp_1 = 'INSTRUMENT :  '
      str_wkp_2 = str_wkp_1 + inst_value
      WIDGET_CONTROL, BAD_ID = I, uv(6), SET_VALUE= str_wkp_2
      SET_LAB_WKP_5
      return
      end
;------------------------------------------------------------------------------------------

PRO   SET_LAB_WKP_5

@lamp.cbk
@mics.cbk


      IF ((inst_value EQ 'IN4') OR (inst_value EQ 'IN5') OR (inst_value EQ 'IN6'))    THEN BEGIN
       norm_value = 'Normalization on Monitor 1'
      ENDIF
      IF ((inst_value EQ 'D11') OR (inst_value EQ 'D22') OR (inst_value EQ 'D17')  $
         OR (inst_value EQ 'D16'))    THEN BEGIN
       norm_value = 'Normalization on Preset 1'
      ENDIF
      IF ((inst_value EQ 'IN10') OR (inst_value EQ 'IN13') OR (inst_value EQ 'IN16')) THEN BEGIN
       norm_value = 'Normalization on Monitor 1'
      ENDIF
      
      WIDGET_CONTROL, label_wkp_5, SET_VALUE = norm_value

      return
      end
;------------------------------------------------------------------------------------------

PRO   SET_LAB_W1_BIS

@lamp.cbk
@mics.cbk

      str_wkp_1 = 'INSTRUMENT :  '
      str_wkp_2 = str_wkp_1 + inst_value
      WIDGET_CONTROL, BAD_ID = I, label_wkp_1, SET_VALUE= str_wkp_2
      return
      end
;------------------------------------------------------------------------------------------

PRO   SET_LAB_WKP_8

@lamp.cbk
@mics.cbk
                  
      str_wkp_lab_8 = STRARR(5)
      IF (choice_access_data NE 3) THEN BEGIN      
       IF (inst_value eq 'D11') OR   $
          (inst_value eq 'D16') OR   $       
          (inst_value eq 'D17') OR   $
          (inst_value eq 'D22') THEN BEGIN
        str_wkp_lab_8(0) = 'W1 = El. Bckgd'
        str_wkp_lab_8(1) = 'W2 = Water Bckgd'
        str_wkp_lab_8(2) = 'W3 = Water'
        str_wkp_lab_8(3) = 'W4 = Sample Bckgd'              
        str_wkp_lab_8(4) = 'W5 = Sample'
        FOR n = 0, n_wk_max-1 DO BEGIN
         WIDGET_CONTROL, BAD_ID = I, lab_wkp_8(n), SET_VALUE = str_wkp_lab_8(n)
        ENDFOR
       ENDIF
      
       IF (inst_value eq 'IN4')  OR   $
          (inst_value eq 'IN5')  OR   $
          (inst_value eq 'D7')   OR   $
          (inst_value eq 'IN6')  OR   $
          (inst_value eq 'IN13') OR   $
          (inst_value eq 'IN16') OR   $
          (inst_value eq 'IN10') THEN BEGIN
        str_wkp_lab_8(0) = 'W1 = Sample'
        str_wkp_lab_8(1) = 'W2 = Empty can'
        str_wkp_lab_8(2) = 'W3 = Vanadium'
        str_wkp_lab_8(3) = 'W4 = Cadmium'
        str_wkp_lab_8(4) = 'W5 = Other'                     
        FOR n = 0, n_wk_max-1 DO BEGIN
         WIDGET_CONTROL, BAD_ID = I, lab_wkp_8(n), SET_VALUE = str_wkp_lab_8(n)
        ENDFOR
       ENDIF
      ENDIF ELSE BEGIN
       str_wkp_lab_8(0) = 'W1'
       str_wkp_lab_8(1) = 'W2'
       str_wkp_lab_8(2) = 'W3'
       str_wkp_lab_8(3) = 'W4'              
       str_wkp_lab_8(4) = 'W5'
       FOR n = 0, n_wk_max-1 DO BEGIN
        WIDGET_CONTROL, BAD_ID = I, lab_wkp_8(n), SET_VALUE = str_wkp_lab_8(n)
       ENDFOR
      ENDELSE      
      
      return
      end
;------------------------------------------------------------------------------------------

PRO   INIT_FMT_1_B, event, uv

@lamp.cbk
@mics.cbk

      date   = SYSTIME(0)
      
      date   = STRING(date(0))
      l_date = STRLEN(date)
      year   = STRMID(date, l_date-2, 2)
      month  = STRMID(date, 4, 3)
      IF (month eq 'Jan') OR (month eq 'Feb') THEN rank = '1'
      IF (month eq 'Mar') OR (month eq 'Apr') THEN rank = '2'
      IF (month eq 'May') OR (month eq 'Jun') THEN rank = '3'
      IF (month eq 'Jul') OR (month eq 'Aug') THEN rank = '4'      
      IF (month eq 'Sep') OR (month eq 'Oct') THEN rank = '5'
      IF (month eq 'Nov') OR (month eq 'Dec') THEN rank = '6'      
      cycle = year + rank
      lamp_database = lamp_data
      IF (lamp_database EQ '') THEN BEGIN
       IF (flag_acces EQ 1) THEN BEGIN
        path_for_online = '/usr1/data-1/' + STRLOWCASE(inst_value) + lamp_dvd
       ENDIF
       IF (flag_acces EQ 5) THEN BEGIN
        path_for_online = '/usr1/data/'   + STRLOWCASE(inst_value) + lamp_dvd
       ENDIF
      ENDIF ELSE BEGIN
       IF (flag_acces EQ 1) THEN BEGIN
        path_for_online = lamp_database + '-1/'+ STRLOWCASE(inst_value) + lamp_dvd 
       ENDIF
       IF (flag_acces EQ 5) THEN BEGIN
        path_for_online = lamp_database + lamp_dvd  + STRLOWCASE(inst_value) + lamp_dvd
       ENDIF
      ENDELSE
      WIDGET_CONTROL, BAD_ID = I, uv(7), SET_VALUE = path_for_online
      return
      end
;------------------------------------------------------------------------------------------

PRO   INIT_FMT_1_A, event, uv
      
@lamp.cbk
@mics.cbk

      WIDGET_CONTROL, event.id, GET_VALUE = inst_value
      path_for_current = ''                 
      WIDGET_CONTROL, BAD_ID = I, uv(7), SET_VALUE = path_for_current
      return
      end            
;------------------------------------------------------------------------------------------

PRO   INIT_FMT_1_C, event, uv

@mics.cbk

	cd,current=mee
        path_for_spectra=''
        WIDGET_CONTROL, BAD_ID = I, uv(7), SET_VALUE = mee
        return
        end      
;------------------------------------------------------------------------------------------

PRO   INIT_FMT_1_D, event, uv

@lamp.cbk
@mics.cbk

      WIDGET_CONTROL, event.id, GET_VALUE = inst_value
      path_for_test = ''                 
      WIDGET_CONTROL, BAD_ID = I, uv(7), SET_VALUE = path_for_test
      return
      end
;------------------------------------------------------------------------------------------

PRO   TEST_FMT_1, event, uv

@lamp.cbk
@mics.cbk

	fil=findfile('*',count=cnt)
	if cnt gt 0 then last_run=fil(cnt-1) else last_run=''
        path_for_current=last_run
        WIDGET_CONTROL, BAD_ID = I, uv(7), SET_VALUE = path_for_current

      return
      end
;------------------------------------------------------------------------------------------

PRO   ENTRY_FMT_1, event

@lamp.cbk
@mics.cbk

      nb_file = -1
      
      IF ((choice_access_data EQ 2) AND (flag_acces EQ 1)) THEN BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE = path_for_online
       l_data1  = STRPOS(path_for_online(0),'data-1')
       l_data1  = l_data1 + 7
       cycle    = STRMID(path_for_online(0),l_data1,3)
       len_path = STRLEN(path_for_online)
       P_MAC_LABINS
      ENDIF

      IF ((choice_access_data EQ 2) AND (flag_acces EQ 5)) THEN BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE = path_for_online
       len_path = STRLEN(path_for_online)
      ENDIF

      IF ((choice_access_data EQ 2) AND (flag_acces EQ 2)) THEN BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE = path_for_spectra
       len_path = STRLEN(path_for_spectra)
      ENDIF
                            
      IF ((choice_access_data EQ 3) AND (flag_acces EQ 3)) THEN BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE = file_path
       file_found = FINDFILE(file_path(0), COUNT = nb_file)
       IF (nb_file eq 0) THEN BEGIN
        error_base = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
        error_text = WIDGET_LABEL(error_base, VALUE = 'FILE "' + file_path(0) + '" NOT FOUND  !!!', $
                                  FONT=ft_b_normal)
        error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                   FONT = ft_b_normal)
        WIDGET_CONTROL,    error_base,  GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL,    error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF
       IF (nb_file gt 0) THEN BEGIN
        READ_DATA
        nb_file = -1
       ENDIF
      ENDIF


      IF ((choice_access_data EQ 3) AND (flag_acces EQ 4)) THEN BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE = path_for_test
       file_found  = FINDFILE(path_for_test(0), COUNT = nb_file)       
       IF (nb_file eq 0) THEN BEGIN
        error_base  = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
        error_text1 = WIDGET_LABEL(error_base, VALUE = 'FILE NOT FOUND  !!!', $
                                   FONT=ft_b_normal)
        error_butt1 = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                    FONT = ft_b_normal)
        WIDGET_CONTROL,    error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL,    error_butt1, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base,  event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF
       IF (nb_file gt 0) THEN BEGIN
        READ_DATA
        TO_DON_HISTORY, nwk_select , 0 , 'w'+swk_select+'=RDRUN('+file_found(0)+') ;'+string_norm(nwk_select-1)     
        nb_file = -1
       ENDIF       
      ENDIF

      IF (choice_access_data eq 1) AND (flag_acces EQ 0) THEN BEGIN
       WIDGET_CONTROL, event.id, GET_VALUE = path_for_current
       file_found  = FINDFILE(path_for_current(0), COUNT = nb_file)       
       IF (nb_file eq 0) THEN BEGIN
        error_base  = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
        error_text1 = WIDGET_LABEL(error_base, VALUE = 'FILE NOT FOUND  !!!', $
                                   FONT=ft_b_normal)
        error_butt1 = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                    FONT = ft_b_normal)
        WIDGET_CONTROL,    error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL,    error_butt1, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base,  event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF
       IF (nb_file gt 0) THEN BEGIN
        READ_DATA
        TO_DON_HISTORY, nwk_select , 0 , 'w'+swk_select+'=RDRUN('+file_found(0)+') ;'+string_norm(nwk_select-1)     
        nb_file = -1
       ENDIF       
      ENDIF
        
      return
      end
;------------------------------------------------------------------------------------------

PRO   READ_TOF

@lamp.cbk
@mics.cbk
   			
IF (inst_value eq 'D7') THEN READ_D7 ELSE BEGIN

;     -----------------------------------
;     READ-IN IN4, IN5, IN6 & D7 TOF DATA
;     -----------------------------------
      ON_IOERROR, no_file_tof
      GET_LUN, ilun

      OPENR, ilun, file_found(0)

      ON_IOERROR, end_read_tof

      numor=LONG(w_numor(nwk_select))
         
;     -------------------------------
;     READ HEADER & PARAMETERS BLOCKS
;     -------------------------------
      line = STRARR(1)  & line(0)='' & lin=''
      READF, ilun, line
      num = LONG(0)
      READF, ilun, num & numor=num
      READF, ilun, line
      READF, ilun, line & lin=line(0)+' 0' & num1=0 & num2=0 & reads,lin,num1,num2
      
      instdate = STRARR(1)
      READF, ilun, instdate
      inst  = STRMID(instdate,0 ,4)
      numexp= STRMID(instdate,4 ,10)
      date  = STRMID(instdate,14,18)
;------------------------------------
      READF, ilun, line
      nf0 = LONG(0)
      READF, ilun, nf0
      block0 = INTARR(nf0)
      READF, ilun, block0
      nb_spc = block0(0)  & nb_chn = block0(1)
      nx_global = nb_chn  & ny_global = nb_spc
      nblk_to_read = LONG(0)
      FOR nb = 1, 8 DO BEGIN			; Maximum Header Blocks = 8
       IF (block0(2*nb) NE 0) THEN nblk_to_read = nblk_to_read+1
      ENDFOR
;------------------------------------
      FOR nbl = 0, nblk_to_read-1 DO BEGIN
       READF, ilun, line
       IF (line(0) EQ aline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        nline  = nfield/80
        nf_ll  = nfield-(nline*80)
        IF (nf_ll NE 0) THEN nline = nline+1
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=STRARR(nline)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
       IF (line(0) EQ fline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=FLTARR(nfield)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
       IF (line(0) EQ iline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=LONARR(nfield)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
      ENDFOR
;------------------------------------
      main_title = STRMID(block1(0),0,60)
      sub_title  = STRMID(block1(0),60,20)
      start_time = STRMID(block1(1),0,20)      
      stop_time  = STRMID(block1(1),20,20)
;------------------------------------
      m1         = FLTARR(block0(1)-1)                  
      

;------------------------------------------------------------------------------------------
;                                           IN5
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN5') THEN BEGIN     
       FOR ns = 0, block0(0)-9 DO BEGIN
        IF (block2(ns+39) lt 129.0) AND  (block2(ns+39) ne 0.0) THEN nb_spc = ns + 1
       ENDFOR             
       n_buf  = FLTARR(block0(1)-1,3)      ; Monitors M1-M2-M3
       w_buf  = FLTARR(block0(1)-1,nb_spc)
       w_tmp  = FLTARR(block0(1)-1)
       y_buf  = block2(39:39+nb_spc-1)
       x_buf  = INDGEN(block0(1)-1)+1
       nn_global = 3

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN 
        READF, ilun, w_tmp
        IF (ns le 2) THEN n_buf(*,ns) = w_tmp
        IF (ns gt 7) AND (ns le nb_spc + 7) THEN w_buf(*,ns-8) = w_tmp
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF 
       ENDFOR
      
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(2)		; Run duration (seconds)
       p_buf(1)   = block2(5)		; Time in 0.1 seconds    
       p_buf(2)   = block3(1)		; Number of reserved channels 
       p_buf(3)   = block2(3)		; Counts in M1
       p_buf(4)   = block2(4)		; Counts in M2 
       p_buf(5)   = block2(6)		; Counts in M3      
       p_buf(6)   = block2(7)		; Total counts 
       p_buf(7)   = 0.0     		; Not used
       p_buf(8)   = block2(17)		; Sample angle (deg.) 
       p_buf(9)   = block3(8)		; Elastic peak position (channel)
       p_buf(10)  = block3(9)		; Numor
       p_buf(11)  = block3(10)		; Sample temperature (K)
       p_buf(12)  = block2(15)		; Digital Voltmeter Reading 
       p_buf(13)  = block3(12)		; Repetition period (microsec.)     
       p_buf(14)  = block3(13)		; Multiplier for repetition period   
       p_buf(15)  = 0.0     		; Not used
       p_buf(16)  = 0.0     		; Not used
       p_buf(17)  = 0.0     		; Not used      
       p_buf(18)  = block3(17)		; Channel width (microsec.)  
       p_buf(19)  = block0(1)-1		; Number of channels used
       p_buf(20)  = block3(19)		; TOF delay (microsec.) 
       p_buf(21)  = block3(20)		; Wavelength (angstroms)    
       p_buf(22)  = block3(21)		; Distance CH4 - M1     (meter) 
       p_buf(23)  = block3(22)		; Distance CH4 - Sample (meter)
       p_buf(24)  = 0.0     		; Not used 
       p_buf(25)  = block3(24)		; Distance M1  - M2     (meter)      
       p_buf(26)  = block3(25)		; Distance M1  - M3     (meter) 
       p_buf(27)  = block3(26)		; Distance Det - Sample (meter)
       p_buf(28)  = block2(0)		; Contents scaler 1 
       p_buf(29)  = block2(1)		; Contents scaler 2       
       p_buf(30)  = nb_spc      	; Number of angles

;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Run duration (seconds)             ='
       par_txt(nwk_select,1)   = 'Time in 0.1 seconds                ='    
       par_txt(nwk_select,2)   = 'Number of reserved channels        =' 
       par_txt(nwk_select,3)   = 'Counts in M1                       ='
       par_txt(nwk_select,4)   = 'Counts in M2                       ='
       par_txt(nwk_select,5)   = 'Counts in M3                       ='     
       par_txt(nwk_select,6)   = 'Total counts                       ='
       par_txt(nwk_select,7)   = 'Not used                           ='
       par_txt(nwk_select,8)   = 'Sample angle (deg.)                =' 
       par_txt(nwk_select,9)   = 'Elastic peak position (channel)    ='
       par_txt(nwk_select,10)  = 'Numor                              ='
       par_txt(nwk_select,11)  = 'Sample temperature (K)             ='
       par_txt(nwk_select,12)  = 'Digital Voltmeter Reading          =' 
       par_txt(nwk_select,13)  = 'Repetition period (microsec.)      ='     
       par_txt(nwk_select,14)  = 'Multiplier for repetition period   ='   
       par_txt(nwk_select,15)  = 'Not used                           ='
       par_txt(nwk_select,16)  = 'Not used                           ='
       par_txt(nwk_select,17)  = 'Not used                           ='      
       par_txt(nwk_select,18)  = 'Channel width (microsec.)          ='  
       par_txt(nwk_select,19)  = 'Number of channels used            ='
       par_txt(nwk_select,20)  = 'TOF delay (microsec.)              =' 
       par_txt(nwk_select,21)  = 'Wavelength (angstroms)             ='    
       par_txt(nwk_select,22)  = 'Distance CH4 - M1     (meter)      =' 
       par_txt(nwk_select,23)  = 'Distance CH4 - Sample (meter)      ='
       par_txt(nwk_select,24)  = 'Not used                           =' 
       par_txt(nwk_select,25)  = 'Distance M1  - M2     (meter)      ='      
       par_txt(nwk_select,26)  = 'Distance M1  - M3     (meter)      =' 
       par_txt(nwk_select,27)  = 'Distance Det - Sample (meter)      ='
       par_txt(nwk_select,28)  = 'Contents scaler 1                  =' 
       par_txt(nwk_select,29)  = 'Contents scaler 2                  ='       
       par_txt(nwk_select,30)  = 'Number of angles                   =' 
     ENDIF      

;------------------------------------------------------------------------------------------
;                                           IN6
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN6') THEN BEGIN
       if block0(0) gt 300 then begin off=3  & mom=3
	             endif else begin off=21 & mom=4 & endelse
       nb_spc = block0(0)-off            
       n_buf  = FLTARR(block0(1)-1,mom)      ; Monitors M1-M2-M3(-M4)
       w_buf  = FLTARR(block0(1)-1,nb_spc)
       w_tmp  = FLTARR(block0(1)-1)
       y_buf  = block2(31+off:31+off+nb_spc-1)
       x_buf  = INDGEN(block0(1)-1)+1
       nn_global = 3

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN 
        READF, ilun, w_tmp
        IF (ns lt mom) THEN n_buf(*,ns)     = w_tmp
        IF (ns ge off) THEN w_buf(*,ns-off) = w_tmp
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF 
       ENDFOR
      
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(2)		; Run duration (seconds)
       p_buf(1)   = block2(5)		; Time in 0.1 seconds    
       p_buf(2)   = block3(1)		; Number of reserved channels 
       p_buf(3)   = block2(3)		; Counts in M1
       p_buf(4)   = block2(4)		; Counts in M2 
       p_buf(5)   = block2(6)		; Counts in M3      
       p_buf(6)   = block2(7)		; Total counts 
       p_buf(7)   = 0.0     		; Not used
       p_buf(8)   = block2(17)		; Sample angle (deg.) 
       p_buf(9)   = block3(8)		; Elastic peak position (channel)
       p_buf(10)  = block3(9)		; Numor
       p_buf(11)  = block3(10)		; Sample temperature (K)
       p_buf(12)  = block2(15)		; Digital Voltmeter Reading 
       p_buf(13)  = block3(12)		; Repetition period (microsec.)     
       p_buf(14)  = block3(13)		; Multiplier for repetition period   
       p_buf(15)  = 0.0     		; Not used
       p_buf(16)  = 0.0     		; Not used
       p_buf(17)  = 0.0     		; Not used      
       p_buf(18)  = block3(17)		; Channel width (microsec.)  
       p_buf(19)  = block0(1)-1		; Number of channels used
       p_buf(20)  = block3(19)		; TOF delay (microsec.) 
       p_buf(21)  = block3(20)		; Wavelength (angstroms)    
       p_buf(22)  = block3(21)		; Distance CH4 - M1     (meter) 
       p_buf(23)  = block3(22)		; Distance CH4 - Sample (meter)
       p_buf(24)  = 0.0     		; Not used 
       p_buf(25)  = block3(24)		; Distance M1  - M2     (meter)      
       p_buf(26)  = block3(25)		; Distance M1  - M3     (meter) 
       p_buf(27)  = block3(26)		; Distance Det - Sample (meter)
       p_buf(28)  = block2(0)		; Contents scaler 1 
       p_buf(29)  = block2(1)		; Contents scaler 2       
       p_buf(30)  = nb_spc      	; Number of angles

;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Run duration (seconds)             ='
       par_txt(nwk_select,1)   = 'Time in 0.1 seconds                ='    
       par_txt(nwk_select,2)   = 'Number of reserved channels        =' 
       par_txt(nwk_select,3)   = 'Counts in M1                       ='
       par_txt(nwk_select,4)   = 'Counts in M2                       ='
       par_txt(nwk_select,5)   = 'Counts in M3                       ='     
       par_txt(nwk_select,6)   = 'Total counts                       ='
       par_txt(nwk_select,7)   = 'Not used                           ='
       par_txt(nwk_select,8)   = 'Sample angle (deg.)                =' 
       par_txt(nwk_select,9)   = 'Elastic peak position (channel)    ='
       par_txt(nwk_select,10)  = 'Numor                              ='
       par_txt(nwk_select,11)  = 'Sample temperature (K)             ='
       par_txt(nwk_select,12)  = 'Digital Voltmeter Reading          =' 
       par_txt(nwk_select,13)  = 'Repetition period (microsec.)      ='     
       par_txt(nwk_select,14)  = 'Multiplier for repetition period   ='   
       par_txt(nwk_select,15)  = 'Not used                           ='
       par_txt(nwk_select,16)  = 'Not used                           ='
       par_txt(nwk_select,17)  = 'Not used                           ='      
       par_txt(nwk_select,18)  = 'Channel width (microsec.)          ='  
       par_txt(nwk_select,19)  = 'Number of channels used            ='
       par_txt(nwk_select,20)  = 'TOF delay (microsec.)              =' 
       par_txt(nwk_select,21)  = 'Wavelength (angstroms)             ='    
       par_txt(nwk_select,22)  = 'Distance CH4 - M1     (meter)      =' 
       par_txt(nwk_select,23)  = 'Distance CH4 - Sample (meter)      ='
       par_txt(nwk_select,24)  = 'Not used                           =' 
       par_txt(nwk_select,25)  = 'Distance M1  - M2     (meter)      ='      
       par_txt(nwk_select,26)  = 'Distance M1  - M3     (meter)      =' 
       par_txt(nwk_select,27)  = 'Distance Det - Sample (meter)      ='
       par_txt(nwk_select,28)  = 'Contents scaler 1                  =' 
       par_txt(nwk_select,29)  = 'Contents scaler 2                  ='       
       par_txt(nwk_select,30)  = 'Number of angles                   ='       
      ENDIF
      
;------------------------------------------------------------------------------------------
;                               IN4 - IN4 - IN4 - IN4 - IN4
;
;                     GROUPING OF POSITIVE AND NEGATIVE DETECTOR ANGLES
;                MUST BE INCLUDED IN A MACRO COMMAND (G.J. KEARLEY interface)
;
;             HERE SPECTRA ARE SORTED FROM NEGATIVE TO POSITIVE DETECTOR ANGLES
;------------------------------------------------------------------------------------------

      IF (inst_value eq 'IN4') THEN BEGIN       
       nb_spc = block0(0)-16       
       n_buf  = FLTARR(block0(1)-1,2)   	 ; Monitors M1-M2          
       w_buf  = FLTARR(block0(1)-1,nb_spc)
       w_tmp  = FLTARR(block0(1)-1)
       y_buf  = block2(47:47+nb_spc-1)
       x_buf  = INDGEN(block0(1)-1)+1
       nn_global = 2

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN 
        READF, ilun, w_tmp
        IF (ns le 1) THEN n_buf(*,ns) = w_tmp
        IF (ns gt 15) AND (ns le nb_spc + 15) THEN w_buf(*,ns-16) = w_tmp
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF 
       ENDFOR
              
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(2)		; Run duration (seconds)
       p_buf(1)   = block2(5)		; Time in 0.1 seconds    
       p_buf(2)   = block3(1)		; Number of reserved channels 
       p_buf(3)   = block2(3)		; Counts in M1
       p_buf(4)   = block2(4)		; Counts in M2 
       p_buf(5)   = 0.0 		; Not used     
       p_buf(6)   = block2(7)		; Total counts 
       p_buf(7)   = 0.0     		; Not used
       p_buf(8)   = block2(17)		; Sample angle (deg.) 
       p_buf(9)   = block3(8)		; Elastic peak position (channel)
       p_buf(10)  = block3(9)		; Numor
       p_buf(11)  = block3(10)		; Sample temperature (K)
       p_buf(12)  = block2(15)		; Digital Voltmeter Reading 
       p_buf(13)  = block3(12)		; Repetition period (microsec.)     
       p_buf(14)  = block3(13)		; Multiplier for repetition period   
       p_buf(15)  = 0.0     		; Not used
       p_buf(16)  = 0.0     		; Not used
       p_buf(17)  = 0.0     		; Not used      
       p_buf(18)  = block3(17)		; Channel width (microsec.)  
       p_buf(19)  = block0(1)-1		; Number of channels used
       p_buf(20)  = block3(19)		; TOF delay (microsec.) 
       p_buf(21)  = block3(20)		; Wavelength (angstroms)    
       p_buf(22)  = block3(21)		; Distance CH4 - M1     (meter) 
       p_buf(23)  = block3(22)		; Distance CH4 - Sample (meter)
       p_buf(24)  = 0.0     		; Not used 
       p_buf(25)  = block3(24)		; Distance M1  - M2     (meter)      
       p_buf(26)  = 0.0 		; Not used 
       p_buf(27)  = block3(26)		; Distance Det - Sample (meter)
       p_buf(28)  = block2(0)		; Contents scaler 1 
       p_buf(29)  = block2(1)		; Contents scaler 2       
       p_buf(30)  = nb_spc		; Number of angles

;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Run duration (seconds)             ='
       par_txt(nwk_select,1)   = 'Time in 0.1 seconds                ='    
       par_txt(nwk_select,2)   = 'Number of reserved channels        =' 
       par_txt(nwk_select,3)   = 'Counts in M1                       ='
       par_txt(nwk_select,4)   = 'Counts in M2                       ='
       par_txt(nwk_select,5)   = 'Not used                           ='     
       par_txt(nwk_select,6)   = 'Total counts                       ='
       par_txt(nwk_select,7)   = 'Not used                           ='
       par_txt(nwk_select,8)   = 'Sample angle (deg.)                =' 
       par_txt(nwk_select,9)   = 'Elastic peak position (channel)    ='
       par_txt(nwk_select,10)  = 'Numor                              ='
       par_txt(nwk_select,11)  = 'Sample temperature (K)             ='
       par_txt(nwk_select,12)  = 'Digital Voltmeter Reading          =' 
       par_txt(nwk_select,13)  = 'Repetition period (microsec.)      ='     
       par_txt(nwk_select,14)  = 'Multiplier for repetition period   ='   
       par_txt(nwk_select,15)  = 'Not used                           ='
       par_txt(nwk_select,16)  = 'Not used                           ='
       par_txt(nwk_select,17)  = 'Not used                           ='      
       par_txt(nwk_select,18)  = 'Channel width (microsec.)          ='  
       par_txt(nwk_select,19)  = 'Number of channels used            ='
       par_txt(nwk_select,20)  = 'TOF delay (microsec.)              =' 
       par_txt(nwk_select,21)  = 'Wavelength (angstroms)             ='    
       par_txt(nwk_select,22)  = 'Distance CH4 - M1     (meter)      =' 
       par_txt(nwk_select,23)  = 'Distance CH4 - Sample (meter)      ='
       par_txt(nwk_select,24)  = 'Not used                           =' 
       par_txt(nwk_select,25)  = 'Distance M1  - M2     (meter)      ='      
       par_txt(nwk_select,26)  = 'Not used                           =' 
       par_txt(nwk_select,27)  = 'Distance Det - Sample (meter)      ='
       par_txt(nwk_select,28)  = 'Contents scaler 1                  =' 
       par_txt(nwk_select,29)  = 'Contents scaler 2                  ='       
       par_txt(nwk_select,30)  = 'Number of angles                   =' 
      ENDIF

;     -------------------------------
;     ASSIGNING WORKSPACE VARIABLES
;     -------------------------------
       junk   = EXECUTE('x'+swk_select+'=x_buf')
       junk   = EXECUTE('y'+swk_select+'=y_buf')
       junk   = EXECUTE('w'+swk_select+'=w_buf')
       junk   = EXECUTE('n'+swk_select+'=n_buf')

       junk   = EXECUTE('p'+swk_select+'=p_buf')
     
;     -------------------------------
;     NORMALISATION ON M1 - OR NOT -
;     -------------------------------
      IF (flag_norm(nwk_select-1) EQ 1) THEN BEGIN
       deltap_norm = par_norm
       junk        = EXECUTE('m1=n'+swk_select+'(*,0)')              
       max_m1      = MAX(m1)
       pos_max_m1  = !C
       n_ok        = 0
       REPEAT BEGIN
	deltap_norm = deltap_norm/2
        IF (((pos_max_m1-LONG(deltap_norm)) GE 0) AND $
            ((pos_max_m1+LONG(deltap_norm)) LE (block0(1)-1))) THEN n_ok =1
       ENDREP UNTIL (n_ok EQ 1) 
       IF ((deltap_norm LE 16) AND (pos_max_m1-2*LONG(deltap_norm) LT 0)) THEN BEGIN
        np3         = pos_max_m1+LONG(deltap_norm)
        np4         = block0(1)-2
        background  = LONG(0)
        aver_bckgd  = 0.0
        background  = TOTAL(m1(np3:np4))
        aver_bckgd  = background/(np4-np3+1)
       ENDIF
       IF ((deltap_norm LE 16) AND (pos_max_m1+2*LONG(deltap_norm) GT block0(1)-1)) THEN BEGIN
        np1         = 0
        np2         = pos_max_m1-LONG(deltap_norm)                                                   
        background  = LONG(0)
        aver_bckgd  = 0.0
        background  = TOTAL(m1(np1:np2))
        aver_bckgd  = background/(np2-np1+1)
       ENDIF ELSE BEGIN
        np1         = 0
        np2         = pos_max_m1-LONG(deltap_norm)                                                   
        np3         = pos_max_m1+LONG(deltap_norm)
        np4         = block0(1)-2
        background  = LONG(0)
        aver_bckgd  = 0.0
        background  = TOTAL(m1(np1:np2))+TOTAL(m1(np3:np4))
        aver_bckgd  = background/((np2-np1+1)+(np4-np3+1))
       ENDELSE
       norm_m1     = TOTAL(m1)-(block0(1)-1)*aver_bckgd
       junk        = EXECUTE('w'+swk_select+'=w'+swk_select+'*f_norm_tof/norm_m1')
       deltap_norm = par_norm
       z_tit(nwk_select) = 'Counts (*'+sf_norm_tof+') / M1'
      ENDIF ELSE BEGIN
       z_tit(nwk_select) = 'Counts'
      ENDELSE
      
      w_tit    (nwk_select) = strtrim(main_title,2)
      x_tit    (nwk_select) = 'Channels'
      y_tit    (nwk_select) = 'Spectrum #'
      other_tit(nwk_select) = w_numor(nwk_select)+' '+sub_title+' Start:'+start_time
                  
;     ---------------------------------
;     APPLY AFTER READ SPECIAL FUNCTION
;     ---------------------------------

      head_tit(nwk_select,0) = sub_title
      head_tit(nwk_select,1) = main_title
      head_tit(nwk_select,2) = inst_value
      head_tit(nwk_select,3) = STRING(numor)
      head_tit(nwk_select,4) = start_time
      head_tit(nwk_select,5) = ''
      head_tit(nwk_select,6) = x_tit(nwk_select)
      head_tit(nwk_select,7) = y_tit(nwk_select)
      head_tit(nwk_select,8) = z_tit(nwk_select)
      head_tit(nwk_select,9) = ''
	      
end_read_tof:
no_file_tof:FREE_LUN, ilun

ENDELSE
     
return
end
;------------------------------------------------------------------------------------------

PRO   READ_BSC

@lamp.cbk
@mics.cbk
   			
;     ----------------------------------------------
;     READ-IN IN10, IN13 & IN16 BACK-SCATTERING DATA
;     ----------------------------------------------
      
      ON_IOERROR, end_read_bsc
      GET_LUN, ilun
      OPENR, ilun, file_found(0)
;     -------------------------------
;     READ HEADER & PARAMETERS BLOCKS
;     -------------------------------
      line = STRARR(1)  & line(0)='' & lin=''
      READF, ilun, line
      num = LONG(0)
      READF, ilun, num
      READF, ilun, line
      READF, ilun, line
      instdate = STRARR(1)
      READF, ilun, instdate
      inst   = STRMID(instdate,0,4)
      numexp = STRMID(instdate,4,10)
      date   = STRMID(instdate,14,18)
;------------------------------------
      READF, ilun, line
      nf0 = LONG(0)
      READF, ilun, nf0
      block0 = INTARR(nf0)
      READF, ilun, block0
      nb_spc = block0(0)  & nb_chn = block0(1)
      nblk_to_read = LONG(0)
      FOR nb = 1, 8 DO BEGIN			; Maximum Header Blocks = 8
       IF (block0(2*nb) NE 0) THEN nblk_to_read = nblk_to_read+1
      ENDFOR
;------------------------------------
      FOR nbl = 0, nblk_to_read-1 DO BEGIN
       READF, ilun, line
       IF (line(0) EQ aline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        nline  = nfield/80
        nf_ll  = nfield-(nline*80)
        IF (nf_ll NE 0) THEN nline = nline+1
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=STRARR(nline)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
       IF (line(0) EQ fline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=DBLARR(nfield)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
       IF (line(0) EQ iline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=LONARR(nfield)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
      ENDFOR          
;------------------------------------
     IF (inst_value eq 'IN10') or (inst_value eq 'IN16') THEN BEGIN
       main_title = STRMID(block1(0),0,60)
       sub_title1 = STRMID(block1(0),60,20)
       sub_title2 = STRMID(block1(1),0,20)
       sub_title  = sub_title1 + sub_title2      
       exp_name   = STRMID(block1(1),20,20)
       start_time = STRMID(block1(1),40,20)      
       stop_time  = STRMID(block1(1),60,20)
      ENDIF
      IF (inst_value eq 'IN13') THEN BEGIN
       main_title = STRMID(block1(0),0,60)
       sub_title1 = STRMID(block1(0),60,20)
       sub_title2 = STRMID(block1(1),0,20)
       sub_title  = sub_title1 + sub_title2      
       start_time = STRMID(block1(1),20,20)      
       user_name  = STRMID(block1(1),40,20)
      ENDIF
     
;------------------------------------------------------------------------------------------
;                         SPECIFIC READ-IN DEPENDING ON INSTRUMENT  
;------------------------------------------------------------------------------------------
;                                IN10 - IN10 - IN10 - IN10
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;                                      IN10 DOPPLER
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN10') AND (block2(21) eq 0) THEN BEGIN
;
; Insist on one monitor gjk 
       if block2(20) le 0 then block2(20)=1
       nb_spc = block0(0)-block2(20)     
       n_buf  = FLTARR(block0(1),block2(20))      ; Monitors          
       w_buf  = FLTARR(block0(1),nb_spc)
       w_tmp  = FLTARR(block0(1))
       y_buf  = block3(0:nb_spc-1)
       x_buf  = INDGEN(block0(1))+1
       nn_global = block2(20) & nx_global = block0(1)  & ny_global = nb_spc

                     
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le block2(19)-1) THEN w_buf(*,ns) = w_tmp
        IF (ns gt block2(19)-1) THEN n_buf= w_tmp
        IF (ns lt block0(0)-1)  THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF         
       ENDFOR
       
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(21)		; Type of scan                 (index)
       p_buf(1)   = block2(0)		; Duration of scan             (seconds)
       p_buf(2)   = block2(1)		; Max. Doppler frequency       (Hz)    
       p_buf(3)   = block2(2)		; Min. Doppler frequency       (Hz) 
       p_buf(4)   = block2(3)		; Chopper frequency            (RPM) 
       p_buf(5)   = block2(81)		; Lattice param. Monochromator (ang.)
       p_buf(6)   = block2(83)		; Lattice param. Analyser      (ang.)
       p_buf(7)   = block2(82)		; Lattice param. Deflector     (ang.)
       p_buf(8)   = block2(84)		; TOF Sample-Det.              (microsec.)
       p_buf(9)   = block2(85)		; TOF Sample-M1                (microsec.)
       p_buf(10)  = block2(86)		; TOF Sample-M2                (microsec.)
       p_buf(11)  = block2(87)		; T1                           (microsec.)
       p_buf(12)  = block2(88)		; T2                           (microsec.)
       p_buf(13)  = block2(19)		; Number of detectors in use
       p_buf(14)  = block2(20)		; Number of monitors  in use
       p_buf(15)  = block2(6)		; Channel limit
       p_buf(16)  = block2(23)		; Number of points in spectrum
       p_buf(17)  = block3(0)		; Scattering angle 1 (2*theta) (deg.)      
       p_buf(18)  = block3(1)		; Scattering angle 2 (2*theta) (deg.)      
       p_buf(19)  = block3(2)		; Scattering angle 3 (2*theta) (deg.)      
       p_buf(20)  = block3(3)		; Scattering angle 4 (2*theta) (deg.)      
       p_buf(21)  = block3(4)		; Scattering angle 5 (2*theta) (deg.)      
       p_buf(22)  = block3(5)		; Scattering angle 6 (2*theta) (deg.)      
       p_buf(23)  = block3(6)		; Scattering angle 7 (2*theta) (deg.)      
       p_buf(24)  = block3(7)		; Scattering angle 8 (2*theta) (deg.)      
       p_buf(25)  = block2(50)		; Deflector  angle Theta-g     (deg.)      
       p_buf(26)  = 0.0  		; Not used     
       p_buf(27)  = 0.0  		; Not used     
       p_buf(28)  = 0.0  		; Not used     
       p_buf(29)  = 0.0  		; Not used     
       p_buf(30)  = 0.0  		; Not used     


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Duration of scan (seconds)           ='    
       par_txt(nwk_select,2)   = 'Max. Doppler frequency (Hz)          =' 
       par_txt(nwk_select,3)   = 'Min. Doppler frequency (Hz)          ='
       par_txt(nwk_select,4)   = 'Chopper frequency (RPM)              ='
       par_txt(nwk_select,5)   = 'Lattice param. Monochromator (ang.)  ='     
       par_txt(nwk_select,6)   = 'Lattice param. Analyser      (ang.)  ='
       par_txt(nwk_select,7)   = 'Lattice param. Deflector     (ang.)  ='
       par_txt(nwk_select,8)   = 'TOF Sample-Det. (microsec.)          =' 
       par_txt(nwk_select,9)   = 'TOF Sample-M1   (microsec.)          ='
       par_txt(nwk_select,10)  = 'TOF Sample-M2   (microsec.)          ='
       par_txt(nwk_select,11)  = 'T1              (microsec.)          ='
       par_txt(nwk_select,12)  = 'T2              (microsec.)          =' 
       par_txt(nwk_select,13)  = 'Number of detectors in use           ='     
       par_txt(nwk_select,14)  = 'Number of monitors  in use           ='   
       par_txt(nwk_select,15)  = 'Channel limit                        ='
       par_txt(nwk_select,16)  = 'Number of points in spectrum         ='
       par_txt(nwk_select,17)  = 'Scattering angle 1 (2*theta) (deg.)  ='      
       par_txt(nwk_select,18)  = 'Scattering angle 2 (2*theta) (deg.)  ='  
       par_txt(nwk_select,19)  = 'Scattering angle 3 (2*theta) (deg.)  ='
       par_txt(nwk_select,20)  = 'Scattering angle 4 (2*theta) (deg.)  =' 
       par_txt(nwk_select,21)  = 'Scattering angle 5 (2*theta) (deg.)  ='    
       par_txt(nwk_select,22)  = 'Scattering angle 6 (2*theta) (deg.)  =' 
       par_txt(nwk_select,23)  = 'Scattering angle 7 (2*theta) (deg.)  ='
       par_txt(nwk_select,24)  = 'Scattering angle 8 (2*theta) (deg.)  =' 
       par_txt(nwk_select,25)  = 'Deflector  angle Theta-g     (deg.)  ='      
       par_txt(nwk_select,26)  = 'Not used =' 
       par_txt(nwk_select,27)  = 'Not used ='
       par_txt(nwk_select,28)  = 'Not used =' 
       par_txt(nwk_select,29)  = 'Not used ='       
       par_txt(nwk_select,30)  = 'Not used =' 
      ENDIF


;------------------------------------------------------------------------------------------
;                                 IN10 ELASTIC SCAN
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN10') AND ((block2(21) eq 1) OR (block2(21) eq 2)) THEN BEGIN
       nb_spc = block0(0)-block2(20)-1     
       n_buf  = FLTARR(block2(23),block2(20))      ; Monitors          
       w_buf  = FLTARR(block2(23),nb_spc)
       w_tmp  = FLTARR(block0(1))
       y_buf  = block3(0:nb_spc-1)
       x_buf  = FLTARR(block2(23))
       nn_global = block2(20) & nx_global = block2(23)  & ny_global = nb_spc


       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le block2(19)-1) THEN w_buf(*,ns) = w_tmp(0:block2(23)-1)
        IF (ns gt block2(19)-1) AND (ns le block2(19)+block2(20)-1) THEN BEGIN
         n_buf(*,ns-block2(19)) = w_tmp(0:block2(23)-1)
        ENDIF ELSE BEGIN
         x_buf(*) = w_tmp(0:block2(23)-1)/block2(22)
        ENDELSE
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF         
       ENDFOR
                             
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(21)		; Type of scan                 (index)
       p_buf(1)   = block2(0)		; Duration of scan             (seconds)
       p_buf(2)   = block2(3)		; Chopper frequency            (RPM) 
       p_buf(3)   = block2(81)		; Lattice param. Monochromator (ang.)
       p_buf(4)   = block2(83)		; Lattice param. Analyser      (ang.)
       p_buf(5)   = block2(82)		; Lattice param. Deflector     (ang.)
       p_buf(6)   = block2(84)		; TOF Sample-Det.              (microsec.)
       p_buf(7)   = block2(85)		; TOF Sample-M1                (microsec.)
       p_buf(8)   = block2(86)		; TOF Sample-M2                (microsec.)
       p_buf(9)   = block2(87)		; T1                           (microsec.)
       p_buf(10)  = block2(88)		; T2                           (microsec.)
       p_buf(11)  = block2(19)		; Number of detectors in use
       p_buf(12)  = block2(20)		; Number of monitors  in use
       p_buf(13)  = block2(23)		; Number of points in spectrum
       p_buf(14)  = 0.0  		; Not used     
       p_buf(15)  = 0.0  		; Not used     
       p_buf(16)  = 0.0  		; Not used     
       p_buf(17)  = 0.0  		; Not used     
       p_buf(18)  = 0.0  		; Not used     
       p_buf(19)  = 0.0  		; Not used     
       p_buf(20)  = 0.0  		; Not used     
       p_buf(21)  = 0.0  		; Not used     
       p_buf(22)  = 0.0  		; Not used     
       p_buf(23)  = 0.0  		; Not used     
       p_buf(24)  = 0.0  		; Not used     
       p_buf(25)  = 0.0  		; Not used     
       p_buf(26)  = 0.0  		; Not used     
       p_buf(27)  = 0.0  		; Not used     
       p_buf(28)  = 0.0  		; Not used     
       p_buf(29)  = 0.0  		; Not used     
       p_buf(30)  = 0.0  		; Not used     


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Duration of scan (seconds)           ='    
       par_txt(nwk_select,2)   = 'Chopper frequency (RPM)              ='
       par_txt(nwk_select,3)   = 'Lattice param. Monochromator (ang.)  ='     
       par_txt(nwk_select,4)   = 'Lattice param. Analyser      (ang.)  ='
       par_txt(nwk_select,5)   = 'Lattice param. Deflector     (ang.)  ='
       par_txt(nwk_select,6)   = 'TOF Sample-Det. (microsec.)          =' 
       par_txt(nwk_select,7)   = 'TOF Sample-M1   (microsec.)          ='
       par_txt(nwk_select,8)   = 'TOF Sample-M2   (microsec.)          ='
       par_txt(nwk_select,9)   = 'T1              (microsec.)          ='
       par_txt(nwk_select,10)  = 'T2              (microsec.)          =' 
       par_txt(nwk_select,11)  = 'Number of detectors in use           ='     
       par_txt(nwk_select,12)  = 'Number of monitors  in use           ='   
       par_txt(nwk_select,13)  = 'Number of points in spectrum         ='
       par_txt(nwk_select,14)  = 'Not used =' 
       par_txt(nwk_select,15)  = 'Not used ='
       par_txt(nwk_select,16)  = 'Not used =' 
       par_txt(nwk_select,17)  = 'Not used ='       
       par_txt(nwk_select,18)  = 'Not used =' 
       par_txt(nwk_select,19)  = 'Not used =' 
       par_txt(nwk_select,20)  = 'Not used ='
       par_txt(nwk_select,21)  = 'Not used =' 
       par_txt(nwk_select,22)  = 'Not used ='       
       par_txt(nwk_select,23)  = 'Not used =' 
       par_txt(nwk_select,24)  = 'Not used =' 
       par_txt(nwk_select,25)  = 'Not used ='
       par_txt(nwk_select,26)  = 'Not used =' 
       par_txt(nwk_select,27)  = 'Not used ='       
       par_txt(nwk_select,28)  = 'Not used ='        
       par_txt(nwk_select,29)  = 'Not used ='       
       par_txt(nwk_select,30)  = 'Not used =' 
      ENDIF
      

;------------------------------------------------------------------------------------------
;                                 IN10 Monochromator-T
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN10') AND (block2(21) eq 13) THEN BEGIN
       nb_spc = block2(19)    
       n_buf  = FLTARR(block2(23),block2(20))      ; Monitors          
       w_buf  = FLTARR(block2(23),nb_spc)
       w_tmp  = FLTARR(block0(1))
       y_buf  = block3(0:nb_spc-1)
       x_buf  = FLTARR(block2(23))
       nn_global = block2(20) & nx_global = block2(23)  & ny_global = nb_spc

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le block2(19)-1) THEN w_buf(*,ns) = w_tmp(0:block2(23)-1)
        IF (ns gt block2(19)-1) AND (ns le block2(19)+block2(20)-1) THEN BEGIN
         n_buf(*,ns-block2(19)) = w_tmp(0:block2(23)-1)
        ENDIF ELSE BEGIN
         x_buf(*) = w_tmp(0:block2(23)-1)/1000.00
        ENDELSE
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF         
       ENDFOR
       
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(21)		; Type of scan                   (index)
       p_buf(1)   = block2(0)		; Duration of scan               (seconds)
       p_buf(2)   = block2(3)		; Chopper frequency              (RPM) 
       p_buf(3)   = block2(66)		; Monochromator coeff. A0
       p_buf(4)   = block2(67)		; Monochromator coeff. A1
       p_buf(5)   = block2(68)		; Monochromator coeff. A2
       p_buf(6)   = block2(69)		; Monochromator coeff. A3
       p_buf(7)   = block2(70)		; Monochromator coeff. B0
       p_buf(8)   = block2(71)		; Monochromator coeff. B1
       p_buf(9)   = block2(72)		; Monochromator coeff. B2
       p_buf(10)  = block2(73)		; Monochromator coeff. B3
       p_buf(11)  = block2(74)		; Coeff. transition  temperature (K)
       p_buf(12)  = block2(75)		; Max. monochromator temperature (K)      
       p_buf(13)  = block2(83)		; Lattice param. Analyser        (ang.)
       p_buf(14)  = block2(82)		; Lattice param. Deflector       (ang.)
       p_buf(15)  = block2(84)		; TOF Sample-Det.                (microsec.)
       p_buf(16)  = block2(85)		; TOF Sample-M1                  (microsec.)
       p_buf(17)  = block2(86)		; TOF Sample-M2                  (microsec.)
       p_buf(18)  = block2(87)		; T1                             (microsec.)
       p_buf(19)  = block2(88)		; T2                             (microsec.)
       p_buf(20)  = block2(19)		; Number of detectors in use
       p_buf(21)  = block2(20)		; Number of monitors  in use
       p_buf(22)  = block2(23)		; Number of points in spectrum
       p_buf(23)  = block3(0)		; Scattering angle 1 (2*theta)   (deg.)
       p_buf(24)  = block3(1)		; Scattering angle 2 (2*theta)   (deg.)     
       p_buf(25)  = block3(2)		; Scattering angle 3 (2*theta)   (deg.)     
       p_buf(26)  = block3(3)		; Scattering angle 4 (2*theta)   (deg.)    
       p_buf(27)  = block3(4)		; Scattering angle 5 (2*theta)   (deg.)    
       p_buf(28)  = block3(5)		; Scattering angle 6 (2*theta)   (deg.)    
       p_buf(29)  = block3(6)		; Scattering angle 7 (2*theta)   (deg.)    
       p_buf(30)  = block3(7)		; Scattering angle 8 (2*theta)   (deg.)   


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Duration of scan (seconds)           ='    
       par_txt(nwk_select,2)   = 'Chopper frequency (RPM)              ='
       par_txt(nwk_select,3)   = 'Monochromator coeff. A0              ='     
       par_txt(nwk_select,4)   = 'Monochromator coeff. A1              ='
       par_txt(nwk_select,5)   = 'Monochromator coeff. A2              ='
       par_txt(nwk_select,6)   = 'Monochromator coeff. A3              =' 
       par_txt(nwk_select,7)   = 'Monochromator coeff. B0              ='
       par_txt(nwk_select,8)   = 'Monochromator coeff. B1              ='
       par_txt(nwk_select,9)   = 'Monochromator coeff. B2              ='
       par_txt(nwk_select,10)  = 'Monochromator coeff. B3              =' 
       par_txt(nwk_select,11)  = 'Coeff. transition  temperature (K)   ='     
       par_txt(nwk_select,12)  = 'Max. monochromator temperature (K)   ='   
       par_txt(nwk_select,13)  = 'Lattice param. Analyser      (ang.)  ='
       par_txt(nwk_select,14)  = 'Lattice param. Deflector     (ang.)  ='
       par_txt(nwk_select,15)  = 'TOF Sample-Det. (microsec.)          =' 
       par_txt(nwk_select,16)  = 'TOF Sample-M1   (microsec.)          ='
       par_txt(nwk_select,17)  = 'TOF Sample-M2   (microsec.)          ='
       par_txt(nwk_select,18)  = 'T1              (microsec.)          ='
       par_txt(nwk_select,19)  = 'T2              (microsec.)          =' 
       par_txt(nwk_select,20)  = 'Number of detectors in use           ='     
       par_txt(nwk_select,21)  = 'Number of monitors  in use           ='   
       par_txt(nwk_select,22)  = 'Number of points in spectrum         ='
       par_txt(nwk_select,23)  = 'Scattering angle 1 (2*theta) (deg.)  ='      
       par_txt(nwk_select,24)  = 'Scattering angle 2 (2*theta) (deg.)  ='  
       par_txt(nwk_select,25)  = 'Scattering angle 3 (2*theta) (deg.)  ='
       par_txt(nwk_select,26)  = 'Scattering angle 4 (2*theta) (deg.)  =' 
       par_txt(nwk_select,27)  = 'Scattering angle 5 (2*theta) (deg.)  ='    
       par_txt(nwk_select,28)  = 'Scattering angle 6 (2*theta) (deg.)  =' 
       par_txt(nwk_select,29)  = 'Scattering angle 7 (2*theta) (deg.)  ='
       par_txt(nwk_select,30)  = 'Scattering angle 8 (2*theta) (deg.)  ='  
      ENDIF
                 
;------------------------------------------------------------------------------------------
;                                 IN10 ANGLE-SCAN
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN10') AND (block2(21) ge 3) AND (block2(21) le 12) THEN BEGIN
       nb_spc = block2(19)    
       n_buf  = FLTARR(block2(23),block2(20))          ; Monitors          
       w_buf  = FLTARR(block2(23),nb_spc)
       w_tmp  = FLTARR(block0(1))
       y_buf  = FLTARR(nb_spc)
       x_buf  = FLTARR(block2(23))
       nn_global = block2(20) & nx_global = block2(23)  & ny_global = nb_spc

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le block2(19)-1) THEN w_buf(*,ns) = w_tmp(0:block2(23)-1)
        IF (ns gt block2(19)-1) AND (ns le block2(19)+block2(20)-1) THEN BEGIN
         n_buf(*,ns-block2(19)) = w_tmp(0:block2(23)-1)
        ENDIF ELSE BEGIN
         x_buf(*) = w_tmp(0:block2(23)-1)/block2(22)
        ENDELSE
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF
       ENDFOR         
       
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(21)		; Type of scan                 (index)
       p_buf(1)   = block2(0)		; Duration of scan             (seconds)
       p_buf(2)   = block2(3)		; Chopper frequency            (RPM) 
       p_buf(3)   = block2(81)		; Lattice param. Monochromator (ang.)
       p_buf(4)   = block2(83)		; Lattice param. Analyser      (ang.)
       p_buf(5)   = block2(82)		; Lattice param. Deflector     (ang.)
       p_buf(6)   = block2(84)		; TOF Sample-Det.              (microsec.)
       p_buf(7)   = block2(85)		; TOF Sample-M1                (microsec.)
       p_buf(8)   = block2(86)		; TOF Sample-M2                (microsec.)
       p_buf(9)   = block2(87)		; T1                           (microsec.)
       p_buf(10)  = block2(88)		; T2                           (microsec.)
       p_buf(11)  = block2(19)		; Number of detectors in use
       p_buf(12)  = block2(20)		; Number of monitors  in use
       p_buf(13)  = block2(23)		; Number of points in spectrum
       p_buf(14)  = 0.0  		; Not used     
       p_buf(15)  = 0.0  		; Not used     
       p_buf(16)  = 0.0  		; Not used     
       p_buf(17)  = 0.0  		; Not used     
       p_buf(18)  = 0.0  		; Not used     
       p_buf(19)  = 0.0  		; Not used     
       p_buf(20)  = 0.0  		; Not used     
       p_buf(21)  = 0.0  		; Not used     
       p_buf(22)  = 0.0  		; Not used     
       p_buf(23)  = 0.0  		; Not used     
       p_buf(24)  = 0.0  		; Not used     
       p_buf(25)  = 0.0  		; Not used     
       p_buf(26)  = 0.0  		; Not used     
       p_buf(27)  = 0.0  		; Not used     
       p_buf(28)  = 0.0  		; Not used     
       p_buf(29)  = 0.0  		; Not used     
       p_buf(30)  = 0.0  		; Not used     


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Duration of scan (seconds)           ='    
       par_txt(nwk_select,2)   = 'Chopper frequency (RPM)              ='
       par_txt(nwk_select,3)   = 'Lattice param. Monochromator (ang.)  ='     
       par_txt(nwk_select,4)   = 'Lattice param. Analyser      (ang.)  ='
       par_txt(nwk_select,5)   = 'Lattice param. Deflector     (ang.)  ='
       par_txt(nwk_select,6)   = 'TOF Sample-Det. (microsec.)          =' 
       par_txt(nwk_select,7)   = 'TOF Sample-M1   (microsec.)          ='
       par_txt(nwk_select,8)   = 'TOF Sample-M2   (microsec.)          ='
       par_txt(nwk_select,9)   = 'T1              (microsec.)          ='
       par_txt(nwk_select,10)  = 'T2              (microsec.)          =' 
       par_txt(nwk_select,11)  = 'Number of detectors in use           ='     
       par_txt(nwk_select,12)  = 'Number of monitors  in use           ='   
       par_txt(nwk_select,13)  = 'Number of points in spectrum         ='
       par_txt(nwk_select,14)  = 'Not used =' 
       par_txt(nwk_select,15)  = 'Not used ='
       par_txt(nwk_select,16)  = 'Not used =' 
       par_txt(nwk_select,17)  = 'Not used ='       
       par_txt(nwk_select,18)  = 'Not used =' 
       par_txt(nwk_select,19)  = 'Not used =' 
       par_txt(nwk_select,20)  = 'Not used ='
       par_txt(nwk_select,21)  = 'Not used =' 
       par_txt(nwk_select,22)  = 'Not used ='       
       par_txt(nwk_select,23)  = 'Not used =' 
       par_txt(nwk_select,24)  = 'Not used =' 
       par_txt(nwk_select,25)  = 'Not used ='
       par_txt(nwk_select,26)  = 'Not used =' 
       par_txt(nwk_select,27)  = 'Not used ='       
       par_txt(nwk_select,28)  = 'Not used ='        
       par_txt(nwk_select,29)  = 'Not used ='       
       par_txt(nwk_select,30)  = 'Not used =' 
      ENDIF

                  
;------------------------------------------------------------------------------------------
;                                IN16 - IN16 - IN16 - IN16
;------------------------------------------------------------------------------------------
;                                      IN16 DOPPLER
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN16') AND (block2(14) eq 0) THEN BEGIN
       nb_spc = block2(7)
       nb_mon = block2(8)
       nb_chn = block2(6)
       md_pos = block2(64)

       n_buf  = FLTARR(nb_chn,nb_mon)      ; Monitors          
       w_buf  = FLTARR(nb_chn,nb_spc)
       w_tmp  = FLTARR(nb_chn)
       y_buf  = FLTARR(nb_spc)
       x_buf  = INDGEN(nb_chn)+1

       n_det_md   = 20
       ndind      = 0       

       IF (nb_spc GT 20) THEN BEGIN
        ndind = nb_spc - n_det_md
        y_buf(0:ndind-1) = block3(20:20+ndind-1)
        FOR nd = 0L, n_det_md-1 DO BEGIN
         IF (md_pos EQ 1) THEN y_buf(ndind+nd) = block3(0+nd)
         IF (md_pos EQ 2) THEN y_buf(ndind+nd) = block3(0+nd)+5.0
         IF (md_pos EQ 3) THEN y_buf(ndind+nd) = block3(0+nd)+10.0
         IF (md_pos EQ 4) THEN y_buf(ndind+nd) = block3(0+nd)+15.0
        ENDFOR
       ENDIF

       IF (nb_spc EQ 20) THEN BEGIN
        ndind = 0
        FOR nd = 0L, n_det_md-1 DO BEGIN
         IF (md_pos EQ 1) THEN y_buf(ndind+nd) = block3(0+nd)
         IF (md_pos EQ 2) THEN y_buf(ndind+nd) = block3(0+nd)+5.0
         IF (md_pos EQ 3) THEN y_buf(ndind+nd) = block3(0+nd)+10.0
         IF (md_pos EQ 4) THEN y_buf(ndind+nd) = block3(0+nd)+15.0
        ENDFOR
       ENDIF

       IF (nb_spc LT 20) THEN BEGIN
        ndind = nb_spc    ;nb_spec??
        y_buf(0:ndind-1) = block3(20:20+ndind-1)
       ENDIF

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block2(6)/10
       nrest = block2(6)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
            
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le nb_spc-1) THEN w_buf(*,ns) = w_tmp
        IF (ns gt nb_spc-1) THEN n_buf(*,ns-nb_spc) = w_tmp
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF         
       ENDFOR
   
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(14)		; Type of scan                 (index)
       p_buf(1)   = block2(6)		; Number of channels
       p_buf(2)   = block2(2)		; Average Doppler frequency
       p_buf(3)   = block2(7)		; Number of detectors
       p_buf(4)   = block2(8)		; Number of monitors
       p_buf(5)   = block2(0)		; Duration of scan             (seconds)
       p_buf(6)   = block2(1)		; Counts in Monitor 1
       p_buf(7)   = block2(69)		; Monochromator d-spacing      (ang.)
       p_buf(8)   = block2(79)		; Analyser      d-spacing      (ang.)
       p_buf(9)   = block2(9)		; Average sample temperature   (K)
       p_buf(10)  = block2(20)		; Deflector Chopper frequency  (Hz)
       p_buf(11)  = block2(83)		; Number of dead channels
       p_buf(12)  = block2(59)		; T1                           (microsec.)
       p_buf(13)  = block2(60)		; T2                           (microsec.)
       p_buf(14)  = 0.0  		; Not used     
       p_buf(15)  = 0.0  		; Not used     
       p_buf(16)  = 0.0  		; Not used     
       p_buf(17)  = 0.0  		; Not used     
       p_buf(18)  = 0.0  		; Not used     
       p_buf(19)  = block2(64)  	; MD position     
       p_buf(20)  = block3(20)		; Single scattering angle 1    (deg.)     
       p_buf(21)  = block3(21)		; Single scattering angle 2    (deg.)     
       p_buf(22)  = block3(22)		; Single scattering angle 3    (deg.)     
       p_buf(23)  = block3(23)		; Single scattering angle 4    (deg.)     
       p_buf(24)  = block3(24)		; Single scattering angle 5    (deg.)     
       p_buf(25)  = block3(25)		; Single scattering angle 6    (deg.)     
       p_buf(26)  = block3(26)		; Single scattering angle 7    (deg.)     
       p_buf(27)  = block3(27)		; Single scattering angle 8    (deg.)     
       p_buf(28)  = block3(0)		; First angle MD-tube          (deg.)
       p_buf(29)  = block3(1)-block3(0)	; Angle increment MD           (deg.)   
       p_buf(30)  = 20.0		; Number of MD-tubes 

;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      

       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Number of channels                   ='
       par_txt(nwk_select,2)   = 'Average Doppler frequency            ='
       par_txt(nwk_select,3)   = 'Number of detectors                  ='
       par_txt(nwk_select,4)   = 'Number of monitors                   ='
       par_txt(nwk_select,5)   = 'Duration of scan (seconds)           ='
       par_txt(nwk_select,6)   = 'Counts in Monitor 1                  ='
       par_txt(nwk_select,7)   = 'Monochromator d-spacing (ang.)       ='
       par_txt(nwk_select,8)   = 'Analyser      d-spacing (ang.)       ='
       par_txt(nwk_select,9)   = 'Average sample temperature   (K)     ='
       par_txt(nwk_select,10)  = 'Deflector Chopper frequency  (Hz)    ='
       par_txt(nwk_select,11)  = 'Number of dead channels              ='
       par_txt(nwk_select,12)  = 'T1 (microsec.)                       ='
       par_txt(nwk_select,13)  = 'T2 (microsec.)                       ='
       par_txt(nwk_select,14)  = 'Not used                             ='    
       par_txt(nwk_select,15)  = 'Not used                             ='     
       par_txt(nwk_select,16)  = 'Not used                             ='     
       par_txt(nwk_select,17)  = 'Not used                             ='     
       par_txt(nwk_select,18)  = 'Not used                             ='     
       par_txt(nwk_select,19)  = 'Not used                             ='     
       par_txt(nwk_select,20)  = 'Single scattering angle 1    (deg.)  ='     
       par_txt(nwk_select,21)  = 'Single scattering angle 2    (deg.)  ='     
       par_txt(nwk_select,22)  = 'Single scattering angle 3    (deg.)  ='     
       par_txt(nwk_select,23)  = 'Single scattering angle 4    (deg.)  ='     
       par_txt(nwk_select,24)  = 'Single scattering angle 5    (deg.)  ='     
       par_txt(nwk_select,25)  = 'Single scattering angle 6    (deg.)  ='     
       par_txt(nwk_select,26)  = 'Single scattering angle 7    (deg.)  ='     
       par_txt(nwk_select,27)  = 'Single scattering angle 8    (deg.)  ='     
       par_txt(nwk_select,28)  = 'First angle MD-tube          (deg.)  ='
       par_txt(nwk_select,29)  = 'Angle increment MD           (deg.)  ='   
       par_txt(nwk_select,30)  = 'Number of MD-tubes                   ='
      ENDIF
;------------------------------------------------------------------------------------------
;                                 IN16 ELASTIC SCAN
;                                      SAMPLE_T
;------------------------------------------------------------------------------------------
     
      IF (inst_value eq 'IN16') AND (block2(14) eq 1) THEN BEGIN
       
       nb_spc = block2(7)
       nb_mon = block2(8)
       nb_chn = block2(6)

       md_pos = block2(64)
   
       n_buf  = FLTARR(nb_chn,nb_spc)      ; Monitors          
       w_buf  = FLTARR(nb_chn,nb_spc)
       w_tmp  = FLTARR(nb_chn)
       y_buf  = FLTARR(nb_spc)
       x_buf  = FLTARR(nb_chn)

       n_det_md   = 20
       ndind      = 0       

       IF (nb_spc GT 20) THEN BEGIN
        ndind = nb_spc - n_det_md
        y_buf(0:ndind-1) = block3(20:20+ndind-1)
        FOR nd = 0L, n_det_md-1 DO BEGIN
         IF (md_pos EQ 1) THEN y_buf(ndind+nd) = block3(0+nd)
         IF (md_pos EQ 2) THEN y_buf(ndind+nd) = block3(0+nd)+5.0
         IF (md_pos EQ 3) THEN y_buf(ndind+nd) = block3(0+nd)+10.0
         IF (md_pos EQ 4) THEN y_buf(ndind+nd) = block3(0+nd)+15.0
        ENDFOR
       ENDIF

       IF (nb_spc EQ 20) THEN BEGIN
        ndind = 0
        FOR nd = 0L, n_det_md-1 DO BEGIN
         IF (md_pos EQ 1) THEN y_buf(ndind+nd) = block3(0+nd)
         IF (md_pos EQ 2) THEN y_buf(ndind+nd) = block3(0+nd)+5.0
         IF (md_pos EQ 3) THEN y_buf(ndind+nd) = block3(0+nd)+10.0
         IF (md_pos EQ 4) THEN y_buf(ndind+nd) = block3(0+nd)+15.0
        ENDFOR
       ENDIF

       IF (nb_spc LT 20) THEN BEGIN
        ndind = nb_spc    ;nb_spec???
        y_buf(0:ndind-1) = block3(20:20+ndind-1)
       ENDIF

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le block2(7)-1) THEN w_buf(*,ns) = w_tmp(0:block2(6)-1)
        IF (ns gt block2(7)-1) AND (ns le block2(7)+block2(8)-1) THEN BEGIN
         n_buf(*,ns-block2(7)) = w_tmp(0:block2(6)-1)
        ENDIF ELSE BEGIN
         x_buf(*) = w_tmp(0:block2(6)-1)/1000.00
        ENDELSE
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF         
       ENDFOR
                     
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(14)		; Type of scan                 (index)
       p_buf(1)   = block2(6)		; Number of channels
       p_buf(2)   = block2(7)		; Number of detectors
       p_buf(3)   = block2(8)		; Number of monitors
       p_buf(4)   = block2(15)		; Mesuring time per step       (seconds)
       p_buf(5)   = 0.0  		; Not used 
       p_buf(6)   = 0.0  		; Not used 
       p_buf(7)   = block2(69)		; Monochromator d-spacing      (ang.)
       p_buf(8)   = block2(79)		; Analyser      d-spacing      (ang.)
       p_buf(9)   = block2(9)		; Average sample temperature   (K)
       p_buf(10)  = block2(20)		; Deflector Chopper frequency  (Hz)
       p_buf(11)  = block2(83)		; Number of dead channels
       p_buf(12)  = block2(59)		; T1                           (microsec.)
       p_buf(13)  = block2(60)		; T2                           (microsec.)
       p_buf(14)  = 0.0  		; Not used     
       p_buf(15)  = 0.0  		; Not used     
       p_buf(16)  = 0.0  		; Not used     
       p_buf(17)  = 0.0  		; Not used     
       p_buf(18)  = 0.0  		; Not used     
       p_buf(19)  = block2(64)  	; MD position     
       p_buf(20)  = block3(20)		; Single scattering angle 1    (deg.)     
       p_buf(21)  = block3(21)		; Single scattering angle 2    (deg.)     
       p_buf(22)  = block3(22)		; Single scattering angle 3    (deg.)     
       p_buf(23)  = block3(23)		; Single scattering angle 4    (deg.)     
       p_buf(24)  = block3(24)		; Single scattering angle 5    (deg.)     
       p_buf(25)  = block3(25)		; Single scattering angle 6    (deg.)     
       p_buf(26)  = block3(26)		; Single scattering angle 7    (deg.)     
       p_buf(27)  = block3(27)		; Single scattering angle 8    (deg.)     
       p_buf(28)  = block3(0)		; First angle MD-tube          (deg.)
       p_buf(29)  = block3(1)-block3(0)	; Angle increment MD           (deg.)   
       p_buf(30)  = 20.0		; Number of MD-tubes


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      

       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Number of channels                   ='
       par_txt(nwk_select,2)   = 'Number of detectors                  ='
       par_txt(nwk_select,3)   = 'Number of monitors                   ='
       par_txt(nwk_select,4)   = 'Mesuring time per step (seconds)     ='
       par_txt(nwk_select,5)   = 'Not used                             ='    
       par_txt(nwk_select,6)   = 'Not used                             ='    
       par_txt(nwk_select,7)   = 'Monochromator d-spacing (ang.)       ='
       par_txt(nwk_select,8)   = 'Analyser      d-spacing (ang.)       ='
       par_txt(nwk_select,9)   = 'Average sample temperature   (K)     ='
       par_txt(nwk_select,10)  = 'Deflector Chopper frequency  (Hz)    ='
       par_txt(nwk_select,11)  = 'Number of dead channels              ='
       par_txt(nwk_select,12)  = 'T1 (microsec.)                       ='
       par_txt(nwk_select,13)  = 'T2 (microsec.)                       ='
       par_txt(nwk_select,14)  = 'Not used                             ='    
       par_txt(nwk_select,15)  = 'Not used                             ='     
       par_txt(nwk_select,16)  = 'Not used                             ='     
       par_txt(nwk_select,17)  = 'Not used                             ='     
       par_txt(nwk_select,18)  = 'Not used                             ='     
       par_txt(nwk_select,19)  = 'Not used                             ='     
       par_txt(nwk_select,20)  = 'Single scattering angle 1    (deg.)  ='     
       par_txt(nwk_select,21)  = 'Single scattering angle 2    (deg.)  ='     
       par_txt(nwk_select,22)  = 'Single scattering angle 3    (deg.)  ='     
       par_txt(nwk_select,23)  = 'Single scattering angle 4    (deg.)  ='     
       par_txt(nwk_select,24)  = 'Single scattering angle 5    (deg.)  ='     
       par_txt(nwk_select,25)  = 'Single scattering angle 6    (deg.)  ='     
       par_txt(nwk_select,26)  = 'Single scattering angle 7    (deg.)  ='     
       par_txt(nwk_select,27)  = 'Single scattering angle 8    (deg.)  ='     
       par_txt(nwk_select,28)  = 'First angle MD-tube          (deg.)  ='
       par_txt(nwk_select,29)  = 'Angle increment MD           (deg.)  ='   
       par_txt(nwk_select,30)  = 'Number of MD-tubes                   ='
      ENDIF
      

;------------------------------------------------------------------------------------------
;                                 IN16 Monochromator-T
;                                     To implement
;------------------------------------------------------------------------------------------

                 
;------------------------------------------------------------------------------------------
;                                 IN16 ANGLE-SCAN
;       ThetaD1, CD1, ThetaD2, 2ThetaA, ThetaM, GM, G1S, G2S, ThetaS, ZS, ThetaA
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'IN16') AND (block2(14) ge 2) AND (block2(14) le 12) THEN BEGIN
       nb_spc = block2(7)
       nb_mon = block2(8)
       nb_chn = block2(6)
       md_pos = block2(64)   
       n_buf  = FLTARR(nb_chn,nb_spc)      ; Monitors          
       w_buf  = FLTARR(nb_chn,nb_spc)
       w_tmp  = FLTARR(nb_chn)
       y_buf  = FLTARR(nb_spc)
       x_buf  = FLTARR(nb_chn)

       n_det_md   = 20
       ndind      = 0       

       IF (nb_spc GT 20) THEN BEGIN
        ndind = nb_spc - n_det_md
        y_buf(0:ndind-1) = block3(20:20+ndind-1)
        FOR nd = 0L, n_det_md-1 DO BEGIN
         IF (md_pos EQ 1) THEN y_buf(ndind+nd) = block3(0+nd)
         IF (md_pos EQ 2) THEN y_buf(ndind+nd) = block3(0+nd)+5.0
         IF (md_pos EQ 3) THEN y_buf(ndind+nd) = block3(0+nd)+10.0
         IF (md_pos EQ 4) THEN y_buf(ndind+nd) = block3(0+nd)+15.0
        ENDFOR
       ENDIF

       IF (nb_spc EQ 20) THEN BEGIN
        ndind = 0
        FOR nd = 0L, n_det_md-1 DO BEGIN
         IF (md_pos EQ 1) THEN y_buf(ndind+nd) = block3(0+nd)
         IF (md_pos EQ 2) THEN y_buf(ndind+nd) = block3(0+nd)+5.0
         IF (md_pos EQ 3) THEN y_buf(ndind+nd) = block3(0+nd)+10.0
         IF (md_pos EQ 4) THEN y_buf(ndind+nd) = block3(0+nd)+15.0
        ENDFOR
       ENDIF

       IF (nb_spc LT 20) THEN BEGIN
        ndind = nb_spc    ;nb_spec???
        y_buf(0:ndind-1) = block3(20:20+ndind-1)
       ENDIF
       
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
                     
       FOR ns = 0L, block0(0)-1 DO BEGIN     
        READF, ilun, w_tmp
        IF (ns le block2(7)-1) THEN w_buf(*,ns) = w_tmp(0:block2(6)-1)
        IF (ns gt block2(7)-1) AND (ns le block2(7)+block2(8)-1) THEN BEGIN
         n_buf(*,ns-block2(7)) = w_tmp(0:block2(6)-1)
        ENDIF ELSE BEGIN
         x_buf(*) = w_tmp(0:block2(6)-1)/100.00
        ENDELSE
        IF (ns lt block0(0)-1) THEN BEGIN
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
         READF, ilun, line
        ENDIF         
       ENDFOR
      
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(14)		; Type of scan                 (index)
       p_buf(1)   = block2(6)		; Number of channels
       p_buf(2)   = block2(7)		; Number of detectors
       p_buf(3)   = block2(8)		; Number of monitors
       p_buf(4)   = block2(15)		; Mesuring time per step       (seconds)
       p_buf(5)   = 0.0  		; Not used 
       p_buf(6)   = 0.0  		; Not used 
       p_buf(7)   = block2(69)		; Monochromator d-spacing      (ang.)
       p_buf(8)   = block2(79)		; Analyser      d-spacing      (ang.)
       p_buf(9)   = block2(9)		; Average sample temperature   (K)
       p_buf(10)  = block2(20)		; Deflector Chopper frequency  (Hz)
       p_buf(11)  = block2(83)		; Number of dead channels
       p_buf(12)  = block2(59)		; T1                           (microsec.)
       p_buf(13)  = block2(60)		; T2                           (microsec.)
       p_buf(14)  = 0.0  		; Not used     
       p_buf(15)  = 0.0  		; Not used     
       p_buf(16)  = 0.0  		; Not used     
       p_buf(17)  = 0.0  		; Not used     
       p_buf(18)  = 0.0  		; Not used     
       p_buf(19)  = block2(64)  	; MD position     
       p_buf(20)  = block3(20)		; Single scattering angle 1    (deg.)     
       p_buf(21)  = block3(21)		; Single scattering angle 2    (deg.)     
       p_buf(22)  = block3(22)		; Single scattering angle 3    (deg.)     
       p_buf(23)  = block3(23)		; Single scattering angle 4    (deg.)     
       p_buf(24)  = block3(24)		; Single scattering angle 5    (deg.)     
       p_buf(25)  = block3(25)		; Single scattering angle 6    (deg.)     
       p_buf(26)  = block3(26)		; Single scattering angle 7    (deg.)     
       p_buf(27)  = block3(27)		; Single scattering angle 8    (deg.)     
       p_buf(28)  = block3(0)		; First angle MD-tube          (deg.)
       p_buf(29)  = block3(1)-block3(0)	; Angle increment MD           (deg.)   
       p_buf(30)  = 20.0		; Number of MD-tubes


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      

       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Number of channels                   ='
       par_txt(nwk_select,2)   = 'Number of detectors                  ='
       par_txt(nwk_select,3)   = 'Number of monitors                   ='
       par_txt(nwk_select,4)   = 'Mesuring time per step (seconds)     ='
       par_txt(nwk_select,5)   = 'Not used                             ='    
       par_txt(nwk_select,6)   = 'Not used                             ='    
       par_txt(nwk_select,7)   = 'Monochromator d-spacing (ang.)       ='
       par_txt(nwk_select,8)   = 'Analyser      d-spacing (ang.)       ='
       par_txt(nwk_select,9)   = 'Average sample temperature   (K)     ='
       par_txt(nwk_select,10)  = 'Deflector Chopper frequency  (Hz)    ='
       par_txt(nwk_select,11)  = 'Number of dead channels              ='
       par_txt(nwk_select,12)  = 'T1 (microsec.)                       ='
       par_txt(nwk_select,13)  = 'T2 (microsec.)                       ='
       par_txt(nwk_select,14)  = 'Not used                             ='    
       par_txt(nwk_select,15)  = 'Not used                             ='     
       par_txt(nwk_select,16)  = 'Not used                             ='     
       par_txt(nwk_select,17)  = 'Not used                             ='     
       par_txt(nwk_select,18)  = 'Not used                             ='     
       par_txt(nwk_select,19)  = 'Not used                             ='     
       par_txt(nwk_select,20)  = 'Single scattering angle 1    (deg.)  ='     
       par_txt(nwk_select,21)  = 'Single scattering angle 2    (deg.)  ='     
       par_txt(nwk_select,22)  = 'Single scattering angle 3    (deg.)  ='     
       par_txt(nwk_select,23)  = 'Single scattering angle 4    (deg.)  ='     
       par_txt(nwk_select,24)  = 'Single scattering angle 5    (deg.)  ='     
       par_txt(nwk_select,25)  = 'Single scattering angle 6    (deg.)  ='     
       par_txt(nwk_select,26)  = 'Single scattering angle 7    (deg.)  ='     
       par_txt(nwk_select,27)  = 'Single scattering angle 8    (deg.)  ='     
       par_txt(nwk_select,28)  = 'First angle MD-tube          (deg.)  ='
       par_txt(nwk_select,29)  = 'Angle increment MD           (deg.)  ='   
       par_txt(nwk_select,30)  = 'Number of MD-tubes                   ='
      ENDIF
      
      
;     -------------------------------
;     ASSIGNING WORKSPACE VARIABLES
;     -------------------------------
      IF (inst_value EQ 'IN16') THEN BEGIN
          IF nb_spc GT 20 THEN BEGIN
	     www_buf=w_buf
	     www_buf(*,0:ndind-1)=w_buf(*,20:20+ndind-1)     ; Normal detectors to beginning spectra
	     www_buf(*,ndind:19+ndind)=w_buf(*,0:19)         ; Multidet to end  
	     w_buf=www_buf  
	  ENDIF
      ENDIF

      junk   = EXECUTE('x'+swk_select+'=x_buf')
      junk   = EXECUTE('y'+swk_select+'=y_buf')
      junk   = EXECUTE('w'+swk_select+'=w_buf')
      junk   = EXECUTE('n'+swk_select+'=n_buf')        
      junk   = EXECUTE('p'+swk_select+'=p_buf')

;     ---------------------------------------
;     NORMALISATION ON M1 - OR NOT - FOR IN10
;     ---------------------------------------
      IF ((inst_value EQ 'IN10') AND (flag_norm(nwk_select-1) EQ 1)) THEN BEGIN
       m1   = FLTARR(nx_global)
       junk = EXECUTE('m1=n'+swk_select+'(*,0)') 
       junk = EXECUTE('w_buf=w'+swk_select)
       FOR npt = 0, nx_global-1 DO BEGIN
        IF (m1(npt) EQ 0) THEN BEGIN
         w_buf(npt,*) = 0
        ENDIF ELSE BEGIN
         w_buf(npt,*) = w_buf(npt,*) * f_norm_bck / m1(npt)
        ENDELSE
       ENDFOR
       junk = EXECUTE('w'+swk_select+'=w_buf')
       z_tit(nwk_select) = 'Counts (*'+sf_norm_bck+') / M1'
      ENDIF ELSE BEGIN
       z_tit(nwk_select) = 'Counts'
      ENDELSE 
         
      x_tit    (nwk_select) = 'Channels'
      y_tit    (nwk_select) = 'Spectrum number'
      w_tit    (nwk_select) = strtrim(main_title,2)             
      other_tit(nwk_select) = w_numor(nwk_select)+' '+sub_title+' Start:'+start_time

;     ---------------------------
;     TITLES FOR HEADER SHOW OUT
;     ---------------------------
      IF (inst_value EQ 'IN10') THEN BEGIN
       IF (p_buf(0) EQ 0) THEN BEGIN
        x_tit(nwk_select) = 'Channels'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 1)  THEN BEGIN
        x_tit(nwk_select) = 'T/K'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 2)  THEN BEGIN
        x_tit(nwk_select) = 'T/K'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 3)  THEN BEGIN
        x_tit(nwk_select) = 'CHI_M  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 4)  THEN BEGIN
        x_tit(nwk_select) = 'THETA_M2  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 5)  THEN BEGIN
        x_tit(nwk_select) = 'THETA_M1  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 6)  THEN BEGIN
        x_tit(nwk_select) = 'CHI_E1  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF
       IF (p_buf(0) EQ 7)  THEN BEGIN
        x_tit(nwk_select) = 'CHI_E2  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 8)  THEN BEGIN
        x_tit(nwk_select) = 'THETA_G  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 9)  THEN BEGIN
        x_tit(nwk_select) = 'CHI_G  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 10) THEN BEGIN
        x_tit(nwk_select) = '2THETA_G  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 11) THEN BEGIN
        x_tit(nwk_select) = 'OMEGA_E1  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 12) THEN BEGIN
        x_tit(nwk_select) = 'OMEGA_E2  (deg.)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF         
       IF (p_buf(0) EQ 13) THEN BEGIN
        x_tit(nwk_select) = 'Ef-Ei  (ueV)'
        y_tit(nwk_select) = 'Spectrum number'
       ENDIF
       head_tit(nwk_select,0) = sub_title2
       head_tit(nwk_select,1) = main_title
       head_tit(nwk_select,2) = inst_value
       head_tit(nwk_select,3) = STRING(LONG(num))
       head_tit(nwk_select,4) = start_time
       head_tit(nwk_select,5) = ''
       head_tit(nwk_select,6) = x_tit(nwk_select)
       head_tit(nwk_select,7) = y_tit(nwk_select)
       head_tit(nwk_select,8) = z_tit(nwk_select)
       head_tit(nwk_select,9) = ''                
      ENDIF      

      IF (inst_value EQ 'IN16') THEN BEGIN
       IF (p_buf(0) EQ 0) THEN BEGIN
        x_tit(nwk_select) = 'Channels'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 1)  THEN BEGIN
        x_tit(nwk_select) = 'T/K'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 2)  THEN BEGIN
        x_tit(nwk_select) = 'Gamma-1 (deg.)'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 3)  THEN BEGIN
        x_tit(nwk_select) = 'Gamma-2 (deg.)'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 4)  THEN BEGIN
        x_tit(nwk_select) = 'THETA-S (deg.)'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 5)  THEN BEGIN
        x_tit(nwk_select) = 'Sample Height'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 6)  THEN BEGIN
        x_tit(nwk_select) = 'Theta-ANALYSER'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF
       IF (p_buf(0) EQ 7)  THEN BEGIN
        x_tit(nwk_select) = '2*Theta-ANALYSER'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 8)  THEN BEGIN
        x_tit(nwk_select) = 'Theta-Monochromator (deg.)'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 9)  THEN BEGIN
        x_tit(nwk_select) = 'Gamma-Monochromator'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 10) THEN BEGIN
        x_tit(nwk_select) = 'Theta-D1  (deg.)'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 11) THEN BEGIN
        x_tit(nwk_select) = 'CD1'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       IF (p_buf(0) EQ 12) THEN BEGIN
        x_tit(nwk_select) = 'Theta-D2  (deg.)'
        y_tit(nwk_select) = '2-Theta (deg.)'
       ENDIF         
       head_tit(nwk_select,0) = sub_title2
       head_tit(nwk_select,1) = main_title
       head_tit(nwk_select,2) = inst_value
       head_tit(nwk_select,3) = STRING(LONG(num))
       head_tit(nwk_select,4) = start_time
       head_tit(nwk_select,5) = ''
       head_tit(nwk_select,6) = x_tit(nwk_select)
       head_tit(nwk_select,7) = y_tit(nwk_select)
       head_tit(nwk_select,8) = z_tit(nwk_select)
       head_tit(nwk_select,9) = ''                
      ENDIF      
end_read_bsc: FREE_LUN, ilun
            
      return
      end
;------------------------------------------------------------------------------------------

PRO   READ_SAS

@lamp.cbk
@mics.cbk
;     --------------------------------------------------
;     READ-IN D11, D22 & D17 SMALL-ANGLE SCATTERING DATA
;     --------------------------------------------------

      ON_IOERROR, end_READ_SAS
      GET_LUN, ilun
      OPENR, ilun, file_found(0)

;     -------------------------------
;     READ HEADER & PARAMETERS BLOCKS
;     -------------------------------
      line = STRARR(1)  & line(0)='' & lin=''
      READF, ilun, line
      num = LONG(0)
      READF, ilun, num
      READF, ilun, line
      READF, ilun, line
      instdate = STRARR(1)
      READF, ilun, instdate
      inst   = STRMID(instdate,0,4)
      numexp = STRMID(instdate,4,10)
      date   = STRMID(instdate,14,18)
;------------------------------------
      READF, ilun, line
      nf0 = LONG(0)
      READF, ilun, nf0
      block0 = INTARR(nf0)
      READF, ilun, block0
      nb_spc = block0(0)  & nb_points = block0(1)
      dim    = LONG(SQRT(nb_points))
      nblk_to_read = LONG(0)
      FOR nb = 1, 8 DO BEGIN			; Maximum Header Blocks = 8
       IF (block0(2*nb) NE 0) THEN nblk_to_read = nblk_to_read+1
      ENDFOR
;------------------------------------
      FOR nbl = 0, nblk_to_read-1 DO BEGIN
       READF, ilun, line
       IF (line(0) EQ aline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        nline  = nfield/80
        nf_ll  = nfield-(nline*80)
        IF (nf_ll NE 0) THEN nline = nline+1
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=STRARR(nline)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
       IF (line(0) EQ fline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=FLTARR(nfield)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
       IF (line(0) EQ iline(0)) THEN BEGIN
        nfield = LONG(0)
        READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
        if num2 gt 0 then for i=1,num2 do READF, ilun, lin
        sbl    = STRTRIM(STRING(nbl+1),2)
        junk   = EXECUTE('block'+sbl+'=LONARR(nfield)')
        junk   = EXECUTE('READF, ilun, block'+sbl)
       ENDIF
      ENDFOR  
;------------------------------------ 
      main_title = STRMID(block1(0),0,60)
      sub_title  = STRMID(block1(0),60,20)
      start_time = STRMID(block1(1),0,20)      
      stop_time  = STRMID(block1(1),20,20)
;DID!
      x_tit    (nwk_select) = 'X-detector'
      y_tit    (nwk_select) = 'Y-detector'
      z_tit    (nwk_select) = 'Sample Angle'
      w_tit    (nwk_select) = STRTRIM(main_title,2)
      other_tit(nwk_select) = w_numor(nwk_select)+' '+sub_title+' Start:'+start_time

      head_tit(nwk_select,4) = STRTRIM(start_time,2)
;------------------------------------------------------------------------------------------
;                                 D22 - D22 - D17 - D17
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'D22') OR (inst_value eq 'D17') THEN BEGIN
       x_buf  = INDGEN(dim)
       y_buf  = INDGEN(dim)
       w_buf  = FLTARR(dim,dim)

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1
       READF, ilun, w_buf

;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(0)		; PRESET 1
       p_buf(1)   = block2(1)		; PRESET 2    
       p_buf(2)   = block2(2)		; Run duration (1/10 sec.) 
       p_buf(3)   = block2(3)		; Total detector counts
       p_buf(4)   = block2(14)		; Detector offset angle
       p_buf(5)   = block2(15)		; Coder 1: By    
       p_buf(6)   = block2(16)		; Coder 2: Bx
       p_buf(7)   = block2(17)		; Coder 3: Sample changer/s transl.
       p_buf(8)   = block2(18)		; Coder 4: Detector distance (set)
       p_buf(9)   = block2(25)		; Sample-Detector distance (calc.)
       p_buf(10)  = block2(30)		; Sample Temperature 
       p_buf(11)  = block2(32)		; Value of IEEE-1 at start
       p_buf(12)  = block2(33)		; Value of IEEE-1 at end
       p_buf(13)  = block2(50)		; Beam centre adress X0
       p_buf(14)  = block2(51)		; Beam centre adress Y0
       p_buf(15)  = block2(52)		; Wavelength
       p_buf(16)  = block2(53)		; Wavelength resolution
       p_buf(17)  = block2(57)		; Collimation   
       p_buf(18)  = block2(60)		; Detector angle (set)
       p_buf(19)  = block2(61)		; Detector translation (set)
       p_buf(20)  = block2(62)		; Selector angle
       p_buf(21)  = block2(63)		; Sample rotation
       p_buf(22)  = block2(64)		; Sample angle
       p_buf(23)  = block2(65)		; Changer position
       p_buf(24)  = block2(66)		; Sample height 
       p_buf(25)  = block2(80)		; Shear speed
       p_buf(26)  = num  		; Numor
       p_buf(27)  = 0.0  		; Not used
       p_buf(28)  = 0.0  		; Not used      
       p_buf(29)  = 0.0  		; Not used
       p_buf(30)  = 0.0  		; Not used


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'PRESET 1                             ='
       par_txt(nwk_select,1)   = 'PRESET 2                             ='    
       par_txt(nwk_select,2)   = 'Run duration (1/10 sec.)             =' 
       par_txt(nwk_select,3)   = 'Total detector counts                ='
       par_txt(nwk_select,4)   = 'Detector offset angle (deg.)         ='
       par_txt(nwk_select,5)   = 'Coder 1: By (mm)                     ='    
       par_txt(nwk_select,6)   = 'Coder 2: Bx (mm)                     ='
       par_txt(nwk_select,7)   = 'Coder 3: Sample changer transl. (mm) ='
       par_txt(nwk_select,8)   = 'Coder 4: Detector distance (set) (m) ='
       par_txt(nwk_select,9)   = 'Sample-Detector distance (calc.) (m) ='
       par_txt(nwk_select,10)  = 'Sample Temperature (K)               =' 
       par_txt(nwk_select,11)  = 'Value of IEEE-1 at start             ='
       par_txt(nwk_select,12)  = 'Value of IEEE-1 at end               ='
       par_txt(nwk_select,13)  = 'Beam centre adress X0 (mm)           ='
       par_txt(nwk_select,14)  = 'Beam centre adress Y0 (mm)           ='
       par_txt(nwk_select,15)  = 'Wavelength (angstroms)               ='
       par_txt(nwk_select,16)  = 'Wavelength resolution                ='
       par_txt(nwk_select,17)  = 'Collimation  (m)                     =' 
       par_txt(nwk_select,18)  = 'Detector angle (set) (deg.)          ='
       par_txt(nwk_select,19)  = 'Detector translation (set) (mm)      ='
       par_txt(nwk_select,20)  = 'Selector angle (deg.)                ='
       par_txt(nwk_select,21)  = 'Sample distance (mm)                 ='
       par_txt(nwk_select,22)  = 'Sample rotation (deg.)               ='
       par_txt(nwk_select,23)  = 'Changer position                     ='
       par_txt(nwk_select,24)  = 'Sample height (mm)                   =' 
       par_txt(nwk_select,25)  = 'Shear speed (1/min.)                 ='
       par_txt(nwk_select,26)  = 'Numor                                ='
       par_txt(nwk_select,27)  = 'Not used                             ='
       par_txt(nwk_select,28)  = 'Not used                             ='     
       par_txt(nwk_select,29)  = 'Not used                             ='
       par_txt(nwk_select,30)  = 'Not used                             ='
      ENDIF
                  
;------------------------------------------------------------------------------------------
;                                 D11 - D11 - D11 - D11
;------------------------------------------------------------------------------------------
      
      IF (inst_value eq 'D11') THEN BEGIN
       x_buf  = INDGEN(dim)
       y_buf  = INDGEN(dim)
       w_buf  = FLTARR(dim,dim)

       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       READF, ilun, line
       nline = block0(1)/10
       nrest = block0(1)-nline*10
       IF (nrest NE 0) THEN nline=nline+1

       READF, ilun, w_buf

       w_buf(0,0)=1
       
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = block2(0)		; PRESET 1
       p_buf(1)   = block2(1)		; PRESET 2    
       p_buf(2)   = block2(2)		; Run duration (sec.) 
       p_buf(3)   = block2(3)		; Total detector counts
       p_buf(4)   = block2(14)		; Detector offset angle
       p_buf(5)   = block2(15)		; Coder 1: By    
       p_buf(6)   = block2(16)		; Coder 2: Bx
       p_buf(7)   = block2(17)		; Coder 3: Sample changer/s transl.
       p_buf(8)   = block2(18)		; Coder 4: Detector distance (set)
       p_buf(9)   = block2(25)		; Sample-Detector distance (calc.)
       p_buf(10)  = block2(30)		; Sample Temperature 
       p_buf(11)  = block2(32)		; Value of IEEE-1 at start
       p_buf(12)  = block2(33)		; Value of IEEE-1 at end
       p_buf(13)  = block2(50)		; X0
       p_buf(14)  = block2(51)		; Y0
       p_buf(15)  = block2(52)		; Wavelength
       p_buf(16)  = block2(53)		; Wavelength resolution
       p_buf(17)  = block2(57)		; Collimation   
       p_buf(18)  = block2(60)		; Detector angle (set)
       p_buf(19)  = block2(61)		; Detector translation (set)
       p_buf(20)  = block2(62)		; Selector angle
       p_buf(21)  = block2(63)		; Sample distance
       p_buf(22)  = block2(64)		; Sample rotation
       p_buf(23)  = block2(65)		; Changer position
       p_buf(24)  = block2(66)		; Sample height 
       p_buf(25)  = block2(80)		; Shear speed
       p_buf(26)  = num  		; Numor
       p_buf(27)  = 0.0  		; Not used
       p_buf(28)  = 0.0  		; Not used      
       p_buf(29)  = 0.0  		; Not used
       p_buf(30)  = 0.0  		; Not used


;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      
      
       par_txt(nwk_select,0)   = 'PRESET 1                             ='
       par_txt(nwk_select,1)   = 'PRESET 2                             ='    
       par_txt(nwk_select,2)   = 'Run duration (sec.)                  =' 
       par_txt(nwk_select,3)   = 'Total detector counts                ='
       par_txt(nwk_select,4)   = 'Detector offset angle (deg.)         ='
       par_txt(nwk_select,5)   = 'Coder 1: By (mm)                     ='    
       par_txt(nwk_select,6)   = 'Coder 2: Bx (mm)                     ='
       par_txt(nwk_select,7)   = 'Coder 3: Sample changer transl. (mm) ='
       par_txt(nwk_select,8)   = 'Coder 4: Detector distance (set) (m) ='
       par_txt(nwk_select,9)   = 'Sample-Detector distance (calc.) (m) ='
       par_txt(nwk_select,10)  = 'Sample Temperature (K)               =' 
       par_txt(nwk_select,11)  = 'Value of IEEE-1 at start             ='
       par_txt(nwk_select,12)  = 'Value of IEEE-1 at end               ='
       par_txt(nwk_select,13)  = 'X0 (mm)                              ='
       par_txt(nwk_select,14)  = 'Y0 (mm)                              ='
       par_txt(nwk_select,15)  = 'Wavelength (angstroms)               ='
       par_txt(nwk_select,16)  = 'Wavelength resolution                ='
       par_txt(nwk_select,17)  = 'Collimation  (m)                     =' 
       par_txt(nwk_select,18)  = 'Detector angle (set) (deg.)          ='
       par_txt(nwk_select,19)  = 'Detector translation (set) (mm)      ='
       par_txt(nwk_select,20)  = 'Selector angle (deg.)                ='
       par_txt(nwk_select,21)  = 'Sample distance (mm)                 ='
       par_txt(nwk_select,22)  = 'Sample rotation (deg.)               ='
       par_txt(nwk_select,23)  = 'Changer position                     ='
       par_txt(nwk_select,24)  = 'Sample height (mm)                   =' 
       par_txt(nwk_select,25)  = 'Shear speed (1/min.)                 ='
       par_txt(nwk_select,26)  = 'Numor                                ='
       par_txt(nwk_select,27)  = 'Not used                             ='
       par_txt(nwk_select,28)  = 'Not used                             ='     
       par_txt(nwk_select,29)  = 'Not used                             ='
       par_txt(nwk_select,30)  = 'Not used                             ='
      ENDIF
            
      junk   = EXECUTE('x' +swk_select+'=x_buf')
      junk   = EXECUTE('y' +swk_select+'=y_buf')
      junk   = EXECUTE('z' +swk_select+'=p_buf(22)')
      junk   = EXECUTE('w' +swk_select+'=w_buf')
      junk   = EXECUTE('p' +swk_select+'=p_buf')
      junk   = EXECUTE('n' +swk_select+'=p_buf(0 )')

;     -----------------------------------
;     NORMALISATION ON PRESET1 -OR NOT -
;     -----------------------------------
      IF (flag_norm(nwk_select-1) EQ 1) THEN BEGIN
       junk  = EXECUTE('w'+swk_select+'=w'+swk_select+'/p'+swk_select+'(0)')
      ENDIF
      

end_READ_SAS: FREE_LUN, ilun
      
      return
      end
      
;------------------------------------------------------------------------------------------
PRO   READ_D16

@lamp.cbk
@mics.cbk
   			
;     ----------------------------
;     READ-IN D16 DIFFRACTION DATA
;     ----------------------------

      ON_IOERROR, end_READ_D16
      GET_LUN, ilun

;     For the moment (IDOL is not installed on D16) we can only access
;     D16 data or data-1 on serdon (path: /usr1/data or data-1/d16/d16_0to9/numor)

      d     =lamp_dvd
      pos1  =STRPOS(file_found(0),'d16'+d)
      len1  =STRLEN(file_found(0))
      numd16=STRMID(file_found(0),len1-6,6)
      IF (STRMID(numd16,1,1) EQ '0') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_0'+d + numd16
      IF (STRMID(numd16,1,1) EQ '1') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_1'+d + numd16
      IF (STRMID(numd16,1,1) EQ '2') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_2'+d + numd16
      IF (STRMID(numd16,1,1) EQ '3') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_3'+d + numd16
      IF (STRMID(numd16,1,1) EQ '4') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_4'+d + numd16
      IF (STRMID(numd16,1,1) EQ '5') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_5'+d + numd16
      IF (STRMID(numd16,1,1) EQ '6') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_6'+d + numd16
      IF (STRMID(numd16,1,1) EQ '7') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_7'+d + numd16
      IF (STRMID(numd16,1,1) EQ '8') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_8'+d + numd16
      IF (STRMID(numd16,1,1) EQ '9') THEN file_found = STRMID(file_found(0),0,pos1) + 'd16'+d+'d16_9'+d + numd16
                                                       
      
      OPENR, ilun, file_found(0)

;     -------------------------------
;     READ HEADER & PARAMETERS BLOCKS
;     -------------------------------
      line = STRARR(1)  & line(0)=''
      READF, ilun, line
      num = LONG(0)
      READF, ilun, num
      READF, ilun, line
      READF, ilun, line
      instdate = STRARR(1)
      READF, ilun, instdate
      inst   = STRMID(instdate,0,4)
      users  = STRMID(instdate,4,10)
      date   = STRMID(instdate,14,18)
      READF, ilun, line
      READF, ilun, line
      title_scan = STRARR(1)
      READF, ilun, title_scan
      title  = STRMID(title_scan,0,72)
      scan   = STRMID(title_scan,72,8)
;------------------------------------
      READF, ilun, line
      nf0 = LONG(0)
      READF, ilun, nf0
      block0 = INTARR(nf0)
      READF, ilun, block0
      nversion = block0(0)
      ntypedet = block0(1)
      ncmdctrl = block0(2)
      nscntype = block0(3)
      nangnumb = block0(4)
      nbptrequ = block0(5)
      nbptsave = block0(6)
      ncodecnt = block0(7)
      ncodeang = block0(8)
      ncodeana = block0(9)
      ncodsign = block0(10)
      ntypregt = block0(12)
      ntypmmet = block0(13)
      ntypeaxe = block0(14)
      ntypefsc = block0(15)
      ntypinel = block0(16)
      nsignana = block0(17)
      ncodkikf = block0(18)
      ndatapnt = block0(23)
      ncod1ang = block0(24)
      ncod2ang = block0(25)
      ncod3ang = block0(26)
      ncod4ang = block0(27)
      ncod5ang = block0(28)
      ncod6ang = block0(29)
      ncod7ang = block0(30)
;------------------------------------
      READF, ilun, line
      nf1 = LONG(0)
      READF, ilun, nf1
      block1 = FLTARR(nf1)
      READF, ilun, block1
      phi_val  = block1(3)
      chi_val  = block1(4)
      ome_val  = block1(5)
      gam_val  = block1(6)
      wavlgth  = block1(17)
      ang_sta  = block1(35)
      deltang  = block1(36)
      rangang  = block1(37)
      presetm  = block1(38)
      tp_requ  = block1(45)
      tp_regu  = block1(46)
      tp_samp  = block1(47)
      dist_sd  = block1(34)      
;------------------------------------ 
      main_title = title
      sub_title  = users
      start_time = date      

      x_tit    (nwk_select)  = 'X-detector'
      y_tit    (nwk_select)  = 'Y-detector'
      z_tit    (nwk_select)  = 'Counts'
      w_tit    (nwk_select)  = STRTRIM(main_title,2)
      other_tit(nwk_select)  = w_numor(nwk_select)+' '+sub_title+' Start:'+start_time
      head_tit(nwk_select,4) = STRTRIM(start_time,2)

     
      READF, ilun, line
      READF, ilun, line
      READF, ilun, line
      nf2    = LONG(0)
      nb_points = LONG(1024)
      xdim   = LONG(64) & ydim = LONG(16)
      w_buf  = FLTARR(xdim,ydim)
      
      READF, ilun, nf2
      block2    = FLTARR(nf2-1024)
      
      READF, ilun, block2,w_tmp
      
      w_tmp  = block2(nf2-(xdim*ydim):nf2-1)
      
      x_buf  = INDGEN(xdim)+1
      y_buf  = INDGEN(ydim)+1
             
;     -------------------------------
;     PARAMETER ASSIGNMENT IN Pn
;     -------------------------------
      p_buf      = FLTARR(npars)
      
      p_buf(0)   = presetm		; PRESET
      p_buf(1)   = phi_val		; PHI-Value   
      p_buf(2)   = chi_val		; CHI-Value
      p_buf(3)   = ome_val		; OMEGA-Value
      p_buf(4)   = gam_val		; GAMMA-Value
      p_buf(5)   = ang_sta		; Starting Angle     
      p_buf(6)   = deltang		; Angle variation
      p_buf(7)   = rangang		; Angle range
      p_buf(8)   = tp_requ		; Requested temperature
      p_buf(9)   = tp_regu		; Regulation temperature
      p_buf(10)  = tp_samp		; Sample temperature
      p_buf(11)  = wavlgth		; Wavelength
      p_buf(12)  = 0.0			; Not used
      p_buf(13)  = ncmdctrl		; Data generator command
      p_buf(14)  = nscntype		; Scan type
      p_buf(15)  = nangnumb		; Additional parameters
      p_buf(16)  = nbptrequ		; Nb points requested
      p_buf(17)  = nbptsave		; Nb points saved
      p_buf(18)  = ncodecnt		; Count on Monitor or Time
      p_buf(19)  = ntypregt		; Type of T-regulation
      p_buf(20)  = ntypmmet		; Type of Multi-meter
      p_buf(21)  = dist_sd		; Distance Sample-Det.
      p_buf(22)  = 0.0			; Not used
      p_buf(23)  = 0.0			; Not used
      p_buf(24)  = 0.0			; Not used
      p_buf(25)  = 0.0			; Not used
      p_buf(26)  = 0.0  		; Not used
      p_buf(27)  = 0.0  		; Not used
      p_buf(28)  = 0.0  		; Not used      
      p_buf(29)  = 0.0  		; Not used
      p_buf(30)  = FLOAT(FIX(num)) 	; Numor


;     --------------------------------------
;     PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;     --------------------------------------      
      
      par_txt(nwk_select,0)   = 'PRESET                               ='
      par_txt(nwk_select,1)   = 'PHI-Value                            ='    
      par_txt(nwk_select,2)   = 'CHI-Value                            =' 
      par_txt(nwk_select,3)   = 'OMEGA-Value                          ='
      par_txt(nwk_select,4)   = 'GAMMA-Value                          ='
      par_txt(nwk_select,5)   = 'Starting Angle                       ='    
      par_txt(nwk_select,6)   = 'Angle variation                      ='
      par_txt(nwk_select,7)   = 'Angle range                          ='
      par_txt(nwk_select,8)   = 'Requested temperature                ='
      par_txt(nwk_select,9)   = 'Regulation temperature               ='
      par_txt(nwk_select,10)  = 'Sample temperature                   =' 
      par_txt(nwk_select,11)  = 'Wavelength                           ='
      par_txt(nwk_select,12)  = 'Not Used                             ='
      par_txt(nwk_select,13)  = 'Data generator command               ='
      par_txt(nwk_select,14)  = 'Scan type                            ='
      par_txt(nwk_select,15)  = 'Additional parameters                ='
      par_txt(nwk_select,16)  = 'Nb points requested                  ='
      par_txt(nwk_select,17)  = 'Nb points saved                      =' 
      par_txt(nwk_select,18)  = 'Count on Monitor or Time             ='
      par_txt(nwk_select,19)  = 'Type of T-regulation                 ='
      par_txt(nwk_select,20)  = 'Type of Multi-meter                  ='
      par_txt(nwk_select,21)  = 'Distance Sample-Detector             ='
      par_txt(nwk_select,22)  = 'Not Used                             ='
      par_txt(nwk_select,23)  = 'Not Used                             ='
      par_txt(nwk_select,24)  = 'Not Used                             =' 
      par_txt(nwk_select,25)  = 'Not Used                             ='
      par_txt(nwk_select,26)  = 'Not used                             ='
      par_txt(nwk_select,27)  = 'Not used                             ='
      par_txt(nwk_select,28)  = 'Not used                             ='     
      par_txt(nwk_select,29)  = 'Not used                             ='
      par_txt(nwk_select,30)  = 'Numor                                ='

            
      junk   = EXECUTE('x'+swk_select+'=x_buf')
      junk   = EXECUTE('y'+swk_select+'=y_buf')
      junk   = EXECUTE('w'+swk_select+'=w_buf')
      junk   = EXECUTE('p'+swk_select+'=p_buf')

;     -----------------------------------
;     NORMALISATION ON PRESET  - OR NOT -
;     -----------------------------------
      IF (flag_norm(nwk_select-1) EQ 1) THEN BEGIN
       junk  = EXECUTE('w'+swk_select+'=w'+swk_select+'/p'+swk_select+'(0)')
      ENDIF
      

end_READ_D16: FREE_LUN, ilun
      
      return
      end
;------------------------------------------------------------------------------------------

PRO   READ_D11TOF

@lamp.cbk
@mics.cbk
   			
;     ------------------------------------------------------------
;     READ-IN D11 Time-of-flight spectra (test configuration only)
;     ------------------------------------------------------------
      ON_IOERROR, end_read_d11tof
      GET_LUN, ilun
      OPENR, ilun, file_found(0)

;     -------------------------------
;     READ HEADER & PARAMETERS BLOCKS
;     -------------------------------
      line = STRARR(1)  & line(0)='' & lin=''
      READF, ilun, line
      num = LONG(0)
      READF, ilun, num
      READF, ilun, line
      READF, ilun, line
      instdate = STRARR(1)
      READF, ilun, instdate
      inst   = STRMID(instdate,0,4)
      numexp = STRMID(instdate,4,10)
      date   = STRMID(instdate,14,18)
;------------------------------------
      READF, ilun, line
      nf0          = LONG(0)
      READF, ilun, nf0
      block0       = INTARR(nf0)
      READF, ilun, block0
      nb_points    = block0(0)
      nblk_to_read = LONG(0)
      FOR nb = 1, 8 DO BEGIN			; Maximum Header Blocks = 8
       IF (block0(2*nb) NE 0) THEN nblk_to_read = nblk_to_read+1
      ENDFOR      
;------------------------------------
      FOR nbl = 0, nblk_to_read-1 DO BEGIN
       READF, ilun, line
       IF (line(0) NE sline(0)) THEN BEGIN
        IF (line(0) EQ aline(0)) THEN BEGIN
         nfield = LONG(0)
         READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
         if num2 gt 0 then for i=1,num2 do READF, ilun, lin
         nline  = nfield/80
         nf_ll  = nfield-(nline*80)
         IF (nf_ll NE 0) THEN nline = nline+1
         sbl    = STRTRIM(STRING(nbl+1),2)
         junk   = EXECUTE('block'+sbl+'=STRARR(nline)')
         junk   = EXECUTE('READF, ilun, block'+sbl)
        ENDIF
        IF (line(0) EQ fline(0)) THEN BEGIN
         nfield = LONG(0)
         READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
         if num2 gt 0 then for i=1,num2 do READF, ilun, lin
         sbl    = STRTRIM(STRING(nbl+1),2)
         junk   = EXECUTE('block'+sbl+'=FLTARR(nfield)')
         junk   = EXECUTE('READF, ilun, block'+sbl)
        ENDIF
        IF (line(0) EQ iline(0)) THEN BEGIN
         nfield = LONG(0)
         READF, ilun, lin & lin=lin+' 0' & num2=0 & reads,lin,nfield,num2
         if num2 gt 0 then for i=1,num2 do READF, ilun, lin
         sbl    = STRTRIM(STRING(nbl+1),2)
         junk   = EXECUTE('block'+sbl+'=LONARR(nfield)')
         junk   = EXECUTE('READF, ilun, block'+sbl)
        ENDIF
       ENDIF
      ENDFOR

;------------------------------------------------------------------------------------------
;                                 READING DATA D11-TOF
;------------------------------------------------------------------------------------------
      
      x_buf  = INDGEN(nb_points)+1
      w_buf  = FLTARR(nb_points)

      READF, ilun, line
      READF, ilun, line
      READF, ilun, line
      nbpt   = LONG(0)
      READF, ilun, nbpt
      READF, ilun, w_buf
      
;     -------------------------------
;     PARAMETER ASSIGNMENT IN Pn
;     -------------------------------
      p_buf = FLTARR(npars)
;     --------------------------------------
;     PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;     --------------------------------------            
      par_txt(nwk_select,*)        = 'Not used'
           
      junk   = EXECUTE('x'+swk_select+'=x_buf')
      junk   = EXECUTE('w'+swk_select+'=w_buf')
      junk   = EXECUTE('p'+swk_select+'=p_buf')

end_read_d11tof: FREE_LUN, ilun
            
      return
      end
;------------------------------------------------------------------------------------------

PRO   RESET_OPR_NUM, event, uv

@lamp.cbk
@mics.cbk

    list_numor(*,*)  = 'No numor'
    list_numint(*,*) = 0
    list_oper(*,*)   = ''
    code_numor(*,*)  = 0
    code_oper(*,*)   = -1
    code_op_op(*,*)  = -1
    list_path(*,*)   = 'Not used'

    nw_op(*,*)   = -1
    npos_op(*,*) = -1
    nw_oper(*)   = -1
    npos_oper(*) = -1

    nw_num(*)    = -1
    npos_num(*)  = -1
    nb_num_wk(*) =  0
      
    cnt_oper      = LONG(0)
    cnt_numor     = LONG(0)
    cnt_andto     = LONG(0)
    cnt_and       = LONG(0)
    cnt_sumto     = LONG(0)
    cnt_plus      = LONG(0)
    cnt_minus     = LONG(0)
    cnt_null      = LONG(0)
    cnt_num_multi = LONG(0)
            
    n_err_mult           =  0
    n_op_wrong           =  0
    n_op_op_wrong        =  0
    n_num_id             =  0
    nw_op_wrong(*)       = -1
    nw_op_op_wrong(*)    = -1
    nw_num_id(*)         = -1
    npos_op_wrong(*)     = -1
    npos_op_op_wrong(*)  = -1
    npos_num_id(*)       = -1
    flag_bef_op(*)       =  0
    flag_aft_op(*)       =  0
    flag_both_op(*)      =  0
    nb_num_wk(*)         =  0
    nb_op_wk(*)          =  0
    
    string_hist_num(*,*)   = ''
    string_hist_oper(*,*)  = ''
    string_hist(*)         = ''
    nstr_num(*)            = -1
    nstr_oper(*)           = -1

    numint_single(*)     =  0 
    num_single(*)        = 'No numor' 

    list_err_opnum(*)    = 'No error detected'
    list_err_opop(*)     = 'No error detected'
    list_err_numid(*)    = 'No error detected'
    list_err_mult(*)     = 'No error detected'
      
    where_multi(*) = LONG(-1) 
    where_oper     = LONG(-1)
    where_andto    = LONG(-1)
    where_and      = LONG(-1)
    where_sumto    = LONG(-1)
    where_plus     = LONG(-1)
    where_minus    = LONG(-1)
    where_null     = LONG(-1)
    where_num      = LONG(-1)
    
    err1_wk(*)           =  0
    err2_wk(*)           =  0
    err3_wk(*)           =  0
    err4_wk(*)           =  0
    nb_num_tot(*)        = -1
    list_numor_fin(*)    = 'No numor'
    list_numor_wk(*,*)   = 'No numor'
    flag_list_fin(*)     =  0
    nb_andto(*)          = -1 
    nb_and(*)            = -1
    nb_sumto(*)          = -1
    nb_plus(*)           = -1 
    nb_minus(*)          = -1
    num_bef_andto(*)     =  0
    num_bef_and(*)       =  0
    num_bef_sumto(*)     =  0
    num_bef_plus(*)      =  0
    num_bef_minus(*)     =  0
    num_aft_andto(*)     =  0
    num_aft_and(*)       =  0
    num_aft_sumto(*)     =  0
    num_aft_plus(*)      =  0
    num_aft_minus(*)     =  0
    list_err_elmt(*)     = ''
    list_err_lambda(*)   = ''
    list_err_deltae(*)   = ''
    list_err_elpp(*)     = ''
    list_err_par(*)      = ''
    list_err_scan(*)     = ''
    list_err_dopmin(*)   = ''
    list_err_dopmax(*)   = ''
    list_err_mono(*)     = ''
    list_err_anal(*)     = ''
    list_err_defl(*)     = ''
    list_err_coefmono(*) = ''
    list_err_xval(*)     = ''
    pc1(*)               = 0.0
    pc2(*)               = 0.0
    snumor               = ''
    substr1              = ''
    substr2              = ''
    nerr_elements        = -1      
    nerr_lambda          = -1      
    nerr_deltae          = -1      
    nerr_elpp            = -1
    nerr_par             = -1      
    nerr_scan            = -1      
    nerr_dopmin          = -1      
    nerr_dopmax          = -1      
    nerr_mono            = -1      
    nerr_anal            = -1      
    nerr_defl            = -1      
    nerr_coefmono        = -1      
    nerr_xval            = -1      
    ntot_num             = LONG(-1)
    index2               = LONG(0)
    
        

      FOR nw = 0, n_wk_max-1 DO BEGIN
       FOR nobj = 0, 2*n_rk_max-2 DO BEGIN
        WIDGET_CONTROL, keep_id(nw,nobj), SET_VALUE= ''
       ENDFOR
      ENDFOR
      
      WIDGET_CONTROL, list_wkp_4, SET_VALUE = list_numor(*,*)
      
      return
      end
;------------------------------------------------------------------------------------------

PRO   UPDATE_OPR_NUM, event

@lamp.cbk
@mics.cbk

; Initialization
; Defining how many operators are activated and where
; Defining how many "numor fields" are activated and where
; Testing if operators are activated
; Testing if "numor fields" are activated
; Defining numors to read and performing operations

    nw_op(*,*)    = -1
    npos_op(*,*)  = -1
    nw_oper(*)    = -1
    npos_oper(*)  = -1

    nw_num(*)     = -1
    npos_num(*)   = -1
    nb_num_wk(*)  =  0
    
    nb_num_tot(*) =  0
    ntot_num = 0
    
    nb_plus(*)  = -1
    nb_minus(*) = -1
    nb_and(*)   = -1
    nb_andto(*) = -1
    nb_sumto(*) = -1
      
    cnt_oper      = LONG(0)
    cnt_numor     = LONG(0)
    cnt_andto     = LONG(0)
    cnt_and       = LONG(0)
    cnt_sumto     = LONG(0)
    cnt_plus      = LONG(0)
    cnt_minus     = LONG(0)
    cnt_null      = LONG(0)
    cnt_num_multi = LONG(0)

    n_err_mult           =  0
    n_op_wrong           =  0
    n_op_op_wrong        =  0
    n_num_id             =  0
    nw_op_wrong(*)       = -1
    nw_op_op_wrong(*)    = -1
    nw_num_id(*)         = -1
    npos_op_wrong(*)     = -1
    npos_op_op_wrong(*)  = -1
    npos_num_id(*)       = -1
    flag_bef_op(*)       =  0
    flag_aft_op(*)       =  0
    flag_both_op(*)      =  0
    nb_num_wk(*)         =  0
    nb_op_wk(*)          =  0

    numint_single(*)     =  0 
    num_single(*)        = 'No numor' 

    list_err_opnum(*)    = 'No error detected'
    list_err_opop(*)     = 'No error detected'
    list_err_numid(*)    = 'No error detected'
    list_err_mult(*)     = 'No error detected'
      
    where_multi(*) = LONG(-1) 
    where_oper     = LONG(-1)
    where_andto    = LONG(-1)
    where_and      = LONG(-1)
    where_sumto    = LONG(-1)
    where_plus     = LONG(-1)
    where_minus    = LONG(-1)
    where_null     = LONG(-1)
    where_num      = LONG(-1)    
           
    where_oper  = WHERE(code_oper  GE 0 AND code_oper LE 4, cnt_oper)
    where_andto = WHERE(code_oper  EQ 0,  cnt_andto)
    where_and   = WHERE(code_oper  EQ 1,  cnt_and)
    where_sumto = WHERE(code_oper  EQ 2,  cnt_sumto)
    where_plus  = WHERE(code_oper  EQ 3,  cnt_plus)
    where_minus = WHERE(code_oper  EQ 4,  cnt_minus)
    where_null  = WHERE(code_oper  EQ 5,  cnt_null)
    where_num   = WHERE(code_numor GT 0,  cnt_numor)

    index2      = LONG(0)

; Case of no operator value selected
; Testing if there is only one numor/workspace
; If yes: Individual open/read - If no: Error message

      IF ((cnt_oper EQ 0) AND (cnt_numor GT 0))  THEN BEGIN
       FOR n_num = 1, cnt_numor DO BEGIN
        nw_num(n_num-1)   = where_num(n_num-1) MOD n_wk_max
        npos_num(n_num-1) = (where_num(n_num-1) - nw_num(n_num-1)) / n_wk_max
        FOR nwk = 0, n_wk_max-1 DO BEGIN
         IF (nw_num(n_num-1) EQ nwk) THEN nb_num_wk(nwk) = nb_num_wk(nwk) + 1
        ENDFOR
       ENDFOR
       where_multi = WHERE(nb_num_wk GT 1, cnt_num_multi)        
       IF (cnt_num_multi GT 0) THEN BEGIN
        error_base  = WIDGET_BASE(TITLE = 'Error - Undefined Operator !!!', /COLUMN, RESOURCE_NAME='lampmic')
        FOR n_multi = 0, cnt_num_multi-1 DO BEGIN
         error_text = WIDGET_LABEL(error_base, VALUE = 'More than one numor in Workspace '    + $
                                  STRTRIM(STRING(where_multi(n_multi)+1),2) + $
                                  ' without any operator ...', FONT=ft_b_normal)
        ENDFOR 
        error_text2 = WIDGET_LABEL(error_base, VALUE = 'Defining an operator is thus necessary !!!', $
                                   FONT=ft_b_normal)
        error_text3 = WIDGET_LABEL(error_base, VALUE = 'In addition numors can be located at non-' + $
                                   'successive positions in workspace. Check it !!!', FONT=ft_b_normal)
        error_butt  = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                    FONT = ft_b_normal)
        WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF ELSE BEGIN
        FOR n_num = 1, cnt_numor DO BEGIN
         numint_single(nw_num(n_num-1)) = list_numint(nw_num(n_num-1),npos_num(n_num-1))
         num_single(nw_num(n_num-1))    = list_numor(nw_num(n_num-1),npos_num(n_num-1))
         file_found = FINDFILE(list_path(nw_num(n_num-1),npos_num(n_num-1)), COUNT = nb_file)
         IF (nb_file eq 0) THEN BEGIN
          error_base = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
          error_text = WIDGET_LABEL(error_base, VALUE = 'FILE ' +                        $
                                    STRMID(list_path,len_path,11) +               $
                                    ' NOT FOUND  !!!', FONT=ft_b_normal)
          error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                     FONT = ft_b_normal)
          WIDGET_CONTROL,    error_base, GROUP_LEADER=lamp_mic, /REALIZE
          WIDGET_CONTROL,    error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
          XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
         ENDIF
         IF (nb_file gt 0) THEN BEGIN
          nwk_select = nw_num(n_num-1)+1              
          swk_select = STRTRIM(STRING(nwk_select),2)
          READ_DATA
;         -------------------------------
;         UPDATING HISTORY
;         -------------------------------
	   file_hist  =  file_found(0)   
          IF (flag_acces EQ 2) THEN BEGIN
           pos_dot = STRPOS(file_found(0), '.dat')
           pos_r   = pos_dot - 7
           file_hist  = STRMID(file_found(0), pos_r, 11)
          ENDIF         
          IF ((flag_acces EQ 1) OR (flag_acces EQ 5)) THEN BEGIN
           len_hist   = STRLEN(file_found(0))
           file_hist  = '[' + STRMID(file_found(0), len_hist-6, 6) + ']'
          ENDIF         
          TO_DON_HISTORY, nwk_select , 0 , 'w'+swk_select+'=RDRUN('+file_hist+') ;'+string_norm(nwk_select-1)     
          nb_file = -1
         ENDIF         
        ENDFOR
       ENDELSE       
      ENDIF


; Case of no numor value & no operator value selected

      IF ((cnt_oper EQ 0) AND (cnt_numor EQ 0))  THEN BEGIN
       error_base = WIDGET_BASE(TITLE = 'Error about Operator/Numor Selection', /COLUMN, RESOURCE_NAME='lampmic')
       error_text = WIDGET_LABEL(error_base, VALUE = 'NO RUN NUMBER AND NO OPERATOR HAVE '  $
                                                   + 'BEEN SELECTED  !!!', FONT=ft_b_normal)
       error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                  FONT = ft_b_normal)
       WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
       WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
       XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
      ENDIF


; Case of no numor value selected

      IF ((cnt_oper GT 0) AND (cnt_numor EQ 0))  THEN BEGIN
       error_base = WIDGET_BASE(TITLE = 'Error about Numor Selection', /COLUMN, RESOURCE_NAME='lampmic')
       error_text = WIDGET_LABEL(error_base, VALUE = 'NO RUN NUMBER HAS BEEN SELECTED  !!!', $
                                 FONT=ft_b_normal)
       error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                  FONT = ft_b_normal)
       WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
       WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
       XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
      ENDIF


; Normal case: At least one numor and one operator have been activated
; Location of numor/operator activation as a function of (Workspace,Rank)

      IF ((cnt_oper GT 0) AND (cnt_numor GT 0)) THEN BEGIN
            
; ALL operators
       FOR n_oper = 1, cnt_oper DO BEGIN
        nw_oper(n_oper-1)   = where_oper(n_oper-1) MOD n_wk_max
        npos_oper(n_oper-1) = (where_oper(n_oper-1) - nw_oper(n_oper-1)) / n_wk_max     
       ENDFOR
; ANDTO operators       
       IF (cnt_andto GT 0) THEN BEGIN
        FOR n_andto = 1, cnt_andto DO BEGIN
         nw_op(0,n_andto-1)   = where_andto(n_andto-1) MOD n_wk_max
         npos_op(0,n_andto-1) = (where_andto(n_andto-1) - nw_op(0,n_andto-1)) / n_wk_max       
        ENDFOR
       ENDIF
; AND operators             
       IF (cnt_and GT 0)   THEN BEGIN
        FOR n_and = 1, cnt_and DO BEGIN
         nw_op(1,n_and-1)     = where_and(n_and-1) MOD n_wk_max
         npos_op(1,n_and-1)   = (where_and(n_and-1) - nw_op(1,n_and-1)) / n_wk_max        
        ENDFOR
       ENDIF
; SUMTO operators                    
       IF (cnt_sumto GT 0) THEN BEGIN
        FOR n_sumto = 1, cnt_sumto DO BEGIN
         nw_op(2,n_sumto-1)   = where_sumto(n_sumto-1) MOD n_wk_max
         npos_op(2,n_sumto-1) = (where_sumto(n_sumto-1) - nw_op(2,n_sumto-1)) / n_wk_max        
        ENDFOR
       ENDIF
; PLUS operators                           
       IF (cnt_plus GT 0)  THEN BEGIN
        FOR n_plus = 1, cnt_plus DO BEGIN
         nw_op(3,n_plus-1)    = where_plus(n_plus-1) MOD n_wk_max
         npos_op(3,n_plus-1)  = (where_plus(n_plus-1) - nw_op(3,n_plus-1)) / n_wk_max        
        ENDFOR
       ENDIF
; MINUS operators                                  
       IF (cnt_minus GT 0) THEN BEGIN
        FOR n_minus = 1, cnt_minus DO BEGIN
         nw_op(4,n_minus-1)   = where_minus(n_minus-1) MOD n_wk_max
         npos_op(4,n_minus-1) = (where_minus(n_minus-1) - nw_op(4,n_minus-1)) / n_wk_max        
        ENDFOR
       ENDIF      
; Numors                                  
       FOR n_num = 1, cnt_numor DO BEGIN
        nw_num(n_num-1)   = where_num(n_num-1) MOD n_wk_max
        npos_num(n_num-1) = (where_num(n_num-1) - nw_num(n_num-1)) / n_wk_max        
       ENDFOR
 
; Testing if there is some workspace with more than one numor and no defined operator
       FOR n_num = 1, cnt_numor DO BEGIN
        FOR nwk = 0, n_wk_max-1 DO BEGIN
         IF (nw_num(n_num-1) EQ nwk) THEN nb_num_wk(nwk) = nb_num_wk(nwk) + 1
        ENDFOR
       ENDFOR
       FOR n_opr = 1, cnt_oper DO BEGIN
        FOR nwk = 0, n_wk_max-1 DO BEGIN
         IF (nw_oper(n_opr-1) EQ nwk) THEN nb_op_wk(nwk) = nb_op_wk(nwk) + 1
        ENDFOR
       ENDFOR       
       FOR nwk = 0, n_wk_max-1 DO BEGIN           
        IF (nb_num_wk(nwk) GT 1 AND nb_op_wk(nwk) EQ 0) THEN BEGIN
         n_err_mult = n_err_mult + 1
         list_err_mult(n_err_mult-1) = 'More than one numor in Workspace '  + $
                                       STRTRIM(STRING(nwk+1),2)             + $
                                       ' without any operator ...'
        ENDIF
       ENDFOR 

       IF (n_err_mult GE 1) THEN BEGIN
        error_base  = WIDGET_BASE(TITLE = 'Error - Undefined Operator !!!', /COLUMN, RESOURCE_NAME='lampmic')
        FOR n_err = 1, n_err_mult DO BEGIN
          error_text = WIDGET_LABEL(error_base, VALUE = list_err_mult(n_err_mult-1), FONT=ft_b_normal)
        ENDFOR 
        error_text2 = WIDGET_LABEL(error_base, VALUE = 'Defining an operator is thus necessary !!!', $
                                   FONT=ft_b_normal)
        error_text3 = WIDGET_LABEL(error_base, VALUE = 'In addition numors can be located at non-' + $
                                   'successive positions in workspace. Check it !!!', FONT=ft_b_normal)
        error_butt  = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                    FONT = ft_b_normal)
        WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF  


; Tests on Relative Position of Numors & Operators ... Analysis vs Workspace
; Detection of lacking numors besides an activated operator
; Detection of lacking numor in the first field if first operator field is activated
; Detection of lacking numor in the last field if previous operator field is activated
            
       FOR n_op = 0, cnt_oper-1 DO BEGIN 
        FOR n_num = 0, cnt_numor-1 DO BEGIN
         IF (where_oper(n_op)-where_num(n_num) EQ 0)           THEN flag_bef_op(n_op) = 1     
         IF (where_oper(n_op)-where_num(n_num) EQ (-n_wk_max)) THEN flag_aft_op(n_op) = 1
        ENDFOR
        flag_both_op(n_op) = flag_bef_op(n_op) * flag_aft_op(n_op)        
        IF ((flag_both_op(n_op) EQ 0) AND (flag_bef_op(n_op) EQ 0) $
             AND (flag_aft_op(n_op) EQ 0)) THEN BEGIN
         n_op_wrong = n_op_wrong + 1
         nw_op_wrong(n_op_wrong-1)     = nw_oper(n_op) + 1
         npos_op_wrong(n_op_wrong-1)   = npos_oper(n_op) + 1
         list_err_opnum(n_op_wrong-1)  = 'Operator located at [Wk=' +                              $
                                         STRTRIM(STRING(nw_op_wrong(n_op_wrong-1)),2) + ';Rank=' + $
                                         STRTRIM(STRING(npos_op_wrong(n_op_wrong-1)),2) + ']' +    $
                                         ' has no previous and consecutive numor.'
        ENDIF
        IF ((flag_both_op(n_op) EQ 0) AND (flag_bef_op(n_op) EQ 1) $
             AND (flag_aft_op(n_op) EQ 0)) THEN BEGIN
         n_op_wrong = n_op_wrong + 1
         nw_op_wrong(n_op_wrong-1)     = nw_oper(n_op) + 1
         npos_op_wrong(n_op_wrong-1)   = npos_oper(n_op) + 1
         list_err_opnum(n_op_wrong-1)  = 'Operator located at [Wk=' +                              $
                                         STRTRIM(STRING(nw_op_wrong(n_op_wrong-1)),2) + ';Rank=' + $
                                         STRTRIM(STRING(npos_op_wrong(n_op_wrong-1)),2) + ']' +    $
                                         ' has no consecutive numor.'
        ENDIF
        IF ((flag_both_op(n_op) EQ 0) AND (flag_bef_op(n_op) EQ 0) $
             AND (flag_aft_op(n_op) EQ 1)) THEN BEGIN
         n_op_wrong = n_op_wrong + 1
         nw_op_wrong(n_op_wrong-1)     = nw_oper(n_op) + 1
         npos_op_wrong(n_op_wrong-1)   = npos_oper(n_op) + 1
         list_err_opnum(n_op_wrong-1)  = 'Operator located at [Wk=' +                              $
                                         STRTRIM(STRING(nw_op_wrong(n_op_wrong-1)),2) + ';Rank=' + $
                                         STRTRIM(STRING(npos_op_wrong(n_op_wrong-1)),2) + ']' +    $
                                         ' has no previous numor.'
        ENDIF        
       ENDFOR
       IF (n_op_wrong GT 0) THEN BEGIN
        error_base = WIDGET_BASE(TITLE = 'Error about Relative Position of ' + $
                                         'Operators and Numors', /COLUMN, RESOURCE_NAME='lampmic')
        FOR n_wrong = 1, n_op_wrong DO BEGIN
         error_text = WIDGET_LABEL(error_base, VALUE = list_err_opnum(n_wrong-1), FONT=ft_b_normal)
        ENDFOR
        error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                   FONT = ft_b_normal)
        WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF


; Compatibility of operators ? Does {Numor1 OP1 Numor2 OP2 Numor3} represent a correct operation?
;    OP2   PLUS   MINUS  AND    ANDTO  SUMTO
;  OP1
;  PLUS    Yes    Yes    Yes    No     Yes                   code_op_op = 13    
;  MINUS   Yes    Yes    Yes    No     Yes                   code_op_op = 21       
;  AND     Yes    Yes    Yes    Yes    Yes                   code_op_op =  3     
;  ANDTO   No     No     Yes    Yes    No                    code_op_op =  1     
;  SUMTO   Yes    Yes    Yes    No     Yes                   code_op_op =  6   

       FOR n_op1 = 0, cnt_oper-2 DO BEGIN
        FOR n_op2 = n_op1+1, cnt_oper-1 DO BEGIN
         IF (where_oper(n_op2)-where_oper(n_op1) EQ n_wk_max) THEN BEGIN
          op_plus_op = code_op_op(nw_oper(n_op2),npos_oper(n_op2)) + $
                       code_op_op(nw_oper(n_op1),npos_oper(n_op1))
          IF ((op_plus_op EQ 7) OR (op_plus_op EQ 14) OR (op_plus_op EQ 22)) THEN BEGIN
           n_op_op_wrong = n_op_op_wrong + 1  
           nw_op_op_wrong(n_op_op_wrong-1)   = nw_oper(n_op1)   + 1
           npos_op_op_wrong(n_op_op_wrong-1) = npos_oper(n_op1) + 1
           list_err_opop(n_op_op_wrong-1)    = 'Operator located at [Wk='                           + $
                                               STRTRIM(STRING(nw_op_op_wrong(n_op_op_wrong-1)),2)   + $
                                               ';Rank='                                             + $
                                               STRTRIM(STRING(npos_op_op_wrong(n_op_op_wrong-1)),2) + $
                                               '] is incompatible with the following one.'
          ENDIF
         ENDIF
        ENDFOR        
       ENDFOR
       IF (n_op_op_wrong gt 0) THEN BEGIN
        error_base = WIDGET_BASE(TITLE = 'Error about Operators Incompatibility: [ANDTO / PLUS] - ' + $
                                         '[ANDTO / SUMTO] - [ANDTO / MINUS]', /COLUMN, XSIZE=900,     $
                                         RESOURCE_NAME='lampmic')
        FOR n_wrong = 1, n_op_op_wrong DO BEGIN
         error_text = WIDGET_LABEL(error_base, VALUE = list_err_opop(n_wrong-1), FONT=ft_b_normal)
        ENDFOR
        error_text1 = WIDGET_LABEL(error_base, VALUE = '')
        error_text2 = WIDGET_LABEL(error_base, VALUE = '-----------------------------------------', $
                                   FONT=ft_b_normal)
        error_text3 = WIDGET_LABEL(error_base, VALUE = 'List of Compatible Operations', $
                                   FONT=ft_b_normal)
        error_text4 = WIDGET_LABEL(error_base, VALUE = '-----------------------------------------', $
                                   FONT=ft_b_normal)
        error_text5 = WIDGET_LABEL(error_base, VALUE = 'OP1:OP2       Plus   Minus     And     AndTo    SumTo', $
                                   FONT=ft_normal)
        error_text6 = WIDGET_LABEL(error_base, VALUE = 'Plus              Yes       Yes      Yes          No        Yes    ',   $
                                   FONT=ft_normal)
        error_text7 = WIDGET_LABEL(error_base, VALUE = 'Minus            Yes       Yes      Yes          No        Yes    ',   $
                                   FONT=ft_normal)
        error_text8 = WIDGET_LABEL(error_base, VALUE = 'And              Yes       Yes      Yes          Yes        Yes   ',   $
                                   FONT=ft_normal)
        error_text9 = WIDGET_LABEL(error_base, VALUE = 'AndTo           No        No       Yes          Yes         No   ',   $
                                   FONT=ft_normal)
        error_text10= WIDGET_LABEL(error_base, VALUE = 'SumTo          Yes       Yes       Yes           No        Yes   ',   $
                                   FONT=ft_normal)
        error_text11= WIDGET_LABEL(error_base, VALUE = '')                                   
        error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                   FONT = ft_b_normal)
        WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF
       
; Are two successive numors identical ?
       FOR n_num1 = 0, cnt_numor-2 DO BEGIN
        FOR n_num2 = n_num1+1, cnt_numor-1 DO BEGIN
         IF (where_num(n_num2)-where_num(n_num1) EQ n_wk_max) THEN BEGIN           
          IF (list_numor(nw_num(n_num1),npos_num(n_num1)) EQ   $
              list_numor(nw_num(n_num2),npos_num(n_num2))) THEN BEGIN
               n_num_id = n_num_id + 1  
               nw_num_id(n_num_id-1)     = nw_oper(n_num1)   + 1
               npos_num_id(n_num_id-1)   = npos_oper(n_num1) + 1
               list_err_numid(n_num_id-1) = 'Numor located at [Wk='                    + $
                                            STRTRIM(STRING(nw_num_id(n_num_id-1)),2)   + $
                                            ';Rank='                                   + $
                                            STRTRIM(STRING(npos_num_id(n_num_id-1)),2) + $
                                            '] is identical to the following one.'
          ENDIF
         ENDIF
        ENDFOR        
       ENDFOR
       IF (n_num_id gt 0) THEN BEGIN
        error_base = WIDGET_BASE(TITLE = 'Error about Repeated Numors:', /COLUMN, RESOURCE_NAME='lampmic')
        FOR n_id = 1, n_num_id DO BEGIN
         error_text = WIDGET_LABEL(error_base, VALUE = list_err_numid(n_id-1), FONT=ft_b_normal)
        ENDFOR
        error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                   FONT = ft_b_normal)
        WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
        WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
        XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
       ENDIF



; Once you have passed all the tests, you can determine:
;  - First:  which workspace contains only one numor ... read it immediately
;  - Second: the number of numors to read/workspace and which ones ?
; Operations are then performed after a serie of tests 

       
       FOR nwk = 0, n_wk_max-1 DO BEGIN
        
        IF ((err1_wk(nwk) EQ 1) OR (err2_wk(nwk) EQ 1) OR               $
            (err3_wk(nwk) EQ 1) OR (err4_wk(nwk) EQ 1)) THEN BEGIN

         IF (nb_num_wk(nwk) EQ 1) THEN BEGIN
        
          FOR n_num = 1, cnt_numor DO BEGIN
           IF (nw_num(n_num-1) EQ nwk) THEN BEGIN
            file_found = FINDFILE(list_path(nw_num(n_num-1),npos_num(n_num-1)), COUNT = nb_file)
            IF (nb_file eq 0) THEN BEGIN
             error_base = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
             error_text = WIDGET_LABEL(error_base, VALUE = 'FILE ' +                        $
                                       STRMID(list_path,len_path,11) +               $
                                       ' NOT FOUND  !!!', FONT=ft_b_normal)
             error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                        FONT = ft_b_normal)
             WIDGET_CONTROL,    error_base, GROUP_LEADER=lamp_mic, /REALIZE
             WIDGET_CONTROL,    error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
             XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
            ENDIF
            IF (nb_file gt 0) THEN BEGIN
             ntot_num = ntot_num+1
	     nb_num_tot(nwk) = nb_num_tot(nwk) + 1            
             list_numor_fin(ntot_num-1)           = list_numor(nw_num(n_num-1),npos_num(n_num-1))
             list_numor_wk(nwk,nb_num_tot(nwk)-1) = list_numor(nw_num(n_num-1),npos_num(n_num-1))               
             nwk_select = nw_num(n_num-1)+1              
             swk_select = STRTRIM(STRING(nwk_select),2)
             READ_DATA
;            -------------------------------
;            UPDATING HISTORY
;            -------------------------------     
	      file_hist  = file_found(0)
             IF (flag_acces EQ 2) THEN BEGIN
              pos_dot = STRPOS(file_found, '.dat')
              pos_r   = pos_dot - 7
              file_hist  = STRMID(file_found(0), pos_r, 11)
             ENDIF         
             IF ((flag_acces EQ 1) OR (flag_acces EQ 5)) THEN BEGIN
              len_hist   = STRLEN(file_found)
              file_hist  = '[' + STRMID(file_found(0), len_hist-6, 6) + ']'
             ENDIF         
             TO_DON_HISTORY, nwk_select , 0 , 'w'+swk_select+'=RDRUN('+file_hist+') ;'+string_norm(nwk_select-1)            
             nb_file = -1
            ENDIF
           ENDIF
          ENDFOR
         
         ENDIF ELSE BEGIN
        
          IF (nb_num_wk(nwk) GT 1) THEN BEGIN
           FOR n_oper = 1, cnt_oper DO BEGIN
            IF (nw_oper(n_oper-1) EQ nwk) THEN BEGIN
             FOR n_andto = 1, cnt_andto DO BEGIN
              IF ((nw_oper(n_oper-1)   EQ nw_op(0,n_andto-1))    AND      $
                  (npos_oper(n_oper-1) EQ npos_op(0,n_andto-1))) THEN BEGIN
               nb_andto(nwk) = nb_andto(nwk) + 1
               num_bef_andto(nwk,nb_andto(nwk)) = list_numint(nwk,npos_oper(n_oper-1)) 
               num_aft_andto(nwk,nb_andto(nwk)) = list_numint(nwk,npos_oper(n_oper-1)+1)
              ENDIF
             ENDFOR            
             FOR n_and   = 1, cnt_and   DO BEGIN
              IF ((nw_oper(n_oper-1)   EQ nw_op(1,n_and-1))    AND      $
                  (npos_oper(n_oper-1) EQ npos_op(1,n_and-1))) THEN BEGIN
               nb_and(nwk)   = nb_and(nwk)   + 1
               num_bef_and(nwk,nb_and(nwk))     = list_numint(nwk,npos_oper(n_oper-1)) 
               num_aft_and(nwk,nb_and(nwk))     = list_numint(nwk,npos_oper(n_oper-1)+1)
              ENDIF
             ENDFOR
             FOR n_sumto = 1, cnt_sumto DO BEGIN
              IF ((nw_oper(n_oper-1)   EQ nw_op(2,n_sumto-1))    AND      $
                  (npos_oper(n_oper-1) EQ npos_op(2,n_sumto-1))) THEN BEGIN
               nb_sumto(nwk) = nb_sumto(nwk) + 1
               num_bef_sumto(nwk,nb_sumto(nwk)) = list_numint(nwk,npos_oper(n_oper-1)) 
               num_aft_sumto(nwk,nb_sumto(nwk)) = list_numint(nwk,npos_oper(n_oper-1)+1)
              ENDIF
             ENDFOR            
             FOR n_plus  = 1, cnt_plus  DO BEGIN
              IF ((nw_oper(n_oper-1)   EQ nw_op(3,n_plus-1))    AND      $
                  (npos_oper(n_oper-1) EQ npos_op(3,n_plus-1))) THEN BEGIN
               nb_plus(nwk)  = nb_plus(nwk)  + 1
               num_bef_plus(nwk,nb_plus(nwk))   = list_numint(nwk,npos_oper(n_oper-1)) 
               num_aft_plus(nwk,nb_plus(nwk))   = list_numint(nwk,npos_oper(n_oper-1)+1)
              ENDIF
             ENDFOR            
             FOR n_minus = 1, cnt_minus DO BEGIN
              IF ((nw_oper(n_oper-1)   EQ nw_op(4,n_minus-1))    AND      $
                  (npos_oper(n_oper-1) EQ npos_op(4,n_minus-1))) THEN BEGIN
               nb_minus(nwk) = nb_minus(nwk) + 1
               num_bef_minus(nwk,nb_minus(nwk)) = list_numint(nwk,npos_oper(n_oper-1)) 
               num_aft_minus(nwk,nb_minus(nwk)) = list_numint(nwk,npos_oper(n_oper-1)+1)
              ENDIF
             ENDFOR
            ENDIF
           ENDFOR

;          -------------------------------------------------                      
;          Case of: ANDTO & AND not activated in a Workspace
;          -------------------------------------------------                      

	   IF ((nb_andto(nwk) EQ -1) AND (nb_and(nwk) EQ -1)) THEN BEGIN
	   
;           -------------------------------                      
;           Subcase: SUMTO is not activated
;           -------------------------------                    
	   
	    IF (nb_sumto(nwk) EQ -1) THEN BEGIN
	     FOR n_num =1, cnt_numor DO BEGIN
	      IF (nw_num(n_num-1) EQ nwk) THEN BEGIN
	       nb_num_tot(nwk) = nb_num_tot(nwk) + 1
	       ntot_num = ntot_num + 1
               file_found = FINDFILE(list_path(nw_num(n_num-1),npos_num(n_num-1)), COUNT = nb_file)
               IF (nb_file eq 0) THEN BEGIN
                error_base = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
                error_text = WIDGET_LABEL(error_base, VALUE = 'FILE ' +                        $
                                          STRMID(list_path,len_path,11) +               $
                                          ' NOT FOUND  !!!', FONT=ft_b_normal)
                error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                           FONT = ft_b_normal)
                WIDGET_CONTROL,    error_base, GROUP_LEADER=lamp_mic, /REALIZE
                WIDGET_CONTROL,    error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
                XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
               ENDIF
               IF (nb_file gt 0) THEN BEGIN
                list_numor_fin(ntot_num-1)           = list_numor(nw_num(n_num-1),npos_num(n_num-1))
                list_numor_wk(nwk,nb_num_tot(nwk)-1) = list_numor(nw_num(n_num-1),npos_num(n_num-1))               
                nwk_select = nw_num(n_num-1)+1              
                swk_select = STRTRIM(STRING(nwk_select),2)
                snumor     = STRTRIM(STRING(nb_num_tot(nwk)-1),2)
                READ_DATA              
                junk = EXECUTE('x'+swk_select+'_'+snumor+'=INTARR(nx_global) & x'+swk_select+'_'+snumor+'=x'+swk_select)
                junk = EXECUTE('y'+swk_select+'_'+snumor+'=INTARR(ny_global) & y'+swk_select+'_'+snumor+'=y'+swk_select)
                junk = EXECUTE('w'+swk_select+'_'+snumor+'=LONARR(nx_global,ny_global) & w'+swk_select+'_'+snumor+'=w'+swk_select)
                junk = EXECUTE('p'+swk_select+'_'+snumor+'=FLTARR(npars) & p'+swk_select+'_'+snumor+'=p'+swk_select)
                nb_file = -1
               ENDIF
              ENDIF
             ENDFOR
              
            
;            Checking if same number of elements before operations	     
;            Checking if same wavelength in each numor	     
;            Checking if same channel width in each numor - TOF
; 	     Checking if same elastic position - TOF
;            Checking a lot of parameters for SAS
;            Checking scan type for BCK & some parameters depending on scan type 
	     
             IF (inst_value EQ 'IN10') THEN BEGIN
              FOR n_num1 = 1, nb_num_tot(nwk)-1 DO BEGIN
               FOR n_num2 = n_num1+1, nb_num_tot(nwk) DO BEGIN
                typscan1  = -1
                typscan2  = -1
                substr1   = STRTRIM(STRING(n_num1-1),2)                    
                substr2   = STRTRIM(STRING(n_num2-1),2)                    
                junk = EXECUTE('typscan1 = p'+swk_select+'_'+substr1+'(0)')
                junk = EXECUTE('typscan2 = p'+swk_select+'_'+substr2+'(0)')
                IF (typscan1 NE typscan2) THEN BEGIN
                 nerr_scan = nerr_scan + 1
                 list_err_scan(nerr_scan) = 'Operations including numors ' +                   $
                                            STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                            ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                            ' not possible: different scan types'
                ENDIF
               ENDFOR
              ENDFOR

              IF (nerr_scan GE 0) THEN BEGIN              
               err1_wk(nwk) = 1           
               error_base0  = WIDGET_BASE(TITLE = 'Error on scan type in Workspace '+swk_select, $
                                         /COLUMN, RESOURCE_NAME='lampmic')
               FOR n_err = 0, nerr_scan DO BEGIN
                error_text0 = WIDGET_LABEL(error_base0, VALUE = list_err_scan(n_err), FONT=ft_b_normal)
               ENDFOR
               error_butt0  = WIDGET_BUTTON(error_base0, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                           FONT = ft_b_normal)
               WIDGET_CONTROL, error_base0, GROUP_LEADER=lamp_mic, /REALIZE
               WIDGET_CONTROL, error_butt0, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
               XMANAGER, 'ERROR', error_base0, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
              ENDIF ELSE BEGIN
               FOR n_num1 = 1, nb_num_tot(nwk)-1 DO BEGIN
                FOR n_num2 = n_num1+1, nb_num_tot(nwk) DO BEGIN
                 nel_x1   =    0
                 nel_x2   =    0
                 nel_y1   =    0
                 nel_y2   =    0
                 substr1  = STRTRIM(STRING(n_num1-1),2)                    
                 substr2  = STRTRIM(STRING(n_num2-1),2)                    
                 junk     = EXECUTE('typscan1 = p'+swk_select+'_'+substr1+'(0)')
                 junk     = EXECUTE('typscan2 = p'+swk_select+'_'+substr2+'(0)')
                 junk     = EXECUTE('nel_x1   = N_ELEMENTS(x'+swk_select+'_'+substr1+')')
                 junk     = EXECUTE('nel_x2   = N_ELEMENTS(x'+swk_select+'_'+substr2+')')
                 junk     = EXECUTE('nel_y1   = N_ELEMENTS(y'+swk_select+'_'+substr1+')')
                 junk     = EXECUTE('nel_y2   = N_ELEMENTS(y'+swk_select+'_'+substr2+')')
                 IF (typscan1 EQ 0) THEN BEGIN
                  freqdop_min1 = 0.0 
                  freqdop_min2 = 0.0 
                  freqdop_max1 = 0.0 
                  freqdop_max2 = 0.0
                  d_mono1      = 0.0
                  d_mono2      = 0.0
                  d_anal1      = 0.0
                  d_anal2      = 0.0
                  d_defl1      = 0.0
                  d_defl2      = 0.0
                  junk         = EXECUTE('freqdop_min1 = p'+swk_select+'_'+substr1+'(3)')
                  junk         = EXECUTE('freqdop_min2 = p'+swk_select+'_'+substr2+'(3)')
                  junk         = EXECUTE('freqdop_max1 = p'+swk_select+'_'+substr1+'(2)')
                  junk         = EXECUTE('freqdop_max2 = p'+swk_select+'_'+substr2+'(2)')
                  junk         = EXECUTE('d_mono1 = p'+swk_select+'_'+substr1+'(5)')
                  junk         = EXECUTE('d_mono2 = p'+swk_select+'_'+substr2+'(5)')
                  junk         = EXECUTE('d_anal1 = p'+swk_select+'_'+substr1+'(6)')
                  junk         = EXECUTE('d_anal2 = p'+swk_select+'_'+substr2+'(6)')
                  junk         = EXECUTE('d_defl1 = p'+swk_select+'_'+substr1+'(7)')
                  junk         = EXECUTE('d_defl2 = p'+swk_select+'_'+substr2+'(7)')
                                
                  IF ((nel_x1 NE nel_x2) OR (nel_y1 NE nel_y2)) THEN BEGIN
                   nerr_elements = nerr_elements + 1
                   list_err_elmt(nerr_elements) = 'Operations including numors ' +                   $
                                                  STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                  ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                  ' not possible: different number of elements'
                  ENDIF
                  IF (ABS(freqdop_min1-freqdop_min2) GT tol_doppler*freqdop_min1) THEN BEGIN
                   nerr_dopmin = nerr_dopmin + 1
                   list_err_dopmin(nerr_dopmin) = 'Operations including numors ' +                   $
                                                  STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                  ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                  ' not possible: different Min. Doppler Freq.'
                  ENDIF
                  IF (ABS(freqdop_max1-freqdop_max2) GT tol_doppler*freqdop_max1) THEN BEGIN
                   nerr_dopmax = nerr_dopmax + 1
                   list_err_dopmax(nerr_dopmax) = 'Operations including numors ' +                   $
                                                  STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                  ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                  ' not possible: different Max. Doppler Freq.'
                  ENDIF
                  IF (ABS(d_mono1-d_mono2) GT tol_mono*d_mono1) THEN BEGIN
                   nerr_mono = nerr_mono + 1
                   list_err_mono(nerr_mono) = 'Operations including numors ' +                       $
                                              STRTRIM(list_numor_wk(nwk,n_num1-1),2) +               $
                                              ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +     $
                                              ' not possible: different d-monochromator'
                  ENDIF
                  IF (ABS(d_anal1-d_anal2) GT tol_anal*d_anal1) THEN BEGIN
                   nerr_anal = nerr_anal + 1
                   list_err_anal(nerr_anal) = 'Operations including numors ' +                       $
                                              STRTRIM(list_numor_wk(nwk,n_num1-1),2) +               $
                                              ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +     $
                                              ' not possible: different d-analyser'
                  ENDIF
                  IF (ABS(d_defl1-d_defl2) GT tol_defl*d_defl1) THEN BEGIN
                   nerr_defl = nerr_defl + 1
                   list_err_defl(nerr_defl) = 'Operations including numors ' +                       $
                                              STRTRIM(list_numor_wk(nwk,n_num1-1),2) +               $
                                              ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +     $
                                              ' not possible: different d-deflector'
                  ENDIF
                 ENDIF 
                 IF (typscan1 EQ 13) THEN BEGIN
                  d_anal1      = 0.0
                  d_anal2      = 0.0
                  d_defl1      = 0.0
                  d_defl2      = 0.0
                  coef_mono1   = 0.0
                  coef_mono2   = 0.0
                  xval1        = 0.0
                  xval2        = 0.0
                  junk         = EXECUTE('d_anal1 = p'+swk_select+'_'+substr1+'(6)')
                  junk         = EXECUTE('d_anal2 = p'+swk_select+'_'+substr2+'(6)')
                  junk         = EXECUTE('d_defl1 = p'+swk_select+'_'+substr1+'(7)')
                  junk         = EXECUTE('d_defl2 = p'+swk_select+'_'+substr2+'(7)')
                  junk         = EXECUTE('coef_mono1 = p'+swk_select+'_'+substr1+'(3)')
                  junk         = EXECUTE('coef_mono2 = p'+swk_select+'_'+substr2+'(3)')
                  junk         = EXECUTE('xval1 = x'+swk_select+'_'+substr1)
                  junk         = EXECUTE('xval2 = x'+swk_select+'_'+substr2)
              
                  IF ((nel_x1 NE nel_x2) OR (nel_y1 NE nel_y2)) THEN BEGIN
                   nerr_elements = nerr_elements + 1
                   list_err_elmt(nerr_elements) = 'Operations including numors ' +                   $
                                                  STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                  ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                  ' not possible: different number of elements'
                  ENDIF
                  IF (ABS(d_anal1-d_anal2) GT tol_anal*d_anal1) THEN BEGIN
                   nerr_anal = nerr_anal + 1
                   list_err_anal(nerr_anal) = 'Operations including numors ' +                       $
                                              STRTRIM(list_numor_wk(nwk,n_num1-1),2) +               $
                                              ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +     $
                                              ' not possible: different d-analyser'
                  ENDIF
                  IF (ABS(d_defl1-d_defl2) GT tol_defl*d_defl1) THEN BEGIN
                   nerr_defl = nerr_defl + 1
                   list_err_defl(nerr_defl) = 'Operations including numors ' +                       $
                                              STRTRIM(list_numor_wk(nwk,n_num1-1),2) +               $
                                              ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +     $
                                              ' not possible: different d-deflector'
                  ENDIF
                  IF (ABS(coef_mono1-coef_mono2) GT tol_coefmono*coef_mono1) THEN BEGIN
                   nerr_coefmono = nerr_coefmono + 1
                   list_err_coefmono(nerr_coefmono) = 'Operations including numors ' +                   $
                                                      STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                      ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                      ' not possible: different Monochromator coeff. A0'
                  ENDIF
                  IF (ABS(xval1-xval2) GT tol_xval*xval1) THEN BEGIN
                   nerr_xval = nerr_xval + 1
                   list_err_xval(nerr_xval) = 'Operations including numors ' +                   $
                                              STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                              ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                              ' not possible: different x-values'
                  ENDIF
                 ENDIF
                 IF ((typscan1 NE 13) AND (typscan1 NE 0)) THEN BEGIN
                  IF ((nel_x1 NE nel_x2) OR (nel_y1 NE nel_y2)) THEN BEGIN
                   nerr_elements = nerr_elements + 1
                   list_err_elmt(nerr_elements) = 'Operations including numors ' +                   $
                                                  STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                  ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                  ' not possible: different number of elements'
                  ENDIF
                 ENDIF
                ENDFOR
               ENDFOR
              ENDELSE                 
             ENDIF    
             
             
             
             IF ((inst_value EQ 'IN4')  OR (inst_value EQ 'IN5')  OR            $
                 (inst_value EQ 'IN6')) THEN BEGIN
              FOR n_num1 = 1, nb_num_tot(nwk)-1 DO BEGIN
               FOR n_num2 = n_num1+1, nb_num_tot(nwk) DO BEGIN
                nel_x1  =    0
                nel_x2  =    0
                nel_y1  =    0
                nel_y2  =    0
                lambda1 =  0.0
                lambda2 =  0.0
                deltae1 =  0.0
                deltae2 =  0.0
                elpp1   = -1.0
                elpp2   = -1.0
               
                substr1   = STRTRIM(STRING(n_num1-1),2)                    
                substr2   = STRTRIM(STRING(n_num2-1),2)                    
                junk = EXECUTE('nel_x1  = N_ELEMENTS(x'+swk_select+'_'+substr1+')')
                junk = EXECUTE('nel_x2  = N_ELEMENTS(x'+swk_select+'_'+substr2+')')
                junk = EXECUTE('nel_y1  = N_ELEMENTS(y'+swk_select+'_'+substr1+')')
                junk = EXECUTE('nel_y2  = N_ELEMENTS(y'+swk_select+'_'+substr2+')')
                junk = EXECUTE('lambda1 = p'+swk_select+'_'+substr1+'(21)')
                junk = EXECUTE('lambda2 = p'+swk_select+'_'+substr2+'(21)')
                junk = EXECUTE('deltae1 = p'+swk_select+'_'+substr1+'(18)')
                junk = EXECUTE('deltae2 = p'+swk_select+'_'+substr2+'(18)')
                junk = EXECUTE('elpp1   = p'+swk_select+'_'+substr1+'(9)')
                junk = EXECUTE('elpp2   = p'+swk_select+'_'+substr2+'(9)')
               
                IF ((nel_x1 NE nel_x2) OR (nel_y1 NE nel_y2)) THEN BEGIN
                 nerr_elements = nerr_elements + 1
                 list_err_elmt(nerr_elements) = 'Operations including numors ' +                   $
                                                STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                ' not possible: different number of elements'
                ENDIF
               
                IF (ABS(lambda1-lambda2) GT tol_lambda) THEN BEGIN
                 nerr_lambda = nerr_lambda + 1
                 list_err_lambda(nerr_lambda) = 'Operations including numors ' +                   $
                                                STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                ' not possible: different wavelengths'
                ENDIF

                IF (ABS((deltae1-deltae2)/deltae1) GT tol_deltae) THEN BEGIN
                 nerr_deltae = nerr_deltae + 1
                 list_err_deltae(nerr_deltae) = 'Operations including numors ' +                   $
                                                STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                ' not possible: different channel widths'
                ENDIF

                IF (ABS(elpp1-elpp2) GT tol_elpp) THEN BEGIN
                 nerr_elpp = nerr_elpp + 1
                 list_err_elpp(nerr_elpp) = 'Operations including numors ' +                       $
                                            STRTRIM(list_numor_wk(nwk,n_num1-1),2) +               $
                                            ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +     $
                                            ' not possible: different el. peak positions'
                ENDIF                              
               ENDFOR
              ENDFOR
             ENDIF

             
             IF ((inst_value EQ 'D11') OR (inst_value EQ 'D22') OR (inst_value EQ 'D17')) THEN BEGIN
              FOR n_num1 = 1, nb_num_tot(nwk)-1 DO BEGIN
               FOR n_num2 = n_num1+1, nb_num_tot(nwk)-1 DO BEGIN
                nel_x1  =   0
                nel_x2  =   0
                nel_y1  =   0
                nel_y2  =   0
                pc1(*)  = 0.0
                pc2(*)  = 0.0
               
                substr1   = STRTRIM(STRING(n_num1-1),2)                    
                substr2   = STRTRIM(STRING(n_num2-1),2)                    
                junk = EXECUTE('nel_x1   = N_ELEMENTS(x'+swk_select+'_'+substr1+')')
                junk = EXECUTE('nel_x2   = N_ELEMENTS(x'+swk_select+'_'+substr2+')')
                junk = EXECUTE('nel_y1   = N_ELEMENTS(y'+swk_select+'_'+substr1+')')
                junk = EXECUTE('nel_y2   = N_ELEMENTS(y'+swk_select+'_'+substr2+')')
                junk = EXECUTE('pc1(0)   = p'+swk_select+'_'+substr1+'(4)')
                junk = EXECUTE('pc2(0)   = p'+swk_select+'_'+substr2+'(4)')
                junk = EXECUTE('pc1(1)   = p'+swk_select+'_'+substr1+'(5)')
                junk = EXECUTE('pc2(1)   = p'+swk_select+'_'+substr2+'(5)')
                junk = EXECUTE('pc1(2)   = p'+swk_select+'_'+substr1+'(6)')
                junk = EXECUTE('pc2(2)   = p'+swk_select+'_'+substr2+'(6)')
                junk = EXECUTE('pc1(3)   = p'+swk_select+'_'+substr1+'(7)')
                junk = EXECUTE('pc2(3)   = p'+swk_select+'_'+substr2+'(7)')
                junk = EXECUTE('pc1(4)   = p'+swk_select+'_'+substr1+'(8)')
                junk = EXECUTE('pc2(4)   = p'+swk_select+'_'+substr2+'(8)')
                junk = EXECUTE('pc1(5)   = p'+swk_select+'_'+substr1+'(9)')
                junk = EXECUTE('pc2(5)   = p'+swk_select+'_'+substr2+'(9)')
                junk = EXECUTE('pc1(6)   = p'+swk_select+'_'+substr1+'(13)')
                junk = EXECUTE('pc2(6)   = p'+swk_select+'_'+substr2+'(13)')
                junk = EXECUTE('pc1(7)   = p'+swk_select+'_'+substr1+'(14)')
                junk = EXECUTE('pc2(7)   = p'+swk_select+'_'+substr2+'(14)')
                junk = EXECUTE('pc1(8)   = p'+swk_select+'_'+substr1+'(15)')
                junk = EXECUTE('pc2(8)   = p'+swk_select+'_'+substr2+'(15)')
                junk = EXECUTE('pc1(9)   = p'+swk_select+'_'+substr1+'(16)')
                junk = EXECUTE('pc2(9)   = p'+swk_select+'_'+substr2+'(16)')
                junk = EXECUTE('pc1(10)  = p'+swk_select+'_'+substr1+'(17)')
                junk = EXECUTE('pc2(10)  = p'+swk_select+'_'+substr2+'(17)')
                junk = EXECUTE('pc1(11)  = p'+swk_select+'_'+substr1+'(18)')
                junk = EXECUTE('pc2(11)  = p'+swk_select+'_'+substr2+'(18)')
                junk = EXECUTE('pc1(12)  = p'+swk_select+'_'+substr1+'(19)')
                junk = EXECUTE('pc2(12)  = p'+swk_select+'_'+substr2+'(19)')
                junk = EXECUTE('pc1(13)  = p'+swk_select+'_'+substr1+'(20)')
                junk = EXECUTE('pc2(13)  = p'+swk_select+'_'+substr2+'(20)')
                junk = EXECUTE('pc1(14)  = p'+swk_select+'_'+substr1+'(21)')
                junk = EXECUTE('pc2(14)  = p'+swk_select+'_'+substr2+'(21)')
                junk = EXECUTE('pc1(15)  = p'+swk_select+'_'+substr1+'(22)')
                junk = EXECUTE('pc2(15)  = p'+swk_select+'_'+substr2+'(22)')
                junk = EXECUTE('pc1(16)  = p'+swk_select+'_'+substr1+'(23)')
                junk = EXECUTE('pc2(16)  = p'+swk_select+'_'+substr2+'(23)')
                                
               
                IF ((nel_x1 NE nel_x2) OR (nel_y1 NE nel_y2)) THEN BEGIN
                 nerr_elements = nerr_elements + 1
                 list_err_elmt(nerr_elements) = 'Operations including numors ' +                   $
                                                STRTRIM(list_numor_wk(nwk,n_num1-1),2) +           $
                                                ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) + $
                                                ' not possible: different number of elements'
                ENDIF

                FOR np = 0, npar_sas-1 DO BEGIN
                 IF ((pc1(np)-pc2(np)) GT (pc1(np)*tol_pc(np))) THEN BEGIN
                  nerr_par = nerr_par + 1
                  list_err_par(nerr_par) = 'Operations including numors ' +                         $
                                           STRTRIM(list_numor_wk(nwk,n_num1-1),2) +                 $
                                           ' and ' + STRTRIM(list_numor_wk(nwk,n_num2-1),2) +       $
                                           ' not possible: some different experimental parameters'
                 ENDIF
                ENDFOR
               ENDFOR
              ENDFOR
             ENDIF              

             IF (nerr_elements GE 0) THEN BEGIN
              err1_wk(nwk) = 1           
              error_base1 = WIDGET_BASE(TITLE = 'Error on Dimensions in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_elements DO BEGIN
               error_text1 = WIDGET_LABEL(error_base1, VALUE = list_err_elmt(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt1 = WIDGET_BUTTON(error_base1, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base1, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt1, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base1, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF
           
             IF (nerr_lambda GE 0) THEN BEGIN
              err1_wk(nwk) = 1           
              error_base2 = WIDGET_BASE(TITLE = 'Error on Wavelength in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_lambda DO BEGIN
               error_text2 = WIDGET_LABEL(error_base2, VALUE = list_err_lambda(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt2 = WIDGET_BUTTON(error_base2, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base2, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt2, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base2, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_deltae GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base3 = WIDGET_BASE(TITLE = 'Error on TOF Channel Width in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_deltae DO BEGIN
               error_text3 = WIDGET_LABEL(error_base3, VALUE = list_err_deltae(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt3 = WIDGET_BUTTON(error_base3, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base3, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt3, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base3, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_elpp GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base4 = WIDGET_BASE(TITLE = 'Error on TOF Elastic Peak Position in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_elpp DO BEGIN
               error_text4 = WIDGET_LABEL(error_base4, VALUE = list_err_elpp(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt4 = WIDGET_BUTTON(error_base4, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base4, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt4, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base4, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_par GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base5 = WIDGET_BASE(TITLE = 'Error on SAS Experimental Parameters in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_par DO BEGIN
               error_text5 = WIDGET_LABEL(error_base5, VALUE = list_err_par(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt5 = WIDGET_BUTTON(error_base5, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base5, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt5, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base5, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_dopmin GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base6 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_dopmin DO BEGIN
               error_text6 = WIDGET_LABEL(error_base6, VALUE = list_err_dopmin(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt6 = WIDGET_BUTTON(error_base6, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base6, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt6, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base6, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_dopmax GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base7 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_dopmax DO BEGIN
               error_text7 = WIDGET_LABEL(error_base7, VALUE = list_err_dopmax(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt7 = WIDGET_BUTTON(error_base7, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base7, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt7, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base7, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_mono GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base8 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_mono DO BEGIN
               error_text8 = WIDGET_LABEL(error_base8, VALUE = list_err_mono(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt8 = WIDGET_BUTTON(error_base8, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base8, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt8, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base8, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_anal GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base9 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                        /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_anal DO BEGIN
               error_text9 = WIDGET_LABEL(error_base9, VALUE = list_err_anal(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt9 = WIDGET_BUTTON(error_base9, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
              WIDGET_CONTROL, error_base9, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt9, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base9, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_defl GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base10 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                         /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_defl DO BEGIN
               error_text10 = WIDGET_LABEL(error_base10, VALUE = list_err_defl(n_err), $
                                           FONT=ft_b_normal)
              ENDFOR
              error_butt10 = WIDGET_BUTTON(error_base10, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                           FONT = ft_b_normal)
              WIDGET_CONTROL, error_base10, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt10, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base10, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_coefmono GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base11 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                         /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_coefmono DO BEGIN
               error_text11 = WIDGET_LABEL(error_base11, VALUE = list_err_coefmono(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt11 = WIDGET_BUTTON(error_base11, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                           FONT = ft_b_normal)
              WIDGET_CONTROL, error_base11, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt11, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base11, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF

             IF (nerr_xval GE 0) THEN BEGIN
              err1_wk(nwk) = 1                      
              error_base12 = WIDGET_BASE(TITLE = 'Error on BACK SC. Experimental Parameters in Workspace '+swk_select, $
                                         /COLUMN, RESOURCE_NAME='lampmic')
              FOR n_err = 0, nerr_xval DO BEGIN
               error_text12 = WIDGET_LABEL(error_base12, VALUE = list_err_xval(n_err), FONT=ft_b_normal)
              ENDFOR
              error_butt12 = WIDGET_BUTTON(error_base12, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                           FONT = ft_b_normal)
              WIDGET_CONTROL, error_base12, GROUP_LEADER=lamp_mic, /REALIZE
              WIDGET_CONTROL, error_butt12, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
              XMANAGER, 'ERROR', error_base12, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
             ENDIF
             

;            ---------------------
;            Processing Operations
;            ---------------------
             
             IF ((nerr_par EQ -1)      AND (nerr_deltae EQ -1) AND (nerr_lambda EQ -1)     AND         $
                 (nerr_elements EQ -1) AND (nerr_elpp EQ -1)   AND (nerr_dopmin EQ -1)     AND         $
                 (nerr_dopmax   EQ -1) AND (nerr_scan EQ -1)   AND (nerr_mono   EQ -1)     AND         $
                 (nerr_anal     EQ -1) AND (nerr_defl EQ -1)   AND (nerr_coefmono EQ -1))  THEN BEGIN
              err1_wk(nwk) = 0
              err2_wk(nwk) = 0
              err3_wk(nwk) = 0
              err4_wk(nwk) = 0                            
              junk = EXECUTE('w'+swk_select+'=w'+swk_select+'_0')
              FOR n1 = 0, nb_plus(nwk) DO BEGIN
               FOR n_num = 1, nb_num_tot(nwk) DO BEGIN
                IF (FIX(list_numor_wk(nwk,n_num-1)) EQ num_aft_plus(nwk,n1)) THEN BEGIN
                 snum = STRTRIM(STRING(n_num-1),2)
                 junk = EXECUTE('w'+swk_select+'=w'+swk_select+'+w'+swk_select+'_'+snum)
                ENDIF
               ENDFOR
              ENDFOR
              FOR n2 = 0, nb_minus(nwk) DO BEGIN
               FOR n_num = 1, nb_num_tot(nwk) DO BEGIN
                IF (FIX(list_numor_wk(nwk,n_num-1)) EQ num_aft_minus(nwk,n2)) THEN BEGIN
                 snum = STRTRIM(STRING(n_num-1),2)
                 junk = EXECUTE('w'+swk_select+'=w'+swk_select+'-w'+swk_select+'_'+snum)
                ENDIF
               ENDFOR
              ENDFOR                                     
             ENDIF                          
            ENDIF ELSE BEGIN
            
;           ---------------------------                      
;           Subcase: SUMTO is activated
;           ---------------------------

;            Number of runs

             nb_num_tot(nwk) = nb_num_wk(nwk) - (nb_sumto(nwk)+1)
             FOR n_sumto = 0, nb_sumto(nwk) DO BEGIN
              IF (num_aft_sumto(nwk,n_sumto) GT num_bef_sumto(nwk,n_sumto)) THEN BEGIN
               nb_num_tot(nwk) = nb_num_tot(nwk) + (num_aft_sumto(nwk,n_sumto)-num_bef_sumto(nwk,n_sumto))
              ENDIF
              IF (num_aft_sumto(nwk,n_sumto) LT num_bef_sumto(nwk,n_sumto)) THEN BEGIN

               error_base = WIDGET_BASE(TITLE = 'Error concerning SUMTO Operators', $
                                        /COLUMN, RESOURCE_NAME='lampmic')
               error_text = WIDGET_LABEL(error_base, VALUE = 'The SUMTO syntax needs ascending' + $
                                         ' sorting for runs',FONT=ft_b_normal)
               error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                          FONT = ft_b_normal)
               WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
               WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
               XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
              ENDIF
             ENDFOR
             
;            ---------------------------                      
;            Subcase: No PLUS & No MINUS
;            ---------------------------
;            List of runs
             IF ((nb_plus(nwk) EQ -1) AND (nb_minus(nwk) EQ -1)) THEN BEGIN 
              FOR n_sumto = 0, nb_sumto(nwk) DO BEGIN
               IF (n_sumto EQ 0) THEN BEGIN
                index2 = LONG(0)
                ns1    = num_bef_sumto(nwk,n_sumto)
                ns2    = num_aft_sumto(nwk,n_sumto)
               ENDIF ELSE BEGIN
                ns1    = num_bef_sumto(nwk,n_sumto)+1
                ns2    = num_aft_sumto(nwk,n_sumto)
               ENDELSE
               FOR index1 = ns1, ns2 DO BEGIN
                index2 = index2 + 1
                list_numor_fin(ntot_num-1+index2) = STRTRIM(STRING(index1),2)
               ENDFOR
              ENDFOR
             ENDIF ELSE BEGIN
;            ---------------------------                      
;            Subcase: Some PLUS or MINUS
;            ---------------------------
             ENDELSE
                
             ntot_num = ntot_num + nb_num_tot(nwk)

              
             err1_wk(nwk) = 0
             err2_wk(nwk) = 0
             err3_wk(nwk) = 0
             err4_wk(nwk) = 0
            ENDELSE	                    
           ENDIF
          ENDIF
         ENDELSE             
        
;        -------------------------------
;        UPDATING HISTORY
;        -------------------------------
         nstr_num(nwk)    = -1
         nstr_oper(nwk)   = -1
         string_hist(nwk) = '['
         FOR n_num = 1, cnt_numor DO BEGIN
          IF (nw_num(n_num-1) EQ nwk) THEN BEGIN
           nstr_num(nwk) = nstr_num(nwk) + 1
           string_hist_num(nwk,nstr_num(nwk)) = STRTRIM(list_numor(nwk,nstr_num(nwk)),2) + ' '
          ENDIF
         ENDFOR
         FOR n_oper = 1, cnt_oper DO BEGIN
          IF (nw_oper(n_oper-1) EQ nwk) THEN BEGIN
           nstr_oper(nwk) = nstr_oper(nwk) + 1
           string_hist_oper(nwk,nstr_oper(nwk)) = STRTRIM(list_oper(nwk,nstr_oper(nwk)),2)
           IF (string_hist_oper(nwk,nstr_oper(nwk)) EQ 'PLUS')  THEN string_hist_oper(nwk,nstr_oper(nwk)) = '+ '
           IF (string_hist_oper(nwk,nstr_oper(nwk)) EQ 'MINUS') THEN string_hist_oper(nwk,nstr_oper(nwk)) = '- '
           IF (string_hist_oper(nwk,nstr_oper(nwk)) EQ 'AND')   THEN string_hist_oper(nwk,nstr_oper(nwk)) = '& '
           IF (string_hist_oper(nwk,nstr_oper(nwk)) EQ 'ANDTO') THEN string_hist_oper(nwk,nstr_oper(nwk)) = '&& '
           IF (string_hist_oper(nwk,nstr_oper(nwk)) EQ 'SUMTO') THEN string_hist_oper(nwk,nstr_oper(nwk)) = '++ '
          ENDIF
         ENDFOR
         FOR nrk = 0, n_rk_max-2 DO BEGIN
          string_hist(nwk) = string_hist(nwk)+string_hist_num(nwk,nrk)+string_hist_oper(nwk,nrk)
         ENDFOR
         string_hist(nwk) = string_hist(nwk) + STRTRIM(string_hist_num(nwk,n_rk_max-1),2)          
         TO_DON_HISTORY, nwk+1 , 0 , 'w'+STRTRIM(STRING(nwk+1),2)+'=RDOPR('+string_hist(nwk)+') ;'+string_norm(nwk)
        ENDIF
       ENDFOR


;      ------------------------------------------------       
;      Sorting List After Examination of all Workspaces
;      ------------------------------------------------       

       list_fin_bis  = list_numor_fin(SORT(list_numor_fin))
       list_fin_ter  = list_numor_fin(SORT(list_numor_fin))
       nel_list_fin  = N_ELEMENTS(list_fin_bis)
       FOR n1 = 0, nel_list_fin-2 DO BEGIN
        IF (flag_list_fin(n1) LT 1) THEN BEGIN
         FOR n2 = n1+1, nel_list_fin-1 DO BEGIN
          IF ((list_fin_bis(n1) EQ list_fin_bis(n2)) AND $
              (list_fin_bis(n1) NE 'No numor')) THEN flag_list_fin(n2) = 1
          IF ((list_fin_bis(n1) EQ list_fin_bis(n2)) AND $
              (list_fin_bis(n1) EQ 'No numor')) THEN BEGIN
           flag_list_fin(n1) = 2             
           flag_list_fin(n2) = 2
          ENDIF
         ENDFOR
        ENDIF
       ENDFOR
       where_flag2_fin  = WHERE(flag_list_fin EQ 2, n_flag2_fin)
       where_flag1_fin  = WHERE(flag_list_fin EQ 1, n_flag1_fin)
       where_flag0_fin  = WHERE(flag_list_fin EQ 0, n_flag0_fin)
       where_no_num_fin = WHERE(list_numor_fin EQ 'No numor', nb_nonum_fin)
       IF ((n_flag0_fin EQ nmax_num) OR  $
           (n_flag2_fin EQ nmax_num)) THEN BEGIN
        WIDGET_CONTROL, list_wkp_4, SET_VALUE = list_fin_bis
       ENDIF ELSE BEGIN      
        list_fin_ter(nmax_num - nb_nonum_fin - n_flag1_fin:nmax_num-1) = 'No numor'  
        list_fin_ter(0:nmax_num - nb_nonum_fin - n_flag1_fin - 1) = list_fin_bis(where_flag0_fin)
        WIDGET_CONTROL, list_wkp_4, SET_VALUE = list_fin_ter
       ENDELSE
       
      ENDIF
                       
      return
      end
;------------------------------------------------------------------------------------------

PRO   NUMOR_ENTRY, event, uv 

@lamp.cbk
@mics.cbk
            
      WIDGET_CONTROL, event.id, GET_VALUE = numor
      numor_bis = FIX(numor(0))
      
      length_numor = STRLEN(numor)

      IF ((length_numor(0) GT 6) OR (numor_bis EQ 0)) THEN BEGIN
       error_base = WIDGET_BASE(TITLE = 'Error about Numor Entry', /COLUMN, RESOURCE_NAME='lampmic')
       error_text = WIDGET_LABEL(error_base, VALUE = 'NUMOR MUST BE 0 < n <= 999999 !!!', FONT=ft_b_normal)
       error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                  FONT = ft_b_normal)
       WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
       WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
       XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
      ENDIF
      file_path=''
      IF (flag_acces EQ 1) THEN BEGIN      
       IF (length_numor(0) EQ 1) THEN file_path = path_for_online(0)  + '00000'  + numor(0)
       IF (length_numor(0) EQ 2) THEN file_path = path_for_online(0)  + '0000'   + numor(0)
       IF (length_numor(0) EQ 3) THEN file_path = path_for_online(0)  + '000'    + numor(0)
       IF (length_numor(0) EQ 4) THEN file_path = path_for_online(0)  + '00'     + numor(0)
       IF (length_numor(0) EQ 5) THEN file_path = path_for_online(0)  + '0'      + numor(0)
       IF (length_numor(0) EQ 6) THEN file_path = path_for_online(0)  + ''       + numor(0)
      ENDIF
      IF (flag_acces EQ 5) THEN BEGIN      
       IF (length_numor(0) EQ 1) THEN file_path = path_for_online(0)  + '00000'  + numor(0)
       IF (length_numor(0) EQ 2) THEN file_path = path_for_online(0)  + '0000'   + numor(0)
       IF (length_numor(0) EQ 3) THEN file_path = path_for_online(0)  + '000'    + numor(0)
       IF (length_numor(0) EQ 4) THEN file_path = path_for_online(0)  + '00'     + numor(0)
       IF (length_numor(0) EQ 5) THEN file_path = path_for_online(0)  + '0'      + numor(0)
       IF (length_numor(0) EQ 6) THEN file_path = path_for_online(0)  + ''       + numor(0)
      ENDIF
      IF (flag_acces EQ 2) THEN BEGIN      
       IF (length_numor(0) EQ 1) THEN file_path = path_for_spectra(0) + 'r00000' + numor(0) + '.dat'
       IF (length_numor(0) EQ 2) THEN file_path = path_for_spectra(0) + 'r0000'  + numor(0) + '.dat'
       IF (length_numor(0) EQ 3) THEN file_path = path_for_spectra(0) + 'r000'   + numor(0) + '.dat'
       IF (length_numor(0) EQ 4) THEN file_path = path_for_spectra(0) + 'r00'    + numor(0) + '.dat'
       IF (length_numor(0) EQ 5) THEN file_path = path_for_spectra(0) + 'r0'     + numor(0) + '.dat'
       IF (length_numor(0) EQ 6) THEN file_path = path_for_spectra(0) + 'r'      + numor(0) + '.dat'
      ENDIF
      
      IF (length_numor(0) EQ 0) THEN BEGIN
       list_numor(uv(3),uv(4))  = 'No numor'
       code_numor(uv(3),uv(4))  = 0
       list_numint(uv(3),uv(4)) = 0
                                ENDIF ELSE BEGIN 
       list_numor(uv(3),uv(4))  = numor(0)
       code_numor(uv(3),uv(4))  = 1
ON_IOERROR, error_fix      
       list_numint(uv(3),uv(4)) = FIX(numor(0))
error_fix:      list_path(uv(3),uv(4))   = file_path(0)
      ENDELSE

      IF ((flag_acces EQ 1) OR (flag_acces EQ 2)) THEN BEGIN
       w_numor(uv(3)+1)= list_numor(uv(3),0)
      ENDIF

      list_bis      = list_numor(SORT(list_numor))
      list_ter      = list_numor(SORT(list_numor))
      flag_list_bis = INTARR(n_wk_max*n_rk_max) & flag_list_bis(*) = 0
      nel_list_bis  = N_ELEMENTS(list_bis)
      FOR n1 = 0, nel_list_bis-2 DO BEGIN
       IF (flag_list_bis(n1) LT 1) THEN BEGIN
        FOR n2 = n1+1, nel_list_bis-1 DO BEGIN
         IF ((list_bis(n1) EQ list_bis(n2)) AND $
             (list_bis(n1) NE 'No numor')) THEN flag_list_bis(n2) = 1
         IF ((list_bis(n1) EQ list_bis(n2)) AND $
             (list_bis(n1) EQ 'No numor')) THEN BEGIN
              flag_list_bis(n1) = 2             
              flag_list_bis(n2) = 2
         ENDIF             
        ENDFOR
       ENDIF
      ENDFOR
      where_flag2    = WHERE(flag_list_bis EQ 2, n_flag2)
      where_flag1    = WHERE(flag_list_bis EQ 1, n_flag1)
      where_flag0    = WHERE(flag_list_bis EQ 0, n_flag0)
      where_no_numor = WHERE(list_numor EQ 'No numor', nb_nonum)
      IF ((n_flag0 EQ (n_wk_max*n_rk_max)) OR  $
          (n_flag2 EQ (n_wk_max*n_rk_max))) THEN BEGIN
       WIDGET_CONTROL, uv(5), SET_VALUE = list_bis
      ENDIF ELSE BEGIN      
       list_ter(n_wk_max*n_rk_max-nb_nonum-n_flag1:n_wk_max*n_rk_max-1) = 'No numor'  
       list_ter(0:n_wk_max*n_rk_max-nb_nonum-n_flag1-1) = list_bis(where_flag0(*))
       WIDGET_CONTROL, uv(5), SET_VALUE = list_ter
      ENDELSE
            
      file_found = FINDFILE(file_path(0), COUNT = nb_file)
      IF (nb_file eq 0) THEN BEGIN
       error_base  = WIDGET_BASE(TITLE = 'Error Message', /COLUMN, RESOURCE_NAME='lampmic')
       error_text1 = WIDGET_LABEL(error_base, VALUE = 'NUMOR ' + numor(0) + ' NOT EXISTING  !!!', $
                                  FONT=ft_b_normal)
       error_text2 = WIDGET_LABEL(error_base,                                                      $
                                  VALUE = 'Please would you make corrections before updating !!!', $
                                  FONT=ft_b_normal)
       error_butt = WIDGET_BUTTON(error_base, VALUE = 'EXIT FROM ERROR MESSAGE', $
                                  FONT = ft_b_normal)
       WIDGET_CONTROL, error_base, GROUP_LEADER=lamp_mic, /REALIZE
       WIDGET_CONTROL, error_butt, BAD_ID = I, SET_UVALUE=[-88,199,0,0,0,0,0,0]
       XMANAGER, 'ERROR', error_base, event_handler = 'LAMP_EVENT_PARSER', /JUST_REG
      ENDIF
      IF (nb_file gt 0) THEN BEGIN      
       nwk_select     = uv(3) + 1
       swk_select     = STRTRIM(STRING(nwk_select),2)
       err2_wk(uv(3)) = 1
      ENDIF
             
      return
      end
;------------------------------------------------------------------------------------------

PRO   READ_DATA

@lamp.cbk
@mics.cbk
      IF (inst_value eq 'D7')      THEN READ_TOF
      IF (inst_value eq 'D11')     THEN READ_SAS
      IF (inst_value eq 'D22')     THEN READ_SAS      
      IF (inst_value eq 'D17')     THEN READ_SAS 
      IF (inst_value eq 'D16')     THEN READ_D16 
      IF (inst_value eq 'IN4')     THEN READ_TOF
      IF (inst_value eq 'IN5')     THEN READ_TOF 
      IF (inst_value eq 'IN6')     THEN READ_TOF
      IF (inst_value eq 'IN10')    THEN READ_BSC   
      IF (inst_value eq 'IN13')    THEN READ_BSC
      IF (inst_value eq 'IN16')    THEN READ_BSC   
;     IF (inst_value eq 'IN15')    THEN READ_IN15
      IF (inst_value eq 'D11-TOF') THEN READ_D11TOF

      IF  monimon ge 0  THEN BEGIN ws=swk_select & wi=nwk_select
			iii=execute('READ_DATA_MON, w'+ws+',y'+ws+',n'+ws+',x_tit,wi,inst_value')
      ENDIF

      return
      end

;------------------------------------------------------------------------------------------            

PRO READ_DATA_MON, wws , yws , nws , titi,wi , inst_v

	  szw=size(wws)
	  IF szw(0) gt 0 THEN BEGIN
	     IF szw(0) lt 2 then szw(2)=1
	     IF (inst_v eq 'IN10') or (inst_v eq 'IN16') THEN BEGIN avm=1.
		 wws=FLOAT(wws)
		 nws=nws(*,0)>1
		 avm=round(total(nws)/szw(1))
		 for i =0,szw(2)-1 do wws(*,i)=wws(*,i)/nws
		 wws=wws*avm
		 nws(*)=avm
		 titi(wi)=titi(wi)+' Normalized'
	     ENDIF
	     ok =0
	     iii=execute('iii=rdid()')
	     P_DID_CALDO, strlowcase(inst_v),wws ,yws ,ok
	     IF  ok THEN BEGIN
		 titi(wi)=titi(wi)+' pre-Calibrated'
		 IF (inst_v eq 'IN6') THEN BEGIN GROUPY,wws ,yws ,/average
						 monu= 500000.
						 tot = monu/total(nws(*,0))
						 wws = wws*tot
						 nws(*,0) =0 & nws(0,0)=monu
		 ENDIF
	     ENDIF
	  ENDIF
END

;------------------------------------------------------------------------------------------            

FUNCTION MKPULL_GETLINE, unit, data, idx, n, label, value

ret = -1
value = ''

if (unit eq 0) then not_eof = (idx lt n) else not_eof = (not eof(unit))
while ((not_eof) and (ret eq -1)) do begin
  if (unit eq 0) then begin
    value = data(idx)
    idx = idx + 1
  endif else begin
    readf, unit, value
  endelse
  value = strtrim(value, 2)		; Leading/trailing whitespace
  delim = strmid(value, 0, 1)
  case delim of
    "" :
    "#" :
    "}" : ret = 2
    else: begin
      value = strmid(value, 1, 100000)
      pos = strpos(value, delim)
      if (pos eq -1) then begin
	message, "Bad delimiter in line: " + delim + value, /INFORM
      endif else begin
        label = strmid(value, 0, pos)
        value = strtrim(strmid(value, pos+1, 100000), 2)
        if (value eq "{") then begin
	  value = "" 
	  ret = 1
        endif else begin
          if (strlen(value) eq 0) then value = label
	  ret = 0
        endelse
      endelse
    end
  endcase
if (unit eq 0) then not_eof = (idx lt n) else not_eof = (not eof(unit))
endwhile

if ((unit ne 0) and (ret eq -1)) then begin free_lun, unit & unit = 0 & end

return, ret

end


;========================================================================================
;========================================================================================
;========================================================================================


PRO MKPULL_PULLDOWN, parent, unit, data, idx, n, font, lamp_uv
;
; unit - A file LUN or 0.
; data - If Unit is 0, data is a string array containing the menu
;	description.
; idx  - If Unit is 0, idx is an integer giving the current index into data.
; n    - If Unit is 0, n is an integer giving the # of elements in data.

while 1 do begin
  ret = mkpull_getline(unit, data, idx, n, label, value)

  case ret of
    -1 : return
    0  : begin
	     if font ne '' then begin
		 but = WIDGET_BUTTON(parent, value=label, uvalue=value, font=font)
	     endif else begin
		but = WIDGET_BUTTON(parent, value=label, uvalue=value)
	     endelse
	     WIDGET_CONTROL, but, bad_id=i, set_uvalue = lamp_uv
	     lamp_uv(2)=lamp_uv(2)+1
	 end
    1  : begin
	     if font ne '' then begin
		 but = WIDGET_BUTTON(parent, value=label, MENU = 2, font=font)
	     endif else begin
		 but = WIDGET_BUTTON(parent, value=label, MENU = 2)
	     endelse
	     mkpull_pulldown, but, unit, data, idx, n, font, lamp_uv
         end
    2  : return
  endcase
endwhile

end


;========================================================================================
;========================================================================================
;========================================================================================


PRO LAMP_PDM, DESC, PARENT, BASE=BASE, FRAME=FRAME, TITLE=TITLE,	$
	      COLUMN=COLUMN, FONT=FONT, LAMP_PDM_UVALUE=lamp_uv

  s = size(parent)
  if (s(s(0) + 1) eq 0) then begin
    ; No parent is specified.
    parent = 0
    if (not keyword_set(TITLE)) then TITLE='Menu'
  endif else begin
    if (s(0) ne 0) then message, 'PARENT must be a scalar value.'
    if (s(1) ne 3) then message, 'PARENT must be a long integer.'
  endelse

  s = size(desc)
  if (s(s(0)+1) ne 7) then $
	message,'Description argument must be of type string.'
  if (s(0) eq 0) then begin
    OPENR, unit, desc, /GET_LUN
    n = 0
 endif else begin
    if (s(0) ne 1) then message, 'String array must be 1-D.'
    unit = 0
    n = s(1)
  endelse

  if (not keyword_set(frame)) then frame = 0
  if (not keyword_set(font)) then font = ''

  if (parent eq 0) then $
      IF(KEYWORD_SET(COLUMN)) THEN $
      	  base = WIDGET_BASE(/COLUMN, TITLE=TITLE, FRAME=FRAME) $
      ELSE $
	  base = WIDGET_BASE(/ROW, TITLE=TITLE, FRAME=FRAME) $
  else $
      IF(KEYWORD_SET(COLUMN)) THEN $
      	  base = WIDGET_BASE(parent, /COLUMN, FRAME=FRAME) $
      ELSE $
	  base = WIDGET_BASE(parent, /ROW, FRAME=FRAME)

  mkpull_pulldown, base, unit, desc, 0, n, font, lamp_uv

end


;========================================================================================
;========================================================================================
;========================================================================================
PRO P_MIC_SETRUN, run, wi, text, param

; lamp.com:  setenv LAMP_EXEC ~richard/lamp/r_mic.so  (or r_mic_SGI.so)
; lamp.pro:  lamp_exec  = getenv('LAMP_EXEC')
; lamp.pro:  lamp_entry = r_mic or r_micc or r_micc_ (depending on version.os: vms, idol, unix)
;
; list of parameters in CALL EXTERNAL (DIDS.PRO) : I=input & O=output
; and for Helga's routines [idol_fil_param (A), idol_fil (B)]
;  - instru     : I.       integer coding for instrument (1->20) - A & B
;  - get        : I.and O. status 0=get parameters - 1=get parameters and data - A & B
;  - run        : I.       run number - A & B
;  - channel    : O.       number of channels/subspectrum - A & B
;  - spect      : O.       number of subspectra - A & B
;  - np         : ???      Special parameter for DIDS, doesn't exist in Helga's CALL
;                          This parameter is surely for D19, IN15, etc ...
;  - text       : O.       block 2 of current data file (ascii characters) - A & B
;  - param      : O.       block 3 & 4 of current data file - A & B
;  - 0 or nppar : I.       Size of data buffer
;  - 0 or w     : O.       workspace to put DATA in - B
;
; list of status returned from IDOL
;	- 1  : client/server connection on the local node could not be established
;	- 2  : client/server connection on the router node could not be established
;	- 3  : local node cannot access the server node
;	- 4  : router node cannot access VME crate
;	- 5  : VME memory read error
;	- 6  : No host defined
;	- 7  : Sequence error in data transfer
;	- 8  : buffer too small for requested temperature
;	- 9  : parameter error
;	- 10 : router is busy with other transfer
;	- 11 : file read error on router
;
; Entry point for MICS in DIDS.PRO: P_DID_GETRUN, run, wi, full
; where run=numor, wi=workspace, full=status

@lamp.cbk
@mics.cbk
      nwk_select = wi
      swk_select = STRTRIM(STRING(nwk_select),2)


;========================================================================================================
;     IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5  -  IN5
;========================================================================================================
      IF (inst_value EQ 'IN5') THEN BEGIN      
      ENDIF

;========================================================================================================
;     IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6  -  IN6
;========================================================================================================
      IF (inst_value EQ 'IN6') THEN BEGIN      
      ENDIF

;========================================================================================================
;     D22 -  D22 -  D22 -  D22 -  D22  -  D17 -  D17 -  D17 -  D17 -  D17 -  D17 -  D17  -  D17
;========================================================================================================
      IF (inst_value EQ 'D22') OR (inst_value eq 'D17') OR (inst_value eq 'D11') THEN BEGIN
;      --------------------------------------
;      CONVERTING BYTE IN STRING
;      EXTRACTING STRING VARIABLES
;      --------------------------------------
       main_title             = STRING(text(0:59))
       sub_title              = STRING(text(60:79))
       start_time             = STRING(text(80:97))      
       stop_time              = STRING(text(100:117))

       w_numor(nwk_select)    = STRTRIM(STRING(run),2)       
       x_tit(nwk_select)      = 'X-direction'
       y_tit(nwk_select)      = 'Y-direction'
       z_tit(nwk_select)      = 'Sample angle'
       w_tit(nwk_select)      = STRTRIM(main_title,2)
       other_tit(nwk_select)  = w_numor(nwk_select)+' '+ $
       				STRCOMPRESS(STRING(text(60:79))) + ' ' + $
                                STRCOMPRESS(STRING(text(80:97))) + ' ' + $
                                STRCOMPRESS(STRING(text(100:117)))
       
       head_tit(nwk_select,0) = STRTRIM(sub_title,2)
       head_tit(nwk_select,1) = STRTRIM(main_title,2)
       head_tit(nwk_select,2) = inst_value
       head_tit(nwk_select,3) = STRTRIM(STRING(run),2)
       head_tit(nwk_select,4) = STRTRIM(start_time,2)
       head_tit(nwk_select,5) = ''
       head_tit(nwk_select,6) = x_tit(nwk_select)
       head_tit(nwk_select,7) = y_tit(nwk_select)
       head_tit(nwk_select,8) = z_tit(nwk_select)
       head_tit(nwk_select,9) = ''
       
;      --------------------------------------
;      ASSIGNING VALUES to Xn
;      ASSIGNING VALUES to Yn
;      --------------------------------------
       dim       = LONG(64)
       junk      = EXECUTE('w'+swk_select+'=w'+swk_select)
       junk      = EXECUTE('dim=size(w'+swk_select+')')
       nb_points = dim(dim(0)+2)
       x_buf     = INDGEN(dim(1))
       y_buf     = INDGEN(dim(2))
       
                        
;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------     
       par_txt(nwk_select,0)   = 'PRESET 1                             ='
       par_txt(nwk_select,1)   = 'PRESET 2                             ='    
       par_txt(nwk_select,2)   = 'Run duration (1/10 sec.)             =' 
       par_txt(nwk_select,3)   = 'Total detector counts                ='
       par_txt(nwk_select,4)   = 'Detector offset angle (deg.)         ='
       par_txt(nwk_select,5)   = 'Coder 1: By (mm)                     ='    
       par_txt(nwk_select,6)   = 'Coder 2: Bx (mm)                     ='
       par_txt(nwk_select,7)   = 'Coder 3: Sample changer transl. (mm) ='
       par_txt(nwk_select,8)   = 'Coder 4: Detector distance (set) (m) ='
       par_txt(nwk_select,9)   = 'Sample-Detector distance (calc.) (m) ='
       par_txt(nwk_select,10)  = 'Sample Temperature (K)               =' 
       par_txt(nwk_select,11)  = 'Value of IEEE-1 at start             ='
       par_txt(nwk_select,12)  = 'Value of IEEE-1 at end               ='
       par_txt(nwk_select,13)  = 'Beam centre adress X0 (mm)           ='
       par_txt(nwk_select,14)  = 'Beam centre adress Y0 (mm)           ='
       par_txt(nwk_select,15)  = 'Wavelength (angstroms)               ='
       par_txt(nwk_select,16)  = 'Wavelength resolution                ='
       par_txt(nwk_select,17)  = 'Collimation  (m)                     =' 
       par_txt(nwk_select,18)  = 'Detector angle (set) (deg.)          ='
       par_txt(nwk_select,19)  = 'Detector translation (set) (mm)      ='
       par_txt(nwk_select,20)  = 'Selector angle (deg.)                ='
       par_txt(nwk_select,21)  = 'Sample distance (mm)                 ='
       par_txt(nwk_select,22)  = 'Sample rotation (deg.)               ='
       par_txt(nwk_select,23)  = 'Changer position                     ='
       par_txt(nwk_select,24)  = 'Sample height (mm)                   =' 
       par_txt(nwk_select,25)  = 'Shear speed (1/min.)                 ='
       par_txt(nwk_select,26)  = 'Not used                             ='
       par_txt(nwk_select,27)  = 'Not used                             ='
       par_txt(nwk_select,28)  = 'Not used                             ='     
       par_txt(nwk_select,29)  = 'Not used                             ='
       par_txt(nwk_select,30)  = 'Not used                             ='
       
       
;      --------------------------------------
;      PARAMETER VALUES ASSIGNMENT IN Pn
;      --------------------------------------          
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = param(0)		; PRESET 1
       p_buf(1)   = param(1)		; PRESET 2    
       p_buf(2)   = param(2)		; Run duration (sec.) 
       p_buf(3)   = param(3)		; Total detector counts
       p_buf(4)   = param(14)		; Detector offset angle
       p_buf(5)   = param(15)		; Coder 1: By    
       p_buf(6)   = param(16)		; Coder 2: Bx
       p_buf(7)   = param(17)		; Coder 3: Sample changer/s transl.
       p_buf(8)   = param(18)		; Coder 4: Detector distance (set)
       p_buf(9)   = param(25)		; Sample-Detector distance (calc.)
       p_buf(10)  = param(30)		; Sample Temperature 
       p_buf(11)  = param(32)		; Value of IEEE-1 at start
       p_buf(12)  = param(33)		; Value of IEEE-1 at end
       p_buf(13)  = param(50)		; X0
       p_buf(14)  = param(51)		; Y0
       p_buf(15)  = param(52)		; Wavelength
       p_buf(16)  = param(53)		; Wavelength resolution
       p_buf(17)  = param(57)		; Collimation   
       p_buf(18)  = param(60)		; Detector angle (set)
       p_buf(19)  = param(61)		; Detector translation (set)
       p_buf(20)  = param(62)		; Selector angle
       p_buf(21)  = param(63)		; Sample rotation
       p_buf(22)  = param(64)		; Sample angle
       p_buf(23)  = param(65)		; Changer position
       p_buf(24)  = param(66)		; Sample height 
       p_buf(25)  = param(80)		; Shear speed
       p_buf(26)  = 0.0  		; Not used
       p_buf(27)  = 0.0  		; Not used
       p_buf(28)  = 0.0  		; Not used      
       p_buf(29)  = 0.0  		; Not used
       p_buf(30)  = 0.0  		; Not used

       junk       = EXECUTE('x'+swk_select+'=x_buf')
       junk       = EXECUTE('y'+swk_select+'=y_buf')
       junk       = EXECUTE('z'+swk_select+'=p_buf(22)')
       junk       = EXECUTE('p'+swk_select+'=p_buf')
       junk       = EXECUTE('n'+swk_select+'=p_buf(0 )')

;       TO_DON_HISTORY, nwk_select , 0 , 'w'+swk_select+ '=RDRUN(' +STRTRIM(STRING(run),2)+ ')'
                                     
      ENDIF

;========================================================================================================
;     IN10  -  IN10  -  IN10  -  IN10  -  IN10  -  IN10  -  IN10  -  IN10 
;========================================================================================================
   IF (inst_value EQ 'IN10') THEN BEGIN      
      ENDIF
;========================================================================================================
;     IN16  -  IN16  -  IN16  -  IN16  -  IN16  -  IN16  -  IN16  -  IN16 
;========================================================================================================
   IF (inst_value EQ 'IN16') THEN BEGIN
       paramsize=size(param)
;      --------------------------------------
;      CONVERTING BYTE IN STRING
;      EXTRACTING STRING VARIABLES
;      --------------------------------------
       main_title             = STRING(text(0:59))
       sub_title              = STRING(text(60:79))
       start_time             = STRING(text(80:97))      
       stop_time              = STRING(text(100:117))


       w_numor(nwk_select)    = STRTRIM(STRING(run),2)       
       x_tit(nwk_select)      = 'Channels'
       y_tit(nwk_select)      = 'Spectra'
       z_tit(nwk_select)      = 'Counts'
       w_tit(nwk_select)      = STRTRIM(main_title,2)
       other_tit(nwk_select)  = w_numor(nwk_select)+' '+ $
       				STRCOMPRESS(STRING(text(60:79))) + ' ' + $
                                STRCOMPRESS(STRING(text(80:97))) + ' ' + $
                                STRCOMPRESS(STRING(text(100:117)))
   
       head_tit(nwk_select,0) = STRTRIM(sub_title,2)
       head_tit(nwk_select,1) = STRTRIM(main_title,2)
       head_tit(nwk_select,2) = inst_value
       head_tit(nwk_select,3) = STRTRIM(STRING(run),2)
       head_tit(nwk_select,4) = STRTRIM(start_time,2)
       head_tit(nwk_select,5) = ''
       head_tit(nwk_select,6) = x_tit(nwk_select)
       head_tit(nwk_select,7) = y_tit(nwk_select)
       head_tit(nwk_select,8) = z_tit(nwk_select)
       head_tit(nwk_select,9) = ''

       junk   = EXECUTE('ww_buf=FLOAT(w'+swk_select+')')
       run_size=size(ww_buf)
       n_ys=run_size(2)
       n_chans=run_size(1)
       n_buf     = FLTARR(n_chans,1)  ; Monitor M1
       x_buf     = INDGEN(n_chans)+1
       y_buf     = fltarr(n_ys-param(8))

;
; Only treat doppler scan for now gjk
     IF PARAM(14) EQ 0 THEN BEGIN
     
;      --------------------------------------
;      PARAMETER TEXT  ASSIGNMENT IN PAR_TXT
;      --------------------------------------      

       par_txt(nwk_select,0)   = 'Type of scan (index)                 ='
       par_txt(nwk_select,1)   = 'Number of channels                   ='
       par_txt(nwk_select,2)   = 'Doppler Frequency                    ='
       par_txt(nwk_select,3)   = 'Number of detectors                  ='
       par_txt(nwk_select,4)   = 'Number of monitors                   ='
       par_txt(nwk_select,5)   = 'Mesuring time per step (seconds)     ='
       par_txt(nwk_select,6)   = 'Not used                             ='    
       par_txt(nwk_select,7)   = 'Not used                             ='    
       par_txt(nwk_select,8)   = 'Monochromator d-spacing (ang.)       ='
       par_txt(nwk_select,9)   = 'Analyser      d-spacing (ang.)       ='
       par_txt(nwk_select,10)   = 'Average sample temperature   (K)     ='
       par_txt(nwk_select,11)  = 'Deflector Chopper frequency  (Hz)    ='
       par_txt(nwk_select,12)  = 'Number of dead channels              ='
       par_txt(nwk_select,13)  = 'T1 (microsec.)                       ='
       par_txt(nwk_select,14)  = 'T2 (microsec.)                       ='
       par_txt(nwk_select,15)  = 'Not used                             ='     
       par_txt(nwk_select,16)  = 'Not used                             ='     
       par_txt(nwk_select,17)  = 'Not used                             ='     
       par_txt(nwk_select,18)  = 'Not used                             ='     
       par_txt(nwk_select,19)  = 'MD position                          ='     
       par_txt(nwk_select,20)  = 'Single scattering angle 1    (deg.)  ='     
       par_txt(nwk_select,21)  = 'Single scattering angle 2    (deg.)  ='     
       par_txt(nwk_select,22)  = 'Single scattering angle 3    (deg.)  ='     
       par_txt(nwk_select,23)  = 'Single scattering angle 4    (deg.)  ='     
       par_txt(nwk_select,24)  = 'Single scattering angle 5    (deg.)  ='     
       par_txt(nwk_select,25)  = 'Single scattering angle 6    (deg.)  ='     
       par_txt(nwk_select,26)  = 'Single scattering angle 7    (deg.)  ='     
       par_txt(nwk_select,27)  = 'Single scattering angle 8    (deg.)  ='     
       par_txt(nwk_select,28)  = 'First angle MD-tube          (deg.)  ='
       par_txt(nwk_select,29)  = 'Angle increment MD           (deg.)  ='   
       par_txt(nwk_select,30)  = 'Number of MD-tubes                   ='
     
;      -------------------------------
;      PARAMETER ASSIGNMENT IN Pn
;      -------------------------------      
       p_buf      = FLTARR(npars)
      
       p_buf(0)   = param(14)		; Type of scan                 (index)
       p_buf(1)   = param(6)		; Number of channels
       p_buf(2)   = param(2)		; Doppler frequency
       p_buf(3)   = param(7)		; Number of detectors
       p_buf(4)   = param(8)		; Number of monitors
       p_buf(5)   = param(0)		; Duration of scan             (seconds)
       p_buf(6)   = param(1)		; Counts in Monitor 1
       p_buf(7)   = param(2)		; Average Doppler frequency
       p_buf(8)   = param(69)		; Monochromator d-spacing      (ang.)
       p_buf(9)   = param(79)		; Analyser      d-spacing      (ang.)
       p_buf(10)  = param(9)		; Average sample temperature   (K)
       p_buf(11)  = param(20)		; Deflector Chopper frequency  (Hz)
       p_buf(12)  = param(83)		; Number of dead channels
       p_buf(13)  = param(59)		; T1                           (microsec.)
       p_buf(14)  = param(60)		; T2                           (microsec.)
       p_buf(15)  = 0.0  		; Not used     
       p_buf(16)  = 0.0  		; Not used     
       p_buf(17)  = 0.0  		; Not used     
       p_buf(18)  = 0.0  		; Not used     
       p_buf(19)  = param(64)  	        ; MD position     
;      p_buf(20)  = param?(20)		; Single scattering angle 1    (deg.)     
;      p_buf(21)  = param?(21)		; Single scattering angle 2    (deg.)     
;      p_buf(22)  = param?(22)		; Single scattering angle 3    (deg.)     
;      p_buf(23)  = param?(23)		; Single scattering angle 4    (deg.)     
;      p_buf(24)  = param?(24)		; Single scattering angle 5    (deg.)     
;      p_buf(25)  = param?(25)		; Single scattering angle 6    (deg.)     
;      p_buf(26)  = param?(26)		; Single scattering angle 7    (deg.)     
;      p_buf(27)  = param?(27)		; Single scattering angle 8    (deg.)     
;      p_buf(28)  = param?(0)		; First angle MD-tube          (deg.)
;      p_buf(29)  = param?(1)-param?(0) ; Angle increment MD           (deg.)   
       p_buf(30)  = 20.0		; Number of MD-tubes
;======================================================================
;  			GET DETECTORS IN ORDER
;========================================================================
;
;
;If scan type is 0 then the last spectra are the monitors, otherwise its some
;other crap.
     n_specs=n_ys-param(8)

;
          p2_ost=128   
;If there are more than 19 spectra then the multi-detector is in use!
	n_mult=20-1		
        IF N_SPECS GT 20 THEN BEGIN
;multi(0->19) dets(20->28)
;
; Get multidetector angles
          y_buf(0:n_mult)=param(p2_ost:p2_ost+n_mult)
;
; Get angles of normal detectors
          n_dets =n_specs-(n_mult+1)
	  p_start=p2_ost+n_mult+1
	  p_end=p_start+(n_dets-1)
	  y_buf(n_mult+1:n_specs-1)=param(p_start:p_end)
	  if n_dets gt 10  then n_dets=10
          p_buf(20:20+(n_dets-1))=param(p_start:p_end)
;
       ENDIF ELSE BEGIN
       y_buf(0:n_specs-1)=param(p2_ost:p2_ost+(n_specs-1))
       ENDELSE
;
;  Reshuffle if multi det and ordinary dets
       mon1=param(7)
       mon2=(param(7)+param(8))-1
        if n_specs gt 20 then begin
	  www_buf=ww_buf
	  www_buf(*,mon1:mon2)=ww_buf(*,mon1:mon2)      
	  yyy_buf=fltarr(n_specs)
	  www_buf(*,0:n_dets-1)=ww_buf(*,20:n_specs-1)  ; Normal detectors to beginning spectra
	  www_buf(*,n_dets:19+n_dets)=ww_buf(*,0:19)           ; Multidet to end  
	  yyy_buf(0:n_dets-1)=y_buf(20:n_specs-1)  ; Normal detectors to beginning angles
	  yyy_buf(n_dets:19+n_dets)=y_buf(0:19)           ; Multidet to end
	  y_buf=yyy_buf
	  ww_buf=www_buf  
	endif
;
;Monitors are last spectra
       n_buf= ww_buf(*,mon1:mon2)  
       x_buf     = INDGEN(n_chans)+1
       w_buf=fltarr(n_chans,param(7)-1)
       w_buf=ww_buf(*,0:param(7)-1)
       junk       = EXECUTE('x'+swk_select+'=x_buf')
       junk       = EXECUTE('y'+swk_select+'=y_buf')
       junk       = EXECUTE('n'+swk_select+'=n_buf')      
       junk       = EXECUTE('p'+swk_select+'=p_buf')
       junk       = EXECUTE('w'+swk_select+'=w_buf')
;       TO_DON_HISTORY, nwk_select , 0 , 'w'+swk_select+ '=RDRUN(' +STRTRIM(STRING(run),2)+ ')'                           
      ENDIF
  ENDIF

      IF  monimon ge 0  THEN BEGIN ws=swk_select & wi=nwk_select
			iii=execute('READ_DATA_MON, w'+ws+',y'+ws+',n'+ws+',x_tit,wi,inst_value')
      ENDIF

return
end

pro mics
;** ****
;** For compilation.
return
end
