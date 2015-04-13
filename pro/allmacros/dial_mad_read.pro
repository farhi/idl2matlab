
pro make_mad_struc
;** **************
;**
;** structures t_NOTHER, t_RES, t_STATUS, t_PARA,...
;** are used for MAD memory mapping.

common madshare, mad_home_dir, t_PARA,   t_NOTHER, t_RES, t_XBU, t_COUNTS, t_ERREURS, t_PILE,$
                               t_MOTORS, t_STATUS, t_CHOPPER,   geoMadStr, geoHytec, geoData, geoLimits

;**************************************
;* User define table here !!!!!!!!!!! *
;**************************************
;**************************************************************************************************
;*               Dial.Type        Structure      min.Freq   Extract value                         *
;**************************************************************************************************
   geoMadStr = [['motors'	, 't_nother'	, '0.5'	, ' t_nother.actang'			],$
		['t_nother'	, 't_nother'	, '0.5'	, ' t_nother'				],$
		['phase'	, 't_res'	, '0.5'	, '[t_res.phase_nb,t_res.phase_mes]'	],$
		['temp'		, 't_res'	, '0.5'	, ' t_res.tempea'			],$
		['t_res'	, 't_res'	, '0.5'	, ' t_res'				],$
		['d22cps'	, 't_status'	, '0.2'	, '[t_status.det_cps,t_status.det_sum]' ],$
		['monitor1'	, 't_status'	, '0.2'	, ' t_status.mon1'			],$
		['monitor2'	, 't_status'	, '0.2'	, ' t_status.mon2'			],$
		['hytecsum'	, 't_status'	, '0.2'	, ' t_status.hytec_sum'			],$
		['pmcsum'	, 't_status'	, '0.2'	, ' t_status.pmc_sum'			],$
		['time'		, 't_status'	, '0.2'	, ' t_status.duree_l'			],$
		['status'	, 't_status'	, '0.0'	, ' '					],$
		['t_status'	, 't_status'	, '0.0'	, ' t_status'				],$
		['flagus'	, 't_status'	, '0.0'	, ' '					],$
		['wave'		, 't_para'	, '0.9'	, ' t_para.wave'			],$
		['t_para'	, 't_para'	, '0.5'	, ' t_para'				],$
		['chopper1'	, 't_chopper1'	, '0.5'	, ' t_chopper'				],$
		['chopper2'	, 't_chopper2'	, '0.5'	, ' t_chopper'				],$
		['hytec'	, 'hytec'	, '0.5'	, ' '					],$
		['pmc'  	, 'pmc'	        , '0.5'	, ' '					],$
		['data'		, 'data'	, '0.4'	, ' '					],$
		['limits'	, 'limits'	, '2.0'	, ' '					],$
		['log'		, 'log'		, '1.5'	, ' '					],$
		['currents'	, 't_nother'	, '0.5'	, ' t_nother.actbob'			]]
;**************************************************************************************************
;**************************************************************************************************
;*!You have to restart George whenever you modify this table!*
;*************************************************************

Mach=strlowcase(getenv('HOST')) & id=strpos(Mach,'.')
if id gt 0 then Mach=strmid(Mach,0,id)
mad_home_dir="/users/"+Mach+"/"

if Mach eq 'd7'  then GeoData  =lonarr(16896)
if Mach eq 'd7'  then GeoHytec =lonarr(64)
if Mach eq 'd17' then GeoData  =lonarr(560000)
if Mach eq 'd22' then GeoData  =lonarr(128,128)
if Mach eq 'in6' then GeoData  =lonarr(512,300)
if Mach eq 'd20' then GeoData  =lonarr(1600)
                      GeoLimits=fltarr(48)

DET_NO   =1        
IND_ang1 =6        
IND_ang2 =6       
N_CHOP   =1	& if Mach eq 'in6' then N_CHOP=2
NB_CHAG  =6            
RES_MD_X =128        
RES_MD_Y =128       
SEL_MAX  =3              

str80 =bytarr(80) & str40 =bytarr(40) & str39 =bytarr(39) & str20= bytarr(20)
str18= bytarr(18) & str11= bytarr(11) & str10= bytarr(10) & str8 = bytarr(8)
str3 = bytarr(3)  & str2 = bytarr(2)

;****************************************  T_RES  *****************************************************
;****************************************  T_RES  *****************************************************
IF Mach eq 'd7' THEN $
t_RES   = { tt_res,  $
        LASNUM:0L, ISODIS:0L, NKMES:0L, JCACQ:0L, JC:0L, ICHECK:0L,$
        CHOP_STATUS:lonarr(N_CHOP), NWSAVE:0L,  NFSAVE:0L, NKSAVE:0L, NWHICH:0L,  $
        NFUNC:0L, NKEY:0L, RST_COMMI:str80, N_RST_COMMAND:0L, NPMES:0L,$
        N_TOTAL_SPECT:0L, N_MON_SPECT:0L, N_SINGLE_SPECT:0L, ISWITCH_DYN:0L,$
        NSWITCH:0L, LUN_TR:0L, LOG_FILE:0L, IXBFIL:0L, NXFILE:0L, NREPET:0L,$
        NPREP:0L, TYPE_MES:0L, PHASE_NB:0L, PHASE_MES:0L, free_01:0L,$
        num_temp:0L, rota_nb:0L, rota_mes:0L, preset_b_cur:0L, rest_time:0L,$
        hytec_sum:0L, chan_sum:0L, ss_timer:0L, monitor1:0L, monitor2:0L,$
        numor_to_send:0L, lastsend:0L,$
        free1:lonarr(79), check1:0L,$
        TEMPEA:fltarr(4), TEMPE:0.0, XC:0.0, SPEED_ACT:fltarr(N_CHOP),$
        PHASE_ACT:fltarr(N_CHOP), SPEED_REQ:fltarr(N_CHOP), PHASE_REQ:fltarr(N_CHOP),$
        update_data:0.0, free2:fltarr(98), check2:0.0}
	
IF Mach eq 'd22' THEN $
t_RES   = { tt_res,   $
	LASNUM:0L,        ISODIS:0L,            NKMES:0L,$
	JC:0L,            ICHECK:0L,            CHOP_STATUS:lonarr(N_CHOP),$
	NFUNC:0L,         NKEY:0L,              RST_COMMI:str80,$
	N_RST_COMMAND:0L, NPMES:0L,             NREPET:0L,$
	NPREP:0L,         NSERIE:0L,            num_temp:0L,$
	numor_to_send:0L, lastsend:0L,          xc:0L,$
	BEAM_CHANGE:0L,   BEAM_PHASE:0L,        LAST_SEL_DAY:0L,$
	LAST_SEL_SEC:0L,  NB_SPECT:0L,          par_tof:lonarr(7),$
	init_tof_done:0L, first_update_data:0L, DAC_LAST_CON:lonarr(2),$
	free1:lonarr(97), check1:0L,$
	TEMPEA:fltarr(4), free2:fltarr(100),    check2:0.  }
	
IF Mach eq 'in6' THEN $
t_RES   ={tt_res,     $
	LASNUM:0L,ISODIS:0L,NKMES:0L,JC:0L,ICHECK:0L,CHOP_STATUS:lonarr(N_CHOP),Bidonnerie:0L,$ ;<----------- Bidonnerie
        NFUNC:0L,NKEY:0L,RST_COMMI:str80,NPMES:0L,N_TOTAL_SPECT:0L,N_MON_SPECT:0L, $
        N_SINGLE_SPECT:0L,LOG_FILE:0L,IXBFIL:0L,NXFILE:0L,NREPET:0L,NPREP:0L, $
        num_temp:0L,numor_to_send:0L,lastsend:0L,xc:0L,xc_base:0L,xc_cumul:0L,$
        rota_preset:0L,cpt_time:0L,cpt_monitor:0L,cpt_c2:0L,cpt_c3:0L,update_data:0L, $
        free1:lonarr(90), check1:0L,$
	TEMPEA:fltarr(4), TEMPE:0.0, XC:0.0, SPEED_REQ:fltarr(N_CHOP),$
	PHASE_REQ:fltarr(N_CHOP), SPEED_ACT:fltarr(N_CHOP), PHASE_ACT:fltarr(N_CHOP),$
	TREQ:0.0, free2:fltarr(95), check2:0.0}
	
IF Mach eq 'd17' THEN $
t_res={tt_res, LASNUM:0L,ISODIS:0L,NKMES:0L,JC:0L,ICHECK:0L,$ 
	free11:0L, NFUNC:0L,$ 
	NKEY:0L, RST_COMMI: str80 ,N_RST_COMMAND:0L, NPMES:0L,NREPET:0L,$ 
	NPREP:0L,NSERIE:0L,NUM_TEMP:0L,NUMOR_TO_SEND:0L, lastsend:0L,$ 
	XC:0L,BEAM_CHANGE:0L, BEAM_PHASE:0L,LAST_SEL_DAY:0L,LAST_SEL_SEC:0L,$ 
	NB_SPECT:0L,par_tof:lonarr(9),init_tof_done:0L,FIRST_UPDATE_DATA:0L,$ 
	free1:lonarr(96), check1:0L, TEMPEA:lonarr(4),CONBOB:lonarr(5),$
        free2:fltarr(101), check2:0.}
	
IF Mach eq 'd20' THEN $
t_res={tt_res,$
	lasnum:0L,       nscan:0L,            iscan:0L,            ntgv:0L,        $
	isodis:0L,       nk_scan:0L,          np_scan:0L,          numdun:0L,       $
	ipsi:0L,         jcacq:0L,            jcscan:0L,           jc:0L,           $
	k_func:0L,       nb_ang:0L,           icoupled:0L,         nbg:0L,          $
	iftest:0L,       nhamil:0L,           rst_comm:str20,      n_rst_comm:0L,     $
	n_in_scan:0L,    iang_scan:lonarr(4), ndata_to_save:0L,    idata_to_save:lonarr(7),$
	nfile:0L,        ixbfil:0L,           nxfile:0L,           icheck:0L,$
	nkmes:0L,        LOG_FILE:0L,         nfunc:0L,            moni:0L, $
	acqsum:0L,       ss_counts:lonarr(4), nmotor:0L,           lastsend:0L,$
	lastsendidefix:0L, nbpt:0L,           nbpt2:0L,            nkpt:0L,$
	nkpt2:0L,        freen:0L,            nrin_scan:0L,        ncin_scan:0L,$
	moni2:0L,        time_co_unit:0L,     free1:lonarr(66),    check1:0L,      $
	h:fltarr(3),           angle:fltarr(4),  factor:0.,  dx:0.,           $
	psi:0.,                volte:0.,         tempe:0.,   xc:0.,       $
	dscan:0.,              fscan:0.,         step:0.,    dscan2:0.,       $
	fscan2:0.,             step2:0.,         xcacq:0.,   xcscan:0.,$
	ang_in_scan:fltarr(4), qshmin:0.,        qskmin:0.,  qslmin:0.,$
	qshmax:0.,             qskmax:0.,        qslmax:0.,  champ:0.,$
	free2 :fltarr(95),     check2:0L}


;****************************************  T_NOTHER  **************************************************
;****************************************  T_NOTHER  **************************************************
IF Mach eq 'd7' THEN   $
t_NOTHER = {tt_nother, $
         PCODE:0L, PSTATE:0L, CSTATE:0L, XBU_STATE:0L, MAD_TERM:str8,$
         NWHICHR:0L, NFUNC:0L, NKEY:0L, NKEY1:0L, IREMOT:0L, IXBFIL:0L,$
         CHOP_STOP_NUMBER:lonarr(N_CHOP), CHOP_STOP_TIME:lonarr(N_CHOP),$
         SUM_DET:lonarr(N_CHOP), SUM_MON:lonarr(2), SUM_TOTALE:0L, NUM_DURATION:0L, $
         IPCP1:0L, IPCP2:0L, IPCP3:0L, RESET:0L, FLAG:lonarr(8), TEMPEA:0.,$
         SUCCES:0L, INST_NAME:str8, INST_LEN:0L, actang:fltarr(16), actbob:fltarr(8),$
         count_flag:0L, vme_option:0L, xbu_inter:0L, MAD_WARNING:0L,$
         tab_fils:lonarr(20), nb_fils:0L, NSWITCH:0L,  $
         mon1:lonarr(6), mon2:lonarr(6), duree_l:lonarr(6), $
         hytec_sum:lonarr(6),pmc_sum:lonarr(6), $
         free3:lonarr(58), check:0L }

IF Mach eq 'd22' THEN  $
t_NOTHER = {tt_nother, $
	PCODE:0L,            PSTATE:0L,         CSTATE:0L,$
	XBU_STATE:0L,        NWHICHR:0L,        NFUNC:0L,$
	NKEY:0L,             NKEY1:0L,          CHOP_STOP_NUMBER:lonarr(N_CHOP),$
	CHOP_STOP_TIME:lonarr(N_CHOP), NUM_DURATION:0L, PRESET_REST:0L,$
	IPCP1:0L,            IPCP2:0L,          pcp_kill:0L,$
	pcp_pause:0L,        pcp_continue:0L,   SEL_UNIT_ACT:0L,$
	sel_state:0L,        selector_speed:0L, inst_state:0L,$
	att_state:0L,        ACT_SAM_DIAPH:0L,  det_cps:0L,$
	mon_cps:0L,          sample_no:0L,      det_sum:0L,$
	direct_beam:0L,      count_flag:0L,     motors_succes:0L,$
	vme_option:0L,       SUCCES:0L,         xbu_inter:0L,$
	tab_fils:lonarr(20), int_nb_fils:0L,    NSWITCH:0L,$
	time_update_gui:0L,  chek01:0L,         chek02:0L,$
	NUM_TIME:0.,         actang:fltarr(16), ACT_SEL_SPEED:0.,$
	ACT_WAVE:0.,         act_ieee:fltarr(2),act_adc:fltarr(2),$
	ACT_COLL:0.,         coll_state:0.,     TEMPEA:fltarr(4),$
	ACT_POWER:0.,        start_time:str18,  al1:str2,$
	stop_time:str18,     al12:str2,         start_num:str8, $
	cstate_flags:0L,     delta:0.,          delta1:0.,$
	CONBOB:fltarr(4),    ACTBOB:fltarr(4),  auto_free:0L,$
	act_ieee_deb:fltarr(3), act_ieee3:0.,   motor_flags:0L,$
	colf:0L, colb:0L,    free3:lonarr(78),  check:0L }

IF Mach eq 'in6' THEN  $
t_NOTHER= { tt_nother, $
	 PCODE:0L, PSTATE:0L, CSTATE:0L, XBU_STATE:0L, $
	 NWHICHR:0L, NFUNC:0L, NKEY:0L, NKEY1:0L, IREMOT:0L, IXBFIL:0L,$
	 CHOP_STOP_NUMBER:lonarr(N_CHOP), CHOP_STOP_TIME:lonarr(N_CHOP),$
         SUM_DET:lonarr(3), SUM_MON:lonarr(3), SUM_TOTALE:0L,NUM_TIME:0L, NUM_DURATION:0L, $
	 IPCP1:0L, IPCP2:0L, IPCP3:0L, RESET:0L, $
	 SUCCES:0L, INST_NAME:str8, INST_LEN:0L, $
	 count_flag:0L, vme_option:0L, xbu_inter:0L, $
         tab_fils:lonarr(20), nb_fils:0L, file_flun1:0L,NSWITCH:0L, $
         free3:lonarr(107), check:0L }

IF Mach eq 'd17' THEN  $
t_NOTHER= {tt_nother, PCODE:0L, PSTATE:0L, CSTATE:0L, XBU_STATE:0L, NWHICHR:0L,$
	NNFUNC:0L, NNKEY:0L, NNKEY1:0L, free11:0L , $
	free12:0L, NUM_DURATION:0L,$	
	PRESET_REST:0L, IPCP1:0L,IPCP2:0L, SEL_UNIT_ACT:0L, sel_state:0L,$
	selector_speed:0L, inst_state:0L, att_state:lonarr(3), free_det_cps:0L, $
	free_mon_cps:0L, sample_no:0L, free_det_sum:0L, free_mon_sum:0L,$
	count_flag:0L, $
	motors_succes:0L,vme_option:0L, SUCCES:0L, XBU_INTER:0L, $
	tab_fils:lonarr(20), nb_fils:0L, NSWITCH:0L, time_update_gui:0L,$ 
	chek01:0L,chek02:0L, tt1:0L,tt2:0L, NUM_TIME:0.,$
	det_cps:0.,mon_cps:0.,det_sum:0.,mon_sum:0.,$ 
	free_actang:fltarr(12),$
	ACT_SEL_SPEED:0.,ACT_WAVE:0.,act_ieee:fltarr(2),act_adc:fltarr(2),$
	ACT_COLL:0., coll_state:0.,NTEMPEA:fltarr(4),$
	start_time:str18, al1:0L,$
	stop_time:str18, al2:0L, start_num:str8, $
	cstate_flags:0L, motor_flags:0L,delta:0.,$
	delta1:0.,ss_count:lonarr(4),total:0L,ss_rates:lonarr(4),$
	npoll_rate:0L,jcode:0L,icons:0L,$
	colf:0L,colb  :0L,pcp_kill :0L,pcp_pause :0L,pcp_continue :0L,$
	actual_spectrum_length :0L, total_spectrum_length :0L,$
	actang:fltarr(40), actdiaphcom:fltarr(10), actbob:fltarr(5),$
        free3:lonarr(10), check :0L}

IF Mach eq 'd20' THEN  $
t_NOTHER= {tt_nother,$
	pcode:0L,        pstate:0L,      cstate:0L,          mad_term:str8,$
	nwhich:0L,       nother:0L,      nkey:0L,            nkey1:0L,  $
	iremot:0L,       free:lonarr(2), iercde:0L,          iersum:0L,    $
	iercam:0L,       nbrec:0L,       nbword:0L,          ipcp1:0L,    $
	ipcp2:0L,        reset:0L,       nswitch:0L,         succes:0L,    $
	inst_name:str8,  inst_len:0L,    libre_34:0L,        num_disp:0L,   $
	last_sent:0L,    nactive:0L,     visu:0L,            ydyvis:0.,$
	last_count:0L,   ipost_scan:0L,  vme_option:0L,      actang:fltarr(16),   $
	xbu_state:0L,    xbu_inter:0L,   count_flag:0L,      cur_phase:0L,$
	Palpha1:0.,      Palpha2:0.,     tab_fils:lonarr(20), nb_fils:0L,   $
	colf:0L,         colb:0L,        pcp_kill:0L,         pcp_pause:0L,$
	pcp_continue:0L, cstate_flags:0L,motor_flags:0L,     icons:0L,$
	jcode:0L,        ss_rates:lonarr(4), flun1:0L,       free3:lonarr(61),$
	check:0L} 


;****************************************  T_COUNTS  **************************************************
;****************************************  T_COUNTS  **************************************************
IF Mach eq 'd7'  THEN  $
t_COUNTS = {tt_counts, $
          mon1:fltarr(500), mon2:fltarr(500), dect:fltarr(32000), $
          som_dect:fltarr(500), ANG:fltarr(500) }
	  
IF Mach eq 'd17' THEN  $
t_COUNTS = {tt_counts, $
          mon1:fltarr(100), mon2:fltarr(100), dect:fltarr(100), $ 
          som_dect:fltarr(100), ANG:fltarr(100) }
	  
IF Mach eq 'in6' THEN  $
t_COUNTS = {tt_counts, $
          mon1:fltarr(100), mon2:fltarr(100), dect:fltarr(100), $ 
          som_dect:fltarr(100), ANG:fltarr(100) }
	  
IF Mach eq 'd20' THEN  $
t_COUNTS={tt_COUNTS,$
	start_time:str18,$
        flag:0L,$
        stop_time:str18,$
        data:lonarr(417792),$
        ss_counts:lonarr(4),$
        cou:fltarr(2000),$
        ang:fltarr(1996)}

t_XBU    = {tt_xbu, NAME:str8, NCH:0L, LOOPS:0L, NLOOP:0L, NLINE:0L, LINES:0L, Ncheck:0L} 

t_ERREUR = {tt_erreur,degre:0L, txt:str80}

t_ERREURS= {tt_erreurs, nbr_err:0, err:replicate({tt_erreur},20)}

t_PILE   = {tt_pile, old_cmd:replicate(string(replicate(32b,8)), 100),ind_cmd:0L}

t_MOTORS= {tt_motors,motnam_ch:bytarr(8),COEF_VME:0.,UNIT_VME:0L,$
	MKT_VME:0L,PREC:0.,unit:bytarr(4),enab:0L,$
	errsum:0L,name2:bytarr(2),type:0L, param:lonarr(40),retries:0L,$
	but_min:0.,but_max:0.,off_small:0.,off_large:0.,bckl:0.,start:0.,$
	actual:0.,req:0.,action:0L,error:0L,adresse_diaph:0L}
	

;****************************************  T_PARA  ****************************************************
;****************************************  T_PARA  ****************************************************
IF Mach eq 'd7' THEN $
t_PARA   = {tt_para, $
         TST_BEAM :0L, JCODE :0L,    ISOR     :0L, ISWITCH :0L, $
         ISORT :0L, TOF_CHA_RESOL:0L, TOF_CHA_WIDTH:0L, TOF_DIVID:0L,$
         NUM_CHAN_USED:0L, COMPUTE_PAR :0L, MIN_RATE :0L, MAX_RATE:0L,$
         filer :0L, UPDATE_TIMER:0L, STATE_TIMER :0L, OUTPUT_TIMER:0L,$
         ALARM_TIMER:0L, CHOP_OPTION:0L, ALIGN:0L, SCAN_UNIT:0L,$
         ICRY:0L, IRVOLT:0L, IVOLT:0L, ITVOLT:0L, IREGUL:0L,$
         SLOT_CHOP:lonarr(N_CHOP), PICK_CHOP:lonarr(N_CHOP),$
         TSWITCH_TERM:str8, long_TSWITCH_TERM:0L,$
         MAD_REMOTE_TERM:str8, long_MAD_REMOTE_TERM:0L,$
         C_TXT:str80, long_TXT:0L, C_USER:str8, long_C_USER:0L,$
         C_LC:str8, long_C_LC:0L, SUB_TITLE:str80,  long_PROPAL:0L,$
         BOOL_TOFF:0L, PRESET_BASE:0L, tgv:0L,   ask_tgv:0L,$
         free1:lonarr(87), check1: 0L, $
         WAVE:0., ENERGY:0., SPE_CHOPPER:fltarr(N_CHOP), PERIOD_CHOP:0.,$
         CHA_WIDTH:0., TOF_DELAY:0., CONS:0., SCAN_VALDEP:0.,$
         SCAN_VALFIN:0., SCAN_STEP:0.,$
         ZCURRENT:fltarr(5), XCURRENT:fltarr(5), YCURRENT:fltarr(5),$
         EMIN:0., EMAX:0., EL_PEAK:0.,$
         BANK1:fltarr(3),BANK2:fltarr(3),BANK3:fltarr(3),BANK4:fltarr(3),$
         THE_BOB_DEP:0.,THE_BOB_LAST:0., THE_BOB2_DEP:0.,THE_BOB2_LAST:0.,$
         PRESET_COEF_XYZ:0., PRESET_COEF_Z:0., ZPO_FLI:0.,ZPO_COR:0.,$
         THE_BOB_DEPX:0., THE_BOB_LASTX:0., THE_BOB2_DEPX:0., THE_BOB2_LASTX:0.,$
         THE_BOB_DEPY:0., THE_BOB_LASTY:0., THE_BOB2_DEPY:0., THE_BOB2_LASTY:0.,$
         free3:fltarr(50),   $
         check2:0.     }

IF Mach eq 'd22' THEN $
t_PARA  = { tt_para,  $
	TST_BEAM:0L,       TOF_CHA_RESOL:0L,    TOF_DIVID:0L,$
	COMPUTE_PAR:0L,    MIN_RATE:0L,         MAX_RATE:0L, $
	UPDATE_TIMER:0L,   STATE_TIMER:0L,      OUTPUT_TIMER:0L,$
	ALARM_TIMER:0L,    ISWITCH:0L,          ALIGN:0L,$
	SCAN_UNIT:0L,      use_cry:0L,          use_volt:lonarr(2), IRVOLT:lonarr(2),$
	ITVOLT:lonarr(2),  IREGUL:0L,           DET_MASK:lonarr(1), $
	MD_NSPECT:0L,      MD_RADIUS:0L,        MODE:0L,$
	grouping_mode:0L,  X1_MD:0L,            X2_MD:0L,$
	Y1_MD:0L,          Y2_MD:0L,            cal_option:0L,$
	sel_unit:0L,       sel_const:lonarr(3), attenu_cur:0L,$
	beam_stop_unit:0L, changer_cur:0L,      sample_min:0L,$
	sample_max:0L,       attenu_pos:0L,     selector_speed_cor:0L,$
	scan_area:lonarr(4), use_cha:0L,        use_she:0L,$
	use_ste:0L,        use_mag:0L,          use_cho:0L,$
	use_pre:0L,        use_spe:0L,          use_adc:lonarr(2),$
	ENVIRONMENT:0L,    MD_MASK_FILE:str20,  C_TXT:str39,$
	long_TXT:0L,       SUB_TITLE:str20,     long_SUB_TITLE:0L,$
	C_USER:str10,      long_C_USER:0L,      PROPOSAL_NUMBER:str8,$
	long_PROPOSAL_NUMBER:0L, LOG_BOOK_N:str3, long_LOG_BOOK_N:0L,$
	EXP_START_TIME:str11,    long_EXP_START_TIME:0L, ieee_cmds:str80,$
	speed_nb:0L,       speed_l:lonarr(10),  speed_h:lonarr(10),$
	API_OPTION:0L,     use_volt3:0L,        IRVOLT3:0L,$
	ITVOLT3:0L,        ieee3_cmds:str40,    rs232_speed:0L,$
	rs232_c_end:0L,    rs232_type:0L,       tof:0L,$
	total_spec:0L,     industri_option:0L,  pardesc:0L,$
	free1:lonarr(59),  check1:0L,$
	WAVE_TOL:0.,       wave_offset:fltarr(3), CHA_WIDTH:0.,$
	TOF_DELAY:0.,      CONS:0.,             SCAN_VALDEP:0.,$
	SCAN_VALFIN:0.,    SCAN_STEP:0.,        DIS_S_D:0.,$
	SDI1:0.,           SDI2:0.,             SDI3:0.,$
	IDI:0.,            CD11:0.,             CD12:0.,$
	SAL:0.,            SAR:0.,              SALOW:0.,$
	SAUP:0.,           X0:0.,               Y0:0.,$
	BEAM_X:0.,         BEAM_Y:0.,           DET_DIST:0.,$
	DET_ANG:0.,        filer_ang:fltarr(12),CHANGER_ONE:fltarr(NB_CHAG),$
	CHANGER_INC:fltarr(NB_CHAG), CHANGER_NB:fltarr(NB_CHAG),MD_X0:0., $
	MD_Y0:0.,          amp_limit:0.,        volt_limit:0.,$
	resolution:0.,     bath_offset:0.,      bath_slope:0.,$
	free2:fltarr(97),  check2:0.  }

IF Mach eq 'in6' THEN $
t_PARA    ={ tt_para, $
          TST_BEAM:0L, JCODE:0L, TOF_CHA_RESOL:0L, TOF_CHA_WIDTH:0L, $
          TOF_DIVID:0L,DET_MASK:lonarr(8),MON_MASK:lonarr(8),NUM_CHAN_USED:0L, $
          COMPUTE_PAR:0L,MIN_RATE:0L,MAX_RATE:0L,UPDATE_TIMER:0L, $
          STATE_TIMER:0L,OUTPUT_TIMER:0L,ALARM_TIMER:0L,CHOP_OPTION:0L,$
          ENVIRONMENT:0L,ICRY:0L,IRVOLT:0L,IVOLT:0L,ITVOLT:0L,IREGUL:0L,$
          ISWITCH:0L,mad_remote_term:str8, C_TXT:str80, long_TXT:0L, $
          C_USER:str8, long_C_USER:0L,$
          C_LC:str8, long_C_LC:0L, SUB_TITLE:str80,  long_PROPAL:0L,$
          ELPEAK:0L, free1:lonarr(69), check1: 0L, $
	  WAVE:0.,RAT_CHOPPER:0., SPE_CHOPPER:fltarr(N_CHOP), $
          DEPH_CHOPPER:fltarr(N_CHOP),DEPH_OFFSET:0.,PERIOD:0., $
          CHA_WIDTH:0.,TOF_DELAY:0.,CONS:0.,res_chopper:0.,ei_chopper:0., $
          qmin_chopper:0.,qmax_chopper:0.,emin_chopper:0.,emax_chopper:0., $
          DISTANCE:fltarr(6),DIS_S_M:0.,SAMPLE_ANG:0., $
          free2:fltarr(89),    check2:0.    }

IF Mach eq 'd17' THEN $
t_para={tt_para,TST_BEAM:0L ,TOF_CHA_RESOL:0L,TOF_DIVID:0L,$
	COMPUTE_PAR:0L, MIN_RATE:0L,MAX_RATE:0L,UPDATE_TIMER:0L,$
	STATE_TIMER:0L,OUTPUT_TIMER:0L,ALARM_TIMER:0L,ISWITCH:0L,$
	ALIGN:0L,SCAN_UNIT:0L,ICRY:0L,IVOLT:lonarr(2),IRVOLT:lonarr(2),$
	ITVOLT:lonarr(2),IREGUL:0L,DET_MASK:lonarr(1),MD_NSPECT:0L,$
	MD_RADIUS:0L,MODE:0L,grouping_mode:0L,X1_MD:0L,X2_MD:0L,$
	Y1_MD:0L,Y2_MD:0L,cal_option:0L,sel_unit:0L,sel_const:lonarr(3),$
	attenu_cur:0L,beam_stop_unit:0L,changer_cur:0L,sample_min:0L,$
	sample_max:0L,attenu_pos:0L,selector_speed_cor:0L,$
	scan_area:lonarr(4),use_cha:0L,use_she:0L,use_ste:0L, use_mag:0L,$
	use_cho:0L,use_pre:0L,use_spe:0L,use_adc:lonarr(2),$
	ENVIRONMENT:0L,MD_MASK_FILE:bytarr(20),C_TXT:bytarr(39),$
	long_TXT:0L,SUB_TITLE:bytarr(20),long_SUB_TITLE:0L,$
	C_USER:bytarr(10) ,long_C_USER:0L,PROPOSAL_NUMBER:bytarr(8),$
	long_PROPOSAL_NUMBER:0L,LOG_BOOK_N:bytarr(3),long_LOG_BOOK_N:0L,$
	EXP_START_TIME:bytarr(11),long_EXP_START_TIME:0L,$
	ieee_cmds:bytarr(40,2),$
	speed_nb:0L,speed_l:lonarr(10), speed_h:lonarr(10),$
	nx_tof:0L, ny_tof:0L, pend_preset:0L,pend_preset_type:0L,$
	free1:lonarr(75), check1:0L ,WAVE_TOL:0. ,$
	wave_offset:fltarr(3),CHA_WIDTH:0.,TOF_DELAY:0., CONS:0.,$
	SCAN_VALDEP:0.,SCAN_VALFIN:0. ,SCAN_STEP:0. ,$
	DIS_S_D:0. ,SDI1:0. ,SDI2:0. ,SDI3:0. ,IDI:0., CD11:0.,$
	CD12:0.,CD21:0. ,CD22:0.,CD31:0.,CD32:0.,X0:0. ,Y0:0.,$
	BEAM_X:0.,BEAM_Y:0. ,DET_DIST:0.,DET_ANG:0.,filer_ang:fltarr(12) ,$
	CHANGER_ONE:fltarr(NB_CHAG),CHANGER_INC:fltarr(NB_CHAG),$
	CHANGER_NB:fltarr(NB_CHAG),MD_X0:0. ,MD_Y0:0. ,amp_limit:0. ,$
	volt_limit:0.,$
	off_diah_comb:fltarr(10),pend_down:0. , pend_up:0. ,$
	free2:fltarr(88),check2:0.}

IF Mach eq 'd20' THEN $
t_para={tt_para,$
	lcode:0L ,           j_code:0L ,       ido_tgv:0L,       ifit:0L,      $
	isor:0L,             ivisu:0L,         irescl:0L,        isorl:0L, $
	ih:0L,               ik:0L,            iomega:0L,        isorte:0L,       $
	npsca:0L,            nkscan:0L,        idelta:0L,        nbas:0L,    $
	mex:lonarr(9),       nkb:0L,           ihamil:0L,        iquad:0L,    $
	ipara:0L,            iblin1:0L,        iblin2:0L,        ichi_zero_top:0L,$
	ispin:0L,            iref:0L,          ntest:0L,         nbtest:0L,$
	nspeci:0L,           iglob:0L,         itilt:0L,         icry:0L,      $
	ianal:0L,            isn:0L,           ichi:0L,          icyce:0L,   $
	iscce:0L,            ilicen:0L,        imort:0L,         ido_volt:0L,    $
	 nkcefi:0L,           imode:0L,         flgkif:0L,        icyfit:0L,   $
	ifacto:0L,           ivisch:0L,        ivisty:0L,        ivistt:0L,       $
	ilasti:0L,           ichadi:0L,        ifaisc:0L,        ihard:0L,       $
	icelim:lonarr(3),    icycle:0L,        isa:0L,           iswitch:0L,    $
	isort:0L,            stipos:0L,        swa3:0L,          ix:0L,  $
	iecr:0L,             iregd:0L,         naxe:0L,          nkcen:0L,       $
	swpol:0L,            iber:0L,          ido_dti:0L,       icorec:0L,       $
	mad_rem_iclt:bytarr(8), par_term:bytarr(8), ibob:lonarr(10), c_txt:bytarr(80),  $
	k1:0L,               c_user:bytarr(8),   k2:0L,          c_lc:bytarr(8),      $
	k3:0L,               sub_title:bytarr(80), k4:0L,        proposal:bytarr(80), $
	iwp:0L,              itvolt:0L,        irvolt:0L,        imub:0L,         $
	fixpl:0L,            ivariz:0L,        mult_sing:0L,     nb_det:0L,       $
	nblocks_sdata:0L,    mboxx:0L,         mboxy:0L,         scan_unit:0L,$
	mot_unit:0L,         strombo_mode:0L,  nb_cycles:0L,     nb_slices:0L,    $
	type_mes:0L,         tranche:lonarr(768), unit_slice:lonarr(256), slice_t:lonarr(7,4),$
	check_slice:0L,      rtrpower:0L,      D19:0L,           OS:0L,$
	SeqType:0L,          check12:0L,       tempphasei:fltarr(4), p_phase1:fltarr(3),  $
	p_phase2:fltarr(3),  p_phase3:fltarr(3), ido_mag:0L,     free1:lonarr(31), $
	check1:0L,$
	wav:0.,       rhmin:0.,          rhmax:0.,          rkmin:0.,       $
	rkmax:0.,     rlmin:0.,          rlmax:0.,          psimin:0.,          $
	psimax:0.,    paspsi:0.,         thmin:0.,          thmax:0.,     $
	ub:fltarr(9), valmin:0.,         tscan:0.,          deltav:0.,        $
	factor:0.,    tbas:fltarr(5),    dbas:fltarr(5),    apha:0.,          $
	vimoy:0.,     tfit:0.,           cell:fltarr(6),    grandd:0.,       $
	petitd:0.,    thetac:fltarr(3),  domegc:fltarr(3),  ratio:0.,       $
	tctest:0.,    ameil:0.,          coumin:0.,         tenmax:0.,        $
	counor:0.,    part:fltarr(3),    vmort:0.,          vmaxi:0.,        $
	vtest:0.,     atemp:0.,          btemp:0.,          ymin:0.,          $
	ymax:0.,      vnorm:0.,          a4:fltarr(4),      shchi:0.,         $
	themoy:0.,    chref:fltarr(12),  ptest:0.,          pasbg:0.,       $
	consbg:0.,    cons:0.,           temmon:0.,         temreg:0.,   $
	errreg:0.,    cmvohm:0.,         aplan:fltarr(6),   a1:fltarr(2),       $
	st_alim_m:0., st_alim_a:0.,      dm:0.,             da:0.,       $
	pash:0.,      pask:0.,           pasl:0.,           eregsy:0.,       $
	cimini:0.,    thetax:fltarr(5),  xx:fltarr(5),      a2:fltarr(5),       $
	a3:fltarr(5), vmavis:0.,         fcar:fltarr(4),    scar:fltarr(4),  $
	mcu:fltarr(5),mpg:fltarr(5),     ominli:0.,         omaxli:0.,       $
	bmin:0.,      bmax:0.,           aglob:0.,          bglob:0.,   $
	cglob:0.,     dglob:0.,          pfstep:0.,         pfmon:0.,       $
	qh:0.,        qk:0.,             ql:0.,             dqh:0., $
	dqk:0.,       dql:0.,            en:0.,             den:0.,     $
	valkif:0.,    omzert:fltarr(5),  omzerv:fltarr(5),  xdist:0.,       $
	dstab1:0.,    dstab2:0.,         conf:fltarr(25),   det:fltarr(15),$
	free2:fltarr(60),  check2:0. }


;****************************************  T_STATUS  **************************************************
;****************************************  T_STATUS  **************************************************
IF Mach eq 'd7' THEN  $
t_STATUS= {tt_status, $
        PCODE:0L, PSTATE:0L, CSTATE:0L, count_flag:0L, JC:0L, SCAN_UNIT:0L,  $
        time_counts:0L, moni1_counts:0L,moni2_counts:0L , $
        sum_pmc:0L, sum_hytec:0L, $ 
        mon1:lonarr(6), mon2:lonarr(6), duree_l:lonarr(6), $
        hytec_sum:lonarr(6),pmc_sum:lonarr(6), $
        free2:lonarr(18), check1:0L}

IF Mach eq 'd17' THEN $
t_STATUS= {tt_status, $
	PCODE:0L, PSTATE:0L, CSTATE:0L,$
	JC:0L, det_cps:0L, mon_cps:0L,$
	det_sum:0L, ss_counts:lonarr(2), count_flag:0L,$
	cstate_flags:0L, motor_flags:0L, free2:lonarr(18),$
	check1:0L }

IF Mach eq 'd22' THEN $
t_STATUS= {tt_status, $
	PCODE:0L, PSTATE:0L, CSTATE:0L,$
	JC:0L, det_cps:0L, mon_cps:0L,$
	det_sum:0L, ss_counts:lonarr(2), count_flag:0L,$
	cstate_flags:0L, motor_flags:0L, free2:lonarr(18),$
	check1:0L }
	
IF Mach eq 'in6' THEN $
t_STATUS= {tt_status, $
	PCODE:0L, PSTATE:0L, CSTATE:0L, count_flag:0L, JC:0L,  $
	time_counts:0L, vsum:fltarr(8),$
	free2:lonarr(18), check1:0L}
	
IF Mach eq 'd20' THEN $
t_STATUS= {tt_status, $
	PCODE:0L, PSTATE:0L, CSTATE:0L,$
	JC:0L, det_cps:0L, mon_cps:0L,$
	det_sum:0L, ss_counts:lonarr(2), count_flag:0L,$
	cstate_flags:0L, motor_flags:0L, free2:lonarr(18),$
	check1:0L }
	
IF Mach eq 'd17' THEN $
t_CHOPPER={tt_chopper,$ 
	frequence:0L, moy_phase:0L, phi:0L, phi_long:0L, dphase:0L,$ 
	oscillation:0L, cadran:0L, regulation:0L, couple:0L, nb_poles:0L,$ 
	gene_type:0L, nb_pickup:0L, sign_pickup:0L, clock_mode:0L, lost_phase_low:0L,$
	lost_phase_high:0L, dt_pickup_counter:0L, dt_counter:0L, phase_hist:lonarr(101)}

end
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
function GET_MAD_STRUC, pth, txt, struc
;******* *************
;**
if (n_elements(struc) eq 0) or (pth eq '') then return,3.14
return, CALL_EXTERNAL (pth, txt, struc) 
end

FUNCTION DIAL_MAD_READ, DialType, TimeSeq, aString
;******* *************
;**
;**
common geoMad_rface, geoMadSeq, geoMadPth, geoMadLog, geoMadPtr
common madshare
V='3.14'
S= 0.
if n_elements(TimeSeq) ne 1 then begin TimeSeq=systime(1) & aString='' & endif
;**********
;First call
;**********
if n_elements(geoMadPth) eq 0 then begin MAKE_MAD_STRUC & geoMadPth='/users/mad/IDL_MAD_SERVER/ex1.so'
					 bid=FINDFILE(geoMadPth,count=nn) & if nn lt 1 then geoMadPth =''
					 geoMadLog=mad_home_dir+'LOG_FILE/MAD.LOG'
					 bid=FINDFILE(geoMadLog,count=nn)
					 if nn lt 1 then begin	geoMadLog=mad_home_dir+'mad.log'
					 			bid=FINDFILE(geoMadLog,count=nn) & endif
					 if nn lt 1 then geoMadLog =''
					 geoMadPtr=-1
					 geoMadSeq={hytec:   -1D ,$
					            data:    -1D ,$
					            limits:  -1D ,$
					            status:  -1D ,$	;**** Structure T_STATUS ****
					            para:    -1D ,$	;**** Structure T_PARA   ****
					            nother:  -1D ,$	;**** Structure T_NOTHER ****
					            res:     -1D ,$	;**** Structure T_RES    ****
					            counts:  -1D ,$	;**** Structure T_COUNTS ****
					            motors:  -1D ,$	;**** Structure T_MOTORS ****
					            chopper1:-1D ,$	;**** Structure T_CHOPPER****
					            chopper2:-1D ,$	;**** Structure T_CHOPPER****
					            log:     -1D }
						    
endif


idx=where(geoMadStr(0,*) eq DialType) & idx=idx(0)
IF  idx ge 0 THEN BEGIN

;********************************
;Time to update our structures ??
;********************************
   Mins=FLOAT(geoMadStr(2,idx))
   CASE       geoMadStr(1,idx) of
   
   't_nother':	if TimeSeq - geoMadSeq.nother	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_nother' , t_nother)
								   geoMadSeq.nother  =TimeSeq & endif
   't_res':	if TimeSeq - geoMadSeq.res	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_res'    , t_res)
								   geoMadSeq.res     =TimeSeq & endif
   't_para':	if TimeSeq - geoMadSeq.para	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_para'   , t_para)
								   geoMadSeq.para    =TimeSeq & endif
   't_status':	if TimeSeq - geoMadSeq.status	ge Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_status' , t_status)
								   geoMadSeq.status  =TimeSeq & endif
   't_chopper1':if TimeSeq - geoMadSeq.chopper1	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_chopper_status'  , t_chopper)
								   geoMadSeq.chopper1=TimeSeq & endif
   't_chopper2':if TimeSeq - geoMadSeq.chopper2	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_chopper2_status' , t_chopper)
								   geoMadSeq.chopper2=TimeSeq & endif
   'hytec':	if TimeSeq - geoMadSeq.hytec	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_data_hytec' , geoHytec)
								   geoMadSeq.hytec   =TimeSeq & endif
   'pmc':	if TimeSeq - geoMadSeq.hytec	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_data_pmc'   , geoData)
								   geoMadSeq.hytec   =TimeSeq & endif
   'data':	if TimeSeq - geoMadSeq.data	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_data'       , GeoData)
								   geoMadSeq.data    =TimeSeq & endif
   'limits':	if TimeSeq - geoMadSeq.limits	gt Mins	then begin S=GET_MAD_STRUC(geoMadPth,'get_mad_mot_limit'  , GeoLimits)
								   geoMadSeq.limits  =TimeSeq & endif
   'log':	if TimeSeq - geoMadSeq.log	gt Mins	then begin V=''
								   if aString gt ' ' then fil=aString else fil=geoMadLog
								   IF fil ne '' then begin
								     U=-1 & on_ioerror,misMO
								     openr,U,fil,/get_lun
								     st=fstat(U)
								     if geoMadPtr  ge 0 then begin Ptr=geoMadPtr>(st.size-5000)
									lin='' & point_lun,U,Ptr & readf,U,V
									while(1) do begin readf,U,lin & V=[V,lin] & endwhile
									endif
								     misMO: if U gt 0 then begin geoMadPtr=st.size
									                         free_lun,U  & endif
								   ENDIF
								   geoMadSeq.log    =TimeSeq
							endif else V=''
   ELSE:
   ENDCASE

;********************
;Give back the values
;********************
   IF S ne 3.14 then CASE DialType of

   'data':	return, GeoData
   'pmc':	return, GeoData
   'hytec':	return, geoHytec
   'limits':	return, geoLimits
   'log':
   'flagus':	if (t_status.PCODE eq 2 ) then V= t_status.cstate else V=-1
   'status':	if (t_status.PCODE eq 0 ) then V= 'MAD IS NOT RUNNING' else $
		if (t_status.PCODE eq 1 ) then V= 'MAD INIT PHASE'     else $
		if (t_status.PCODE eq 2 ) then begin
		 case t_status.cstate of 
		 0:   V= 'Idle'
		 1:   V= 'COUNTING'
		 2:   V= 'POSITIONNING'
		 3:   V= 'ENCODER READING'
		 4:   V= 'TEMPERATURE SETTING'
		 5:   V= 'WAITING'
		 6:   V= 'WAVELENGTH SETTING'
		 7:   V= 'CHOPPER SETTING'
		 8:   V= 'DATA UPDATING'
		 9:   V= 'COLLIMATION SETTING'
		 10:  V= 'ATTENUATOR SETTING'
		 11:  V= 'BEAM STOP SETTING'
		 else:V= 'UNDEFINED'
		 endcase
		endif  

   ELSE:	ii=EXECUTE('V='+geoMadStr(3,idx))
   ENDCASE
   IF S eq 3.14 then IF DialType eq 'status' then V= 'Idle' ;FOR TESTS
   
ENDIF
return, V
end
