;+
;	*********************
	    pro myinit_d20
;	*********************

; You can insert here the codes you want to execute just after LAMP is loaded.
; This special procedure is thought for D20 (DIF) mainly.
; Last modification:	Thomas C Hansen,	November 2000	(Documentation, MK_HTML_HELP)
;-
if !version.release ge '5.0' then ii=execute('FORWARD_FUNCTION RDID, sys_dep')
common calibration, pathcal, cal_d19 , cal_d2b , ang_d2b , cal_d1a , ang_d1a , cal_in13 $
		  	   , cal_d16 , cal_in5 , idx_in5 , shf_in5 , cal_in6 , idx_in6  $
			   , shf_in6 , cal_d20 , ang_d20 ,inf_d20 $
			   , inf_d2b , inf_d1a , inf_in13, inf_d16 , inf_in5 , inf_in6
common c_lamp,	lamp_b1,lamp_act,lamp_focus,lamp_mic,lamp_don,lamp_did,lamp_ben,lamp_wrd,$
        		lamp_data,lamp_host,lamp_dir,lamp_sys,lamp_exec,lamp_entry,lamp_devps,$
        		lamp_hlp,lamp_siz,lamp_loc ,lamp_cyc,lamp_man ,lamp_dvd,lamp_asite,lamp_fsite,$
        		lamp_ins,lamp_proc,lamp_grp,lamp_ali,lamp_path,lamp_touch,lamp_macro,$
        		lamp_ziz,lamp_6,lamp_proxy,lamp_wrti,lamp_wrtp
IF N_ELEMENTS(LAMP_DEVPS) GT 0 THEN BEGIN
  IF STRLEN(LAMP_DEVPS) LE 1 THEN LAMP_DEVPS='lj1_d20'
ENDIF ELSE LAMP_DEVPS='lj1_d20'
IF (sys_dep('MACHINE') eq 'unix') THEN SPAWN,'rm -r .HS*'
p_lambda,plambda
IF (sys_dep('MACHINE') eq 'unix') THEN SPAWN,'rm -r '+plambda+'.HS*'
RDSET,inst='D20',base='ON_Line'
username=sys_dep('GETENV','USER')
PRINT,'User : ',username
cd,current=a
PRINT,'Current Working Directory : ',a
IF STRMID(username(0),0,6) EQ 'hansen' THEN BEGIN
  MK_HTML_HELP,'/home/cs/lambda/macros/DIF','/home/cs/lambda/macros/DIF/macp.html' ,TITLE='LAMP macros for DIF - serhom.ill.fr:/home/cs/lambda/macros'            
ENDIF
;if (sys_dep('MACHINE') eq 'mac') THEN MK_HTML_HELP,'HansenG3:lambda:macros','HansenG3:lambda:macros:macp.html' ,TITLE='MACP external LAMP macros - porthansen.ill.fr/HansenG3:lambda:macros:'
print,' '
a=rdid(0)
a=0
flag;,/eff
;pathcal
PRINT,pathcal
print,' '
print,'Type "functions" or "procedures" for a list of functions/procedures'
print,'Type "idl" for some hints on IDL - the basic language of LAMP'
print,'Type "workspaces" for some informations about this basic LAMP concept'
print,'Type w6=rdrun(34444), w7=rdsum(2344,2346) or w8= rdand(56798,56890) to read data'
print,' '
;d20_widget
end
