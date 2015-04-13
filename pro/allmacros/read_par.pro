FUNCTION READ_PAR , inst ,path, filename, status, datp
;-----------------------------------------------------

CASE inst OF 

'demo'	: RETURN,read_tmp	(['demo',' ']	,path,filename,status,datp)
'IN4'	: RETURN,rdid		(['IN4','TOF']	,path,filename,status,datp)
'IN5'	: RETURN,rdid		(['IN5','TOF']	,path,filename,status,datp)
'IN6'	: RETURN,rdid		(['IN6','TOF']	,path,filename,status,datp)
'IN10'	: RETURN,rdid		(['IN10','TOF']	,path,filename,status,datp)
'IN16'	: RETURN,rdid		(['IN16','TOF']	,path,filename,status,datp)
'D7'	: RETURN,rdid		(['D7','TOF']	,path,filename,status,datp)
'D11'	: RETURN,rdid		(['D11','LSS']	,path,filename,status,datp)
'D17'	: RETURN,rdid		(['D17','LSS']	,path,filename,status,datp)
'D22'	: RETURN,rdid		(['D22','LSS']	,path,filename,status,datp)
'PN1'	: RETURN,ill		(['PN1','NFP']	,path,filename,status,datp)
'INX'	: RETURN,inx_in		(['INX','TOF']	,path,filename,status,datp)
'D1B'	: RETURN,rdid		(['D1B','DIF']	,path,filename,status,datp)
'IN8'	: RETURN,rdid		(['IN8','3Axes'],path,filename,status,datp)
'D19'	: RETURN,rdid		(['D19','DIF']	,path,filename,status,datp)
'D9'	: RETURN,rdid		(['D9','DIF']	,path,filename,status,datp)
'D1A'	: RETURN,rdid		(['D1A','DIF']	,path,filename,status,datp)
'D16'	: RETURN,rdid		(['D16','LSS']	,path,filename,status,datp)
'DB21'	: RETURN,rdid		(['DB21','LSS']	,path,filename,status,datp)
'IN1'	: RETURN,rdid		(['IN1','3Axes'],path,filename,status,datp)
'lamp'	: RETURN,rlamp		(['lamp',' ']	,path,filename,status,datp)
'IN14'	: RETURN,rdid		(['IN14','3Axes'],path,filename,status,datp)
'IN20'	: RETURN,rdid		(['IN20','3Axes'],path,filename,status,datp)
'D2B'	: RETURN,rdid		(['D2B','DIF']	,path,filename,status,datp)
'D20'	: RETURN,rdid		(['D20','DIF']	,path,filename,status,datp)
'IN13'	: RETURN,rdid		(['IN13','TOF']	,path,filename,status,datp)
'D11tof'	: RETURN,rdid		(['D11tof','LSS'],path,filename,status,datp)
'XRAY'	: RETURN,rdspe		(['XRAY','LON']	,path,filename,status,datp)
'inx.'	: RETURN,write_inx	(['inx.',' ']	,path,filename,status,datp)
't13a'	: RETURN,rdid		(['t13a','LON']	,path,filename,status,datp)
't13c'	: RETURN,rdid		(['t13c','LON']	,path,filename,status,datp)
'NeXus'	: RETURN,read_nexus	(['NeXus',' ']	,path,filename,status,datp)
'DCSasc'	: RETURN,read_dcsasc	(['DCSasc','NIST'],path,filename,status,datp)
'DCSbin'	: RETURN,read_dcsbin	(['DCSbin','NIST'],path,filename,status,datp)
'D10'	: RETURN,rdid		(['D10','DIF']	,path,filename,status,datp)
'D15'	: RETURN,rdid		(['D15','DIF']	,path,filename,status,datp)
'Spec'	: RETURN,read_spec	(['Spec',' ']	,path,filename,status,datp)
'EDF'	: RETURN,rdedf		(['EDF','ESRF']	,path,filename,status,datp)
'dat.'	: RETURN,export_dat	(['dat.','DIF']	,path,filename,status,datp)
'gsas.'	: RETURN,export_gsa	(['gsas.','DIF'],path,filename,status,datp)
'fpcyc.'	: RETURN,export_cyc	(['fpcyc.','DIF'],path,filename,status,datp)
'xy.'	: RETURN,export_xy	(['xy.','DIF']	,path,filename,status,datp)
'prn.'	: RETURN,export_prn	(['prn.','DIF']	,path,filename,status,datp)
'gsas'	: RETURN,rd_d2b		(['gsas','DIF']	,path,filename,status,datp)
'fil'	: RETURN,rdinstr		(['fil','DIF']	,path,filename,status,datp)
'FPdat'	: RETURN,rdinstr		(['FPdat','DIF'],path,filename,status,datp)
'd20cal'	: RETURN,rddat		(['d20cal','DIF'],path,filename,status,datp)
'prf'	: RETURN,rddat		(['prf','DIF']	,path,filename,status,datp)
'xyz'	: RETURN,rddat		(['xyz','DIF']	,path,filename,status,datp)
'FPcyc'	: RETURN,rddat		(['FPcyc','DIF'],path,filename,status,datp)
'MiBeMol'	: RETURN,read_mibemol	(['MiBeMol','LLB'],path,filename,status,datp)
'QENS'	: RETURN,read_qens	(['QENS','IPNS'],path,filename,status,datp)
'res'	: RETURN,read_res	(['res','DIF']	,path,filename,status,datp)
'cufe'	: RETURN,read_res	(['cufe','DIF']	,path,filename,status,datp)
'fat'	: RETURN,read_res	(['fat','DIF']	,path,filename,status,datp)
'eth.'	: RETURN,export_eth	(['eth.','DIF']	,path,filename,status,datp)
'test'	: RETURN,read_res	(['test','DIF']	,path,filename,status,datp)
'FAT1'	: RETURN,read_res	(['FAT1','DIF']	,path,filename,status,datp)
'init'	: BEGIN  Status=0

  ttinst   = ['demo']		;exec
  ttinst   = [ttinst,'IN4']	;exec
  ttinst   = [ttinst,'IN5']	;exec
  ttinst   = [ttinst,'IN6']	;exec
  ttinst   = [ttinst,'IN10']	;exec
  ttinst   = [ttinst,'IN16']	;exec
  ttinst   = [ttinst,'D7']	;exec
  ttinst   = [ttinst,'D11']	;exec
  ttinst   = [ttinst,'D17']	;exec
  ttinst   = [ttinst,'D22']	;exec
  ttinst   = [ttinst,'PN1']	;exec
  ttinst   = [ttinst,'INX']	;exec
  ttinst   = [ttinst,'D1B']	;exec
  ttinst   = [ttinst,'IN8']	;exec
  ttinst   = [ttinst,'D19']	;exec
  ttinst   = [ttinst,'D9']	;exec
  ttinst   = [ttinst,'D1A']	;exec
  ttinst   = [ttinst,'D16']	;exec
  ttinst   = [ttinst,'DB21']	;exec
  ttinst   = [ttinst,'IN1']	;exec
  ttinst   = [ttinst,'lamp']	;exec
  ttinst   = [ttinst,'IN14']	;exec
  ttinst   = [ttinst,'IN20']	;exec
  ttinst   = [ttinst,'D2B']	;exec
  ttinst   = [ttinst,'D20']	;exec
  ttinst   = [ttinst,'IN13']	;exec
  ttinst   = [ttinst,'D11tof']	;exec
  ttinst   = [ttinst,'XRAY']	;exec
  ttinst   = [ttinst,'inx.']	;exec
  ttinst   = [ttinst,'t13a']	;exec
  ttinst   = [ttinst,'t13c']	;exec
  ttinst   = [ttinst,'NeXus']	;exec
  ttinst   = [ttinst,'DCSasc']	;exec
  ttinst   = [ttinst,'DCSbin']	;exec
  ttinst   = [ttinst,'D10']	;exec
  ttinst   = [ttinst,'D15']	;exec
  ttinst   = [ttinst,'Spec']	;exec
  ttinst   = [ttinst,'EDF']	;exec
  ttinst   = [ttinst,'dat.']	;exec
  ttinst   = [ttinst,'gsas.']	;exec
  ttinst   = [ttinst,'fpcyc.']	;exec
  ttinst   = [ttinst,'xy.']	;exec
  ttinst   = [ttinst,'prn.']	;exec
  ttinst   = [ttinst,'gsas']	;exec
  ttinst   = [ttinst,'fil']	;exec
  ttinst   = [ttinst,'FPdat']	;exec
  ttinst   = [ttinst,'d20cal']	;exec
  ttinst   = [ttinst,'prf']	;exec
  ttinst   = [ttinst,'xyz']	;exec
  ttinst   = [ttinst,'FPcyc']	;exec
  ttinst   = [ttinst,'MiBeMol']	;exec
  ttinst   = [ttinst,'QENS']	;exec
  ttinst   = [ttinst,'res']	;exec
  ttinst   = [ttinst,'cufe']	;exec
  ttinst   = [ttinst,'fat']	;exec
  ttinst   = [ttinst,'eth.']	;exec
  ttinst   = [ttinst,'test']	;exec
  ttinst   = [ttinst,'FAT1']	;exec

  ttproc   = ['read_tmp']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'ill']	;exec
  ttproc   = [ttproc,'inx_in']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rlamp']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdspe']	;exec
  ttproc   = [ttproc,'write_inx']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'read_nexus']	;exec
  ttproc   = [ttproc,'read_dcsasc']	;exec
  ttproc   = [ttproc,'read_dcsbin']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'rdid']	;exec
  ttproc   = [ttproc,'read_spec']	;exec
  ttproc   = [ttproc,'rdedf']	;exec
  ttproc   = [ttproc,'export_dat']	;exec
  ttproc   = [ttproc,'export_gsa']	;exec
  ttproc   = [ttproc,'export_cyc']	;exec
  ttproc   = [ttproc,'export_xy']	;exec
  ttproc   = [ttproc,'export_prn']	;exec
  ttproc   = [ttproc,'rd_d2b']	;exec
  ttproc   = [ttproc,'rdinstr']	;exec
  ttproc   = [ttproc,'rdinstr']	;exec
  ttproc   = [ttproc,'rddat']	;exec
  ttproc   = [ttproc,'rddat']	;exec
  ttproc   = [ttproc,'rddat']	;exec
  ttproc   = [ttproc,'rddat']	;exec
  ttproc   = [ttproc,'read_mibemol']	;exec
  ttproc   = [ttproc,'read_qens']	;exec
  ttproc   = [ttproc,'read_res']	;exec
  ttproc   = [ttproc,'read_res']	;exec
  ttproc   = [ttproc,'read_res']	;exec
  ttproc   = [ttproc,'export_eth']	;exec
  ttproc   = [ttproc,'read_res']	;exec
  ttproc   = [ttproc,'read_res']	;exec

  ttgroup  = [' ']		;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'LSS']	;exec
  ttgroup  = [ttgroup,'LSS']	;exec
  ttgroup  = [ttgroup,'LSS']	;exec
  ttgroup  = [ttgroup,'NFP']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'3Axes']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'LSS']	;exec
  ttgroup  = [ttgroup,'LSS']	;exec
  ttgroup  = [ttgroup,'3Axes']	;exec
  ttgroup  = [ttgroup,' ']	;exec
  ttgroup  = [ttgroup,'3Axes']	;exec
  ttgroup  = [ttgroup,'3Axes']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'TOF']	;exec
  ttgroup  = [ttgroup,'LSS']	;exec
  ttgroup  = [ttgroup,'LON']	;exec
  ttgroup  = [ttgroup,' ']	;exec
  ttgroup  = [ttgroup,'LON']	;exec
  ttgroup  = [ttgroup,'LON']	;exec
  ttgroup  = [ttgroup,' ']	;exec
  ttgroup  = [ttgroup,'NIST']	;exec
  ttgroup  = [ttgroup,'NIST']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,' ']	;exec
  ttgroup  = [ttgroup,'ESRF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'LLB']	;exec
  ttgroup  = [ttgroup,'IPNS']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec
  ttgroup  = [ttgroup,'DIF']	;exec

  ttsymbol = ['Current Path']			;exec
  ttsymbol = [ttsymbol,'Current Cycle']		;exec
  ttsymbol = [ttsymbol,'Previous Cycle']	;exec
  ttsymbol = [ttsymbol,'On_Line']		;exec
  ttsymbol = [ttsymbol,'C_Year 1995']		;exec
  ttsymbol = [ttsymbol,'C_Year 1996']		;exec
  ttsymbol = [ttsymbol,'C_Year 1997']		;exec
  ttsymbol = [ttsymbol,'C_Year 1998']		;exec
  ttsymbol = [ttsymbol,'C_Year 1999']		;exec
  ttsymbol = [ttsymbol,'C_Year 2000']		;exec
  ttsymbol = [ttsymbol,'C_Year 2001']		;exec
  ttsymbol = [ttsymbol,'C_Year 2002']		;exec
  ttsymbol = [ttsymbol,'d2b']		;exec
  ttsymbol = [ttsymbol,'in10']		;exec
  ttsymbol = [ttsymbol,'in16']		;exec
  ttsymbol = [ttsymbol,'d20']		;exec
  ttsymbol = [ttsymbol,'d16']		;exec
  ttsymbol = [ttsymbol,'xray']		;exec
  ttsymbol = [ttsymbol,'in5']		;exec
  ttsymbol = [ttsymbol,'in6']		;exec
  ttsymbol = [ttsymbol,'d11']		;exec
  ttsymbol = [ttsymbol,'d22']		;exec
  ttsymbol = [ttsymbol,'in4']		;exec
  ttsymbol = [ttsymbol,'d1a']		;exec
  ttsymbol = [ttsymbol,'d7']		;exec
  ttsymbol = [ttsymbol,'d17']		;exec
  ttsymbol = [ttsymbol,'d15']		;exec
  ttsymbol = [ttsymbol,'d10']		;exec
  ttsymbol = [ttsymbol,'db21']		;exec
  ttsymbol = [ttsymbol,'in5_864']		;exec
  ttsymbol = [ttsymbol,'in4_862']		;exec
  ttsymbol = [ttsymbol,'in6_882']		;exec
  ttsymbol = [ttsymbol,'in4_893']		;exec
  ttsymbol = [ttsymbol,'in6_892']		;exec

  ttpath   = ['.']				;exec
  ttpath   = [ttpath,'/usr/illdata/data']		;exec
  ttpath   = [ttpath,'/usr/illdata/data-1']		;exec
  ttpath   = [ttpath,'rpc']			;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/usr/illdata']		;exec
  ttpath   = [ttpath,'/hosts/d2b/users/data']		;exec
  ttpath   = [ttpath,'/hosts/in10/users/data']		;exec
  ttpath   = [ttpath,'/hosts/in16/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d20/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d16/users/data']		;exec
  ttpath   = [ttpath,'/usr/illdata/xray/xraydur']		;exec
  ttpath   = [ttpath,'/hosts/in5/users/data']		;exec
  ttpath   = [ttpath,'/hosts/in6/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d11/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d22/users/data']		;exec
  ttpath   = [ttpath,'/hosts/in4c/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d1a/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d7/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d17/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d10a/users/data']		;exec
  ttpath   = [ttpath,'/hosts/d10/users/data']		;exec
  ttpath   = [ttpath,'/hosts/db21/users/data']		;exec
  ttpath   = [ttpath,'/usr/illdata/864/in5']		;exec
  ttpath   = [ttpath,'/usr/illdata/862/in4']		;exec
  ttpath   = [ttpath,'/usr/illdata/882/in6']		;exec
  ttpath   = [ttpath,'/usr/illdata/893/in4']		;exec
  ttpath   = [ttpath,'/usr/illdata/892/in6']		;exec

  ttouch   = '/home/cs/TOUCH_BASE'			;exec

  ttmacro  = '/home/cs/lambda/macros'		;exec

  ttaccess = 'rdfilter'				;exec

  ttsite   = 'd20_widget'				;exec

  ttmagi   = '6' ;exec

  ttwall   = 'out' ;exec

  ttpars   = '120' ;exec

  datp     = {a:ttinst,  b:ttproc,  c:ttgroup,  $
              d:ttsymbol,e:ttpath,  f:ttouch,   $
              g:ttmacro, h:ttaccess,i:ttsite,j:ttmagi,k:ttwall,l:ttpars}

  return,0

         END

ELSE :

ENDCASE

Status = 14

return,0

END

