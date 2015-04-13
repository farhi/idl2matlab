
;******************************************************************************
;------------------------------------------------------------------------------
;
	FUNCTION dial_PAD_init_d7, dummy
;
;       Creates the D7 MAD pad
;
;						JRS 16/5/02
;
;******************************************************************************
;------------------------------------------------------------------------------

PAR1= [        ' '              ,' '                                                                            ,'i'   ,'mad'        ,'0']
PAR1= [[PAR1],['Count ->'       ,'COUNT'                                                                        ,'s'   ,'d7count'    ,'0']]
PAR1= [[PAR1],['Stop ->'        ,' '                                                                            ,'-'   ,' '          ,'0']]
PAR1= [[PAR1],[  '-next'        ,'stop next'                                                                    ,'s'   ,'mad'        ,'0']]
PAR1= [[PAR1],[  '-last'        ,'stop last'                                                                    ,'s'   ,'mad'        ,'0']]
PAR1= [[PAR1],['Pause'          ,'pause'                                                                        ,'s'   ,'mad'        ,'0']]
PAR1= [[PAR1],['Resume'         ,'resume'                                                                       ,'s'   ,'mad'        ,'0']]
PAR1= [[PAR1],['Set Par ->'     ,'PARAMS'                                                                       ,'s'   ,'d7params'   ,'0']]
PAR1= [[PAR1],['LAUNCH ->'		,' '                                                                    ,'-'   ,' '          ,'0']]
PAR1= [[PAR1],[  '-Flipper'      	,'flipper_d7'                                                           ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Status'	 	,'d7status'                                                          	,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-MAD Log'      	,'madview'                                                              ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Pol HYTEC Data'	,'hytec_data'                                                          	,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Nopo HYTEC Data'	,'hytec_data_all'                                                       ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-TOF Data'    	,'tof_data'                                                             ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Nopo TOF Data'    	,'tof_data_all'                                                         ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Temp Scan'		,'temp_ramp'                                                          	,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Furnace Ramp'	,'furnace_ramp'                                                         ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Rotate Omega'	,'omega_rot'                                                            ,'s'   ,'startup'    ,'0']]
PAR1= [[PAR1],[  '-Monitor 1'		,'d7monitor1'                                                          	,'s'   ,'startup'    ,'0']]
                                 
RETURN, PAR1
END
