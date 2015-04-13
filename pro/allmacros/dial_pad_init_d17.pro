function dial_PAD_init_d17, dummy
;******* *****************
;**      See Manual about Dials for a good understanding.
;**      This is a template to construct a PAD in the GEORGE-LAMP main interface.
;**      The PAD is used to send control-commands to dial_'generic'_send procedure.
;**      You must return a 5*n string area.
;**      First line is for the input widget_text, the others for buttons.
;**      ACTION 's' to send the COMMAND , 't' to put the COMMAND into the WIDGET_TEXT
;**             'c' to create the command from a GUI.
;**             'i' concern the first line (Important: generic must be the same for 'i' & 't')
;**      
;**            LABEL                  COMMAND          ACTION   PROCEDURE      WAIT
;**     'Input Widget_text'      ''                      'i'   ,'generic'    ,'check'
;**     'Button label'           'Command to send'       's'   ,'generic'    ,'check'
;**     'Button label'           'Command to show'       't'   ,'generic'    ,'check'
;**     'Button  menu'           ''                      '-'   ,''           ,' '
;**       '-sub  button'         'Command to send'       's'   ,'generic'    ,'check'
;**       '-sub  button'         'Command to show'       't'   ,'generic'    ,'check'
;**       '-sub    menu'         ''                      '-'   ,''           ,' '
;**         '--sub sub but'      'Command to send'       's'   ,'generic'    ,'check'
;**etc..

PAR1= [        ' '              ,' '                    ,'i'   ,'mad'        ,'0']
PAR1= [[PAR1],['D17 data'       ,'d17data'              ,'s'   ,'startup'   ,'0']]
PAR1= [[PAR1],['D17 spy'        ,'d17status'            ,'s'   ,'startup'   ,'0']]

                                 
return, PAR1
end
