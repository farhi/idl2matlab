function dial_PAD_init, dummy
;******* *************
;**      See Manual about Dials for a good understanding.
;**      This is a template to construct a PAD in the GEORGE-LAMP main interface.
;**      The PAD is used to send control-commands to dial_'generic'_send procedure.
;**      You must return a 5*n string area.
;**      First line is for the input widget_text, the others for buttons.
;**      ACTION 's' to send the COMMAND , 't' to put the COMMAND into widget_text.
;**             'c' to create the command from a GUI.
;**             '-' for a menu button.
;**             'i' concern the first line (Important: generic must be the same for 'i' & 't').
;**      
;**            LABEL                     COMMAND          ACTION   PROCEDURE      WAIT seconds
;**     'Input Widget_text'         ''                      'i'   ,'generic'     ,'0'
;**     'Button label'              'Command to send'       's'   ,'generic'     ,'0'
;**     'Button label'              'Command to show'       't'   ,'generic'     ,' '
;**     'Button  menu'              ''                      '-'   ,''            ,' '
;**       '-sub  button'            'Command to send'       's'   ,'generic'     ,' '
;**       '-sub  button'            'Command to show'       't'   ,'generic'     ,' '
;**       '-sub    menu'            ''                      '-'   ,''            ,' '
;**         '--sub sub but'         'Command to send'       's'   ,'generic'     ,' '
;**etc..

PAR1= [        ' '                 ,'show,systime()'      ,'i'   ,'lamp'        ,' ' ]

PAR1= [[PAR1],[' '                 ,'show,systime()'      ,'s'   ,'lamp'        ,' ' ]]
PAR1= [[PAR1],[' '                 ,'print,systime()'     ,'s'   ,'lamp'        ,' ' ]]
PAR1= [[PAR1],[' '                 ,'print,systime()'     ,'s'   ,'lamp'        ,' ' ]]
PAR1= [[PAR1],[' '                 ,'show,systime()'      ,'s'   ,'lamp'        ,' ' ]]

PAR1= [[PAR1],['Tornade'           ,' '                   ,'-'   ,' '           ,' ' ]]
PAR1= [[PAR1],[ '-Start  Tornade ' ,'tornade'             ,'s'   ,'startdial'   ,' ' ]]
PAR1= [[PAR1],[ '-Stop   Tornade ' ,'tornade'             ,'s'   ,'stopdial'    ,' ' ]]
PAR1= [[PAR1],[ '-Resume Tornade ' ,'tornade'             ,'s'   ,'resumedial'  ,' ' ]]
PAR1= [[PAR1],[ '-Clear  Tornade ' ,'tornade'             ,'s'   ,'cleardial'   ,' ' ]]

PAR1= [[PAR1],[ 'Template'         ,' '                   ,'-'   ,' '           ,' ' ]]
PAR1= [[PAR1],[ '-Start Templates' ,'template1'           ,'s'   ,'startdial'   ,' ' ]]
PAR1= [[PAR1],[ '-Clear Templates' ,'template1~template2~template3~template4' ,'s'   ,'cleardial'   ,' ' ]]

PAR1= [[PAR1],['Cron'              ,' '                   ,'-'   ,' '           ,' ' ]]
PAR1= [[PAR1],[ '-Start SaveSess.' ,'savsession'          ,'s'   ,'startdial'   ,' ' ]]
PAR1= [[PAR1],[ '-Clear SaveSess.' ,'savsession'          ,'s'   ,'cleardial'   ,' ' ]]
PAR1= [[PAR1],[' '                 ,'print,systime()'     ,'s'   ,'lamp'        ,' ' ]]

return, PAR1
end
