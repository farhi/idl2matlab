function dial_PAD_init_d22, dummy
;******* *****************
;**      See Manual about Dials for a good understanding.
;**      This is a template to construct a PAD in the GEORGE-LAMP main interface.
;**      The PAD is used to send control-commands to dial_'generic'_send procedure.
;**      You must return a 5*n string area.
;**      First line is for the input widget_text, the others for buttons.
;**      ACTION 's' to send the COMMAND 
;**             't' to put the COMMAND into the WIDGET_TEXT
;**             'c' to create the command from a GUI.
;**             'i' concern the first line (Important: generic must be the same for 'i' & 't')
;**      
;**            LABEL                  COMMAND          ACTION   PROCEDURE      WAIT
;**     'Input Widget_text'    , ''                    , 'i'   , 'generic'    , 'check'
;**     'Button label'         , 'Command to send'     , 's'   , 'generic'    , 'check'
;**     'Button label'         , 'Command to show'     , 't'   , 'generic'    , 'check'
;**     'Button  menu'         , ''                    , '-'   , ''           , ' '
;**       '-sub  button'       , 'Command to send'     , 's'   , 'generic'    , 'check'
;**       '-sub  button'       , 'Command to show'     , 't'   , 'generic'    , 'check'
;**       '-sub    menu'       , ''                    , '-'   , ''           , ' '
;**         '--sub sub but'    , 'Command to send'     , 's'   , 'generic'    , 'check'
;**etc..

PAR1= [        ' '              ,' '                                           , 'i', 'mad', '0']
PAR1= [[PAR1],[' Setup  '      ,' '                                            , '-', ' '  , '0']]

PAR1= [[PAR1],[  '-Experiment information','startupdata'                       , 's', 'startup', '0']]
PAR1= [[PAR1],[  '-Q-range'               ,'d22qrange'                         , 's', 'startup', '0']]
PAR1= [[PAR1],[  '-Beam centre'           ,'d22centre'                         , 's', 'startup', '0']]
PAR1= [[PAR1],[  '-Changer file'          ,'changer_file'                      , 's', 'startup', '0']]
PAR1= [[PAR1],[  '-Kinetics files'        ,'d22kin'                            , 's', 'startup', '0']]

PAR1= [[PAR1],[  '-Instrument settings '  ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[    '--All settings'       ,' ~det~Detector distance (m)~ ~ ~col~Collimation (m)~ ~ ~dtr~offset (mm)~ ~ ~bx~Bx (mm)~ ~ ~by~By (mm)'  ,'c' ,'mad' ,'0']]
PAR1= [[PAR1],[    '--Detector distance'  ,' ~det~Detector distance (m)~ '     , 'c' ,'mad', '0']]
PAR1= [[PAR1],[    '--Collimation'        ,' ~coll~Collimation (m)~ '          , 'c' ,'mad', '0']]
PAR1= [[PAR1],[    '--Detector offset'    ,' ~dtr~offset (mm)~ '               , 'c' ,'mad', '0']]
PAR1= [[PAR1],[    '--Beamstop'           ,' ~bx~Bx (mm)~ ~ ~by~By (mm)~ '     , 'c' ,'mad', '0']]
PAR1= [[PAR1],[    '--Wavelength'         ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[    '---Wavelength'        ,' ~wav   ~Wavelength (Angstrom)~ '  , 'c', 'mad' ,'0']]
PAR1= [[PAR1],[    '---Selector speed'    ,' ~sel~Selector speed (rpm)~    '   , 'c', 'mad', '0']]

PAR1= [[PAR1],[  '-Rack  ', ' '                                                , '-', ' ',   '0']]
PAR1= [[PAR1],[  '--Read parameters'      ,'par cha'                           , 's', 'mad', '0']]
PAR1= [[PAR1],[  '--Choose rack'          ,' ~par cha set~rack #~'             , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '--Set parameters'       ,' ~par cha~rack #~1~pos.1~~incr.~-40~number~22', 'c', 'mad', '0']]

PAR1= [[PAR1],[  '-Temperature  '         ,' '                                 , '-', ' ' ,'0']]
PAR1= [[PAR1],[  '--Cryo'                 ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '---Set cryo temp'       ,'   ~pte~Set point(K)~    '         , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '---enable'              ,'par cryo 1'                        , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---disable'             ,'par cryo 0'                        , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---line speed'          ,'   ~par  rs232~line speed~9600'    , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '--Bath temp (DAC)'      ,'   ~bath~temperature (C)~    '     , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '--Bath temp RS232'      ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '---temperature   '      ,'   ~pte~temperature (C)~    '      , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '---line speed'          ,'   ~par  rs232~line speed~9600'    , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '--Eurotherm'            ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '---command'             ,'   ~eudev~ch. string~    '         , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '---line speed'          ,'   ~par  rs232~line speed~4800'    , 'c', 'mad', '0']]

PAR1= [[PAR1],[  '-Voltmeter'             ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '--1'                    ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '---reading'             ,'   ~ieee1'                         , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto Volt params'    ,'   ~par ieee1 1 1 2'               , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto Amps params'    ,'   ~par ieee1 1 3 2'               , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto Ohms params'    ,'   ~par ieee1 1 2 2'               , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto off'            ,'   ~par ieee1 0'                   , 's', 'mad', '0']]
PAR1= [[PAR1],[  '--2'                    ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '---reading'             ,'   ~ieee2'                         , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto Volt params'    ,'   ~par ieee2 1 1 2'               , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto Amps params'    ,'   ~par ieee2 1 3 2'               , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto Ohms params'    ,'   ~par ieee2 1 2 2'               , 's', 'mad', '0']]
PAR1= [[PAR1],[  '---auto off'            ,'   ~par ieee2 0'                   , 's', 'mad', '0']]

PAR1= [[PAR1],['  Dials   '               ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '-D22 data'              ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '--Start'                ,'d22data'                           , 's', 'startup', '0']]
;PAR1= [[PAR1],[  '--log plot'             ,'d22data~LOG~1'                     , 's', 'properties', '0']]
;PAR1= [[PAR1],[  '--lin plot'             ,'d22data~lOG~0'                     , 's', 'properties', '0']]
PAR1= [[PAR1],[  '--log plot'             ,'DialTag,"d22data",TAG="LOG",SET=1' , 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--lin plot'             ,'DialTag,"d22data",TAG="LOG",SET=0' , 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--sqr plot'             ,'DialTag,"d22data",TAG="LOG",SET=2' , 's', 'lamp', '0']]
PAR1= [[PAR1],[  '-Diagram'               ,' '                                 , '-', ' '  , '0']]
;PAR1= [[PAR1],[  '--log plot'             ,'d22data~DLOG~1'                    , 's', 'properties', '0']]
;PAR1= [[PAR1],[  '--lin plot'             ,'d22data~DlOG~0'                    , 's', 'properties', '0']]
PAR1= [[PAR1],[  '--log plot'             ,'DialTag,"d22data",TAG="DLOG",SET=1', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--lin plot'             ,'DialTag,"d22data",TAG="DLOG",SET=0', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--sqr plot'             ,'DialTag,"d22data",TAG="DLOG",SET=2', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '-X projection'          ,' '                                 , '-', ' '  , '0']]
;PAR1= [[PAR1],[  '--log plot'            ,'d22data~XLOG~1'                    , 's', 'properties', '0']]
;PAR1= [[PAR1],[  '--lin plot'            ,'d22data~XlOG~0'                    , 's', 'properties', '0']]
PAR1= [[PAR1],[  '--log plot'             ,'DialTag,"d22data",TAG="XLOG",SET=1', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--lin plot'             ,'DialTag,"d22data",TAG="XLOG",SET=0', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--sqr plot'             ,'DialTag,"d22data",TAG="XLOG",SET=2', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '-Y projection'          ,' '                                 , '-', ' '  , '0']]
;PAR1= [[PAR1],[  '--log plot'             ,'d22data~YLOG~1'                    , 's', 'properties', '0']]
;PAR1= [[PAR1],[  '--lin plot'             ,'d22data~YlOG~0'                    , 's', 'properties', '0']]
PAR1= [[PAR1],[  '--log plot'             ,'DialTag,"d22data",TAG="YLOG",SET=1', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--lin plot'             ,'DialTag,"d22data",TAG="YLOG",SET=0', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '--sqr plot'             ,'DialTag,"d22data",TAG="YLOG",SET=2', 's', 'lamp', '0']]
PAR1= [[PAR1],[  '-Status'                ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '--Start'                ,'status'                            , 's', 'startup', '0']]

PAR1= [[PAR1],['Acquisition'              ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '-Sample title  '        ,' ~par sub~sample title (20 char.)~1_dummy' , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '-Changer       '        ,' ~cha    ~position #~ '            , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '-Run           '        ,' ~run    ~duration  ~ ~t(s)/m~t~times~1', 'c', 'mad', '0']]
PAR1= [[PAR1],[  '-Kinetics      '        ,' ~kin    ~time file name   ~ ~repetitions~1~no/save~s '              ,'c', 'mad', '0']]
;PAR1= [[PAR1],[  '-Kinetic status'        ,'unix kin'                          , 's', 'mad', '0']]
PAR1= [[PAR1],[  '-Check         '        ,' ~check  ~command file name~ ~repetitions~1','c', 'mad', '0']]
PAR1= [[PAR1],[  '-Start         '        ,' ~start  ~command file name~ ~repetitions~1','c', 'mad', '0']]
PAR1= [[PAR1],[  '-Scan          '        ,' ~scan   ~motor name~ ~from~ ~to~ ~step~ ~ ~run~time~10~t/m~t~no/align~a'         ,'c', 'mad', '0']]
PAR1= [[PAR1],[  '-Adjust        '        ,' ~adjust ~on position~ '           , 'c', 'mad', '0']]
PAR1= [[PAR1],[  '-Wait          '        ,' ~wait    ~time [sec]~ '           , 'c', 'mad', '0']]

PAR1= [[PAR1],['     Stop     '           ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],[  '-no save'               ,'stop n'                            , 's', 'mad', '0']]
PAR1= [[PAR1],[  '-save'                  ,'stop s'                            , 's', 'mad', '0']]

PAR1= [[PAR1],[' Pause '                  ,'pause'                             , 's', 'mad', '0']]

PAR1= [[PAR1],['Resume'                   ,'resume'                            , 's', 'mad', '0']]


PAR1= [[PAR1],['Attenuators'              ,' '                                 , '-', ' '  , '0']]
PAR1= [[PAR1],['-3 (1/3000)   '           ,'att c 3'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-2 (1/900)    '           ,'att c 2'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-1 (1/150)    '           ,'att c 1'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-in           '           ,'att in '                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-out          '           ,'att out'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-4 (ap.  5 mm)'           ,'att c 4'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-5 (ap. 10 mm)'           ,'att c 5'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-6 (ap. 20 mm)'           ,'att c 6'                           , 's', 'mad', '0']]
PAR1= [[PAR1],['-7 (ap. 30 mm)'           ,'att c 7'                           , 's', 'mad', '0']]
                               
PAR1= [[PAR1],['    Help      '           ,' '                                 , '-', 'mad', '0']]
PAR1= [[PAR1],['-not yet ...'             ,' '                                 , 's', 'mad', '0']]
PAR1= [[PAR1],['-sorry      '             ,' '                                 , 's', 'mad', '0']]

return, PAR1
end
