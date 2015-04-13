disp('Traduction IDL -> Matlab')
disp('idl2scilab(''pro/desstest.pro'',''resultats'');')
idl2scilab('pro/desstest.pro','resultats');
disp('[Presser une touche]')
pause

disp('Init Matlab I2M lib')
disp('path_init')
disp('i2m_init')
path_init
i2m_init
disp('[Presser une touche]')
pause

disp('On lance le code traduit')
disp('desstest')
desstest
disp('[Presser une touche]')
pause

disp('Traduction IDL -> Scilab depuis Matlab')
disp('idl2scilab(''-C'',''demo/traductionsimple.pro'',''resultats'');')
idl2scilab('-C','demo/traductionsimple.pro','resultats');
disp('[Presser une touche]')
pause

