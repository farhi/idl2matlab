echo on
disp('Traduction IDL -> Matlab')
idl2scilab('pro/desstest.pro','resultats');
disp('[Presser une touche]')
pause

disp('Init Matlab I2M lib')
path_init
i2m_init
disp('[Presser une touche]')
pause

disp('On lance le code traduit')
desstest
disp('[Presser une touche]')
pause

disp('Traduction IDL -> Scilab depuis Matlab')
idl2scilab('-C','demo/traductionsimple.pro','resultats');
disp('[Presser une touche]')
pause

