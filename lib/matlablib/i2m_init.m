%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction i2m_init
%            pour initialiser les constantes et variables
%            systeme d'IDL
% Auteurs :
%                 Didier Richard
%                 Bourtembourg Reynald
% Date creation : ?
% Modifications : 21 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% revoir la variable systeme !VERSION ??
% pour computer = 'HP700', on peut avoir !VERSION.OS='Linux'

function i2m_init
%******* ********

global  ft_biggest ft_bigger ft_normal ft_smaller ft_smallest ft_b_bigger ft_b_norma ft_propor
 ft_biggest ='ft_biggest';
 ft_bigger ='ft_bigger';
 ft_normal ='ft_normal';
 ft_smaller ='ft_smaller';
 ft_smallest ='ft_smallest';
 ft_b_bigger ='ft_b_bigger';
 ft_b_normal ='ft_b_normal';
 ft_propor ='ft_propor';

% couleur de fond lorsque l'on fait des widgets 
global i2m_wcolor i2m_wcolorOn
  i2m_wcolor=[0 1 1];
  i2m_wcolorOn = 0; % =1 pout couleur unique des widgets

% Coefficient multiplicateur entre la taille des fonts IDL et celles de Matlab, charsizeIDL = coefTailleCar*fontSizeMatlab
global coefTailleCar
  coefTailleCar = 6;

% nombre de pixels pour un caractere selon x et selon y + gap between widgets
global I2Mfcar I2Mfgap
  I2Mfcar=[7 17]; I2Mfgap=4;

global  i2mvs_dpi i2mvs_pi i2mvs_dtor i2mvs_radeg i2mvs_err i2mvs_error i2mvs_except i2mvs_more i2mvs_quiet i2mvs_prompt i2mvs_order
% i2mvs_err=0;  i2mvs_error=0;
  i2mvs_pi =pi; i2mvs_dpi=pi;  i2mvs_dtor=pi/180;   i2mvs_radeg=180/pi; i2mvs_except=1;
  i2mvs_more=0; i2mvs_quiet=0; i2mvs_prompt='I2M>'; i2mvs_order=0;

global light_axe light_ambient light_diffuse light_strength light_exponent light_reflection
  light_axe=[0,0,1]; light_ambient=0.3; light_diffuse=0.7; light_strength=1; light_exponent=1; light_reflection=1;
 
global  i2mvs_version i2mvs_mouse i2mvs_dir i2mvs_path i2mvs_d i2mvs_p i2mvs_x i2mvs_y i2mvs_z i2mvs_error_state
  a1='Mips'; a2='IRIX'; a3='UNIX'; a4='5.4'; a5='Sep 25 2000';
  switch computer,
        case 'ALPHA', a2='OSF';
        case 'HP700', a2='HP-UX';
        case 'HPUX' , a2='HP-UX';
        case 'IBM_RS',a2='AIX';
        case 'GLNX86',a2='linux'; a1='x86';
        case 'PCWIN', a2='Win32'; a1='x86'; a3='Windows';
        case 'SOL2',  a2='sunos'; otherwise, end;
        
  i2mvs_version=struct('arch',a1, 'os',a2, 'os_family',a3, 'release',a4, 'build_date',a5);
  
% i2mvs_mouse  =struct('x',0,'y',0,'button',0,'time',0);

  i2mvs_dir=' '; i2mvs_path=path;

  i2mvs_error_state = varsysError; %*** Object ***
 
% a1='X'; a2=640; a3=512; a4=640; a5=512; a6=7; a7=10; a8=32; a9=32; a10=16777216; a11=256; a12=1; a13=-1; a14=0; a15=uint32(328124); a16=[0 0]; a17=[1 1];
% if strcmp(i2mvs_version.os_family,'Windows'), a1='WIN'; end;
% i2mvs_d=struct('name',a1, 'x_size',a2, 'y_size',a3, 'x_vsize',a4, 'y_vsize',a5, 'x_ch_size',a6, 'y_ch_size',a7, 'x_px_cm',a8, 'y_px_cm',a9, 'n_colors',a10, 'table_size',a11, 'fill_dist',a12, 'window',a13, 'unit',a14, 'flags',a15, 'origin',a16, 'zoom',a17);
  i2mvs_d = varsysD;               %*** Object ***
 
% a1=0; a2=0; a3=0; a4=[0 0 639 511 0 0]; a5=16777215; a6=-1; a7=0; a8=[0 0 0 0 0]; a9=0; a10=0; a11=0; a12=[0 0 0 0]; a13=0; a14=[0 0 0 0]; a15=''; a16=0; a17=zeros(4); a18=0; a19=0; a20=''; a21=0.02; a22=0;
% i2mvs_p=struct('background',a1, 'charsize',a2, 'charthick',a3, 'clip',a4, 'color',a5, 'font',a6, 'linestyle',a7, 'multi',a8, 'noclip',a9, 'noerase',a10, 'nsum',a11, 'position',a12, 'psym',a13, 'region',a14, 'subtitle',a15, 'symsize',a16, 't',a17, 't3d',a18, 'thick',a19, 'title',a20, 'ticklen',a21, 'channel',a22);
  i2mvs_p = varsysP;               %*** Object ***

% a1=''; a2=0; a3=0; a4=0; a5=0; a6=0; a7=[0 0]; a8=[0 0]; a9=[0 1]; a10=[10 3]; a11=[0 0]; a12=[0 0]; a13=[0 0]; a14=0; a15=0; a16=zeros(1,60); a17=zeros(1,60); a18=0; a19=''; a20=0; a21=0; a22=strarr(10);
% i2mvs_x=struct('title',a1, 'type',a2, 'style',a3, 'ticks',a4, 'ticklen',a5, 'thick',a6, 'range',a7, 'crange',a8, 's',a9, 'margin',a10, 'omargin',a11, 'window',a12, 'region',a13, 'charsize',a14, 'minor',a15, 'tickv',a16, 'tickname',a17, 'gridstyle',a18, 'tickformat',a19, 'tickinterval',a20, 'ticklayout',a21, 'tickunits',a22);
  i2mvs_x = varsysX;               %*** Object ***

% a10=[4 2];
% i2mvs_y=struct('title',a1, 'type',a2, 'style',a3, 'ticks',a4, 'ticklen',a5, 'thick',a6, 'range',a7, 'crange',a8, 's',a9, 'margin',a10, 'omargin',a11, 'window',a12, 'region',a13, 'charsize',a14, 'minor',a15, 'tickv',a16, 'tickname',a17, 'gridstyle',a18, 'tickformat',a19, 'tickinterval',a20, 'ticklayout',a21, 'tickunits',a22);
  i2mvs_y = varsysY;               %*** Object ***

% a10=[0 0];
% i2mvs_z=struct('title',a1, 'type',a2, 'style',a3, 'ticks',a4, 'ticklen',a5, 'thick',a6, 'range',a7, 'crange',a8, 's',a9, 'margin',a10, 'omargin',a11, 'window',a12, 'region',a13, 'charsize',a14, 'minor',a15, 'tickv',a16, 'tickname',a17, 'gridstyle',a18, 'tickformat',a19, 'tickinterval',a20, 'ticklayout',a21, 'tickunits',a22);
  i2mvs_z = varsysZ;               %*** Object ***
 
  set(0,'DefaultFigureMenuBar','none');      % Pour retirer les barres de menus par defaut
 %set(0,'DefaultAxesBox','on');              % pour que les axes soient entourent toute la figure comme en IDL
  set(0,'DefaultAxesNextPlot','replace');
  set(0,'DefaultFigureNextPlot','add');
  set(0,'DefaultSurfaceEdgeColor','none');   % pour que les aretes ne soient pas affichees dans les 'shade_surf'

global i2m_limTicklen % variable definissant la longueur limite des ticks que l'on va specifier a la propriete Matlab limTicklen
  i2m_limTicklen = 0.3;
% set(0,'DefaultAxesGridLineStyle','none');  % En commentaire car incompatible avec la fonction Matlab polar
% ScrSize = get(0,'ScreenSize');             % Pour recuperer la resolution de l'ecran qui sera contenue dans ScrSize(3) et ScrSize(4)
% affichage des fenetres par defaut en haut a droite de l'ecran :
% set(0,'DefaultFigurePosition',[ScrSize(3)-i2mvs_d.x_size,ScrSize(4)-22-i2mvs_d.y_size,i2mvs_d.x_size,i2mvs_d.y_size]); % On suppose que la barre de titre de la fenetre fait 22 pixels de hauteur

%%%%%%%%%%%%%%%% DEVICES %%%%%%%%%%%%%%%%%%%%%%%%%
global i2mZbuff i2mOldGcf i2mPs
  i2mOldGcf = [];
  i2mZbuff = 9998;                           % Le numero de la figure reservee pour le Z-BUFFER
  
%%%%%%%%%%%%%%%% GESTION DU DEVICE POSTSCRIPT (PS)
  i2mPs    = 9997;                           % Le numero de la figure reservee au device 'PS'
global reservedFigures kgNoerase i2mDecomposed i2mFilename i2mDevPsColor i2mDevPsPreview i2mPsCanWrite i2mPsCanWrite
  reservedFigures = [i2mZbuff i2mPs];        % tableau contenant les indices des figures reservees pour traiter les devices comme le Z-buffer ou ps
  kgNoerase = 0;                             % Variable globale utile pour savoir si le KG noerase a ete passe ou non a la fonction precedente
  i2mDecomposed = 1;                         % pour savoir si on est en mode decomposed = 0 ou 1
  i2mFilename = 'idl';                       % Le nom de fichier par defaut
  i2mDevPsColor = 0;
  global i2mDevPsPreview;                    % Pour gerer l'activation ou non des apercus dans les fichiers postscript (apres avoir fait 'set_plot,'ps' puis 'device, preview = val')
  i2mDevPsPreview = 0;
% si i2mPsCanWrite = 0, alors, en mode 'ps' (apres set_plot,'ps'), on n'enregistrera pas le contenu de la fenetre PS avant l'appel a une fonction du type plot, shade_surf, surface, contour, ...
% si i2mPsCanWrite = 1, alors,en mode 'ps', on enregistrera le contenu de la fenetre PS en mode normal (va ecraser le fichier PS si il existait) avant l'appel a une fonction du type plot, shade_surf, surface, contour, ...
% si i2mPsCanWrite ~=0 et ~=1, alors,en mode 'ps', on enregistrera le contenu de la fenetre PS en mode append avant l'appel a une fonction du type plot, shade_surf, surface, contour, ...
  i2mPsCanWrite = 0;
  if ishandle(i2mPs)
	 set(i2mPs,'PaperOrientation','portrait'); % Orientation de la sortie postscript, portrait par defaut
	 set(i2mPs,'PaperUnits','centimeters');    % Unites utilisees pour specifier XOFFSET, YOFFSET, XSIZE et YSIZE de la sortie postscript
  end
%%%%%%%%%%%%%%%% FIN DE GESTION DU DEVICE POSTSCRIPT

%%%%%%%%%%%%%%%% TABLE DE COULEURS %%%%%%%%%%%%%%%%%
global B_W_LINEAR                            % La table de couleur IDL par defaut
  defColorTables;                            % On definit les tables de couleurs pre-definies d'IDL
  i2mColormap(B_W_LINEAR);                   % On modifie la table de couleurs 'IDL' actuelle
