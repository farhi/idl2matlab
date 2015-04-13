% device
% --------------------------------------------
% Equivalent to :
% function DEVICE
%               [, /CLOSE{Z}]
%               [, SET_RESOLUTION=[width, height]{Z}]
%               [, WINDOW_STATE=variable{MAC, WIN, X}]
%               [, /DECOMPOSED{MAC, WIN, X}]
%               [, GET_DECOMPOSED=variable{MAC, WIN, X}]
%               [, RETAIN={0 | 1 | 2}{MAC, WIN, X}]
%               [, COPY=[Xsource, Ysource, cols, rows, Xdest, Ydest [, Window_index]]{MAC, WIN, X}]
%               [, SET_GRAPHICS_FUNCTION=code{3 or 6}{MAC, WIN, X, Z}]
%               [, GET_GRAPHICS_FUNCTION=variable{MAC, WIN, X, Z}]
%               [, FILENAME=filename{PS}]
%               [, /COLOR{PS}]
%               [, /INCHES{PS}]
%               [, /LANDSCAPE | , /PORTRAIT{PS}]
%               [, /CLOSE_FILE{PS}]
% in IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction device
% Auteur :
%                 Bourtembourg Reynald
% Date creation : 26 / 05 / 2003
% Modifications : 13 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RETAIN, n'a pas d'equivalent en Matlab, en effet, en Matlab, il n'y a qu'une facon de gerer l'affichage des fenetres et c'est bien gere, donc on ne va rien faire quand on va rencontrer ce Keyword
% BYPASS_TRANSLATION, pas a traduire
% Set_graphics_function : pas d'equivalent, sauf peut etre xor(6) et gxcopy (3)
% get_graphics_function : On renvoie quelque chose ou non ?
% revoir COPY => pbs avec tvrd et tv


function [varargout]=device(varargin)

I2Mkwn=char('close' ,'set_resolution','get_screen_size','window_state','get_window_position','decomposed','get_decomposed','retain','get_graphics_function', 'set_graphics_function','copy' ,'filename','color'  ,'inches','landscape','portrait','close_file',...
	...
	'avantgarde','average_lines','binary','bits_per_pixel','bkman','bold','book','bypass_translation','close_document','colors','courier','cursor_crosshair','cursor_image','cursor_mask','cursor_original','cursor_standard','cursor_xy','demi','depth','direct_color',...
	'eject','encapsulated','encoding','floyd','font_index','font_size','get_current_font','get_fontnames','get_fontnum','get_page_size','get_visual_depth','get_visual_name','get_write_mask','gin_chars','glyph_cache','helvetica','index_color',...
	'isolatin1','italic','light','medium','narrow','ncar','oblique','optimize','ordered','output','palatino','pixels','plot_to','plotter_on_off','polyfill','pre_depth','pre_xsize','pre_ysize','preview','print_file','pseudo_color','reset_string','resolution',...
	'scale_factor','schoolbook','set_character_size','set_colormap','set_colors','set_font','set_string','set_translation','set_write_mask','static_color','static_gray','symbol','tek4014','tek4100','text','threshold','times','translation','true_color','tt_font',...
	'tty','user_font','vt240','vt241','vt340','vt341','xoffset','xon_xoff','xsize','yoffset','ysize','zapfchancery','zapfdingbats','z_buffering','I2M_pos');
    I2Mkwv={'closeZ','set_resolution','get_screen_size','window_state',   'window_position' ,'decomposed','get_decomposed','retain','get_graphics_function', 'set_graphics_function','copie','filename','couleur','inches','landscape','portrait','close_file',...
	...
	'avantgarde','average_lines','binary','bits_per_pixel','bkman','bold','book','bypass_translation','close_document','colors','courier','cursor_crosshair','cursor_image','cursor_mask','cursor_original','cursor_standard','cursor_xy','demi','depth','direct_color',...
	'eject','encapsulated','encoding','floyd','font_index','font_size','get_current_font','get_fontnames','get_fontnum','get_page_size','get_visual_depth','get_visual_name','get_write_mask','gin_chars','glyph_cache','helvetica','index_color',...
	'isolatin1','italic','light','medium','narrow','ncar','oblique','optimize','ordered','output','palatino','pixels','plot_to','plotter_on_off','polyfill','pre_depth','pre_xsize','pre_ysize','preview','print_file','pseudo_color','reset_string','resolution',...
	'scale_factor','schoolbook','set_character_size','set_colormap','set_colors','set_font','set_string','set_translation','set_write_mask','static_color','static_gray','symbol','tek4014','tek4100','text','threshold','times','translation','true_color','tt_font',...
	'tty','user_font','vt240','vt241','vt340','vt341','xoffset','xon_xoff','xsize','yoffset','ysize','zapfchancery','zapfdingbats','z_buffering','I2M_pos'};

	I2MnotTranslated = char('avantgarde','average_lines','binary','bits_per_pixel','bkman','bold','book','bypass_translation','close_document','colors','courier','cursor_crosshair','cursor_image','cursor_mask','cursor_original','cursor_standard','cursor_xy','demi','depth','direct_color',...
	'eject','encapsulated','encoding','floyd','font_index','font_size','get_current_font','get_fontnames','get_fontnum','get_page_size','get_visual_depth','get_visual_name','get_write_mask','gin_chars','glyph_cache','helvetica','index_color',...
	'isolatin1','italic','light','medium','narrow','ncar','oblique','optimize','ordered','output','palatino','pixels','plot_to','plotter_on_off','polyfill','pre_depth','pre_xsize','pre_ysize','preview','print_file','pseudo_color','reset_string','resolution',...
	'scale_factor','schoolbook','set_character_size','set_colormap','set_colors','set_font','set_string','set_translation','set_write_mask','static_color','static_gray','symbol','tek4014','tek4100','text','threshold','times','translation','true_color','tt_font',...
	'tty','user_font','vt240','vt241','vt340','vt341','xoffset','xon_xoff','xsize','yoffset','ysize','zapfchancery','zapfdingbats','z_buffering');

	% Variables utilisees
	pos = [];
	% variables SYSTEME
	global i2mvs_d i2mZbuff i2mPs i2mDecomposed i2mvs_p i2mFilename i2mDevPsColor i2mPsCanWrite
	% initialisation des valeurs des keywords specifiques
	closeZ =''; set_resolution = []; window_state = ''; get_screen_size = ''; window_position = ''; decomposed = '';get_decomposed = ''; retain = '';set_graphics_function='';copie='';get_graphics_function='';filename='';couleur='';inches='';
	landscape='';portrait=''; close_file='';
	% initialisation des valeurs des keywords non traduits.
	for i=1:length(I2MnotTranslated)
		tmpString = [I2MnotTranslated(i,:) + '= ''''' ';'];
		eval(tmpString);
	end

	I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% Traitement du keyword /CLOSE
if ~isempty(closeZ)
	if closeZ ~= 0
		if strcmpi(i2mvs_d.name,'Z')
			% device courant = 'Z' --> OK
			delete(i2mZbuff);
		else
			% device courant ~= 'Z' --> pas OK
			error( 'Keyword CLOSE not allowed in call to: DEVICE. Current device should be ''Z''');
		end
	end
end

% Traitement du keyword SET_RESOLUTION
if ~isempty(set_resolution)
	if strcmpi(i2mvs_d.name,'Z')
		% Device courant = 'Z' on peut continuer
		if length(set_resolution) ~= 2
			error(' Keyword array parameter SET_RESOLUTION must have from 2 to 2 elements.');
		else
			% Set_resolution est bien un tableau de 2 elements
			if (set_resolution(1) <= 0) | (set_resolution(2) <= 0)
				error('DEVICE: Value of Resolution is out of allowed range.');
			else
				pos = get(i2mZbuff,'position');
				pos(3) = set_resolution(1);
				pos(4) = set_resolution(2);
				set(i2mZbuff,'position',pos);
			end
		end
	else
		% Device courant ~= 'Z' --> Keyword non autorise
		error( 'Keyword SET_RESOLUTION not allowed in call to: DEVICE. Current device should be ''Z''');
	end
end

% Traitement du keyword WINDOW_STATE
if ~isempty(window_state)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		window_state = zeros(1,65); % initialisation
		winTab = findobj(0,'type','figure'); % On recupere les numeros de toutes les fenetres ouvertes.
		for i=1:length(winTab)
			if winTab(i) <= 65
				window_state(winTab(i)) = 1;
			end
		end
	else
		% Mauvais device courant
		error( 'Keyword WINDOW_STATE not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword GET_SCREEN_SIZE
if ~isempty(get_screen_size)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		set(0,'Units','pixels');
		screenSize = get(0,'ScreenSize');
		get_screen_size = [screenSize(3) screenSize(4)];
	else
		% Mauvais device courant
		error( 'Keyword GET_SCREEN_SIZE not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword GET_WINDOW_POSITION
if ~isempty(window_position)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		set(gcf,'Units','pixels');
		if i2mvs_d.window == -1
			% Il n'y a pas de fenetre courante, on en cree une
			window;
		end
		wPos = get(i2mvs_d.window+1,'position');
		window_position = [wPos(1) wPos(2)];
	else
		% Mauvais device courant
		error( 'Keyword GET_WINDOW_POSITION not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword GET_DECOMPOSED
if ~isempty(get_decomposed)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		get_decomposed = i2mDecomposed;
	else
		% Mauvais device courant
		error( 'Keyword GET_DECOMPOSED not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword /DECOMPOSED
if ~isempty(decomposed)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		if (decomposed <= -1) | (decomposed >= 1)
			if i2mDecomposed ~= 1
				% On n'etait pas dans ce mode
				i2mDecomposed = 1;
				% On va mettre a jour les composantes RGB des couleurs par defaut :
				% pour !P.COLOR
				set(0,'DefaultTextColor',i2m_index2rgb(i2mvs_p.color));
				set(0,'DefaultAxesColorOrder',i2m_index2rgb(i2mvs_p.color));
				set(0,'DefaultAxesXColor',i2m_index2rgb(i2mvs_p.color));
				set(0,'DefaultAxesYColor',i2m_index2rgb(i2mvs_p.color));
				set(0,'DefaultAxesZColor',i2m_index2rgb(i2mvs_p.color));
				set(0,'DefaultSurfaceEdgeColor',i2m_index2rgb(i2mvs_p.color));
				% pour !P.BACKGROUND
				set(0,'DefaultFigureColor',i2m_index2rgb(i2mvs_p.background));
				set(0,'DefaultAxesColor',i2m_index2rgb(i2mvs_p.background));
			end
		else
			if i2mDecomposed ~= 0
				% On n'etait pas dans ce mode
				i2mDecomposed = 0;
				% On va mettre a jour les composantes RGB des couleurs par defaut :
				c = i2mColormap;
				col = c(mod(i2mvs_p.color,256)+1,:);
				% pour !P.COLOR
				set(0,'DefaultTextColor',col);
				set(0,'DefaultAxesColorOrder',col);
				set(0,'DefaultAxesXColor',col);
				set(0,'DefaultAxesYColor',col);
				set(0,'DefaultAxesZColor',col);
				set(0,'DefaultSurfaceEdgeColor',col);
				% pour !P.BACKGROUND
				set(0,'DefaultFigureColor',c(mod(i2mvs_p.background,256)+1,:));
				set(0,'DefaultAxesColor',c(mod(i2mvs_p.background,256)+1,:));
			end
		end
	else
		% Mauvais device courant
		error( 'Keyword DECOMPOSED not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword GET_GRAPHICS_FUNCTION
if ~isempty(get_graphics_function)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		if strcmpi(get(0,'DefaultEraseMode'),'xor')
			get_graphics_function = 7; % Mode GXxor
		else
			get_graphics_function = 3; % Mode par defaut Gxcopy
		end
else
		% Mauvais device courant
		error( 'Keyword GET_GRAPHICS_FUNCTION not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword SET_GRAPHICS_FUNCTION
if ~isempty(set_graphics_function)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		switch set_graphics_function
			case 3
				% Gxcopy mode par defaut
				set(0,'DefaultLineEraseMode','normal');
				set(0,'DefaultImageEraseMode','normal');
				set(0,'DefaultSurfaceEraseMode','normal');
				set(0,'DefaultTextEraseMode','normal');
				set(0,'DefaultPatchEraseMode','normal');
				set(0,'DefaultRectangleEraseMode','normal');
			case 6
				% gxXor mode
				set(0,'DefaultLineEraseMode','xor');
				set(0,'DefaultImageEraseMode','xor');
				set(0,'DefaultSurfaceEraseMode','xor');
				set(0,'DefaultTextEraseMode','xor');
				set(0,'DefaultPatchEraseMode','xor');
				set(0,'DefaultRectangleEraseMode','xor');
			otherwise
				warning('not implemented ! Set to default (Gxcopy)');
				set(0,'DefaultLineEraseMode','normal');
				set(0,'DefaultImageEraseMode','normal');
				set(0,'DefaultSurfaceEraseMode','normal');
				set(0,'DefaultTextEraseMode','normal');
				set(0,'DefaultPatchEraseMode','normal');
				set(0,'DefaultRectangleEraseMode','normal');
		end
	else
		% Mauvais device courant
		error( 'Keyword SET_GRAPHICS_FUNCTION not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword COPY
if ~isempty(copie)
	if strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC')
		% Bon device courant
		if length(copie) == 7
			% La fenetre depuis laquelle on va copier les pixels a ete specifiee
			fenetre = copie(7); % numero de la fenetre depuis laquelle on va copier les pixels
			cLeft = copie(1);
			cBottom = copie(2);
			cWidth = copie(3);
			cHeight = copie(4);
			xd = copie(5);
			yd = copie(6);
			currFig = i2mvs_d.window; % On stocke la figure courante
			wset(fenetre); % On se place dans la fenetre depuis laquelle on doit copier les pixels
			i2mvs_d.window
			get(0,'currentFigure')
			pic = tvrd(cLeft,cBottom,cWidth,cHeight); % On copie les pixels
			wset(currFig); % On se replace dans la fenetre courante
			tv(pic,xd,yd);
		else
			if length(copie) == 6
			% On va copier a partir de la fenetre courante et dans la fenetre courante.

			else
				error('Keyword array parameter COPY must have from 6 to 7 elements.')
			end
		end

	else
		% Mauvais device courant
		error( 'Keyword COPY not allowed in call to: DEVICE. Current device should be ''X'', ''WIN'' or ''MAC''');
	end
end

% Traitement du keyword FILENAME
if ~isempty(filename)
	if strcmpi(i2mvs_d.name,'CGM') | strcmpi(i2mvs_d.name,'HP') | strcmpi(i2mvs_d.name,'LJ') | strcmpi(i2mvs_d.name,'PCL') | strcmpi(i2mvs_d.name,'PS') | strcmpi(i2mvs_d.name,'REGIS') | strcmpi(i2mvs_d.name,'TEK')
		% Bon device courant
		col = '';
		if i2mDevPsColor == 1
			col = 'c';
		end
		if i2mPsCanWrite == 1
			% On ecrase le fichier precedent
			eval(['print -dps' col ' ' i2mFilename ';']);
		else
			if i2mPsCanWrite ~= 0
			% On enregistre la fenetre 'ps' a la fin du fichier ps de nom i2mFilename (l'ancien)
			eval(['print -dps' col ' -append ' i2mFilename ';']);
			else
				% rien
			end
		end
		i2mFilename = filename;
		i2mPsCanWrite = 0;
	else
		% Mauvais device courant
		error( 'Keyword FILENAME not allowed in call to: DEVICE. Current device should be ''CGM'', ''HP'', ''LJ'', ''PCL'', ''PS'', ''REGIS'' or ''TEK''');
	end
end

% Traitement du keyword /COLOR
if ~isempty(couleur)
	if strcmpi(i2mvs_d.name,'PCL') | strcmpi(i2mvs_d.name,'PS')
		% Bon device courant
		if strcmpi(i2mvs_d.name,'PS')
			i2mDevPsColor = couleur;
		else
			warning('Translation of keyword COLOR for the current device is not yet implemented');
		end
	else
		% Mauvais device courant
		error( 'Keyword COLOR not allowed in call to: DEVICE. Current device should be ''PCL'' or ''PS''');
	end
end

% Traitement du keyword /INCHES
if ~isempty(inches)
	if strcmpi(i2mvs_d.name,'HP') | strcmpi(i2mvs_d.name,'PS') | strcmpi(i2mvs_d.name,'LJ') | strcmpi(i2mvs_d.name,'PCL') | strcmpi(i2mvs_d.name,'PRINTER')
		% Bon device courant
		if strcmpi(i2mvs_d.name,'PS')
			if inches ~= 0
				set(i2mPs,'PaperUnits','inches');
			else
				set(i2mPs,'PaperUnits','centimeters');
			end
		else
			warning('Translation of keyword INCHES for the current device is not yet implemented');
		end
	else
		% Mauvais device courant
		error( 'Keyword INCHES not allowed in call to: DEVICE. Current device should be ''HP'', ''PS'', '' LJ'', ''PCL'' or ''PRINTER''');
	end
end

% Traitement du keyword /PORTRAIT
if ~isempty(portrait)
	if strcmpi(i2mvs_d.name,'HP') | strcmpi(i2mvs_d.name,'PS') | strcmpi(i2mvs_d.name,'LJ') | strcmpi(i2mvs_d.name,'PCL') | strcmpi(i2mvs_d.name,'PRINTER')
		% Bon device courant
		if strcmpi(i2mvs_d.name,'PS')
			if portrait ~= 0
				set(i2mPs,'PaperOrientation','portrait');
			else
				set(i2mPs,'PaperOrientation','landscape');
			end
		else
			warning('Translation of keyword PORTRAIT for the current device is not yet implemented');
		end
	else
		% Mauvais device courant
		error( 'Keyword PORTRAIT not allowed in call to: DEVICE. Current device should be ''HP'', ''PS'', '' LJ'', ''PCL'' or ''PRINTER''');
	end
end

% Traitement du keyword /LANDSCAPE
if ~isempty(landscape)
	if strcmpi(i2mvs_d.name,'HP') | strcmpi(i2mvs_d.name,'PS') | strcmpi(i2mvs_d.name,'LJ') | strcmpi(i2mvs_d.name,'PCL') | strcmpi(i2mvs_d.name,'PRINTER')
		% Bon device courant
		if strcmpi(i2mvs_d.name,'PS')
			if portrait ~= 0
				set(i2mPs,'PaperOrientation','landscape');
			else
				set(i2mPs,'PaperOrientation','portrait');
			end
		else
			warning('Translation of keyword LANDSCAPE for the current device is not yet implemented');
		end
	else
		% Mauvais device courant
		error( 'Keyword LANDSCAPE not allowed in call to: DEVICE. Current device should be ''HP'', ''PS'', '' LJ'', ''PCL'' or ''PRINTER''');
	end
end

% Traitement du keyword /CLOSE_FILE
if ~isempty(close_file)
	if strcmpi(i2mvs_d.name,'HP') | strcmpi(i2mvs_d.name,'PS') | strcmpi(i2mvs_d.name,'LJ') | strcmpi(i2mvs_d.name,'PCL') | strcmpi(i2mvs_d.name,'PRINTER')
		% Bon device courant
		if strcmpi(i2mvs_d.name,'PS')
			if close_file ~= 0
				% On veut fermer le fichier ps courant.
				% On enregistre ce qu'il y a actuellement dans la fenetre 'ps'
				col = '';
				if i2mPsCanWrite ~= 0
					if i2mDevPsColor == 1
						col = 'c';
					end
					if i2mPsCanWrite == 1
						% On ecrase le fichier precedent
						eval(['print -dps' col ' ' i2mFilename ';']);
					else
						% On enregistre la fenetre 'ps' a la fin du fichier ps de nom i2mFilename
						eval(['print -dps' col ' -append ' i2mFilename ';']);
					end
				end
				i2mFilename = 'idl'; % On remet le nom de fichier ps par defaut.
				i2mPsCanWrite = 0; % on n'enregistrera pas le contenu de la fenetre ps lors du prochain appel a devicePS par une fonctions graphique du type plot, shade_surf,...
			else
				% rien close_file = 0, on ne veut pas fermer le fichier.
			end
		else
			warning('Translation of keyword CLOSE_FILE for the current device is not yet implemented');
		end
	else
		% Mauvais device courant
		error( 'Keyword LANDSCAPE not allowed in call to: DEVICE. Current device should be ''HP'', ''PS'', '' LJ'', ''PCL'' or ''PRINTER''');
	end
end

%%%% Traitement des keywords non traduits
for i=1:length(I2MnotTranslated)
	if ~isempty(eval(I2MnotTranslated(i,:)))
		tmpString = ['warning(''Function DEVICE: keyword ' upper(I2MnotTranslated(i,:)) 'not translated '');'];
		eval(tmpString);
	end
end

if I2M_out; eval(I2M_out); end;





