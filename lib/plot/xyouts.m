% xyouts
% --------------------------------------------
% Equivalent to :
% function XYOUTS, [X, Y,] String
%                       [, ALIGNMENT=value{0.0 to 1.0}]
%                       [, CHARSIZE=value]
%                       [, CHARTHICK=value]
%                       [, TEXT_AXES={0 | 1 | 2 | 3 | 4 | 5}]
%                       [, WIDTH=variable]
% en IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction xyouts
% Auteur :
%                 Bourtembourg Reynald
% Date creation : 03 / 04 / 2003
% Modifications : 19 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% faire attention au systeme de coordonnees
% en 3D, la valeur de Z est specifiee avec le keyword Z
% If the optional X and Y arguments are omitted, the text is positioned at the end of the most recently output text string.
% REMARQUE : l'alignement ne peut pas etre traite aussi precisemment qu'en IDL car en Matlab, on a que 3 valeurs possibles (left, centered et right)
% WIDTH n'a pas d'equivalent en Matlab
% TRAITER LE CAS OU L'ARGUMENT TEXTE EST UN TABLEAU !!

function [varargout]=xyouts(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3', 'alignment' , 'charsize'  , 'charthick' , 'text_axes' , 'width' ,'orientation' , 'color' ,'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'texte' , 'align'     , 'taillecar' , 'epaisscar' , 'axes_texte', 'taille',  'angle','colorIndice','I2M_pos'};

	% Variables utilisees
	% variables SYSTEME
	global i2mvs_p
	% initialisation des valeurs des keywords specifiques
	d1 = []; d2 = []; texte = '';
	align = [];
	taillecar = [];
	epaisscar = [];
	axes_texte = [];
	taille = [];
	% Keywords graphiques
	angle = ''; colorIndice = '';

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

nbvar = 1;
theText = '';


if ~isempty(texte)
	nbvar = 3;

	if n_elements(texte) == 1
		% La variable texte n'est pas un tableau

		theText = mat2str(texte); % pour gerer le cas ou l'on veut afficher la valeur d'une variable
		theText = strrep(theText,'!C',sprintf('\n')); % Pour gerer les sauts de lignes d'IDL
		theText = strrep(theText,'!!','!'); % Pour pouvoir afficher les '!'

		mini = min(length(d1),length(d2));
		for i=1:mini
			% On affiche le texte aux mini premiers couples (d1(i),d2(i))
			txt = text(d1(i),d2(i),theText);
			% Gestion des keywords :
			if length(colorIndice) <= 1
				GestionKeywords (txt,1,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
			else
				if length(colorIndice) >= mini
					GestionKeywords (txt,i,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
				else
					error('Array in keyword COLOR doesn''t have enough elements');
				end
			end % if
		end % for
	else
		% La variable texte est un tableau
		mini = min(min(length(d1),length(d2)),n_elements(texte));
		for i=1:mini
			% On affiche les mini premiers elements de la variable texte a leur positions associees
			theText = mat2str(texte(i)); % pour gerer le cas ou l'on veut afficher la valeur d'une variable
			theText = strrep(theText,'!C',sprintf('\n')); % Pour gerer les sauts de lignes d'IDL
			theText = strrep(theText,'!!','!'); % Pour pouvoir afficher les '!'
			txt = text(d1(i),d2(i),theText);
			% Gestion des keywords :
			if length(colorIndice) <= 1;
				GestionKeywords (txt,1,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
			else
				if length(colorIndice) >= mini
					GestionKeywords (txt,i,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
				else
					error('Array in keyword COLOR doesn''t have enough elements');
				end
			end % if
		end % for
	end % if
else
	% Le texte a afficher se trouve dans la variable d1
	if n_elements(d1) == 1
		% La variable d1 n'est pas un tableau

		theText = mat2str(d1); % pour gerer le cas ou l'on veut afficher la valeur d'une variable
		theText = strrep(theText,'!C',sprintf('\n')); % Pour gerer les sauts de lignes d'IDL
		theText = strrep(theText,'!!','!'); % Pour pouvoir afficher les '!'

		txt = text(0,0,theText);
		GestionKeywords (txt,1,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
	else
		% La variable d1 est un tableau
		mini = n_elements(d1);
		for i=1:mini
			% On affiche les mini premiers elements de la variable texte a leur positions associees
			theText = mat2str(d1(i)); % pour gerer le cas ou l'on veut afficher la valeur d'une variable
			theText = strrep(theText,'!C',sprintf('\n')); % Pour gerer les sauts de lignes d'IDL
			theText = strrep(theText,'!!','!'); % Pour pouvoir afficher les '!'
			txt = text(0,0,theText);
			% Gestion des keywords :
			if length(colorIndice) <= 1
				GestionKeywords (txt,1,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
			else
				if length(colorIndice) >= mini
					GestionKeywords (txt,i,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille);
				else
					error('Array in keyword COLOR doesn''t have enough elements');
				end
			end % if
		end % for
	end % if n_elements(d1) == 1
end % if ~isempty(text)

% On met a jour la valeur de ![XYZ].CRANGE
majCrange;

if I2M_out; eval(I2M_out); end;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FONCTION INTERNE POUR GERER LES KEYWORDS SUR UN OBJET DE TYPE TEXT %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [varargout] = GestionKeywords (txt,i,colorIndice,align,taillecar,epaisscar,axes_texte,angle,taille)
% txt = l'objet text a considerer
% i = indice de l'objet text considere ( pour gerer le cas ou il y a des tableaux de couleurs

global i2mvs_p


% traitement du keyword ALIGNMENT
if ~isempty(align)
 align
	if (align >= 0.0) & (align < 0.25 )
		set(txt,'HorizontalAlignment', 'left');
	else
		if (align >= 0.25) & (align <= 0.75)
			set(txt,'HorizontalAlignment','center');
		else
			if (align > 0.75) & (align <= 1.0)
				set(txt,'HorizontalAlignment','right');
			else
				error('Bad value for keyword ALIGNMENT=value{0.0 to 1.0}')
			end
		end
	end
end

% traitement du keyword CHARSIZE
if ~isempty(taillecar)
	if i2mvs_p.font ~= 0
		% Alors le keyword charsize a un sens
		set(txt,'FontUnits','points'); % si jamais la valeur par defaut de FontUnits avait ete modifiee
		if (taillecar > 0)
			set(txt,'FontSize',10*taillecar);
		else
			% cas ou taillecar <= 0
			if taillecar == 0
				set(txt,'FontSize',10)
			else
				% cas ou taillecar < 0 => on n'affiche rien
				set(txt,'Visible','off')
			end
		end
	end
end

% traitement du keyword CHARTHICK
if ~isempty(epaisscar)
	if i2mvs_p.font ~= 0
		% Alors le keyword charthick a un sens
		if epaisscar >= 2
			set(txt,'FontWeight','bold');
		else
			set(txt,'FontWeight','normal');
		end
	else
		% Le keyword n'a pas de sens => on ne fait rien
	end
end

% traitement du keyword TEXT_AXES
if ~isempty(axes_texte)
	if i2mvs_p.font ~= 0
		warning('Function XYOUTS : keyword TEXT_AXES not translated (no equivalent in Matlab');
	else
		% ce keyword n'a aucun effet en IDL quand !P.FONT == 0
	end
end

% traitement du keyword WIDTH
if ~isempty(taille)
	warning('Function XYOUTS : Keyword WIDTH not translated (no equivalent in Matlab');
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%%%%%%%%
% traitement du keyword ORIENTATION
i2m_orientation(angle,txt);
% traitement du keyword COLOR
if ~isempty(colorIndice)
	i2m_color(colorIndice(i),txt,'xyouts');
end
