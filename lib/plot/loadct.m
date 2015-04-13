% loadct
% --------------------------------------------
% Equivalent to :
%                LOADCT [, Table] 
%                       [, BOTTOM=value]
%                       [, FILE=string]
%                       [, GET_NAMES=variable]
%                       [, NCOLORS=value]
%                       [, /SILENT]
% in IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : loadct
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 12 / 05 / 2003
% Modifications : 13 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Ok sauf FILE qui n'a pas d'equivalent en matlab

function [varargout]=loadct(varargin)

I2Mkwn=char('I2M_a1' , 'bottom', 'file',  'get_names' , 'ncolors','silent', 'I2M_pos');
    I2Mkwv= {'table' , 'bas', 'fichier', 'nomsTables', 'nbColors','silence','I2M_pos'};

		% variables utilisées
		global B_W_LINEAR BLUE_WHITE GRN_RED_BLU_WHT RED_TEMPERATURE BLUE_GREEN_RED_YELLOW STD_GAMMA_II PRISM RED_PURPLE GREEN_WHITE_LINEAR GRN_WHT_EXPONENTIAL
		global GREEN_PINK BLUE_RED LEVEL_16 RAINBOW STEPS STERN_SPECIAL Haze Blue_Pastel_Red Pastels Hue_Sat_Lightness_1 Hue_Sat_Lightness_2
		global Hue_Sat_Value_1 Hue_Sat_Value_2 Purple_Red_Stripes Beach Mac_Style Eos_A Eos_B Hardcandy Nature Ocean Peppermint Plasma Blue_Red Rainbow
		global Blue_Waves Volcano Waves Rainbow18 Rainbow_white Rainbow_black
    table = [];
		% variables systeme
		global i2mvs_d i2mvs_p
		% keywords
		bas = []; fichier = []; nomsTables = []; nbColors=[];
		% keywords présent si =1
		silence = [];
    % keywords graphiques
		% Variables locales

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

tables_indices = {'B_W_LINEAR', 'BLUE_WHITE', 'GRN_RED_BLU_WHT', 'RED_TEMPERATURE', 'BLUE_GREEN_RED_YELLOW', 'STD_GAMMA_II', 'PRISM', 'RED_PURPLE', 'GREEN_WHITE_LINEAR', 'GRN_WHT_EXPONENTIAL',...
'GREEN_PINK', 'BLUE_RED', 'LEVEL_16', 'RAINBOW', 'STEPS', 'STERN_SPECIAL', 'Haze', 'Blue_Pastel_Red', 'Pastels', 'Hue_Sat_Lightness_1', 'Hue_Sat_Lightness_2',...
'Hue_Sat_Value_1', 'Hue_Sat_Value_2', 'Purple_Red_Stripes', 'Beach', 'Mac_Style', 'Eos_A', 'Eos_B', 'Hardcandy', 'Nature', 'Ocean', 'Peppermint', 'Plasma', 'Blue_Red', 'Rainbow', ...
'Blue_Waves', 'Volcano', 'Waves', 'Rainbow18', 'Rainbow_white', 'Rainbow_black'};

if ~isempty(fichier)
	warning('Function LOADCT : keyword FILE not translated (no equivalent in Matlab');
end

if ~isempty(nomsTables)
	% GET_NAMES a ete passe en parametres
	nomsTables = 'B_W_LINEAR BLUE_WHITE GRN_RED_BLU_WHT RED_TEMPERATURE BLUE_GREEN_RED_YELLOW STD_GAMMA_II PRISM RED_PURPLE GREEN_WHITE_LINEAR GRN_WHT_EXPONENTIAL GREEN_PINK BLUE_RED LEVEL_16 RAINBOW STEPS STERN_SPECIAL Haze Blue_Pastel_Red Pastels Hue_Sat_Lightness_1 Hue_Sat_Lightness_2 Hue_Sat_Value_1 Hue_Sat_Value_2 Purple_Red_Stripes Beach Mac_Style Eos_A Eos_B Hardcandy Nature Ocean Peppermint Plasma Blue_Red Rainbow Blue_Waves Volcano Waves Rainbow18 Rainbow_white Rainbow_black';
else
	% GET_NAMES n'a pas ete passe en parametres
	if ~isempty(table)
		% l'indice de la table a ete passe en argument
		if (table >= 0) &  (table <= 40)
			if isempty(bas) & isempty(nbColors)
				% Les keywords BOTTOM et NBCOLORS n'ont pas ete precises => On charge toute la table
				eval(['tbcl = ' tables_indices{table+1} '; ']); % tbcl = la table de couleur d'indice table (+1 en Matlab)
				i2mColormap(tbcl);
			else
				% NCOLORS ou BOTTOM a ete precise
				if ~isempty(nbColors)
					% NCOLORS a ete passe en parametres
					nbCol = nbColors;
				else
					% NCOLORS n'a pas ete passe en parametres
					nbCol = i2mvs_d.table_size;
				end % end if ~isempty(nbColors)
				if ~isempty(bas)
					% BOTTOM a ete passe en parametres
					first = bas+1;
				else
					% BOTTOM n'a pas ete passe en parametres
					first = 1;
				end % end if ~isempty(bas)
				c = i2mColormap; % On recupere la table de couleurs 'IDL' actuelle
				eval(['tbcl = ' tables_indices{table+1} '; ']); % tbcl = la table de couleur d'indice table (+1 en Matlab)
				inter = nbCol-first;
				if nbCol+first <= length(c)
					for i=first:first+nbCol-1
						c(i,:) = tbcl(int16((length(c)-1)*(i-first)/(nbCol-1)+1),:);
					end % for
				else
					for i=first:length(c)
						c(i,:) = tbcl(int16((length(c)-1)*(i-first)/(length(c)-first)+1),:);
					end %  for
				end % if nbCol+first <= length(colormap)
				i2mColormap(c);
			end % if isempty(bas) & isempty(nbColors)
			if (isempty(silence) | (silence == 0))
				s = ['LOADCT: Loading table ' tables_indices{table+1}];
				disp(s);
			end
		else
			error('LOADCT: Table number must be from 0 to 40')
		end
	else
			% Aucun indice de table de couleurs n'a ete precise
		disp(sprintf(' 0-        B-W LINEAR   14-             STEPS   28-         Hardcandy\n 1-        BLUE/WHITE   15-     STERN SPECIAL   29-            Nature\n 2-   GRN-RED-BLU-WHT   16-              Haze   30-             Ocean\n 3-   RED TEMPERATURE   17- Blue - Pastel - R   31-        Peppermint\n 4- BLUE/GREEN/RED/YE   18-           Pastels   32-            Plasma\n 5-      STD GAMMA-II   19- Hue Sat Lightness   33-          Blue-Red\n 6-             PRISM   20- Hue Sat Lightness   34-           Rainbow\n 7-        RED-PURPLE   21-   Hue Sat Value 1   35-        Blue Waves\n 8- GREEN/WHITE LINEA   22-   Hue Sat Value 2   36-           Volcano\n 9- GRN/WHT EXPONENTI   23- Purple-Red + Stri   37-             Waves\n10-        GREEN-PINK   24-             Beach   38-         Rainbow18\n11-          BLUE-RED   25-         Mac Style   39-   Rainbow + white\n12-          16 LEVEL   26-             Eos A   40-   Rainbow + black\n13-           RAINBOW   27-             Eos B'));
		t = input('Enter table number: ');
		varargin{length(varargin)+1} = 'I2M_a1';
		varargin{length(varargin)+1} = t;
		loadct(varargin{:});
	end
end

if I2M_out; eval(I2M_out); end;
