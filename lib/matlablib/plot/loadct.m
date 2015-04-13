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
% Modifications : 11 / 08 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ok sauf FILE

function [varargout]=loadct(varargin)

I2Mkwn=char('I2M_a1' , 'bottom', 'file',  'get_names' , 'ncolors' ,'silent' , 'I2M_pos');
I2Mkwv=    {'table'  , 'bas', 'fichier',  'nomsTables', 'nbColors','silence', 'I2M_pos'};

table = []; bas = []; fichier = []; nomsTables = 0.128; nbColors=[]; silence = [];

    I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

global i2mvs_d i2mvs_p

global B_W_LINEAR BLUE_WHITE GRN_RED_BLU_WHT RED_TEMPERATURE BLUE_GREEN_RED_YELLOW STD_GAMMA_II PRISM RED_PURPLE GREEN_WHITE_LINEAR GRN_WHT_EXPONENTIAL
global GREEN_PINK BLUE_RED LEVEL_16 RAINBOW STEPS STERN_SPECIAL Haze Blue_Pastel_Red Pastels Hue_Sat_Lightness_1 Hue_Sat_Lightness_2
global Hue_Sat_Value_1 Hue_Sat_Value_2 Purple_Red_Stripes Beach Mac_Style Eos_A Eos_B Hardcandy Nature Ocean Peppermint Plasma Blue_Red Rainbow
global Blue_Waves Volcano Waves Rainbow18 Rainbow_white Rainbow_black

tables_indices = {'B_W_LINEAR', 'BLUE_WHITE', 'GRN_RED_BLU_WHT', 'RED_TEMPERATURE', 'BLUE_GREEN_RED_YELLOW', 'STD_GAMMA_II', 'PRISM', 'RED_PURPLE', 'GREEN_WHITE_LINEAR', 'GRN_WHT_EXPONENTIAL',...
'GREEN_PINK', 'BLUE_RED', 'LEVEL_16', 'RAINBOW', 'STEPS', 'STERN_SPECIAL', 'Haze', 'Blue_Pastel_Red', 'Pastels', 'Hue_Sat_Lightness_1', 'Hue_Sat_Lightness_2',...
'Hue_Sat_Value_1', 'Hue_Sat_Value_2', 'Purple_Red_Stripes', 'Beach', 'Mac_Style', 'Eos_A', 'Eos_B', 'Hardcandy', 'Nature', 'Ocean', 'Peppermint', 'Plasma', 'Blue_Red', 'Rainbow', ...
'Blue_Waves', 'Volcano', 'Waves', 'Rainbow18', 'Rainbow_white', 'Rainbow_black'};

if ~isempty(fichier), warning('Function LOADCT : keyword FILE not translated'); end

if isempty(nomsTables) | (nomsTables ~= 0.128), % GET_NAMES a ete passe en parametres
   nomsTables = 'B_W_LINEAR BLUE_WHITE GRN_RED_BLU_WHT RED_TEMPERATURE BLUE_GREEN_RED_YELLOW STD_GAMMA_II PRISM RED_PURPLE GREEN_WHITE_LINEAR GRN_WHT_EXPONENTIAL GREEN_PINK BLUE_RED LEVEL_16 RAINBOW STEPS STERN_SPECIAL Haze Blue_Pastel_Red Pastels Hue_Sat_Lightness_1 Hue_Sat_Lightness_2 Hue_Sat_Value_1 Hue_Sat_Value_2 Purple_Red_Stripes Beach Mac_Style Eos_A Eos_B Hardcandy Nature Ocean Peppermint Plasma Blue_Red Rainbow Blue_Waves Volcano Waves Rainbow18 Rainbow_white Rainbow_black';

elseif (table >= 0) & (table <= 40),
	eval(['tbcl = ' tables_indices{table+1} '; ']);
	
	if ~isempty(nbColors), nbcol=nbColors; else, nbcol = 256; end; %or =i2mvs_d.table_size;
	if ~isempty(bas),      first= bas+1;   else, first = 1;   end;
	
	if ((first>1 & first<256) | (nbcol>1 & nbcol<256)) & (first+nbcol-1 <=256),
	    tblc(:,1)=congrid(tblc(first:first+nbcol-1,1),256);
	    tblc(:,2)=congrid(tblc(first:first+nbcol-1,2),256);
	    tblc(:,3)=congrid(tblc(first:first+nbcol-1,3),256);
	end;
	i2mColormap(tbcl);
	if (isempty(silence) | (silence==0)), disp(['LOADCT: Loading table ' tables_indices{table+1}]); end;
end

if ~isempty(I2M_out); eval(I2M_out); end;
