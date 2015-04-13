% polyfill
% --------------------------------------------
% Equivalent to :
% function POLYFILL, X [, Y [, Z]]
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
% Fonction : POLYFILL
%            
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 19 / 05 / 2003
% Modifications : 15 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remarques:
% Les 6 keywords ne sont pas traduits. Apparemment, il n'y a pas d'equivalents Matlab.
% => A verifier.

function [varargout]=polyfill(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' ,'image_coord' ,'image_interp' ,'line_fill' ,'pattern' ,'spacing' ,'transparent' ,...
        'color'      ,'data' ,'device' ,'normal' , 't3d' ,'I2M_pos');
I2Mkwv=    {'x'      , 'y'      , 'z'      ,'image_coordd','image_interpp','line_filll','patternn','spacingg','transparentt',...
        'colorIndice','dataa','devicee','normall','t3dKG', 'I2M_pos'};

% variables utilisées
% keywords
x=[];y=[];z=[];p=[];
image_coordd=[]; patternn=[];spacingg='';transparentt='';
colorIndice='';dataa='';devicee='';normall='';
% keywords présent si =1
image_interpp=''; line_filll='';
% Keywords Graphiques
t3dKG='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% Si la color n'est pas specifiee, on colorie la surface avec la couleur ayant l'indice le plus grand indice
% sauf si c'est la couleur du fond. Dans ce cas, on prend la couleur avec le plus petit indice.
if colorIndice==''
    colorIndice=255;
    global i2mvs_p
    bg=i2mvs_p.background;
    if bg==colorIndice
        colorIndice=0;
    end;
end;

% Affichage du graphe dans le cas ou on a x et y
% On colorie la surface en blanc, on modifiera la couleur lors de l'appel a la fonction color.
if isempty(z)
    h=fill(x,y,'w');
else
    %Affichage du graphe dans le cas ou on a x, y et z
    h=fill3(x,y,z,'w');
end;

% Traitement de /LINE_FILL, SPACING,IMAGE_COORD,IMAGE_INTERP,PATTERN,TRANSPARENT
non_traite(line_filll,spacingg,image_coordd,image_interpp,patternn,transparentt);

% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%%%%%%%%
i2m_color(colorIndice,h,'polyfill');
if isempty(dataa) & isempty(devicee) & isempty(normall)
    dataa=1;
end;
i2m_device(devicee,h,'polyfill');
i2m_data(dataa,h,'polyfill');
i2m_normal(normall,h,'polyfill');
% Traitement de /T3D
i2m_t3d(t3dKG);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if I2M_out; eval(I2M_out); end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Fonction interne: NON_TRAITE(line_filll,spacingg,image_coordd,image_interpp,patternn,transparentt)
%Permet de traiter les keywords LINE_FILL, SPACING
%Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(line_filll,spacingg,image_coordd,image_interpp,patternn,transparentt)

if ~isempty(line_filll)
    warning('Function POLYFILL: keyword LINE_FILL : not translated (no equivalent in Matlab)');
end;

if ~isempty(spacingg)
    warning('Function POLYFILL: keyword SPACING : not translated (no equivalent in Matlab)');
end;

if ~isempty(image_coordd)
    warning('Function POLYFILL: keyword IMAGE_COORD : not translated (no equivalent in Matlab)');
end;

if ~isempty(image_interpp)
    warning('Function POLYFILL: keyword IMAGE_INTERP : not translated (no equivalent in Matlab)');
end;

if ~isempty(patternn)
    warning('Function POLYFILL: keyword PATTERN : not translated (no equivalent in Matlab)');
end;

if ~isempty(transparentt)
    warning('Function POLYFILL: keyword TRANSPARENT : not translated (no equivalent in Matlab)');
end;  

