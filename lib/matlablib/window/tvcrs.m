% tvcrs
% --------------------------------------------
% Equivalent to :
% function TVCRS
%           ou
%          TVCRS [, X, Y] , /CENTIMETERS |/INCHES
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
% Fonction : TVCRS
%            Permet de placer le pointeur a un endroit donne de la figure courante.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 14 / 04 / 2003
% Modifications : 15 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:
% ON_OFF : non traite. A quoi ca sert en IDL?
% /HIDE_CURSOR : non traite
% Dans le cas ou gcf est en icone, il faudrait que le curseur se place en haut a gauche => A faire.
% Pb: pas moyen de savoir quand une fenetre Matlab est en icone.

function [varargout]=tvcrs(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'centimeters' ,'inches' , 'hide_cursor' , 't3d' ,'I2M_pos');
I2Mkwv=    {'X'      , 'Y'      , 'centimeterss','inchess', 'hide_cursorr','t3dKG','I2M_pos'};


X='';Y='';
% keywords

% keywords pr�ent si =1
centimeterss=''; inchess='';hide_cursorr='';
% keywords graphiques
t3dKG='';

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;


% traitement de ON_OFF
if isempty(Y)
    %figure
    warning('Function TVCRS : ON_OFF not translated');
    return;
end;

% Sauvegarde des unites de root de gcf
unitr=get(0,'Units');
unitf=get(gcf,'Units');


% Traitement de /CENTIMETERS et /INCHES
if ~isempty(centimeterss)
    set(0,'Units','centimeters');
    set(gcf,'Units','centimeters');
end;
if ~isempty(inchess)
    set(0,'Units','inches');
    set(gcf,'Units','inches');
end;

% Positionnement du pointeur
pos=get(gcf,'Position');
set(0,'PointerLocation',[X+pos(1) Y+pos(2)]);

% Remise en etat initial de Units de root et gcf
set(gcf,'Units',unitf);
set(0,'Units',unitr);

% Traitement de /HIDE_CURSOR
if ~isempty(hide_cursorr)
    warning('Function TVCRS : keyword /HIDE_CURSOR not translated (no equivalent in Matlab)');
end;

%%%%%%% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%%%%%%%%
% Traitement de /T3D
i2m_t3d(t3dKG);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if I2M_out; eval(I2M_out); end;
