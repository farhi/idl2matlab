% wshow
% --------------------------------------------
% Equivalent to :
% function WSHOW [, Window_Index [, Show]] , /ICONIC
% en IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : WSHOW
%            Permet de montrer ou cacher une fenetre donnee.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 11 / 04 / 2003
% Modifications : 06 / 06 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Remarques:
%Apparemment, il ya un traitement special dans le cas ou l'index de fenetre est un identifiant de widget => A voir.
%ICONIC : pas possible en Matlab

function [varargout]=wshow(varargin)

global i2mvs_d
% On verifie que le device courant est bien le bon, si ce n'est pas le cas, on renvoie tout de suite une erreur
if ~(strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC'))
	% Cette procedure n'est pas definie pour le device courant
	error(' WINDOW: Routine is not defined for current graphics device.');
end

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'iconic' , 'I2M_pos');
I2Mkwv=    {'d1'     , 'd2'     , 'iconicc', 'I2M_pos'};

%variables utilisées
%arguments
d1=''; d2=1;
%keywords
%keywords présent si =1
iconicc='';

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global i2mvs_d

%Choix de la figure a traiter
if isempty(d1)
    try, fig=get(i2mvs_d.window,'parent'); catch, fig=0; end;
else
    fig=get(d1 ,'parent');
end;

%Traitement de d2, pour rendre visible ou non la fenetre
if d2==1
    set(fig,'Visible','on');
else
    set(fig,'Visible','off');
end;

%Traitement de /ICONIC
if ~isempty(iconicc)
    warning('Fonction WSHOW: keyword ICONIC non traduit (pas d''equivalent Matalb)');
end;


if I2M_out; eval(I2M_out); end;
