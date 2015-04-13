% window
% --------------------------------------------
% Equivalent to :
% function WINDOW [, Window_Index], TITLE='...'
%                                 , /FREE
%                                 , /PIXMAP
%                                 , COLORS=int
%                                 , RETAIN={0 | 1 | 2}
%                                 , XPOS=int
%                                 , YPOS=int
%                                 , XSIZE=int
%                                 , YSIZE=int
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
% Fonction : WINDOW
%            Permet de creer une figure et de la placer a un endroit donne.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 10 / 04 / 2003
% Modifications : 06 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Remarques:
%FREE : pas besoin de le traiter en Matlab, le fait automatiquement
%RETAIN : non traite
%PIXMAP : rend la fenetre invisible
%XPOS,YPOS,XSIZE,YSIZE :
%Le point d'origine n'est pas le meme en IDL et en Matlab.
%Il faut recalculer XPOS et YPOS mais il faut tenir compte de la taille de la barre de menu.
%actuellement : ne traite que les 2 cas suivant:
%                               - pas de barre de menu : OK
%                               - barre de menu sur 2 lignes : OK
%Reste a traiter les cas ou la barre de menu tient sur + ou - de 2 lignes. Pb: pas moyen de
%trouver la taille en hauteur de la barre de menu.

function [varargout]=window(varargin)

I2Mkwn=char('I2M_a1' , 'colors' , 'free' , 'pixmap' , 'retain' , 'title' , 'xpos' , 'ypos' , 'xsize' , 'ysize' , 'I2M_pos');
I2Mkwv=    {'d1'     , 'colorss', 'freee', 'pixmapp', 'retainn', 'titlee', 'xposs', 'yposs', 'xsizee', 'ysizee', 'I2M_pos'};

global i2mvs_d

% On verifie que le device courant est bien le bon, si ce n'est pas le cas, on renvoie tout de suite une erreur
if ~(strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC'))
	% Cette procedure n'est pas definie pour le device courant
	error(' WINDOW: Routine is not defined for current graphics device.');
end

% variables utilisées
% arguments
d1='';
% keywords
colorss='';retainn='';titlee='';
xposs='';
yposs='';
xsizee=i2mvs_d.x_size;
ysizee=i2mvs_d.y_size;
% keywords présent si =1
freee='';pixmapp='';

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;



if ~isempty(d1)
    f=figure(d1+1);
		i2mvs_d.window = d1;
		set(f,'NumberTitle','off');
    set(f,'Name','Figure N.' + int2str(d1));
else
    f=figure;
end;

% Traitement de TITLE
if ~isempty(titlee)
    set(f,'Name',titlee);
    % Si on ne met pas cette propriete a faux, la fenetre aura le nom: Figure N° title
    set(f,'NumberTitle','off');
end;

% Traitement de XPOS, YPOS, XSIZE, YSIZE
% En IDL, (0,0)=en haut a gauche
% En Matlab, (1,1)=en bas a gauche
% => Il faut recalculer xposs et yposs en fonction de la taille de l'ecran
if (~isempty(xposs) | ~isempty(yposs) | ~isempty(xsizee) | ~isempty(ysizee))
    position=get(f,'Position');
    if ~isempty(xposs)
        position(1)=xposs+1;
    end;
    if ~isempty(yposs)
        tailleEcran=get(0,'ScreenSize');
        %consty=taille de l'en-tete : on teste si le menu est present ou non
        if (get(f,'MenuBar')=='none')
            consty=22;
        else
            consty=77;
        end;
        position(2)=tailleEcran(4)-yposs-ysizee-consty;
    end;
    if ~isempty(xsizee)
        position(3)=xsizee;
    end;
    if ~isempty(ysizee)
        position(4)=ysizee;
    end;
    set(f,'Position',position);
end;

% Traitement de /PIXMAP
if ~isempty(pixmapp)
    set(f,'Visible','off');
end;

% Traitement de RETAIN
if ~isempty(retainn)
    warning('Fonction WINDOW: keyword RETAIN non traduit');
end;

if I2M_out; eval(I2M_out); end;
