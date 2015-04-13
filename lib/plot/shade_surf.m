% shade_surf
% --------------------------------------------
% Equivalent to :
% function SHADE_SURF, Z,[X,Y] , MIN_VALUE=real
%                           , MAX_VALUE=real
%                           , AX=degree
%                           , AZ=degree
%                           , IMAGE=variable
%                           , /SAVE
%                           , SHADES=[...]
%                           , /XLOG
%                           , /YLOG
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
% Fonction : SHADE_SURF
%            Permet de tracer des surfaces remplies.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 16 / 04 / 2003
% Modifications : 16 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:

% A FAIRE :
% PIXELS :  a faire. Depend de DEVICE

% COMMENTAIRES SUR LES KEYWORDS TRADUITS :
% Revoir SHADES quand on aura traite les couleurs.



function [varargout]=surfacee(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'ax' ,'az' ,'image' ,'pixels' ,'save' ,'shades' ,'max_value' ,'min_value','xlog' ,...
        'ylog' , 'title' , 'xtitle', 'ytitle', 'ztitle' ,'charsize' ,'charthick','thick' ,'position', 'device', 'ticklen', 'background' ,  'color'  ,...
        'xminor'  ,  'yminor'  ,  'zminor'  ,'I2M_pos');
I2Mkwv=    {'initd1' , 'initd2' , 'initd3' , 'e1' ,'azz','imagee','pixelss','savee','shadess','maxi'      ,'mini'     ,'logx' ,...
        'logy' , 'titlee', 'labelx', 'labely', 'labelz' ,'taillecar','epaissCar','epaiss', 'pos'    ,  'dev'  , 'ticlen' , 'backColor','colorIndice',...
        'xminorTick','yminorTick','zminorTick','I2M_pos'};

% variables utilisées

% fonctions à afficher
initd1=[]; initd2=[]; initd3=[];
% keywords
maxi='';mini='';
shadess=[];imagee='';pixelss='';
e1=30; azz=30; %Valeurs par defaut de Matlab pour la fonction view
% keywords présent si =1
logx=''; logy='';
horizontall=''; savee='';
% keywords graphiques
titlee=''; labelx=''; labely=''; labelz=''; taillecar=''; epaissCar=''; epaiss=''; pos=[]; dev=''; ticlen=''; backColor=''; colorIndice='';xminorTick=''; yminorTick=''; zminorTick='';

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% Traitement des plots multiples dans une meme figure :
multiplots
% Mise a jour eventuelle des couleurs de la figure courante
i2m_majColormap;
% Traitement du cas ou on a !D.NAME = PS
devicePS;

% initialisation
nbvar=1;
if ~isempty(initd3),
    nbvar=3;
    d1=initd1;
    d2=initd2;
    d3=initd3;
else
    d1=initd1;
end;

%Traitement de MIN_VALUE et MAX_VALUE
if ~isempty(mini)
    d1(d1 <= mini)=NaN;
end;
if ~isempty(maxi)
    d1(d1 >= maxi)=NaN;
end;

%Affichage de la courbe. Traitement de SHADES
if (nbvar==1)
        s=size(d1);
        d2=(0:s(1)-1);
        d3=(0:s(2)-1);
end;
if isempty(shadess)
    h=surf(d2,d3,d1);
else
    h=surf(d2,d3,d1,shadess);
end;

%Traitement de AX et AZ
view(-azz,e1);

%Traitement de /SAVE
if ~isempty(savee)
    surfr('ax',e1,'az',azz);
end;

%Traitement de IMAGE
if ~isempty(imagee)
    imagee=h;
end;

%traitement de /XLOG
if (logx==1)
  set(gca,'XScale','log');
end;

%traitement de /YLOG
if (logy==1)
  set(gca,'YScale','log');
end;

% traitement de PIXELS
if ~isempty(pixelss)
    warning('Function SHADE_SURF: keyword PIXELS not translated');
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%%%%%%%%

% Traitement de /BACKGROUND
i2m_background(backColor);
% traitement de /COLOR
i2m_color(colorIndice,h,'shade_surf');
% Traitement de /CHARSIZE
i2m_charsize(taillecar);
% Traitement de /CHARTHICK
i2m_charthick(epaissCar);
% Traitement de /THICK
i2m_thick(h,epaiss);
% Traitement de /TICKLEN
i2m_ticklen(ticlen);
% Traitement de /POSITION
i2m_position(pos,dev);
% traitement de /TITLE
i2m_title(titlee);
% traitement de /[XYZ]TITLE
i2m_xtitle(labelx);
i2m_ytitle(labely);
i2m_ztitle(labelz);
% Traitement de /[XYZ]MINOR
i2m_xminor(xminorTick);
i2m_yminor(yminorTick);
i2m_zminor(zminorTick);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Traitement du cas ou on !D.NAME = PS
devicePS;

% On met a jour la valeur de ![XYZ].CRANGE
majCrange;

% Pour supprimer la boite 3D qui apparait autour :
box off;


%%%%%% traitement des plots multiples %%%%%%%%%%%%%%%%%%%%%%%%
multiplotsAfter;

if I2M_out; eval(I2M_out); end;



