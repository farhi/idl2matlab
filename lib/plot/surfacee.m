% surfacee
% --------------------------------------------
% Equivalent to :
% function SURFACE, Z,[X,Y] , MIN_VALUE=real
%                           , MAX_VALUE=real
%                           , AX=degree
%                           , AZ=degree
%                           , /SAVE
%                           , SHADES=[...]
%                           , /HORIZONTAL
%                           , SKIRT=real
%                           , /XLOG
%                           , /YLOG
%                           , /ZLOG
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
% Fonction : SURFACEE
%            Permet de tracer des surfaces.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 14 / 04 / 2003
% Modifications : 16 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:

% KEYWORDS SANS EQUIVALENT MATLAB :
% BOTTOM : pas d'equivalent Matlab
% LOWER_ONLY et UPPER_ONLY : pas d'equivalent Matlab
% ZAXIS : pas d'equivalent Matlab

% KEYWORDS NON TRADUITS MAIS AVEC PEUT-ETRE EQUIVALENT MATLAB :
% /LEGO : en modifiant la fonction bar3.m pour qu'elle accepte x,y,z en argument, il devrait etre possible de traduire ce keyword.

% COMMENTAIRES SUR LES KEYWORDS TRADUITS :
% AX et Az: si ces keywords ne sont pas precise, on laisse les valeurs par defaut de Matlab
% qui ne sont pas tout a fait identiques a celles d'IDL.
% SHADES : si on a SHADES + SKIRT en IDL, les lignes creees par SKIRT auront la couleur par defaut.
%         en Matlab, la couleur sera determinee en fonction des autres couleurs du graphe.
% Revoir SHADES quand on aura traite les couleurs.



function [varargout]=surfacee(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'ax' ,'az' ,'skirt' ,'lego' ,'save' ,'shades' ,'bottom' ,'horizontal' ,'lower_only','upper_only','max_value' ,'min_value','xlog' , 'ylog' ,'zlog', 'zaxis' ,'title' , 'xtitle', 'ytitle', 'charsize' , 'charthick','thick' ,'position','device', 'ticklen', 'linestyle', 'psym' , 'background' ,  'color'  ,  'xminor'  ,  'yminor'  ,  'zminor'  ,'I2M_pos');
I2Mkwv=    {'initd1' , 'initd2' , 'initd3' , 'e1' ,'azz','skirtt','legoo','savee','shadess','bottomm','horizontall','lower'     ,'upper'     ,'maxi'      ,'mini'     ,'logx' , 'logy' ,'logz', 'zaxiss','titlee', 'labelx', 'labely', 'taillecar', 'epaissCar','epaiss', 'pos'    ,  'dev' ,  'ticlen','styleLigne','symbole','backColor','colorIndice','xminorTick','yminorTick','zminorTick','I2M_pos'};

% variables utilisées

% fonctions à afficher
initd1=[]; initd2=[]; initd3=[];
% keywords
maxi='';mini='';zaxiss='';bottomm='';skirtt='';
shadess=[];
e1=30; azz=30; %Valeurs par defaut de Matlab pour la fonction view
% keywords présent si =1
logx=''; logy=''; logz='';lower='';upper='';
horizontall='';legoo=''; savee='';
% keywords graphiques
titlee=''; labelx=''; labely=''; taillecar=''; epaissCar=''; epaiss=''; pos=[]; dev=''; ticlen=''; styleLigne = ''; symbole=''; backColor=''; colorIndice=''; xminorTick = ''; yminorTick = ''; zminorTick = '';

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
    d1=initd1';
    d2=initd2;
    d3=initd3;
else
    d1=initd1';
end;

% Traitement de MIN_VALUE et MAX_VALUE
if ~isempty(mini)
    d1(d1 <= mini)=NaN;
end;
if ~isempty(maxi)
    d1(d1 >= maxi)=NaN;
end;

% Affichage de la courbe. Traitement de SHADES
if (nbvar==1)
        s=size(d1);
        d2=(0:s(1)-1);
        d3=(0:s(2)-1);
end;
% Traitement de SKIRT : si SKIRT est present, on appelle meshzskirt sinon mesh
if ~isempty(skirtt)
    h=meshzskirt(d2,d3,d1,skirtt,shadess); 
else
    if isempty(shadess)
        h=mesh(d2,d3,d1);
    else
        h=mesh(d2,d3,d1,shadess);
    end;
end;
grid off;

% Traitement de AX et AZ
view(-azz,e1);

% Traitement de /SAVE
if ~isempty(savee)
    surfr('ax',e1,'az',azz);
end;

% Traitement de /HORIZONTAL
if ~isempty(horizontall)
    set(h,'MeshStyle','row');
end;

% traitement de /XLOG
if (logx==1)
  set(gca,'XScale','log');
end;

% traitement de /YLOG
if (logy==1)
  set(gca,'YScale','log');
end;

% traitement de /ZLOG
if (logz==1)
  set(gca,'ZScale','log');
end;


% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%%%%%%%%

% Traitement de /BACKGROUND
i2m_background(backColor);
% traitement de /COLOR
i2m_color(colorIndice,h,'surface');
% Traitement de /CHARSIZE
i2m_charsize(taillecar);
% Traitement de /CHARTHICK
i2m_charthick(epaissCar);
% Traitement de /THICK
i2m_thick(h,epaiss)
% Traitement de /LINESTYLE
i2m_linestyle(styleLigne,h,0);
% Traitement de /TICKLEN
i2m_ticklen(ticlen);
% Traitement de /POSITION
i2m_position(pos,dev);
% traitement de /TITLE
i2m_title(titlee);
% traitement de /[XYZ]TITLE
i2m_xtitle(labelx);
i2m_ytitle(labely);
% Traitement de /[XYZ]MINOR
i2m_xminor(xminorTick);
i2m_yminor(yminorTick);
i2m_zminor(zminorTick);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% On met a jour la valeur de ![XYZ].CRANGE
majCrange;

% Affichage d'un warning pour les keywords non traduits
non_traite(bottomm,upper,lower,zaxiss,legoo);

% Pour supprimer la boite 3D qui apparait autour :
box off;


%%%%%% traitement des plots multiples %%%%%%%%%%%%%%%%%%%%%%%%
multiplotsAfter;

if I2M_out; eval(I2M_out); end;




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(bottomm,upper,lower,zaxiss,legoo)
% Permet de traiter les keywords BOTTOM, UPPER_ONLY, LOWER_ONLY, ZAXIS et LEGO.
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(bottomm,upper,lower,zaxiss,legoo)

if ~isempty(bottomm)
    warning('Function SURFACE: keyword BOTTOM not translated (no equivalent in Matlab)');
end;
if ~isempty(upper)
    warning('Function SURFACE: keyword UPPER_ONLY not translated (no equivalent in Matlab)');
end;
if ~isempty(lower)
    warning('Function SURFACE: keyword LOWER_ONLY not translated (no equivalent in Matlab)');
end;
if ~isempty(zaxiss)
    warning('Function SURFACE: keyword ZAXIS not translated (no equivalent in Matlab)');
end;
if ~isempty(legoo)
    warning('Function SURFACE: keyword /LEGO not translated');
end;






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Fonction interne: MESHZSKIRT(x,y,z,skirt,c)
%Permet de traiter le keyword skirt
%Le code de cette fonction est celui de la fonction meshz(x,y,z,c) auquel on a rajoute le traitement de skirt
%On a enleve toute les parties permettant de traiter le cas ou cette fonction serait appelle avec 1,2,ou 3 arguments.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function h=meshzskirt(x,y,z,skirt,c)

  [m,n] = size(z);
  [mx,nx] = size(x);
  [my,ny] = size(y);

  if min(mx,nx)==1 & max(mx,nx)==n
      %we have a vector of the right size
      x=x(:)';  %make sure we have a row vector
      x=x(ones(m,1),:);
  elseif ~isequal(size(x),size(z))
      xmin = min(min(x));
      xmax = max(max(x));
      
      x=[xmin:(xmax-xmin)/(n-1):xmax];
      x=x(ones(m,1),:);
  end;

  if min([my,ny])==1 & max([my,ny])==m
      %we have a vector of the right size: matrixize it
      y=y(:); %make sure we have a column vector
      y = y(:,ones(1, n));
  elseif ~isequal(size(y),size(z))
      % Create x and y vectors that are the same size as z.
      ymin = min(min(y));
      ymax = max(max(y));
      
      y=[ymin:(ymax-ymin)/(n-1):ymax]';
      y = y(:,ones(1, n));
  end;

% Define position of curtains
zref = skirt;


% Define new x,y,z and then call mesh.
zrow = zref*ones(1,n); zcol = zref*ones(m,1);
d = [1 1]; mm = [m m]; nn = [n n];
newZ = [zref zref   zrow   zref   zref;
        zref zref   z(1,:) zref   zref;
        zcol z(:,1) z      z(:,n) zcol;
        zref zref   z(m,:) zref   zref;
        zref zref   zrow   zref   zref];
        
newX = [x(d,d),x(d,:),x(d,nn);x(:,d),x,x(:,nn);x(mm,d),x(mm,:),x(mm,nn)];
newY = [y(d,d),y(d,:),y(d,nn);y(:,d),y,y(:,nn);y(mm,d),y(mm,:),y(mm,nn)];

%Affichage du graphe dans le cas ou on a le keyword SHADES ou non.
if isempty(c)
    hm=mesh(newX,newY,newZ);
else
    cref = (max(max(c(isfinite(c))))+min(min(c(isfinite(c)))))/2;
    crow = cref*ones(2,n); ccol = cref*ones(m,2); cref = cref*ones(2,2);
    c = [cref,crow,cref;ccol,c,ccol;cref,crow,cref];
    hm=mesh(newX,newY,newZ,c);
end;
set(hm,'tag','meshz');

if nargout > 0
    h = hm;
end;


