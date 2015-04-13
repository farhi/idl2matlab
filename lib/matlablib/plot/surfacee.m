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
% Modifications : 08 / 08 / 2003
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
% SHADES : si on a SHADES + SKIRT en IDL, les lignes creees par SKIRT auront la couleur par defaut.
%         en Matlab, la couleur sera determinee en fonction des autres couleurs du graphe.
% Revoir SHADES quand on aura traite les couleurs.



function [varargout]=surfacee(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'ax' ,'az' , 'normal', 'data', 'skirt' ,'lego' ,'save' ,'shades' ,'bottom' ,'horizontal' ,'lower_only','upper_only','max_value' ,'min_value','xlog' , 'ylog' ,'zlog', 'zaxis' ,'title' , 'xtitle', 'ytitle', 'ztitle','charsize' , 'charthick','thick' ,'position','device', 'ticklen', 'linestyle', 'psym' , 'background' ,  'color'  ,  'xminor'  ,  'yminor'  ,  'zminor'  ,'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle','t3d'  ,'xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','xcharsize','ycharsize','zcharsize','font','xticklen','yticklen','zticklen','I2M_pos');
I2Mkwv=    {'initd1' , 'initd2' , 'initd3' , 'e1' ,'azz', 'normal', 'data', 'skirtt','legoo','savee','shadess','bottomm','horizontall','lower'     ,'upper'     ,'maxxi'     ,'minni'    ,'logx' , 'logy' ,'logz', 'zaxiss','titlee', 'labelx', 'labely', 'labelz','taillecar', 'epaissCar','epaiss', 'pos'    ,'device',  'ticlen','styleLigne','symbole','backColor','colorIndice','xminorTick','yminorTick','zminorTick','noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle','t3dKG','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','xcharsize','ycharsize','zcharsize','font','xticklen','yticklen','zticklen','I2M_pos'};

initd1=[]; initd2=[]; initd3=[];                                                % fonctions à afficher
maxxi='';minni='';zaxiss='';bottomm='';skirtt=''; shadess=[]; e1=''; azz='';    % keywords
logx=''; logy=''; logz='';lower='';upper=''; horizontall='';legoo=''; savee=''; % keywords 0 | 1

% keywords graphiques
titlee=''; labelx=''; labely=''; labelz=''; taillecar=''; epaissCar=''; epaiss=''; pos=[]; device=''; ticlen=''; styleLigne = ''; symbole=''; backColor=''; colorIndice=''; xminorTick = ''; yminorTick = ''; zminorTick = '';noerase = '';
nodata='';xgridstyle='';ygridstyle='';zgridstyle='';t3dKG='';xtickname='';ytickname='';ztickname='';xcharsize='';ycharsize='';zcharsize='';font='';
xrange='';yrange='';zrange='';xstyle='';ystyle='';zstyle='';xtick_get='';ytick_get='';ztick_get='';xtickv='';ytickv='';ztickv='';xticks='';yticks='';zticks='';
xticklen='';yticklen='';zticklen=''; normal=''; data='';

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global i2mvs_p

multiplots;                   % Traitement des plots multiples dans une meme figure :
i2m_noerase(noerase,'debut'); % Traitement de /NOERASE
i2m_majColormap;              % Mise a jour eventuelle des couleurs de la figure courante
devicePS;                     % Traitement du cas ou on a !D.NAME = PS

z=initd1; sz=size(z);
if isempty(initd2), x=[0:sz(1)-1]; else, x=initd2; end;
if isempty(initd3), y=[0:sz(2)-1]; else, y=initd3; end;
if isempty(t3dKG), t3dKG=i2mvs_p.t3d; end;

   maxi=maxxi; mini=minni;
   if (~isempty(maxi)), z(z>maxi)=NaN; end;                  % MAX_VALUE= et MIN_VALUE=
   if (~isempty(mini)), z(z<mini)=NaN; end;

   if logx, sclx='log'; set(gca,'XScale',sclx); end;         % traitement de /XLOG
   if logy, scly='log'; set(gca,'YScale',scly); end;         % traitement de /YLOG
   if logz, sclz='log'; set(gca,'ZScale',sclz); end;         % traitement de /ZLOG
   if normal, unit='normal'; elseif device, unit='device'; else, unit='data'; end;

   if ~isempty(zrange), if zrange ~= 0, mini=zrange(1); maxi=zrange(2);   end; end;
   if  (~isempty(maxi) | ~isempty(mini)),
      if isempty(maxi), maxi=maax(z); elseif isempty(mini), mini=miin(z); end; end;
   if  isempty(xrange) | xrange == 0, minx=miin(x); maxx=maax(x); else, minx=xrange(1); maxx=xrange(2); end;
   if  isempty(yrange) | yrange == 0, miny=miin(y); maxy=maax(y); else, miny=yrange(1); maxy=yrange(2); end;

if (~isempty(azz) | ~isempty(e1)) | ~t3dKG,
    if isempty(azz), azz = 30; end;                    % Traitement de AX et AZ
    if isempty(e1) , e1  = 30; end;
    view(-azz,e1);
end;
if ~isempty(savee),  surfr('ax',e1,'az',azz); end;     % Traitement de /SAVE

   %%%%%%% GENERAL KEYWORDS FOR AXES %%%%%%%%%%%%%%%%%%%
   i2m_xyzrange  ([minx maxx],[miny maxy],[mini maxi]);            % Traitement de [XYZ]RANGE
   i2m_background  (backColor);                                    % Traitement de /BACKGROUND
   i2m_title       (titlee);                                       % Traitement de /TITLE
   i2m_xtitle      (labelx);                                       % Traitement de /[XYZ]TITLE
   i2m_ytitle      (labely);
   i2m_ztitle      (labelz);
   i2m_charsize    (taillecar,font,xcharsize,ycharsize,zcharsize); % Traitement de /CHARSIZE
   i2m_charthick   (epaissCar,gca);                                % Traitement de /CHARTHICK
   i2m_ticklen     (ticlen,xticklen,yticklen,zticklen);            % Traitement de /TICKLEN et /[XYZ]TICKLEN
  %i2m_position    (pos,unit);                                     % Traitement de /POSITION
   i2m_xminor      (xminorTick);                                   % Traitement de /[XYZ]MINOR
   i2m_yminor      (yminorTick);
   i2m_zminor      (zminorTick);
   i2m_xyzstyle    (xstyle,ystyle,zstyle,1);                       % Traitement de [XYZ]STYLE
   i2m_xyztickv    (xtickv,xticks,ytickv,yticks,ztickv,zticks);    % Traitement de [XYZ]TICKV et [XYZ]TICKS
   i2m_xyztickname (xtickname,ytickname,ztickname);                % Traitement de [XYZ]TICKNAME
   i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,1);         % courbe en 3D ou 2D
   i2m_t3d(t3dKG);                                                 % Traitement de /T3D
   [xtick_get,ytick_get,ztick_get] = i2m_xyztick_get(xtick_get,ytick_get,ztick_get); % Traitement de [XYZ]TICK_GET

if nodata ~= 1,
   if ~isempty(skirtt),     % Traitement de SKIRT : si SKIRT est present, on appelle meshzskirt sinon mesh
                            h=meshzskirt(x,y,z',skirtt,shadess); 
   elseif isempty(shadess), h=mesh(x,y,z');
                      else, h=mesh(x,y,z',shadess);
   end;
   if ~isempty(horizontall),set(h,'MeshStyle','row'); end;         % Traitement de /HORIZONTAL
   grid off;
   box  off;
end;

%%%%%%% KEYWORDS FOR SURFACES %%%%%%%%%%%%%%%%%%%
if nodata ~= 1,
   i2m_color(colorIndice,h,'surface');                       % traitement de /COLOR
   i2m_thick(h,epaiss);                                      % Traitement de /THICK
   i2m_linestyle(styleLigne,h,0);                            % Traitement de /LINESTYLE
end;

non_traite(bottomm,upper,lower,zaxiss,legoo);          % Affichage d'un warning pour les keywords non traduits
i2m_noerase('','fin');                                 % Traitement de /NOERASE
majCrange;                                             % On Met a jour la valeur de [XYZ].CRANGE
multiplotsAfter;                                       % traitement des plots multiples

if I2M_out; eval(I2M_out); end;




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(bottomm,upper,lower,zaxiss,legoo)
% Permet de traiter les keywords BOTTOM, UPPER_ONLY, LOWER_ONLY, ZAXIS et LEGO.
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(bottomm,upper,lower,zaxiss,legoo)

if ~isempty(bottomm)
    warning('Function SURFACE: keyword BOTTOM not translated');
end;
if ~isempty(upper)
    warning('Function SURFACE: keyword UPPER_ONLY not translated');
end;
if ~isempty(lower)
    warning('Function SURFACE: keyword LOWER_ONLY not translated');
end;
if ~isempty(zaxiss)
    warning('Function SURFACE: keyword ZAXIS not translated');
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

% Affichage du graphe dans le cas ou on a le keyword SHADES ou non.
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


