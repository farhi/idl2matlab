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
% Modifications : 11 / 08 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:

% A FAIRE :
% PIXELS :  a faire. Depend de DEVICE

% COMMENTAIRES SUR LES KEYWORDS TRADUITS :
% Revoir SHADES quand on aura traite les couleurs.



function [varargout]=shade_surf(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'ax' ,'az' ,'image' ,'pixels' ,'save' ,'shades' ,'max_value' ,'min_value','xlog' ,...
        'ylog' , 'normal' , 'data' , 'title' , 'xtitle', 'ytitle', 'ztitle' ,'charsize' ,'charthick','thick' ,'position', 'device', 'ticklen', 'background' ,  'color'  ,...
        'xminor'  ,  'yminor'  ,  'zminor'    ,'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle',...
		't3d'  ,'xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks',...
        'xcharsize','ycharsize','zcharsize','font','xticklen','yticklen','zticklen','I2M_pos');
I2Mkwv=    {'initd1' , 'initd2' , 'initd3' , 'e1' ,'azz','imagee','pixelss','savee','shadess','maxxi'      ,'minni'     ,'logx' ,...
        'logy' , 'norma' , 'data' , 'titlee', 'labelx', 'labely', 'labelz' ,'taillecar','epaissCar','epaiss', 'pos'    ,  'device'  , 'ticlen' , 'backColor','colorIndice',...
        'xminorTick','yminorTick','zminorTick','noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle',...
		't3dKG','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks',...
        'xcharsize','ycharsize','zcharsize','font','xticklen','yticklen','zticklen','I2M_pos'};


initd1=[]; initd2=[]; initd3=[];                                 % fonctions à afficher
maxxi='';minni=''; shadess=[];imagee='';pixelss=''; e1=''; azz=''; % keywords
logx=''; logy=''; horizontall=''; savee='';                      % keywords présent si =1

% keywords graphiques
titlee=''; labelx=''; labely=''; labelz=''; taillecar=''; epaissCar=''; epaiss=''; pos=[]; device=''; ticlen=''; backColor=''; colorIndice='';xminorTick=''; yminorTick=''; zminorTick='';noerase='';
xrange='';yrange='';zrange='';xstyle='';ystyle='';zstyle='';nodata='';xgridstyle='';ygridstyle='';zgridstyle='';t3dKG='';xtick_get='';ytick_get='';ztick_get='';xtickname='';ytickname='';ztickname='';
normal=''; data=''; xtickv='';ytickv='';ztickv='';xticks='';yticks='';zticks='';xcharsize='';ycharsize='';zcharsize='';font='';xticklen='';yticklen='';zticklen='';


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
   if isempty(shadess), h=surf(x,y,z');                 % *** Plot ***
                  else, h=surf(x,y,z',shadess); end;
   set(h,'FaceColor','interp');
   set(h,'EdgeColor','none');
   if ~isempty(imagee), imagee=h; end;                 % Traitement de IMAGE
   box off
end;

if ~isempty(pixelss), warning('Function SHADE_SURF: keyword PIXELS not translated'); end;

%%%%%%% KEYWORDS FOR SURFACES %%%%%%%%%%%%%%%%%%%
if nodata ~= 1,
   i2m_thick    (h,epaiss);                            % Traitement de /THICK
   i2m_color    (colorIndice,h,'shade_surf');          % traitement de /COLOR
end;

i2m_noerase('','fin');                                 % Traitement de /NOERASE
majCrange;                                             % On Met a jour la valeur de [XYZ].CRANGE
multiplotsAfter;                                       % traitement des plots multiples

if I2M_out; eval(I2M_out); end;



