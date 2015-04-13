% plott
% --------------------------------------------
% Equivalent to :
% function PLOT,[X,] Y
%               [, MAX_VALUE=value]
%               [, MIN_VALUE=value]
%               [, /POLAR]
%               [, THICK=value]
%               [, /XLOG] [, /YLOG]
%               [, /YNOZERO]
%               [, /ISOTROPIC]
%               [, NSUM=value]
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
% Fonction : fonction plott
% Auteurs :
%                 Szczuczak Nadege
%                 Cortina Stephane
%                 Bourtembourg Reynald
% Date creation : 01 / 04 / 2003
% Modifications : 08 / 08 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Traiter le cas ou PSYM = 10 => histogramme
% clip,noclip, position, subtitle, margin
% tickformat, tickinterval, ticklayout,tickunits

function [varargout]=plott(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'oplot', 'normal', 'data', 'title'  , 'xtitle' , 'ytitle', 'ztitle','psym'    , 'linestyle'  , 'max_value', 'min_value', 'thick' , 'polar'  ,'nsum' , 'isotropic'  , 'xlog' , 'ylog', 'ynozero', 'charsize' ,'charthick' , 'position', 'device', 'ticklen', 'symsize' ,'background',   'color'   ,  'xminor'  ,  'yminor'  , 'zminor'    , 'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle', 't3d' ,'xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','font','xcharsize','ycharsize','zcharsize','xticklen','yticklen','zticklen','clip','noclip', 'subtitle', 'margin','tickformat', 'tickinterval', 'ticklayout','tickunits','I2M_pos');
    I2Mkwv=    {'d1' , 'd2'     , 'oplot', 'normal', 'data', 'titlee' , 'labelx' ,'labely' , 'labelz','symbole' , 'styleligne' , 'maxxi'    , 'minni'    , 'epaiss', 'polaire','nsum' , 'axis_equal' , 'logx' , 'logy', 'ynoz'   , 'taillecar', 'epaissCar', 'pos'     , 'device', 'ticlen' ,'tailleSym','backColor' ,'colorIndice','xminorTick','yminorTick', 'zminorTick', 'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle','t3dKG','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','font','xcharsize','ycharsize','zcharsize','xticklen','yticklen','zticklen','clip','noclip', 'subtitle', 'margin','tickformat', 'tickinterval', 'ticklayout','tickunits','I2M_pos'};

d1=[]; d2=[];                                                     % fonctions à afficher

symbole=''; styleligne=''; epaiss=''; maxxi=[]; minni=[]; nsum=''; % keywords values

polaire=''; logx=''; logy=''; axis_equal=''; ynoz=''; oplot='';   % keywords 1 or 0

% keywords graphiques
titlee=''; labelx=''; labely=''; labelz = ''; taillecar='';epaissCar=''; pos=[]; device=''; ticlen=''; tailleSym=''; backColor=''; colorIndice='';xminorTick = '';yminorTick = '';zminorTick = '';noerase='';
xrange=''; yrange=''; zrange=''; xstyle=''; ystyle=''; zstyle=''; nodata=''; xgridstyle=''; ygridstyle=''; zgridstyle=''; t3dKG=''; xtick_get=''; ytick_get=''; ztick_get=''; xtickname=''; ytickname=''; ztickname='';
normal=''; data=''; xtickv=''; ytickv=''; ztickv=''; xticks=''; yticks=''; zticks=''; font=''; xcharsize=''; ycharsize=''; zcharsize=''; xticklen='';  yticklen=''; zticklen='';
clip=''; noclip=''; subtitle=''; margin=''; tickformat=''; tickinterval=''; ticklayout=''; tickunits=''; 

    I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

%**************************************
global i2mvs_x i2mvs_y i2mvs_p

if isempty(d2),    y=d1; x=[0:length(d1)-1]; else, y=d2; x=d1; end;
if isempty(t3dKG), t3dKG=i2mvs_p.t3d; end;

maxi=maxxi; mini=minni;
if (~isempty(maxi)), y(y>maxi)=NaN; end;                     % MAX_VALUE= et MIN_VALUE=
if (~isempty(mini)), y(y<mini)=NaN; end;

if nsum > 1, y=congrid(y,round(length(y)/nsum)); x=congrid(x,round(length(x)/nsum)); end;

if isempty(oplot),

   multiplots;                                               % Traitement des plots multiples dans une meme figure :
   i2m_noerase(noerase,'debut');                             % Traitement de /NOERASE
   i2m_majColormap;                                          % Mise a jour eventuelle des couleurs de la figure courante
   devicePS;                                                 % Traitement du cas ou on a !D.NAME = PS

   if axis_equal, axis equal; end;                           % traitement de /ISOTROPIC

   if logx, sclx='log'; set(gca,'XScale',sclx); end;         % traitement de /XLOG
   if logy, scly='log'; set(gca,'YScale',scly); end;         % traitement de /YLOG
   if normal, unit='normal'; elseif device, unit='device'; else, unit='data'; end;

   nz=bitget(i2mvs_y.style,4);
   if (nz == 1 | ynoz), if isempty(mini), mini=miin(y); end; end;  % traitement de /YNOZERO

   if ~isempty(yrange), if yrange ~= 0, mini=yrange(1); maxi=yrange(2);   end; end;
   if  (~isempty(maxi) | ~isempty(mini)),
      if isempty(maxi), maxi=maax(y); elseif isempty(mini), mini=miin(y); end; end;
   if  isempty(xrange) | xrange == 0, minx=miin(x); maxx=maax(x); else, minx=xrange(1); maxx=xrange(2); end;

   %%%%%%% GENERAL KEYWORDS FOR AXES %%%%%%%%%%%%%%%%%%%
   i2m_xyzrange  ([minx maxx],[mini maxi],zrange);                 % Traitement de [XYZ]RANGE
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
   i2m_xyzstyle    (xstyle,ystyle,zstyle,0);                       % Traitement de [XYZ]STYLE
   i2m_xyztickv    (xtickv,xticks,ytickv,yticks,ztickv,zticks);    % Traitement de [XYZ]TICKV et [XYZ]TICKS
   i2m_xyztickname (xtickname,ytickname,ztickname);                % Traitement de [XYZ]TICKNAME
   i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,t3dKG);     % courbe en 3D ou 2D
   i2m_t3d(t3dKG);                                                 % Traitement de /T3D
   [xtick_get,ytick_get,ztick_get] = i2m_xyztick_get(xtick_get,ytick_get,ztick_get); % Traitement de [XYZ]TICK_GET
   
   if nodata ~= 1, if polaire, l=polar(x,y); else, l=plot(x,y); end; end;                 % *** PLOT  ***
else,
   if nodata ~= 1, if polaire, l=polar(x,y); else, l=line('XData',x,'YData',y); end; end; % *** OPLOT ***
end;

%%%%%%% KEYWORDS FOR LINES %%%%%%%%%%%%%%%%%%%
if nodata ~= 1,
   i2m_thick    (l,epaiss);                                        % Traitement de /THICK
   i2m_psym     (symbole,l);                                       % Traitement de /PSYM
   i2m_linestyle(styleligne,l,symbole);                            % Traitement de /LINESTYLE
   i2m_symsize  (tailleSym,l);                                     % Traitement de /SYMSIZE
   if oplot, i2m_color(colorIndice,l,'oplot'); else, i2m_color(colorIndice,l,'plot'); end;
end;

i2m_noerase('','fin');                                             % Traitement de /NOERASE
majCrange;                                                         % On Met a jour la valeur de [XYZ].CRANGE
multiplotsAfter;                                                   % traitement des plots multiples

if ~isempty(I2M_out), eval(I2M_out); end;
