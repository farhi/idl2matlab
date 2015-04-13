% axxis
% --------------------------------------------
% Equivalent to :
% function AXIS [[[, X], Y], Z]
%                      , XAXIS=  {0|1}
%                      , YAXIS = {0|1}
%                      , ZAXIS = {0|1|2|3}
%                      , /SAVE
%                      , /YNOZERO
%                      , /XLOG
%                      , /YLOG
%                      , /ZLOG
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
% Fonction : axxis
% Auteurs :
%                 Cortina Stephane
%                 Bourtembourg Reynald
% Date creation : 14 / 04 / 2003
% Modifications : 08 / 08 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Non traduits :  X,Y,Z, /YNOZERO

function [varargout]=axxis(varargin)

I2Mkwn=char('I2M_a1' ,'I2M_a2' ,'I2M_a3', 'save' ,'xaxis','yaxis' ,'zaxis', 'xlog','ylog','zlog','ynozero', 'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','zvalue','xgridstyle','ygridstyle','zgridstyle', 't3d' ,'xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','data','device','normal' ,'xtitle','ytitle','ztitle','color','charthick','charsize' ,'xcharsize','ycharsize','zcharsize','font','ticklen','xticklen','yticklen','zticklen','xminorTick','yminorTick','zminorTick','xstyle','ystyle','zstyle','I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' ,  'd3' , 'save' , 'xax' , 'yax'  , 'zax' , 'logx','logy','logz', 'ynoz'  , 'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','zvalue','xgridstyle','ygridstyle','zgridstyle','t3dKG','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','data','device','normal' ,'labelx','labely','labelz','color','epaissCar','taillecar','xcharsize','ycharsize','zcharsize','font','ticlen','xticklen','yticklen','zticklen','xminorTick','yminorTick','zminorTick','xstyle','ystyle','zstyle','I2M_pos'};
    
d1=[]; d2=[]; d3=[];              % fonctions à afficher
save=''; xax=''; yax=''; zax='';  % keywords
logx=''; logy='';logz='';ynoz=''; % keywords présent si =1

% keywords graphiques
noerase = '';xrange='';yrange='';zrange='';xstyle='';ystyle='';zstyle='';zvalue='';xgridstyle='';ygridstyle='';zgridstyle=''; t3dKG= '';
xtick_get='';ytick_get='';ztick_get='';xtickname='';ytickname='';ztickname='';xtickv='';ytickv='';ztickv='';xticks='';yticks='';zticks='';
data=''; device=''; normal=''; labelx='';labely='';labelz='';color='';epaissCar='';taillecar='';xcharsize='';ycharsize='';zcharsize='';font='';
ticlen='';xticklen='';yticklen='';zticklen='';xminorTick='';yminorTick='';zminorTick='';xstyle='';ystyle='';zstyle='';

    I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

global i2mvs_p i2mvs_x i2mvs_y i2mvs_z

multiplots;                   % Traitement des plots multiples dans une meme figure :
i2m_noerase(noerase,'debut'); % Traitement de /NOERASE
i2m_majColormap;              % Mise a jour eventuelle des couleurs de la figure courante
devicePS;                     % Traitement du cas ou on a !D.NAME = PS

if isempty(t3dKG), t3dKG=i2mvs_p.t3d; end;
set(gca,'box','off');

if logx, sclx='log'; set(gca,'XScale',sclx); end;         % traitement de /XLOG
if logy, scly='log'; set(gca,'YScale',scly); end;         % traitement de /YLOG
if logz, sclz='log'; set(gca,'ZScale',sclz); end;         % traitement de /ZLOG
if normal, unit='normal'; elseif device, unit='device'; else, unit='data'; end;

if isempty(xax), if ~isempty(d1), xax=0, end; end;
if isempty(yax), if ~isempty(d2), yax=0, end; end;
if isempty(zax), if ~isempty(d3), zax=0, end; end;

if ~isempty(xax),
    if xax == 0, set(gca,'XAxisLocation','bottom'); else, set(gca,'XAxisLocation','top'); end;
    if ~isempty(color), i2m_color(color,gca,'X'); end;
end
if ~isempty(yax),
    if yax == 0, set(gca,'YAxisLocation','left'); else, set(gca,'yAxisLocation','right'); end;
    if ~isempty(color), i2m_color(color,gca,'Y'); end;
end;
if ~isempty(zax),
    if ~isempty(color), i2m_color(color,gca,'Z'); end;

   %%%%%%% GENERAL KEYWORDS FOR AXES %%%%%%%%%%%%%%%%%%%
   i2m_xyzrange  (xrange,yrange,zrange);                           % Traitement de [XYZ]RANGE
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
   i2m_xyzstyle    (xstyle,ystyle,zstyle,t3dKG);                   % Traitement de [XYZ]STYLE
   i2m_xyztickv    (xtickv,xticks,ytickv,yticks,ztickv,zticks);    % Traitement de [XYZ]TICKV et [XYZ]TICKS
   i2m_xyztickname (xtickname,ytickname,ztickname);                % Traitement de [XYZ]TICKNAME
   i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,t3dKG);     % courbe en 3D ou 2D
   i2m_t3d(t3dKG);                                                 % Traitement de /T3D
   [xtick_get,ytick_get,ztick_get] = i2m_xyztick_get(xtick_get,ytick_get,ztick_get); % Traitement de [XYZ]TICK_GET

i2m_noerase('','fin');                                 % Traitement de /NOERASE
majCrange;                                             % On Met a jour la valeur de [XYZ].CRANGE
multiplotsAfter;                                       % traitement des plots multiples
end;
if I2M_out; eval(I2M_out); end;

