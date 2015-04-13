% plot_oo
% --------------------------------------------
% Equivalent to :
% function PLOT_OO,[X,] Y
%               [, MAX_VALUE=value]
%               [, MIN_VALUE=value]
%               [, /POLAR]
%               [, THICK=value]
%               [, /XLOG] [, /YLOG]
%               [, /YNOZERO]
%               [, /ISOTROPIC]
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
% /MAX_VALUE, /MIN_VALUE, /POLAR, /ISOTROPIC, /XLOG, /YLOG, /THICK, /YNOZERO, /NSUM

function [varargout]=plot_oo(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'oplot', 'title'  , 'xtitle' , 'ytitle', 'ztitle','psym'    , 'linestyle'  , 'max_value', 'min_value', 'thick' , 'polar'  ,'nsum' , 'isotropic'  , 'xlog' , 'ylog', 'ynozero', 'charsize' ,'charthick'    , 'position', 'device', 'ticklen', 'symsize' ,'background',   'color'   ,  'xminor'  ,  'yminor'  , 'zminor'    , 'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle', 't3d' ,'xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','font','xcharsize','ycharsize','zcharsize','xticklen','yticklen','zticklen','I2M_pos');
    I2Mkwv=    {'d1' , 'd2'     , 'oplot', 'titlee' , 'labelx' ,'labely' , 'labelz','symbole' , 'styleligne' , 'maxxi'    , 'minni'    , 'epais' , 'polaire','nsum' , 'axis_equal' , 'logx' , 'logy', 'ynoz'   , 'taillecar', 'epaisseurCar', 'pos'     ,  'dev'  , 'ticlen' ,'tailleSym','backColor' ,'colorIndice','xminorTick','yminorTick', 'zminorTick', 'noerase','xrange','yrange','zrange','xstyle','ystyle','zstyle','nodata','xgridstyle','ygridstyle','zgridstyle','t3dKG','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv','xticks','yticks','zticks','font','xcharsize','ycharsize','zcharsize','xticklen','yticklen','zticklen','I2M_pos'};

d1=[]; d2=[];                                                    % fonctions à afficher

symbole=''; styleligne=''; epais=''; maxxi=[]; minni=[];nsum=''; % keywords values

polaire=''; logx=''; logy=''; axis_equal=''; ynoz=''; oplot='';  % keywords 1 or 0

% keywords graphiques
titlee=''; labelx=''; labely=''; labelz = ''; taillecar='';epaisseurCar=''; pos=[]; dev=''; ticlen=''; tailleSym=''; backColor=''; colorIndice='';xminorTick = '';yminorTick = '';zminorTick = '';noerase='';
xrange=''; yrange=''; zrange=''; xstyle=''; ystyle=''; zstyle=''; nodata=''; xgridstyle=''; ygridstyle=''; zgridstyle=''; t3dKG=''; xtick_get=''; ytick_get=''; ztick_get=''; xtickname=''; ytickname=''; ztickname='';
xtickv=''; ytickv=''; ztickv=''; xticks=''; yticks=''; zticks=''; font=''; xcharsize=''; ycharsize=''; zcharsize=''; xticklen='';  yticklen=''; zticklen='';

    I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

%**************************************

varargin{end+1}='xlog'; varargin{end+1}=1;
varargin{end+1}='ylog'; varargin{end+1}=1;
plott (varargin{:});

if I2M_out; eval(I2M_out); end;
