% live_image
% --------------------------------------------
% Equivalent to :
% function LIVE_IMAGE , Image 
%                   , RED=byte_vector
%                   , GREEN=byte_vector
%                   , BLUE=byte_vector
%                   , /BUFFER
%                   , DIMENSIONS=[width, height]{normal units}
%                   , DRAW_DIMENSIONS=[width, height]{devive units}
%                   , ERROR=variable
%                   , /INDEXED_COLOR
%                   , INSTANCING={-1 | 0 | 1}
%                   , LOCATION=[x, y]{normal units}
%                   , /MANAGE_STYLE
%                   , NAME=structure
%                   , /NO_DRAW
%                   , /NO_SELECTION
%                   , /NO_STATUS
%                   , /NO_TOOLBAR
%                   , PARENT_BASE=widget_id | , TLB_LOCATION=[Xoffset, Yoffset]{device units}
%                   , PREFERENCE_FILE=filename{full path}
%                   , REFERENCE_OUT=variable
%                   , RENDERER={0 | 1}
%                   , REPLACE={structure | {0 | 1 | 2 | 3 | 4}}
%                   , STYLE=name_or_reference
%                   , TEMPLATE_FILE=filename
%                   , TITLE=string
%                   , WINDOW_IN=string
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
% Fonction : LIVE_IMAGE
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 02 / 06 / 2003
% Modifications : 02 / 06 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques

% A faire : - tous les keywords.
%           - regarder comment ca fonctionne avec les widgets.

function [varargout]=live_image(varargin)

I2Mkwn=char('I2M_a1' ,'red','green','blue','buffer','dimensions','draw_dimensions','error','indexed_color','instancing',...
        'location','manage_style','name','no_draw','no_selection','no_status','no_toolbar','parent_base','tlb_location','preference_file',...
        'reference_out','renderer','replace','style','template_file','title','window_in','I2M_pos');
I2Mkwv=    { 'img'   ,'red','green','blue','buffer','dimensions','draw_dimensions','error','indexed_color','instancing',...
        'location','manage_style','name','no_draw','no_selection','no_status','no_toolbar','parent_base','tlb_location','preference_file',...
        'reference_out','renderer','replace','style','template_file','title','window_in','I2M_pos'};

% variables utilisées
img=[];
% keywords
dimensions=[];draw_dimensions=[];error=[];instancing=[];red=[];green=[];blue=[];
location=[];name=[];parent_base=[];
tlb_location=[];preference_file=[];reference_out=[];renderer=[];replace=[];style=[];template_file=[];title=[];
window_in=[];xindependent=[];yindependent=[];xrange=[];yrange=[];x_thick=[];y_thick=[];
% keywords présent si =1
buffer=[];double=[];manage_style=[];indexed_color=[];no_draw=[];no_selection=[];no_status=[];no_toolbar=[];
xlog=[];ylog=[];

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% Appel de la fonction image pour afficher l'image
image(img);
set(gca,'units','pixels');
set(gca,'visible','off');
set(gcf,'MenuBar','figure');

if I2M_out; eval(I2M_out); end;
