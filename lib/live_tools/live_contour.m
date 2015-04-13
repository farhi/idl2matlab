% live_contour
% --------------------------------------------
% Equivalent to :
% function LIVE_CONTOUR [, Z1,..., Z9] 
%                   , /BUFFER
%                   , DIMENSIONS=[width, height]{normal units}
%                   , /DOUBLE
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
%                   , {X | Y}INDEPENDENT=value
%                   , {/X | /Y}LOG
%                   , {X | Y}RANGE=[min, max]{data units}
%                   , {X | Y}_TICKNAME=array
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
% Fonction : LIVE_CONTOUR
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

function [varargout]=live_contour(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'I2M_a5' , 'I2M_a6' ,'I2M_a7' , 'I2M_a8' , 'I2M_a9' ,'buffer','dimensions','double','draw_dimensions','error','indexed_color','instancing','location','manage_style','name','no_draw','no_selection','no_status','no_toolbar','parent_base','tlb_location','preference_file','reference_out','renderer','replace','style','template_file','title','window_in','xindependent','yindependent','xlog','ylog','xrange','yrange','x_thickname','y_thickname','I2M_pos');
I2Mkwv=    { 'd1'    , 'd2'     ,   'd3'   ,'d4'      ,'d5'      ,'d6'      , 'd7'    ,   'd8'   ,'d9'      ,'buffer','dimensions','doubl ','draw_dimensions','error','indexed_color','instancing','location','manage_style','name','no_draw','no_selection','no_status','no_toolbar','parent_base','tlb_location','preference_file','reference_out','renderer','replace','style','template_file','title','window_in','xindependent','yindependent','xlog','ylog','xrange','yrange','x_thickname','y_thickname','I2M_pos'};


% variables utilisées
d1=[];d2=[];d3=[];d4=[];d5=[];d6=[];d7=[];d8=[];d9=[];d10=[];d11=[];d12=[];d13=[];d14=[];d15=[];d16=[];d17=[];d18=[];d19=[];d20=[];
d21=[];d22=[];d23=[];d24=[];d25=[];
% keywords
dimensions=[];draw_dimensions=[];error=[];instancing=[];
location=[];name=[];parent_base=[];
tlb_location=[];preference_file=[];reference_out=[];renderer=[];replace=[];style=[];template_file=[];title=[];
window_in=[];xindependent=[];yindependent=[];xrange=[];yrange=[];x_thickname=[];y_thickname=[];
% keywords présent si =1
buffer=[];doubl=[];manage_style=[];indexed_color=[];no_draw=[];no_selection=[];no_status=[];no_toolbar=[];
xlog=[];ylog=[];

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% On cree un tableau contenant les differentes donnees
%tab=cell(1,25);
if ~isempty(d1), tab{1}=d1;end; if ~isempty(d2), tab{2}=d2;end; if ~isempty(d3), tab{3}=d3;end; if ~isempty(d4), tab{4}=d4;end; 
if ~isempty(d5), tab{5}=d5;end; if ~isempty(d6), tab{6}=d6;end; if ~isempty(d7), tab{7}=d7;end; if ~isempty(d8), tab{8}=d8;end; 
if ~isempty(d9), tab{9}=d9;end; 

s=size(tab);
axe = gca;

map = i2mColormap;

global LEVEL_16 RAINBOW
i2mColormap(RAINBOW);

% On cree une nouvelle figure dans laquelle on affiche live_contour
figure('Position',[176 174 452 452],'MenuBar','figure','name','Live Contour','NumberTitle','off')

for i=1:s(2)
    % Affichage : appel de contourr
    contourr(tab{i});
    hold on;
    axis auto
end;

hold off;

% Modification des proprietes de l'axe
set(gca,'color','white','xcolor','black','ycolor','black','zcolor','black','box','off','fontsize',9.5,'fontweight','demi');
set(gcf,'color','white');
set(get(gca,'Title'),'Visible','off')

% On retire les legendes des axes
xlabel('');ylabel('');zlabel('');

% Positionnment du systeme d'axe dans la fenetre
unit = get(gca,'units');
set(gca,'units','normalized');
set(gca,'position',[0.20 0.20 0.62 0.62]);
set(gca,'units',unit);

% On reaffecte gca a l'axe courant du depart
gca = axe;

i2mColormap(map);

if I2M_out; eval(I2M_out); end;







