% erase
% --------------------------------------------
% Equivalent to :
% function ERASE [, Background_Color]
%                [, COLOR=value]
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
% Fonction : ERASE
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 10 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%remarque:
%CHANNEL => a faire

function [varargout]=erase(varargin)

I2Mkwn=char('I2M_a1' , 'channel' , 'color' , 'I2M_pos');
I2Mkwv=    {'d1'     , 'channell', 'colorr', 'I2M_pos'};
    
% variables utilisées
% arguments
d1=''; 
% keywords
colorr=''; channell='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global axes_wd i2mDecomposed
% On efface le contenu de gca si on est dans un widget
% On supprime gca si on est dans une fenetre classique
if ~isempty(axes_wd) & sum(axes_wd(1,:) == gca)~=0
    plot(1:10)
    cla
else
    delete(gca);
end;


% Traitement de Background_color et COLOR
if ( ~isempty(d1) | ~isempty(colorr) )
	if i2mDecomposed == 0
    c=i2mColormap;
    if isempty(d1)
        d1=colorr;
    end;
    % Cas ou on est dans un widget
    if ~isempty(axes_wd) & sum(sum(axes_wd(1,:) == gca))~=0
        set(gca,'Color',c(d1+1,:));
        set(gca,'visible','on');
        set(gca,'xtick',[],'ytick',[]);
        set(gca,'XColor',get(gca,'Color'));
        set(gca,'YColor',get(gca,'Color'));
        [i,j]=find(axes_wd == gca);
        set(axes_wd(2,j),'color',get(gca,'color'));
    % Cas ou l'on est dans une fenetre normale
    else
        set(gcf,'Color',c(d1+1,:));
    end;
	else
		% decomposed == 1
		if isempty(d1)
        d1=colorr;
    end;
		% Cas ou on est dans un widget
    if ~isempty(axes_wd) & sum(sum(axes_wd(1,:) == gca))~=0
        set(gca,'Color',i2m_index2rgb(d1));
        set(gca,'visible','on');
        set(gca,'xtick',[],'ytick',[]);
        set(gca,'XColor',get(gca,'Color'));
        set(gca,'YColor',get(gca,'Color'));
        [i,j]=find(axes_wd == gca);
        set(axes_wd(2,j),'color',get(gca,'color'));
    % Cas ou l'on est dans une fenetre normale
    else
        set(gcf,'Color',i2m_index2rgb(d1));
    end;
	end
end;

% Traitement de CHANNEL
if ~isempty(channell)
    warning('Function ERASE: keyword CHANNEL not translated');
end;  

if I2M_out; eval(I2M_out); end;
