% tvrd
% --------------------------------------------
% Equivalent to :
% function  Result = TVRD([X0 [, Y0 [, Nx [, Ny ]]]])
%                    [,/ORDER]
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
% Fonction : fonction tvrd
% Auteurs :
%                 Cortina Stephane
%                 Szczuczak Nadege
% Date creation : 13 / 05 / 2003
% Modifications : 16 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Remarques
% Fonction pas testee a 100%
% Les keywords ne sont pas traduits. Seuls les arguments X0, Y0, Nx et Ny sont traduits.

function [res]=tvrd(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' ,'I2M_a5', 'channel','order','true','words', 'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'd3' , 'd4' , 'd5', 'chan','order','true','words', 'I2M_pos'};
    %variables utilisées
    %fonctions à afficher
    d1=[]; d2=[]; d3=[]; d4=[]; d5=[];
    %keywords présent si =1
    order=''; 
    %keywords
    chan=0;

    %variables locales
    long='';larg='';posx='';posy='';
    % variables globales
    global axes_wd
    
I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%fin du passage des parametres
%WORDS,TRUE -> a voir avec device

% visible=1;

% if get(gcf,'Visible') == 'off'
%    visible=0;
%    set(gcf,'Visible','on')
% end;

if ~isempty(axes_wd) & sum(axes_wd(1,:) ==gca)~=0
    [i,j]=find(axes_wd==gca);
    % pos=get(axes_wd(2,j),'position');
    tmp = getframe(axes_wd(2,j));
else
    tmp = getframe(gcf);
    % pos=get(gcf,'position');
end;

if (~isempty(d1))
    tmp.cdata(:,1:d1,:)=[];
    size(tmp.cdata)
end;

if (~isempty(d2))
    r=size(tmp.cdata);
    tmp.cdata(r(1):-1:r(1)-d2+1,:,:)=[];
    size(tmp.cdata)
end;

if ~isempty(d3)
    r=size(tmp.cdata);
    tmp.cdata(:,r(1):-1:d3,:)=[];
end;

if ~isempty(d3)
    r=size(tmp.cdata);
    tmp.cdata(r(2):-1:d4,:,:)=[];
end;

% if visible == 0
%    set(gcf,'Visible','off');
% end;

res=double(tmp.cdata)/255;

% on peut lire les images de haut en bas en précisant /ORDER
if (~isempty(order))
    set(gca,'YDir','normal');
end;

% Traitement des keywords non traduits
non_traite(d5,chan,true,words);

if I2M_out; eval(I2M_out); end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Fonction interne: non_traite(d5,chan,true,words)
%Permet de traiter l'argument channel, les keywords CHANNEL, TRUE et WORDS
%Affiche un Warning pour indiquer que le keyword ou l'argument n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(d5,chan,true,words)

if ~isempty(d5)
    warning('Function TVRD : argument channel not translated');
end;
if ~isempty(chan)
    warning('Function TVRD: keyword CHANNEL not translated');
end;
if ~isempty(true)
    warning('Function TVRD: keyword TRUE not translated (no equivalent in Matlab)');
end;
if ~isempty(words)
    warning('Function TVRD: keyword WORDS not translated');
end;
