% tv
% --------------------------------------------
% Equivalent to :
% function  TV, Image [, Position]
%    ou
%           TV, Image [, X, Y [, Channel]]
%                   /CENTIMETERS
%                   /CHANNEL
%                   /INCHES
%                   /ORDER
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
% Fonction : Nom de la fonction Matlab concernee
% Auteurs :
%                 Cortina Stephane
%                 Szczuczak Nadege
% Date creation : 16 / 04 / 2003
% Modifications : 16 / 06 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% arguments à completer :  position avec subplot
% comportement délicat !!

function [varargout]=tv(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' ,'centimeters','channel','inches','order','true','words','xsize','ysize', 'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'd3' , 'd4' ,'cent','chan','inches','order','true','words','sizex','sizey', 'I2M_pos'};
    % variables utilisées
    % fonctions à afficher
    d1=[]; d2=[]; d3=[]; d4=[];
    % keywords présent si =1
    cent=''; inches=''; order=''; 
    % keywords
    chan='';true='';words='';sizex='';sizey='';

    % variables locales
    i='';map='';l='';

    % Variable globale
    global axes_wd
    
    I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres
% XSIZE, YSIZE , WORDS -> /DEVICE
% TRUE -> a voir avec device aussi

% Nadege : tv fonctionne dans widget et dans fenetre normale.
% ATTENTION : pas de verification des keywords.
% ATTENTION : ne fonctionne pas si la matrice passee en parametre est plus grande que la taille en pixels d'une fenetre par defaut.

% unité : pixel par défaut
if isempty(get(gcf,'CurrentAxes'))
       set(gca,'units','pixels');
       set(gca,'Visible','off');
else
       set(gca,'units','pixels');
end; 

% keyword /CENTIMETERS -> unité centimètre
if ~isempty(cent)
   if isempty(get(gcf,'CurrentAxes'))
       set(gca,'units','centimeters');
       set(gca,'Visible','off');
   else
       set(gca,'units','centimeters');
   end;  
end;

% keyword /INCHES -> unité inches
if ~isempty(inches)
   if isempty(get(gcf,'CurrentAxes'))
       set(gca,'units','inches');
       set(gca,'Visible','off');
   else
       set(gca,'units','inches');
   end; 
end;

% on cherche la "colormap" en cours et sa taille
map=get(gcf,'Colormap');
l=length(map);

% d1 : image à afficher
if ~isempty(d1)
    % on initialise la position à l'origine
    if ~isempty(axes_wd) & sum(axes_wd(1,:)==gca)~=0
        [i,j]=find(axes_wd==gca);
        tailleF=get(axes_wd(2,j),'position');
    else
        tailleF=get(gcf,'Position');
        tailleF(1:2)=0;
    end;
    if ~isempty(axes_wd) & sum(axes_wd(1,:)==gca)~=0
        t2=getframe(axes_wd(2,j));
    else
        t2=getframe(gcf);
    end;
    
    t2.cdata = double(t2.cdata)/255;
    
    if ~isempty(d2)
        if ~isempty(d3)
            % on modifie la position de l'image en fonction de d2 et d3
            pos=tailleF;
            s=size(d1);
            r=size(t2.cdata);
            if ndims(d1)==3
                'hello'
                t2.cdata(r(1)-s(1)-d3:r(1)-d3-1,d2:d2+s(2)-1,:)=d1;
            else
                t2.cdata(r(1)-s(1)-d3:r(1)-d3-1,d2:d2+s(2)-1,1)=d1;
                t2.cdata(r(1)-s(1)-d3:r(1)-d3-1,d2:d2+s(2)-1,2)=d1;
                t2.cdata(r(1)-s(1)-d3:r(1)-d3-1,d2:d2+s(2)-1,3)=d1;
            end;
            % traitement du parametre channel
            if ~isempty(d4)
                    switch d4
                        case 1, for i=1:1:l
                                    % rouge
                                    map(i,2)=0;
                                    map(i,3)=0;
                                    colormap(map);
                                 
                                end;
                        case 2, for i=1:1:l
                                    % vert
                                    map(i,1)=0;
                                    map(i,3)=0;
                                    colormap(map);
                                end;         
                        case 3, for i=1:1:l
                                    % bleu
                                    map(i,1)=0;
                                    map(i,2)=0;
                                    colormap(map);
                                end; 
                        % table par défaut
                        otherwise colormap(gray);
                        end;
            end;
            t2.cdata(1:r(1),:,:)=t2.cdata(r(1):-1:1,:,:);
            h=image(t2.cdata);
            set(gca,'position',pos);
        else
            % position 
            % partie à completer
            %subplot(2,2,2);
            t2.cdata(1:r(1),:,:)=t2.cdata(r(1):-1:1,:,:);
            h=image(t2.cdata);
        end;
    else
        % l'image seule est passée en parametre
        s=size(d1);
        r=size(t2.cdata);
        if ndims(d1)==3
            t2.cdata(r(1)-s(1)+1:r(1),1:s(2),:)=d1;
        else
            t2.cdata(r(1)-s(1)+1:r(1),1:s(2),1)=d1;
            t2.cdata(r(1)-s(1)+1:r(1),1:s(2),2)=d1;
            t2.cdata(r(1)-s(1)+1:r(1),1:s(2),3)=d1;
        end;
        t2.cdata(1:r(1),:,:)=t2.cdata(r(1):-1:1,:,:);
        h=image(t2.cdata);
        set(gca,'position',tailleF);
    end;
end;
set(h,'Deletefcn','delete_tv');

% traitement du keyword /CHANNEL
if (~(isempty(chan)))
    % il ne faut pas que channel soit passé aussi en parametre
    if (isempty(d4))
        switch chan
            case 1, for i=1:1:l
                        % rouge
                        map(i,2)=0;
                        map(i,3)=0;
                        colormap(map);
                    end;
            case 2, for i=1:1:l
                        % vert
                        map(i,1)=0;
                        map(i,3)=0;
                        colormap(map);
                    end;         
            case 3, for i=1:1:l
                        % bleu
                        map(i,1)=0;
                        map(i,2)=0;
                        colormap(map);
                    end;  
            % colormap par défaut
            otherwise colormap(map);
            end;
        end;
end; 

% on cache les axes
set(gca,'visible','off');
set(gca,'YDir','normal');
  

% on peut afficher les images de haut en bas en précisant /ORDER
if (~isempty(order))
    set(gca,'YDir','normal');
end;

% Traitement des keywords non traduits
non_traite(true,words,xsize,ysize);

if I2M_out; eval(I2M_out); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Fonction interne: non_traite(true,words,xsize,ysize)
%Permet de traiter les keywords TRUE, WORDS, XSIZE, YSIZE
%Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(true,words,xsize,ysize)

if ~isempty(true)
    warning('Function TV or TVSCL : keyword TRUE not translated');
end;
if ~isempty(words)
    warning('Function TV or TVSCL : keyword WORDS not translated ');
end;
if ~isempty(xsize)
    warning('Function TV or TVSCL : keyword XSIZE not translated');
end;
if ~isempty(ysize)
    warning('Function TV or TVSCL : keyword YSIZE not translated');
end;

