% axxis
% --------------------------------------------
% Equivalent to :
% function AXIS [[[, X], Y], Z]
%                      , XAXIS= "0..1"
%                      , YAXIS = 0..1,
%                      , ZAXIS = 0..4,
%                      , /XLOG
%                      ,/YLOG
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
% Modifications : 16 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ynozero => auto en matlab ??
% fonction axis non terminee :
%   UN seul systeme d'axes -> equivalence avec IDL difficile
%   position modifiee -> axes + courbe déplacés !
% keywords non traduits :  /SAVE   ,/YNOZERO    
% Traiter les keywords graphiques

function [varargout]=axxis(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'save' , 'xaxis', 'yaxis' , 'zaxis' , 'xlog', 'ylog', 'ynozero', 'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'd3' , 'save' , 'xax' , 'yax' , 'zax' , 'logx', 'logy', 'ynoz', 'I2M_pos'};
    % variables utilisées
    % fonctions à afficher
    d1=[]; d2=[]; d3=[];
    % keywords
    save=''; xax=''; yax=''; zax='';
    % keywords présent si =1
    logx=''; logy=''; ynoz='';

    % variables locales
        pas_de_param=0;
        
   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

% couleur du fond afin d' "effacer" les axes non désirés
% bgcolor=get(gca,'color');
set(gca,'Units','pixel');

% valeur par defaut : xax=1
if ((isempty(xax))&(isempty(yax))&(isempty(zax)))
    xax=1;
end

% "taille" des axes
Xrange=get(gca,'XLim');
Yrange=get(gca,'YLim');
Zrange=get(gca,'ZLim');

% 3 coordonnees sont passées en parametre
if ~isempty(d3)
    % 2 coordonnees sont passées en parametre   
else if ~isempty(d2)  
        d3=0;
        % 1 coordonnée passée en parametre
    else if ~isempty(d1)
            d2=0;
            d3=0;
          else
              % pas de param X,Y et Z
              pas_de_param=1;
        end
    end
end
if (pas_de_param==0)    
    % affichage de l'axe des 'X'
    if ~isempty(xax)
        tic=get(gca,'Xtick');
        l=length(tic);
        % on trace une ligne representant l'axe
        if (Yrange(1)<=d2)&(Yrange(2)>=d2)
            line([Xrange(1) Xrange(2)],[d2 d2],[d3 d3]);
            % on affiche les ticks vers l'interieur ou l'exterieur
            if (xax==1)
              for i=1:1:l
                line([tic(i),tic(i)],[d2+1 d2],[d3 d3]);
              end
            end
            if (xax==0)
                for i=1:1:l
                    line([tic(i),tic(i)],[d2-1 d2],[d3 d3]);
                end
            end
       end
   else
    % affichage de l'axe des 'Y'
        if ~isempty(yax)
            tic=get(gca,'Ytick');
            l=length(tic);
            % on trace une ligne representant l'axe
            if (Xrange(1)<=d1)&(Xrange(2)>=d1)
                line([d1 d1], [Yrange(1) Yrange(2)],[d3 d3]);
                % on affiche les ticks vers l'interieur ou l'exterieur
                if (yax==1)
                    for i=1:1:l
                         line([d1 d1+1],[tic(i),tic(i)],[d3 d3]);
                    end
                end
                if (yax==0)
                     for i=1:1:l
                         line([d1 d1-1],[tic(i),tic(i)],[d3 d3]);
                     end
                end
            end
        else
        % affichage de l'axe des 'Z'
            if ~isempty(zax)
                tic=get(gca,'Ztick');
                l=length(tic);
                % on trace une ligne representant l'axe
                if (Xrange(1)<=d1)&(Xrange(2)>=d1)&(Yrange(1)<=d2)&(Yrange(2)>=d2)
                    line([d1 d1], [d2 d2], [Zrange(1) Zrange(2)]);
                    % on affiche les ticks dans la direction voulue
                    if (zax==0)
                        for i=1:1:l
                            line([d1 d1-1],[d2,d2],[tic(i) tic(i)]);
                        end
                    end
                    if (zax==1)
                        for i=1:1:l
                            line([d1 d1+1],[d2,d2],[tic(i) tic(i)]);
                        end
                    end
                    if (zax==2)
                        for i=1:1:l
                            line([d1 d1],[d2,d2-1],[tic(i) tic(i)]);
                        end
                    end
                    if (zax==3)
                        for i=1:1:l
                            line([d1 d1],[d2,d2+1],[tic(i) tic(i)]);
                        end
                    end
                end
            end
        end
    end
    % fin pas_de_param=1
else
    % pas de parametre 'X' , 'Y', 'Z'
    if ~isempty(xax)
        % affichage de l'axe des 'X' sur ou sous la courbe
        if (xax==1)
            set(gca,'XAxisLocation','top');
        end
        if (xax==0)
            set(gca,'XAxisLocation','bottom');
        end
    end
    % affichage de l'axe des 'Y' à gauche ou à droite de la courbe
    if ~isempty(yax)
        if (yax==1)
            set(gca,'YAxisLocation','right');
        end
        if (yax==0)
            set(gca,'YAxisLocation','left');
        end
    end
end

% traitement de /XLOG
if ~isempty(logx)
  set(gca,'XScale','log');
end;

% traitement de /YLOG
if ~isempty(logy)
  set(gca,'YScale','log');
end;

box off;

% Traitement de SAVE, YNOZERO
non_traite(save,ynoz);

% mise à jour de la valeur systeme [XYZ].Crange
%majCrange;

if I2M_out; eval(I2M_out); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(save,ynoz)
% Permet de traiter les keywords /SAVE, /YNOZERO
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(save,ynoz)

if ~isempty(save)
    warning('Function AXIS: keyword SAVE not translated');
end;
if ~isempty(ynoz)
    warning('Function AXIS: keyword YNOZERO not translated');
end;


