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
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Traiter le cas ou PSYM = 10 => histogramme
% ynozero => auto en matlab ??
% /MAX_VALUE, /MIN_VALUE, /POLAR, /ISOTROPIC, /XLOG, /YLOG, /THICK et /YNOZERO : OK
% /NSUM non traduit

function [varargout]=plott(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'title' , 'xtitle' , 'ytitle', 'ztitle','psym' , 'linestyle' , 'max_value', 'min_value', 'thick' , 'polar','nsum', 'isotropic', 'xlog' , 'ylog', 'ynozero', 'charsize','charthick', 'position', 'device', 'ticklen', 'symsize' ,'background',   'color'   ,  'xminor'  ,  'yminor'  ,  'zminor'  ,'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'titlee' , 'labelx' ,'labely', 'labelz','symbole' , 'styleligne' , 'maxi', 'mini', 'epais' , 'polaire', 'nsum','axis_equal' , 'logx', 'logy', 'ynoz', 'taillecar', 'epaisseurCar', 'pos'    ,  'dev'  , 'ticlen' ,'tailleSym','backColor' ,'colorIndice','xminorTick','yminorTick','zminorTick','I2M_pos'};

    % variables utilisées
		global itsAnOplot
    % fonctions à afficher
    d1=[]; d2=[];
    % keywords
    symbole=''; styleligne=''; epais=''; maxi=''; mini='';nsum='';
    % keywords présent si =1
    polaire=''; logx=''; logy=''; axis_equal=''; ynoz='';
		% keywords graphiques
		titlee=''; labelx=''; labely=''; labelz = ''; taillecar='';epaisseurCar=''; pos=[]; dev=''; ticlen=''; tailleSym=''; backColor=''; colorIndice='';xminorTick = '';yminorTick = '';zminorTick = '';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% Traitement de NSUM
if ~isempty(nsum)
    warning('Function PLOT: keyword NSUM not translated');
end;

% Traitement des plots multiples dans une meme figure :
multiplots;
% Mise a jour eventuelle des couleurs de la figure courante
i2m_majColormap;
% Traitement du cas ou on a !D.NAME = PS
devicePS;

% initialisation du nombre de fonctions à afficher
nbvar=1;
if ~isempty(d2),
    nbvar=2;
end;


% traitement de /MAX_VALUE et /MIN_VALUE

    if (nbvar==2)
        if (~isempty(maxi))
            if (maxi > max(d2(:)))
                % si le maxi est plus grand que la valeur maximale à afficher, on ajuste le maxi
                maxi=max(d2(:));
            else
                % on cherche les valeurs supérieures à maxi et on les remplace par NaN
                j=find(d2(:)>maxi);
                d2(j)=NaN;
            end
        else
            % si maxi n'est pas définit, on l'initialise à la plus grande valeur de d2
            maxi=max(d2(:));
        end
        if (~isempty(mini))
            if (mini < min(d2(:)))
                % si le mini est plus petit que la valeur minimale à afficher, on ajuste le mini
                mini=min(d2(:));
            else
                % on cherche les valeurs inférieures au mini et on les remplace par NaN
                i=find(d2(:)<mini);
                d2(i)=NaN;
            end
        else
            mini=min(d2(:));
        end
    elseif (nbvar==1)
        % traitement similaire à nbvar==2
        if (~isempty(maxi)),
            if (maxi > max(d1(:))),
                maxi=max(d1(:));
            else
                j=find(d1(:)>maxi);
                d1(j)=NaN;
            end
        else
            maxi=max(d1(:));
        end
        if (~isempty(mini))
            if (mini < min(d1(:)))
                mini=min(d1(:));
            else
                i=find(d1(:)<mini);
                d1(i)=NaN;
            end
        else
            mini=min(d1(:));
        end
    end
% end traitement de /MAX_VALUE et /MIN_VALUE

%traitement de /POLAR + appel plot
if nbvar==2;
   if ~isempty(polaire)
        l=polar(d1,d2);
    else
        l=plot(d1,d2);
    end
else
 	l=plot((0:length(d1)-1),d1);
end;    

%traitement de /ISOTROPIC
if ~isempty(axis_equal)
    axis equal
end

%traitement de /XLOG
if ~isempty(logx)
  set(gca,'XScale','log');
end;

%traitement de /YLOG
if ~isempty(logy)
  set(gca,'YScale','log');
end;
        
%traitement de /YNOZERO
%Ynozero présent -> comportement par défaut dans IDL
if isempty(ynoz)
    if (nbvar==2)
        if min(d2)>0
           mini=0;
        end
    elseif (nbvar==1)
        if min(d1)>0,
           mini=0;
        end
    end
end

if ~(mini>maxi)
    %appel à la fonction ylim de matlab
    %on redéfinit la limite de l'axe y
    if isempty(polaire)
        ylim([mini maxi]);
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%%%%%%%%
% Traitement de /BACKGROUND
i2m_background(backColor);
% traitement de /COLOR
if ~isempty(itsAnOplot)
	if itsAnOplot == 1
		i2m_color(colorIndice,l,'oplot');
	else
		i2m_color(colorIndice,l,'plot');
	end
else
	i2m_color(colorIndice,l,'plot');
end
% Traitement de /CHARSIZE
i2m_charsize(taillecar);
% Traitement de /CHARTHICK
i2m_charthick(epaisseurCar);
% Traitement de /THICK
i2m_thick(l,epais);
% Traitement de /PSYM
i2m_psym(symbole,l);
% Traitement de /LINESTYLE
i2m_linestyle(styleligne,l,symbole);
% Traitement de /SYMSIZE
i2m_symsize(tailleSym,l);
% Traitement de /TICKLEN
i2m_ticklen(ticlen);
% Traitement de /POSITION
i2m_position(pos,dev);
% Traitement de /TITLE
i2m_title(titlee);
% Traitement de /[XYZ]TITLE
i2m_xtitle(labelx);
i2m_ytitle(labely);
i2m_ztitle(labelz);
% Traitement de /[XYZ]MINOR
i2m_xminor(xminorTick);
i2m_yminor(yminorTick);
i2m_zminor(zminorTick);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% On Met a jour la valeur de [XYZ].CRANGE
majCrange;

%%%%%% traitement des plots multiples %%%%%%%%%%%%%%%%%%%%%%%%
multiplotsAfter;

if I2M_out; eval(I2M_out); end;

