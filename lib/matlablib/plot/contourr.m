% contourr
% --------------------------------------------
% Equivalent to :
% function CONTOUR, Z,[X,Y] , C_ANNOTATION=['string1','string2'....]
%                           , C_CHARSIZE=real
%                           , C_COLORS=[...]
%                           , C_LABELS=[0|1 , 0|1 , ...]
%                           , C_LINESTYLE=[0..5 , 0..5 , ...]
%                           , C_THICK = [int, int, ...]
%                           , /CELL_FILL
%                           , /DOWNHILL
%                           , /FILL
%                           , /FOLLOW
%                           , /IRREGULAR
%                           , /ISOTROPIC
%                           , LEVELS=[real, real, ...]
%                           , MAX_VALUE=real
%                           , MIN_VALUE=real
%                           , NLEVELS = int
%                           , /OVERPLOT
%                           , TRIANGULATION=variable
%                           , /XLOG
%                           , /YLOG
%                           , /ZAXIS
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
% Fonction : CONTOURR
%            Permet de tracer des lignes de niveaux.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 03 / 04 / 2003
% Modifications : 08 / 08 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Remarques:

% KEYWORDS SANS EQUIVALENT MATLAB :
% C_CHARTHICK : pas possible de le faire en Matlab => aucune action
% C_ORIENTATION : pas possible de le faire en Matlab => aucune action
% C_SPACING : pas possible de le faire en Matlab => aucune action
% /CLOSED : pas possible de le faire en Matlab => aucune action

% KEYWORDS NON TRADUITS MAIS AVEC PEUT-ETRE EQUIVALENT MATLAB :
% /PATH_DATA_COORDS : pas traduit
% /PATH_DOUBLE : pas traduit
% PATH_FILENAME : pas traduit
% PATH_INFO : pas traduit
% PATH_XY : pas traduit

% A FAIRE :
% /IRREGULAR : pas encore traduit => A FAIRE
% TRIANGULATION : pas encore traduit => A FAIRE
% /ZAXIS : pas encore traduit => A FAIRE

% COMMENTAIRES SUR LES KEYWORDS TRADUITS :
% C_ANNOTATION : le texte est ecrit horizontalement precede d'un + sur la ligne de niveau concernee
% C_CHARSIZE : on prend la conversion, charsizeMatlab = charsizeIDL*10
% C_LINESTYLE : - pas tout a fait les memes equivalents
%              - le cas (v lt 0.0) pas traite
% /CELL_FILL et /FILL : traite de la meme maniere
% /DOWNHILL : pas identique. En IDL, sur les lignes de niveaux, en Matlab, partout sur le graphe.
% /FOLLOW : mets des labels sur toutes les lignes de niveaux. En IDL, met des labels sur certaines lignes.
% MAX_VALUE : valeur >= pas mises a NaN mais a la valeur de MAX_VALUE
% MIN_VALUE : idem
% NLEVELS : =6 par defaut

function [varargout]=contourr(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' ,'irregular' ,'triangulation' ,'downhill' ,'follow' ,'overplot' ,'max_value' ,...
    'min_value','c_charsize', 'c_colors' ,'c_thick' ,'c_charthick','c_annotation','c_linestyle' ,'c_orientation' , 'c_spacing' ,...
    'closed' ,'levels'  , 'nlevels', 'c_labels' , 'isotropic'  ,'cell_fill','fill'  ,'path_data_coords','path_double','path_filename',...
    'path_info','path_xy','xlog' , 'ylog' , 'zaxis' ,'title' , 'xtitle', 'ytitle', 'ztitle', 'charsize' , 'charthick','position','device' , ...
    'ticklen','background','color','xminor','yminor','zminor','nodata' ,'noerase' ,'xstyle' ,'ystyle' , 'zstyle', 'clip', 'data',...
		'font','noclip','normal','subtitle','t3d','thick','xcharsize','ycharsize','zcharsize','xgridstyle','ygridstyle','zgridstyle',...
		'xrange','yrange','zrange','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv',...
		'xticks','yticks','zticks','zvalue','xticklen','yticklen','zticklen','I2M_pos');
I2Mkwv=    {'initd1' , 'initd2' , 'initd3' ,'irregularr','triangulationn','downhilll','followw','multiplot','maxi'      ,...
    'mini'     ,'charsize'  , 'colors'   ,'thick'   ,'charthick'  ,'annotation'  ,'styleligne'  ,'orientation'   , 'spacing'   ,...
    'closedd','niveaux' , 'nblevel', 'labels'   , 'axis_equal' ,'rempliBis','rempli','data_coords'     ,'pdouble'     ,'filename'     ,...
    'info'     ,'xy'     ,'logx' , 'logy' , 'zaxiss','titlee', 'labelx', 'labely', 'labelz', 'taillecar', 'epaissCar',  'pos'   , 'dev'   ,...
    'ticlen' , 'backColor','colorIndice','xminorTick','yminorTick','zminorTick','nodata','noerase','xstyle','ystyle', 'zstyle','clip', 'data',...
		'police','noclip','normal','subtitle','t3dKG','lthick','xcharsize','ycharsize','zcharsize','xgridstyle','ygridstyle','zgridstyle',...
		'xrange','yrange','zrange','xtick_get','ytick_get','ztick_get','xtickname','ytickname','ztickname','xtickv','ytickv','ztickv',...
		'xticks','yticks','zticks','zvalue','xticklen','yticklen','zticklen','I2M_pos'};

% variables utilisées
txt='';c='';plus='';
% fonctions à afficher
initd1=[]; initd2=[]; initd3=[];
% keywords
nblevel=6; niveaux=[]; labels=[];styleligne=[];annotation=[];
charsize='';charthick='';thick=[];maxi='';mini='';
colors=[]; triangulationn=''; filename=''; info=''; xy='';
orientation=''; spacing='';
% keywords présent si =1
logx=''; logy=''; axis_equal='';rempli='';multiplot='';followw='';
rempliBis='';downhilll='';closedd='';irregularr='';
data_coords=''; pdouble=''; zaxiss='';
% keywords graphiques
titlee=''; labelx=''; labely=''; labelz=''; taillecar=''; epaissCar=''; pos=[]; dev=''; ticlen=''; backColor=''; colorIndice=''; xminorTick = '';yminorTick = '';zminorTick = '';
nodata='';xstyle='';ystyle='';zstyle='';noerase='';clip='';data='';police='';noclip='';normal='';subtitle='';lthick='';xcharsize='';ycharsize='';zcharsize='';xgridstyle='';ygridstyle='';zgridstyle='';
noerase='';xrange='';yrange='';zrange='';xgridstyle='';ygridstyle='';zgridstyle='';t3dKG='';xtick_get='';ytick_get='';ztick_get='';
xtickname='';ytickname='';ztickname='';xtickv='';ytickv='';ztickv='';xticks='';yticks='';zticks='';zvalue='';xticklen='';yticklen='';zticklen='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global i2mColorTable i2mDecomposed i2mvs_p

% Traitement des plots multiples dans une meme figure :
multiplots
% Traitement de /NOERASE
i2m_noerase(noerase,'debut');
% Mise a jour eventuelle des couleurs de la figure courante
i2m_majColormap;
% Traitement du cas ou on a !D.NAME = PS
devicePS;

% initialisation du nombre de fonctions à afficher
nbvar=1;
if ~isempty(initd3),
    nbvar=3;
    d1=initd1;
    d2=initd2;
    d3=initd3;
else
    d1=initd1;
end;

% construction de la chaine d'options = traitement de levels et nlevels
option='';
if ~(length(niveaux)==0)
    option=niveaux;
    nblevel=length(niveaux);
else
    if ~isempty(nblevel)
        option=nblevel;
    end;
end;

% Traitement de MIN_VALUE et MAX_VALUE
% Petite difference d'avec IDL: toutes les valeurs >= MAX_VALUE sont mises a MAX_VALUE et de meme pour MIN_VALUE
% Dans IDL, apparemment elles sont mises a NaN. On peut aussi le faire dans Matlab,
% mais on a un comportement parfois un peu bizarre.
if ~isempty(mini)
    d1(d1 <= mini)=mini;
end;
if ~isempty(maxi)
    d1(d1 >= maxi)=maxi;
end;

% Traitement de /OVERPLOT
if ~isempty(multiplot)
    hold on;
    set(gca,'XLimMode','manual');
    set(gca,'YLimMode','manual');
    set(gca,'ZLimMode','manual');
end;

% Affichage de la courbe (traitement de CELL_FILL et FILL)
if (nbvar==1)
        s=size(d1);
        d2=(0:s(1)-1);
        d3=(0:s(2)-1);
end;
if (~isempty(rempli) | ~isempty(rempliBis))
    % Traitement de CELL_FILL et FILL
    [C,h]=contourf(d2,d3,d1,option);
else
    [C,h]=contour(d2,d3,d1,option);
end;

% Traitement de DOWNHILL
if (~isempty(downhilll)  & isempty(rempli) & isempty(rempliBis))
    [dx,dy]=gradient(d1);
    hold on
    quiver(d2,d3,dx,dy);
end;

% Recupere les valeurs des lignes de niveaux et les coordonnees d'un point de la ligne de niveau de maniere aleatoire
v=[]; coord=[];
index=1;
while index<=length(C)
    v=[v C(1,index)];
    y = fix(1 + (C(2,index)-1) * rand(1));
    coord=[coord C(:,y+index)];
    index=index+C(2,index)+1;
end;

% Traitement de C_ANNOTATION
% On utilise un try-catch car lors de la traduction, le tableau de string de C_ANNOTATION est transforme en un ojbet.
% La fonction size et length ne fonctionnent plus correctement. Elles retournent 1 meme s'il y a plus d'e 1 element dans le tableau.
nbannotation=0;
if (~isempty(annotation) & isempty(rempli) & isempty(rempliBis))
    try
        % On fait une boucle jusqu'a nblevel car annotation ne peut pas marquer plus de lignes de niveaux qu'il n'y en a.
        for i=1:nblevel
            txt(i)=text(coord(1,i),coord(2,i),'  ' + annotation(i));
            plus(i)=text(coord(1,i),coord(2,i),'+','HorizontalAlignment','center');
        end;
    catch
    end;
    % en sortant du try on a i-1=nombre d'element dans annotation
    nbannotation=i-1;
end;

% Traitement de C_LABELS et FOLLOW
% Si C_LABELS + FOLLOW : on ne traite que C_LABELS
if ((~isempty(followw) | ~(length(labels)==0)) & isempty(rempli) & isempty(rempliBis))
    % On retire les lignes de niveaux deja marquees par annotation
    copie=v;
    copie(1:nbannotation)=[];
    if ~isempty(followw)
        w=copie;
    end;
    if ~(length(labels)==0)
        w=[]; j=1;
        for i=1:length(labels)
            if labels(i)~=0
                w(j)=copie(i);
                j=j+1;
            end;
        end;
    end;
    c=clabel(C,h,w);
end;

% Traitement de C_CHARSIZE
if ~(isempty(charsize))
    charsize=charsize*10;
    if ~isempty(c)
        set(c,'FontSize',charsize);
    end;
    if ~isempty(txt)
        set(txt,'FontSize',charsize);
    end;
    if ~isempty(plus)
        set(plus,'FontSize',charsize);
    end;
end;

% Traitement de C_LINESTYLE
sl=['- ';': ';'--';'-.';'-.';'--'];
if (~(length(styleligne)==0) & isempty(rempli) & isempty(rempliBis))
    index=1;
    for i=1:length(h)
        if mod(i,length(styleligne))==1
            index=1;
        end;
        if mod(i,length(styleligne))==0
            index=length(styleligne);
        end;
        s=strrep(sl(styleligne(index)+1,:),' ','');
        set(h(i),'LineStyle',s);
        index=index+1;
    end;
end;

% Traitement de C_COLORS
if ~(length(colors)==0)
   j=1;
    for i=1:length(h)
        if j==length(colors)+1
            j=1;
        end;
        % On colorie la zone si on a CELL_FILL ou FILL et juste la ligne si ils ne sont pas specifies.
        if ( ~isempty(rempli) | ~isempty(rempliBis) )
		set(h(i),'FaceColor',i2mColormap(colors(j)));
        end;
	        set(h(i),'EdgeColor',i2mColormap(colors(j)));
        j=j+1;
    end;
end;

% Traitement de C_THICK
if (~(length(thick)==0) & isempty(rempli) & isempty(rempliBis))
    index=1;
    for i=1:length(h)
        if mod(i,length(thick))==1
            index=1;
        end;
        if mod(i,length(thick))==0
            index=length(thick);
        end;
        set(h(i),'LineWidth',thick(index));
        index=index+1;
    end;
end;

% traitement de /XLOG
if (logx==1)
  set(gca,'XScale','log');
end;

% traitement de /YLOG
if (logy==1)
  set(gca,'YScale','log');
end;

% traitement de /ISOTROPIC
if (axis_equal==1)
    axis equal
end


% Traitement de C_CHARTHICK, C_ORIENTATION, C_SPACING, /CLOSED,
% PATH_DATA_COORDS, PATH_DOUBLE, PATH_FILENAME, PATH_INFO, PATH_XY
non_traite(charthick,orientation,spacing,closedd,data_coords,pdouble,filename,info,xy);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% TRAITEMENT DES KEYWORDS GRAPHIQUES %%%%%%%%%%%%
% Traitement de /BACKGROUND
i2m_background(backColor);
% traitement de /COLOR
i2m_color(colorIndice,h,'contour');
% Traitement de /SUBTITLE
i2m_subtitle(subtitle);
% traitement de /TITLE
i2m_title(titlee);
% traitement de /[XYZ]TITLE
i2m_xtitle(labelx);
i2m_ytitle(labely);
i2m_ztitle(labelz);
% Traitement de /CHARSIZE et de /[XYZ]CHARSIZE
i2m_charsize(taillecar,police,xcharsize,ycharsize,zcharsize);
% Traitement de /CHARTHICK
i2m_charthick(epaissCar,gca);
% Traitement de /POSITION
i2m_position(pos,dev);
% Traitement de /TICKLEN et /[XYZ]TICKLEN
i2m_ticklen(ticlen,xticklen,yticklen,zticklen);
% Traitement de /THICK
i2m_thick(h,lthick);
% Traitement de /[XYZ]MINOR
i2m_xminor(xminorTick);
i2m_yminor(yminorTick);
i2m_zminor(zminorTick);
% Traitement de CLIP
i2m_clip(clip);
% Traitement de DATA
i2m_data(data);
% Traitement de DEVICE
i2m_device(dev);
% Traitement de FONT
i2m_font(police);
% Traitement de NOCLIP
i2m_noclip(noclip);
% Traitement de /NODATA
i2m_nodata(nodata,h);
% Traitement de /NORMAL
i2m_normal(normal);
% Traitement de /T3D
i2m_t3d(t3dKG);
% traitement de ZVALUE
traitementZvalue(zvalue,t3dKG,h)
% Traitement de [XYZ]TICKV et [XYZ]TICKS
i2m_xyztickv(xtickv,xticks,ytickv,yticks,ztickv,zticks);
% Traitement de [XYZ]TICKNAME
i2m_xyztickname(xtickname,ytickname,ztickname);
% Traitement de [XYZ]GRIDSTYLE
if ~isempty(t3dKG)
    if t3dKG == 1
        % courbe en 3D
        i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,1);
    else
        % courbe en 2D
        i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,0);
    end
else
    if i2mvs_p.t3d == 1
        % courbe en 3D
        i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,1);
    else
        % courbe en 2D
        i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,0,0);
    end
end
% Traitement de [XYZ]RANGE
i2m_xyzrange(xrange,yrange,zrange);
% Traitement de [XYZ]STYLE
i2m_xyzstyle(xstyle,ystyle,zstyle,0);
% Traitement de [XYZ]TICK_GET
[xtick_get,ytick_get,ztick_get] = i2m_xyztick_get(xtick_get,ytick_get,ztick_get);
% Traitement de /NOERASE
i2m_noerase(noerase,'fin');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% On met a jour la valeur de [XYZ].CRANGE
majCrange;

% Fin du traitement de /OVERPLOT
if (i2mvs_p.noerase == 0) & (~isempty(multiplot) | ~isempty(downhilll))
    hold off;
end;


%%%%%% traitement des plots multiples %%%%%%%%%%%%%%%%%%%%%%%%
multiplotsAfter;

if I2M_out; eval(I2M_out); end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(charthick,orientation,spacing,closedd,data_coords,double,filename,info,xy)
% Permet de traiter les keywords C_CHARTHICK, C_ORIENTATION, C_SPACING, /CLOSED,
% PATH_DATA_COORDS, PATH_DOUBLE, PATH_FILENAME, PATH_INFO et PATH_XY
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(charthick,orientation,spacing,closedd,data_coords,pdouble,filename,info,xy)

if ~isempty(charthick)
    warning('Function CONTOUR: keyword C_CHARTHICK not translated (no equivalent in Matlab)');
end;
if ~isempty(orientation)
    warning('Function CONTOUR: keyword C_ORIENTATION not translated (no equivalent in Matlab)');
end;
if ~isempty(spacing)
    warning('Function CONTOUR: keyword C_SPACING not translated (no equivalent in Matlab)');
end;
if ~isempty(closedd)
    warning('Function CONTOUR: keyword /CLOSED not translated (no equivalent in Matlab)');
end;
if ~isempty(data_coords)
    warning('Function CONTOUR: keyword PATH_DATA_COORDS not translated');
end;
if ~isempty(pdouble)
    warning('Function CONTOUR: keyword PATH_DOUBLE not translated');
end;
if ~isempty(filename)
    warning('Function CONTOUR: keyword PATH_FILENAME not translated');
end;
if ~isempty(info)
    warning('Function CONTOUR: keyword PATH_INFO not translated');
end;
if ~isempty(xy)
    warning('Function CONTOUR: keyword PATH_XY not translated');
end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: traitementZvalue(zvalue,h)
% permet de traiter le keyword zvalue
% zvalue = valeur du keyword passe en parametres (vide si Keyword non passe)
% h handle des lignes de contour
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function traitementZvalue(zvalue,t3d,h)

global i2mvs_p

if ~isempty(zvalue)
	% ZVALUE a ete passe en parametres
	if ~isempty(t3d)
		if t3d == 1
			% T3D a ete passe en parametres et vaut 1, OK
			zlimites = zlim;
			if zvalue > 1
				zvalue = 1;
			end
			if zvalue < 0
				zvalue = 0;
			end
			zval = zlimites(1) + zvalue * (zlimites(2) - zlimites(1));
			for i=1:length(h)
				tmp = get(h(i),'XData');
				tmp(:) = zval;
				set(h(i),'ZData',tmp);
			end
		end
	else
		% t3d n'a pas ete passe en parametres
		if i2mvs_p.t3d == 1
			% !P.T3D est active, On tient compte de zvalue
			zlimites = zlim;
			if zvalue > 1
				zvalue = 1;
			end
			if zvalue < 0
				zvalue = 0;
			end
			zval = zlimites(1) + zvalue * (zlimites(2) - zlimites(1));
			for i=1:length(h)
				tmp = get(h(i),'XData');
				tmp(:) = zval;
				set(h(i),'ZData',tmp);
			end
		end
	end
end