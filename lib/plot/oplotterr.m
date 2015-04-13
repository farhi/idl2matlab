% oplotterr
% --------------------------------------------
% Equivalent to :
% function OPLOTERR, [ X ,] Y , Err [,psym]
% in IDL
% X : An optional array of X values.
% Y : The array of Y values. Y cannot be of type string.
% Err : The array of error bar values
% PSYM : The plotting symbol to use (default = +7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : fonction oplotterr
% Auteurs :
%                 Cortina Stephane
%                 Bourtembourg Reynald
%                 Szczuczak Nadege
% Date creation : 10 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Remarques
% ATTENTION : On modifie la valeur de XLim => Pb si on a fixe des limites pour les axes avec la variable systeme RANGE.



function [varargout]=oplotterr(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' ,   'psym' ,   'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'd3' ,    'd4' , 'I2M_pos'};

    %variables utilisées
		% variables systeme
		global i2mvs_p
    global itsAnOplot
    %fonctions à afficher
    d1=[]; d2=[]; d3=[];
    % keywords
     d4='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

% initialisation de la variable qui pour l'enventuelle valeur de psym
symbole=7;
if ~isempty(d3)
    if length(d3) == 1
        symbole=d3;
    end;
end;

if ~isempty(d4)
    symbole=d4;
end;

% construction de la chaine d'options
option='-';

%traitement de /PSYM
if ~isempty(symbole)
	switch symbole
		case 0
			% option = option + '-';
		case 1
			option = strcat(option,'+');
		case 2
			option = strcat(option,'*');
		case 3
			option = strcat(option,'.');
		case 4
			option = strcat(option,'d');
		case 5
			option = strcat(option,'^');
		case 6
			option = strcat(option,'s');
		case 7
			option =strcat(option, 'x');
	end
end


% pour le traitement des plots multiples dans une meme figure
if i2mvs_p.multi==0
else
    multi = i2mvs_p.multi;
    multi(1) = mod(multi(1)+1,multi(2)*multi(3));
    i2mvs_p.multi = multi;
end;
i2mvs_p.multioplot = 1; % pour empecher l'effacement de la figure courante dans le cas ou !P.MULTI[0] = 0
multiplots;

%traitement oploterr avec errorbar
hold on;
itsAnOplot = 1;
if isempty(d3) | (~isempty(d3) & length(d3)==1)
    l=errorbar(d1,d2,option);
else
    l=errorbar(d1,d2,d3,option);
end;

% Dans certains cas, l'axe des x ne prend pas les bonnes limites.
% On cherche la valeur max de x pour modifier la propriete XLim.
h=findobj(gca,'type','line');
f=find(h==l(1));
h(f)=[];
xmin=min(get(h(1),'Xdata'));
xmax=max(get(h(1),'Xdata'));
for i=2:length(h)
    if min(get(h(i),'Xdata'))<xmin
        xmin=min(get(h(i),'Xdata'));
    end;
    if max(get(h(i),'Xdata'))>xmax
        xmax=max(get(h(i),'Xdata'));
    end;
end;
r=get(gca,'XLim');
if r(1)<xmin-10 | r(2)>xmax+10
    set(gca,'XLim',[xmin,xmax]);
end;

itsAnOplot = 0;
hold off

% pour le traitement des plots multiples dans une meme figure
i2mvs_p.multioplot = 0; % pour retablir l'effacement automatique de la figure courante dans le cas ou !P.MULTI[0] = 0

% Traitement des plots multiples dans une meme figure :
multiplotsAfter;

if I2M_out; eval(I2M_out); end;



