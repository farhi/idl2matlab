% errplott
% --------------------------------------------
% Equivalent to :
% function ERRPLOT, [ X, ] Low, High
%                [, WIDTH=value]
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
% Fonction : errplott
% Auteurs :
%                 Cortina Stephane
%                 Bourtembourg Reynald
% Date creation : 14 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% /WIDTH : ok

function [varargout]=errplott(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' ,'width', 'I2M_pos');
    I2Mkwv=    {'d1', 'd2' , 'd3' , 'larg' , 'I2M_pos'};

    %variables utilisées
		% variables systeme
		global i2mvs_p
    d1=[]; d2=[]; d3=[];
    % keywords
    larg='';
    d4='';
   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

% traitement de /WIDTH
if ~isempty(larg)
  largeur=larg;
else
  largeur=0.5;
end;

% initialisation du nombre de variables
nbvar=0;
if length(I2M_pos)==3
    nbvar=1;
end;

% initialisation du nombre d'éléments à afficher
taille=length(d2);

% pour le traitement des plots multiples dans une meme figure
multi = i2mvs_p.multi;
multi(1) = mod(multi(1)+1,multi(2)*multi(3));
i2mvs_p.multi = multi;
i2mvs_p.multioplot = 1; % pour empecher l'effacement de la figure courante dans le cas ou !P.MULTI[0] = 0

% Traitement des plots multiples dans une meme figure :
multiplots;

if nbvar==0
    for i=1:1:taille
        a=[i,i];
        b=[d1(i),d2(i)];
        line(a,b,'LineWidth',largeur);
    end
else
    for i=1:1:taille
        a=[d1(i),d1(i)];
        b=[d2(i),d3(i)];
        line(a,b,'LineWidth',largeur);
    end
end;

% On Met a jour la valeur de [XYZ].CRANGE
majCrange;

% Traitement des plots multiples dans une meme figure :
multiplotsAfter;

% pour le traitement des plots multiples dans une meme figure
i2mvs_p.multioplot = 0; % pour retablir l'effacement automatique de la figure courante dans le cas ou !P.MULTI[0] = 0

if I2M_out; eval(I2M_out); end;
