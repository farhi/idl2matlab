 % oplott
% --------------------------------------------
% Equivalent to :
% function OPLOT, [X,] Y 
%              [, MAX_VALUE=value]
%              [, MIN_VALUE=value]
%              [, /POLAR]
%              [, THICK=value]
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
% Fonction : fonction oplott
% Auteurs :
%                 Szczuczak Nadege
%                 Cortina Stephane
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% /MAX_VALUE, /MIN_VALUE, /POLAR, et /THICK : OK
% /NSUM non traduit

function [varargout]=oplott(varargin)


I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'max_value', 'min_value', 'thick' , 'polar', 'nsum','I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'maxi', 'mini', 'epais' , 'polaire', 'nsum','I2M_pos'};

    % variables utilisées
		global itsAnOplot
		% variables systeme
		global i2mvs_p
    % fonctions à afficher
    d1=[]; d2=[];
    % keywords
    epais=''; maxi=''; mini=''; nsum='';
    % keywords présent si =1
    polaire='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

% Traitement de NSUM
if ~isempty(nsum)
    warning('Function OPLOT: keyword NSUM not translated');
end;

% pour le traitement des plots multiples dans une meme figure
if i2mvs_p.multi==0
else
    multi = i2mvs_p.multi;
    multi(1) = mod(multi(1)+1,multi(2)*multi(3));
    i2mvs_p.multi = multi;
end;
i2mvs_p.multioplot = 1; % pour empecher l'effacement de la figure courante dans le cas ou !P.MULTI[0] = 0

% appel plot
hold on;
itsAnOplot = 1;
% l=plott (varargin{:})
plott (varargin{:});
itsAnOplot = 0;
hold off;

% pour le traitement des plots multiples dans une meme figure
i2mvs_p.multioplot = 0; % pour retablir l'effacement automatique de la figure courante dans le cas ou !P.MULTI[0] = 0

if I2M_out; eval(I2M_out); end;

