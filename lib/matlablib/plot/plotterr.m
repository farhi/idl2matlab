% plotterr
% --------------------------------------------
% Equivalent to :
% function PLOTERR, [ X ,] Y , Err
%                [, TYPE={1 | 2 | 3 | 4}]
%                [, PSYM=integer{1 to 10}]
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
% Fonction : fonction plotterr
% Auteurs :
%                 Cortina Stephane
%                 Bourtembourg Reynald
% Date creation : 10 / 04 / 2003
% Modifications : 13 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% /TYPE, /PSYM : ok

function [varargout]=plotterr(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'type' ,  'psym' ,   'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'd3' ,    'type_axe' ,  'symbole' , 'I2M_pos'};

    %variables utilisées
    %fonctions à afficher
    d1=[]; d2=[]; d3=[];
    %keywords
    type_axe=''; symbole='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

% Traitement des plots multiples dans une meme figure :
multiplots
% Traitement du cas ou on a !D.NAME = PS
devicePS;

% initialisation du nombre de fonctions à afficher

nbvar=2;
if ~isempty(d3),
    nbvar=3;
end;

%construction de la chaine d'options
option='-';

% traitement de /PSYM
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

if nbvar==2
    l=errorbar(d1,d2,option);
else
    l=errorbar(d1,d2,d3,option);
end;

% traitement de /TYPE
% configuration des axes : log ou lineaire
% attention : erreur dans l'aide IDL
if ~isempty(type_axe)
	switch type_axe
        case 4
			set(gca,'XScale','linear');
            set(gca,'YScale','linear');
		case 1
            set(gca,'XScale','linear');
			set(gca,'YScale','log');
		case 2
			set(gca,'XScale','log');
            set(gca,'YScale','linear');
		case 3
			set(gca,'XScale','log');
            set(gca,'YScale','log');

	end % switch
end % ~isempty

% traitement des plots multiples
multiplotsAfter;

if I2M_out; eval(I2M_out); end;
