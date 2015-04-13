% tvscl
% --------------------------------------------
% Equivalent to :
% function  TVSCL, Image [, Position]
%    ou
%           TVSCL, Image [, X, Y [, Channel]]
%                   /CENTIMETERS
%                   /CHANNEL
%                   /INCHES
%                   /ORDER
%                   /TRUE  -
%                   /WORDS -
%                   /XSIZE -
%                   /YSIZE -
%                   /NAN    -
%                   /TOP    -
%                   
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
% Date creation : 24 / 04 / 2003
% Modifications : 16 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [varargout]=tvscl(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' ,'centimeters','channel','inches','order','true','words','xsize','ysize','nan','top', 'I2M_pos');
    I2Mkwv=    {'d1'     , 'd2' , 'd3' , 'd4' ,'cent','chan','inches','order','true','words','sizex','sizey','nan','top', 'I2M_pos'};
    % variables utilisées
    % fonctions à afficher
    d1=[]; d2=[]; d3=[]; d4=[];    
    % keywords présent si =1
    cent=''; inches=''; order=''; 
    % keywords
    chan=''; top='';sizex='';sizey='';words='';true='';nan='';
    % variables locales
    i='';map='';map2=[];l=''; clims=[];
    % variable globale
    global axes_wd

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

f(1,1,:)=[0,0,0];
map = i2mColormap;
d = bytscl(d1);
s = size(d);

 if length(s) == 2
    for i=1:s(1)
       for j=1:s(2)
            f(i,j,:)=map(d(i,j)+1,:);
        end;
    end;
end;
size(f)
tv('I2M_a1',f,'I2M_a2',d2 , 'I2M_a3',d3 , 'I2M_a4',d4 ,'centimeters',cent,'channel',chan,'inches',inches,'order',order,'true',true,'words',words,'xsize',sizex,'ysize',sizey,'I2M_pos',I2M_pos);

% Traitement des keywords non traduits
if ~isempty(nan)
	warning('function TVSCL : KEYWORD NAN not translated');
end
if ~isempty(top)
	warning('function TVSCL : KEYWORD TOP not translated');
end

if I2M_out; eval(I2M_out); end;

