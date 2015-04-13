% shade_volume
% --------------------------------------------
% Equivalent to :
% function SHADE_VOLUME , Volume, Value, Vertex, Poly
%                       , /VERBOSE
%                       , XRANGE=vector
%                       , YRANGE=vector
%                       , ZRANGE=vector
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
% Fonction : SHADE_VOLUME
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 28 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:

% A FAIRE :
% SHADES

% KEYWORDS NON TRADUITS MAIS AVEC PEUT-ETRE EQUIVALENT MATLAB :
% /LOW : pas traduit


function [varargout]=shade_volume(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' ,'shades' , 'verbose' ,'xrange' ,'yrange' ,'zrange' ,'low','I2M_pos');
I2Mkwv=    {'volume' , 'value'  , 'vertex' , 'p'      ,'shadess', 'verbosee','xrangee','yrangee','zrangee','low','I2M_pos'};
    
% variables utilisées
% keywords
volume=[];value=[];vertex=[];p=[];
shadess=[];xrangee=[];yrangee=[];zrangee=[];
% keywords présent si =1
verbosee='';low='';

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

vol=volume;

% Traitement de XRANGE
if (~length(xrangee)==0)
    vol=vol(:,xrangee(1)+1:xrangee(2)+1,:);
end;

% Traitement de YRANGE
if (~length(yrangee)==0)
    vol=vol(yrangee(1)+1:yrangee(2)+1,:,:);
end;

% Traitement de ZRANGE
if (~length(zrangee)==0)
    vol=vol(:,:,zrangee(1)+1:zrangee(2)+1);
end;

% Calcul des isosurfaces du volume
[f,vertex]=isosurface(vol,value);

% Creation du vecteur p
j=1;
for i=1:length(f)
    p(j)=3;
    p(j+1:j+3)=f(i,:);
    j=j+4;
end;

% Traitement de SHADES
if ~isempty(shadess)
    warning('Function SHADE_VOLUME: keyword SHADES not translated');
end;
% Traitement de /LOW
if ~isempty(low)
    warning('Function SHADE_VOLUME: keyword LOW not translated');
end;


% Traitement de /VERBOSE
if ~isempty(verbosee)
    fprintf('SHADE_VOLUME: ' + int2str(length(f)) + ' polygones et ' + int2str(length(vertex)) + ' vertices');
end;

if I2M_out; eval(I2M_out); end;
