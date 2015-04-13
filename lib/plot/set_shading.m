% set_shading
% --------------------------------------------
% Equivalent to :
% function SET_SHADING
%               , LIGHT=[x, y, z]
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
% Fonction : SET_SHADING
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 19 / 05 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%remarques:

function [varargout]=set_shading(varargin)

I2Mkwn=char('gouraud' ,'light' ,'reject' ,'values' ,'I2M_pos');
I2Mkwv=    {'gouraudd','lightt','rejectt','valuess','I2M_pos'};

%variables utilisées
%keywords
lightt=[];valuess=[];
%keywords présent si =1
gouraudd='';rejectt='';

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global light_axe light_ambient light_diffuse light_strength light_exponent light_reflection

%Traitement de LIGHT
if length(lightt)~=0
    light_axe=lightt;
end;

non_traite(gouraudd,rejectt,valuess);

if I2M_out; eval(I2M_out); end;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Fonction interne: NON_TRAITE(gouraudd,rejectt,valuess)
%Permet de traiter les keywords GOURAUD, REJECT, VALUES
%Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(gouraudd,rejectt,valuess)

if ~isempty(gouraudd)
    warning('Function SET_SHADING: keyword GOURAUD not translated');
end;

if ~isempty(rejectt)
    warning('Function SET_SHADING: keyword REJECT not translated');
end;

if length(valuess)~=0
    warning('Function SET_SHADING: keyword VALUES not translated');
end;  
