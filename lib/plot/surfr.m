% surfr
% --------------------------------------------
% Equivalent to :
% function SURFR [, AX=degrees] [, AZ=degrees]
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
% Fonction : SURFR 
%            Permet de calculer !P.T matrice de transformation.
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 22 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [varargout] = surfr(varargin)

%%%% Initialization of parameters
  I2Mkwn=char('ax', 'az', 'I2M_pos');
  I2Mkwv={'ax', 'az', 'I2M_pos'};
  ax=[]; az=[];

     I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%%%% End of parameters initialization

  if (eval('n_elements(ax)','0') == 0)
    ax = 30;
  end %if
  if (eval('n_elements(az)','0') == 0)
    az = 30;
  end %if
  % Translate to center about origin
  t3d('reset', 1, 'translate', d1_array(-.5,-.5,-.5));
  % rotate so +Z axis is now +Y
  t3d('rotate', d1_array(-90,az,0));
  t3d('rotate', d1_array(ax,0,0));
  % Scale it
  scale3d;

eval(I2M_out);
return;
% end of function surfr
