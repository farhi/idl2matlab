% scale3
% --------------------------------------------
% Equivalent to :
% function SCALE3   , XRANGE=vector
%                   , YRANGE=vector
%                   , ZRANGE=vector
%                   , AX=degrees
%                   , AZ=degrees
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
% Fonction : SCALE3
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 22 / 04 / 2003
% Modifications : 13 / 06 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Remarques:
%Faire les variables systemes !X !Y !Z

function [varargout] = scale3(varargin)

%%%% Initialization of parameters
  I2Mkwn=char('ax', 'az', 'xrange', 'yrange', 'zrange', 'I2M_pos');
  I2Mkwv={'ax', 'az', 'xr', 'yr', 'zr', 'I2M_pos'};
  ax=[]; az=[]; xr=[]; yr=[]; zr=[];

     I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%%%% End of parameters initialization

  global i2mvs_x i2mvs_y i2mvs_z % SYSTEM VARIABLES

  if (eval('n_elements(ax)','0') == 0)
    % Supply defaults
    ax = 30;
  end %if
  if (eval('n_elements(az)','0') == 0)
    az = 30;
  end %if
  if (eval('n_elements(xr)','0') >= 2)
    i2mvs_x.s = d1_array(-xr(1),1.) ./ (xr(2) - xr(1));
  end %if
  if (eval('n_elements(yr)','0') >= 2)
    i2mvs_y.s = d1_array(-yr(1),1.) ./ (yr(2) - yr(1));
  end %if
  if (eval('n_elements(zr)','0') >= 2)
    i2mvs_z.s = d1_array(-zr(1),1.) ./ (zr(2) - zr(1));
  end %if
  
  % Translate to center about origin, then scale down by 1/sqrt(3)
  % so that the corners don't stick out.
  t3d('reset', 1, 'translate', d1_array(-.5,-.5,-.5), 'scale', replicate(1. ./ sqrt(3),3));
  
  % rotate so +Z axis is now +Y
  t3d('rotate', d1_array(-90,az,0));
  t3d('rotate', d1_array(ax,0,0));
  
  % & back to 0,1 cube
  t3d('translate', d1_array(.5,.5,.5));

eval(I2M_out);
return;
% end of function scale3
