% scale3d
% --------------------------------------------
% Equivalent to :
% function SCALE3D
% en IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : SCALE3D
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 22 / 04 / 2003
% Modifications : 24 / 04 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function scale3d()

  global i2mvs_p; % SYSTEM VARIABLES
  
  f=[0 0 0 1;1 0 0 1;0 1 0 1;1 1 0 1;0 0 1 1;1 0 1 1;0 1 1 1;1 1 1 1];
  
  for i = 0:7,
    % Find 8 corners
    p =  f(i+1,:)*i2mvs_p.t; 
    % normalize homogenous coords
    p = p ./ p(4);
   
    if (i == 0)
      pmin = p;
      pmax = p;
    else
      pmin = min(pmin, p);
      pmax = max(pmax, p);
    end %if
  end %for
 
  if (pmax(3) == pmin(3))
    pmax(3) = pmin(3) + 1;
  end %if

  t3d('tr', d1_array(-pmin(1),-pmin(2),-pmin(3)), 'sc', 1./ (pmax((1):(3)) - pmin((1):(3))));

return;
% end of function scale3d


