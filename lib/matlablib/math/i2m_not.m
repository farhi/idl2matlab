% i2m_not
% --------------------------------------------
% Equivalent to :
% operator NOT  
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
% Fonction : i2m_not
%
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 03 / 09 / 2003
% Modifications : 03 / 09 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function result = i2m_not(var)
    result = -(var+1);
return