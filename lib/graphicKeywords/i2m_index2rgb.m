
% i2m_index2rgb
% --------------------------------------------
% m = i2m_index2rgb(index)
% m(1) = R
% m(2) = G
% m(3) = B

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : fonction i2m_index2rgb
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 02 / 06 / 2003
% Modifications : 02 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function m = i2m_index2rgb(i)

a = 0; b = 0; c = 0;

a = mod(i,256);
c = fix(i/256^2);
b = fix(i/256) - c*256;

m = [a b c]/255.0;
