% i2m_orientation
% --------------------------------------------
% Equivalent to :
% graphic keyword ORIENTATION = ...
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
% Fonction : fonction i2m_orientation
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 02 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_orientation(angle,t)

global i2mvs_p

if isempty(angle)
	% Le Keyword ORIENTATION n'est pas precise
	% On ne fait rien
else
	% ORIENTATION a ete passe en keyword, on utilise la valeur du keyword
	set(t,'rotation',angle);
end
