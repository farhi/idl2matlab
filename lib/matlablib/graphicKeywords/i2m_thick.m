% i2m_thick
% --------------------------------------------
% Equivalent to :
% graphic keyword THICK = ...
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
% Fonction : fonction i2m_thick
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 28 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_thick(h,size)

global i2mvs_p

if isempty(size)
	% Le Keyword THICK n'est pas precise
	% On ne fait rien
	% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
else
	% THICK a ete passe en keyword, on utilise la valeur du keyword
	if size > 0
		set(h,'LineWidth',size);
	else
		set(h,'LineWidth',0.5);
	end % if
end
