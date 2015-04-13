% i2m_xminor
% --------------------------------------------
% Equivalent to :
% graphic keyword XMINOR = ...
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
% Fonction : fonction i2m_xminor
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 19 / 05 / 2003
% Modifications : 21 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_xminor(val)

if isempty(val)
	% Le keyword n'a pas ete passe en parametres
	% La variable systeme i2mvs_x va gerer toute seule le comportement par defaut
else
	if val <= -1
		set(gca,'XMinorTick','off');
	else
		if val > 0
			warning('Keyword XMINOR doesn''t have an exact equivalent : MATLAB will automaticaly determine the number of minor ticks');
			end
		set(gca,'XMinorTick','on');
	end
end
