% i2m_zminor
% --------------------------------------------
% Equivalent to :
% graphic keyword ZMINOR = ...
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
% Fonction : fonction i2m_zminor
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 19 / 05 / 2003
% Modifications : 21 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_zminor(val)

if isempty(val)
	% Le keyword n'a pas ete passe en parametre
	% La variable systeme i2mvs_z va gerer toute seule le comportement par defaut
else
	if val <= -1
		set(gca,'ZMinorTick','off');
	else
		if val > 0
			warning('Keyword ZMINOR doesn''t have an exact equivalent : MATLAB will automaticaly determine the number of minor ticks');
			end
		set(gca,'ZMinorTick','on');
	end
end
