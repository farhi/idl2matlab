% i2m_yminor
% --------------------------------------------
% Equivalent to :
% graphic keyword YMINOR = ...
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
% Fonction : fonction i2m_yminor
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 19 / 05 / 2003
% Modifications : 21 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_yminor(val)

if isempty(val)
	% Le keyword n'a pas ete passe en parametre
	% La variable systeme i2mvs_y va gerer toute seule le comportement par defaut
else
	if val <= -1
		set(gca,'YMinorTick','off');
	else
		if val > 0
			warning('Keyword YMINOR doesn''t have an exact equivalent : MATLAB will automaticaly determine the number of minor ticks');
			end
		set(gca,'YMinorTick','on');
	end
end
