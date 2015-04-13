% i2m_charthick
% --------------------------------------------
% Equivalent to :
% graphic keyword CHARTHICK = ...
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
% Fonction : fonction i2m_charthick
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 28 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_charthick(size)

global i2mvs_p

if isempty(size)
	% Le Keyword CHARTHICK n'est pas precise
	% On ne fait rien
	% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
else
	% CHARTHICK a ete passe en keyword, on utilise la valeur du keyword
	if i2mvs_p.font ~= 0
		% Alors le keyword charthick a un sens
		% h = findobj(gco,'Type','text')
		if size >= 2
			set(gca,'FontWeight','bold');
			% set(h,'FontWeight','bold');
		else
			set(gca,'FontWeight','normal');
			% set(h,'FontWeight','normal');
		end
	else
		% Le keyword n'a pas de sens => on ne fait rien
	end
end
