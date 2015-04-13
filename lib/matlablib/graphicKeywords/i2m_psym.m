% i2m_psym
% --------------------------------------------
% Equivalent to :
% graphic keyword PSYM = ...
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
% Fonction : fonction i2m_psym
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 29 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CAS OU PSYM = 10 => HISTOGRAMME non traite
% CAS ou PSYM = 8 => symbole defini par l'utilisateur non traite
% PB avec la fonction plots

function i2m_psym(symbole,h)

global i2mvs_p

if isempty(symbole)
	% Le Keyword PSYM n'est pas precise
	% On ne fait rien
	% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
else
	% PSYM a ete passe en keyword, on utilise la valeur du keyword
	switch symbole
		case 0
			set(h,'Marker','none');
		case 1
			set(h,'Marker','+');
		case 2
			set(h,'Marker','*');
		case 3
			set(h,'Marker','.');
		case 4
			set(h,'Marker','d');
		case 5
			set(h,'Marker','^');
		case 6
			set(h,'Marker','s');
		case 7
			set(h,'Marker','x');
		otherwise
			warning('Graphic keyword PSYM : bad value or not yet translated')
	end % switch val
end
