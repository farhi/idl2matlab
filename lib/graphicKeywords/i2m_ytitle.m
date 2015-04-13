% i2m_ytitle
% --------------------------------------------
% Equivalent to :
% graphic keyword YTITLE = "..."
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
% Fonction : fonction i2m_ytitle
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 25 / 04 / 2003
% Modifications : 21 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_ytitle(titre)

global i2mvs_y

if isempty(titre)
	% On regarde si !Y.TITLE est definie
	if ~isempty(i2mvs_y.title)
		% On utilise !Y.TITLE comme titre du graphe
		theText = strrep(i2mvs_y.title,'!C',sprintf('\n'));
		theText = strrep(theText,'!!','!');
		ylabel(theText);
	end
else
	% TITLE a ete passe en keyword, on utilise la valeur du keyword
	theText = strrep(mat2str(titre),'!C',sprintf('\n'));
	theText = strrep(theText,'!!','!');
	ylabel(theText);
end
