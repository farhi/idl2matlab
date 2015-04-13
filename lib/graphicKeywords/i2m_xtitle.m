% i2m_xtitle
% --------------------------------------------
% Equivalent to :
% graphic keyword XTITLE = "..."
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
% Fonction : fonction i2m_xtitle
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 25 / 04 / 2003
% Modifications : 21 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_xtitle(titre)

global i2mvs_x

if isempty(titre)
	% On regarde si !X.TITLE est definie
	if ~isempty(i2mvs_x.title)
		% On utilise !X.TITLE comme titre du graphe
		theText = strrep(i2mvs_x.title,'!C',sprintf('\n'));
		theText = strrep(theText,'!!','!');
		xlabel(theText);
	end
else
	% TITLE a ete passe en keyword, on utilise la valeur du keyword
	theText = strrep(mat2str(titre),'!C',sprintf('\n'));
	theText = strrep(theText,'!!','!');
	xlabel(theText);
end
