% i2m_title
% --------------------------------------------
% Equivalent to :
% graphic keyword TITLE = "..."
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
% Fonction : fonction i2m_title
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 25 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_title(titre)

global i2mvs_p

if isempty(titre)
	% On regarde si !P.TITLE est definie
	if ~isempty(i2mvs_p.title)
		% On utilise !P.TITLE comme titre du graphe
		theText = strrep(i2mvs_p.title,'!C',sprintf('\n'));
		theText = strrep(theText,'!!','!');
		title(theText);
	end
else
	% TITLE a ete passe en keyword, on utilise la valeur du keyword
	theText = strrep(mat2str(titre),'!C',sprintf('\n'));
	theText = strrep(theText,'!!','!');
	title(theText);
end
