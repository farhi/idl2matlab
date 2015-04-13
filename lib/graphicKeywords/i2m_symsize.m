% i2m_symsize
% --------------------------------------------
% Equivalent to :
% graphic keyword SYMSIZE = ...
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
% Fonction : fonction i2m_symsize
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 02 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_symsize(size,h)

global i2mvs_p

if isempty(size)
	% Le Keyword SYMSIZE n'est pas precise
	% On ne fait rien
else
	% SYMSIZE a ete passe en keyword
	set(h,'MarkerSize',(size)*6);
end
