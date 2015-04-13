% i2m_nodata
% --------------------------------------------
% Equivalent to :
% graphic keyword NODATA = ...
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
% Fonction : fonction i2m_font
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_nodata(noData)

if ~isempty(noData)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword NODATA not translated');
end

