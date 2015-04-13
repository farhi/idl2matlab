% i2m_xyzstyle.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]STYLE = ...
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
% Fonction : fonction i2m_xyzstyle
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_xyzstyle(xstyle)

if ~isempty(xstyle)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword XSTYLE not translated');
end

if ~isempty(ystyle)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword YSTYLE not translated');
end

if ~isempty(zstyle)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword ZSTYLE not translated');
end

