% i2m_xyzticklayout.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]TICKLAYOUT = ...
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
% Fonction : fonction i2m_xyzticklayout
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_xyzticklayout(xlayoutStyle,ylayoutStyle,zlayoutStyle)

if ~isempty(xlayoutStyle)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword XTICKLAYOUT not translated');
end

if ~isempty(ylayoutStyle)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword YTICKLAYOUT not translated');
end

if ~isempty(zlayoutStyle)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword ZTICKLAYOUT not translated');
end