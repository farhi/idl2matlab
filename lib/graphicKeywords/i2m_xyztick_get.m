% i2m_xyztick_get.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]TICK_GET = ...
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
% Fonction : fonction i2m_xyztick_get
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_xyztick_get(xvalues,yvalues,zvalues)

if ~isempty(xvalues)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword XTICK_GET not translated');
end

if ~isempty(yvalues)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword YTICK_GET not translated');
end

if ~isempty(zvalues)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword ZTICK_GET not translated');
end

