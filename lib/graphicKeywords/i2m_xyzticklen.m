% i2m_xyzticklen.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]TICKLEN = ...
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
% Fonction : fonction i2m_xyzticklen
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_xyzticklen(xlength,ylength,zlength)

if ~isempty(xlength)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword XTICKLEN not translated');
end

if ~isempty(ylength)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword YTICKLEN not translated');
end

if ~isempty(zlength)
	% Affichage d'un warning pour prevenir que le keyword n''est pas traduit pour le moment.
	warning('Graphic Keyword ZTICKLEN not translated');
end


