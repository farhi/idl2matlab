% i2m_xyztickname.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]TICKNAME = ...
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
% Fonction : fonction i2m_xyztickname
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 17 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarque : il y a des problemes avec les matrices de caracteres.
% En matlab il reutilise les labels deja utilises
% En IDL, une fois tous les labels epuises, il les attribue de maniere automatique
% sans reutiliser les premiers labels

function i2m_xyztickname(xname,yname,zname)

if ~isempty(xname)
	% Le KG XTICKNAME a ete precise
		set(gca,'xticklabel',xname(:))
end

if ~isempty(yname)
	% Le KG YTICKNAME a ete precise
	set(gca,'yticklabel',yname(:));
end

if ~isempty(zname)
	% Le KG ZTICKNAME a ete precise
	set(gca,'zticklabel',zname(:));
end


