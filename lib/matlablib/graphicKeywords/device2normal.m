% [xn,yn] = device2normal (xd, yd, h)
% return xn and yn in normalized coordinates
% xd, yd are in device coordinates
% h = handle of the figure or axes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : device2normal
%
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 29 / 07 / 2003
% Modifications : 29 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [xn,yn] = device2normal(xd,yd,h)

pos = get(h,'Position');
xsize = pos(3); % La largeur de l'objet h (axe ou figure)
ysize = pos(4); % La hauteur de l'objet h (axe ou figure)
% On normalise :
xn = xd / xsize;
yn = yd / ysize;
