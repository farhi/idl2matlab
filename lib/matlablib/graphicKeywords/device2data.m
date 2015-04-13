% [xdat,ydat] = device2data (xd, yd,ha)
% return xdat and ydat in data coordinates
% xd, yd are in device coordinates
% ha = handle of the axes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : device2data
%
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 29 / 07 / 2003
% Modifications : 30 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [xdat,ydat] = device2data(xd,yd,ha)

% On recupere la position des axes en pixels
unites = get(ha,'units');
set(ha,'units','pixels');
axesPos = get(ha,'Position');

% On replace le systeme de coordonnees tel qu'il etait avant
set(ha,'units',unites);

% Largeur de l'axe des abscisses :
axeXSize = axesPos(3);
axeYSize = axesPos(4);
x0 = axesPos(1);
y0 = axesPos(2);

% mini et maxi du systeme d'axes
axesXLim = get(ha,'xlim');
axesYLim = get(ha,'ylim');
xMin = axesXLim(1);
xMax = axesXLim(2);
yMin = axesYLim(1);
yMax = axesYLim(2);

if xd <0 | yd < 0
	% Le point considere est en dehors de la fenetre
	% On retourne les coordonnees (en data) du coin inferieur droit de la fenetre
	xdat = xMin - x0 * (xMax - xMin) / axeXSize;
	ydat = yMin - y0 * (yMax - yMin) / axeYSize;
else
	% Le point est a l'interieur de la fenetre
	% coordonnees en pixel dans le systeme d'axes
	xnp = xd - x0;
	ynp = yd - y0;
	xdat = xnp * (xMax - xMin) / axeXSize + xMin;
	ydat = ynp * (yMax - yMin) / axeYSize + yMin;
end