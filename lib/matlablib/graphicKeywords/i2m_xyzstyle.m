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
% Date creation : 10 / 07 / 2003
% Modifications : 17 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarque : pour le bit 3 (no box), si un des keywords xstyle, ystyle a le bit 3 active, alors on desactive la boite
% en effet, en Matlab, il n'est pas possible de desactiver la boite sur un seul axe
% Pour les courbes en 3D, la boite ne doit pas etre activee
% Bit 0 : Exact => par defaut en Matlab, on ne change pas
% Bit 1 : Extend axis range => impossible a faire et peu utilise
% Bit 2 : OK
% Bit 3 : Si le bit 3 est active pour un des axes, alors, on va desactiver la boite pour tous les axes
% Bit 4 : (ynozero), a faire ...

function i2m_xyzstyle(xstyle,ystyle,zstyle,D3)
% D3 = 1 si on est dans une courbe en 3 dimensions
% D3 = 0 sinon


% variables globales
global i2mvs_x i2mvs_y i2mvs_z

% variables locales
xynozero = 0; xnoBox= 0 ; xnoAxis = 0; xextend = 0; xexact = 0;
yynozero = 0; ynoBox= 0 ; ynoAxis = 0; yextend = 0; yexact = 0;
zynozero = 0; znoBox= 0 ; znoAxis = 0; zextend = 0; zexact = 0;

if ~isempty(xstyle)
	% Le Keyword XSTYLE a ete passe en parametres
	if (16 & xstyle) ~= 0
		% Le bit 4 est active
		xynozero = 1;
	end
	if (8 & xstyle) ~= 0
		% Le bit 3 est active
		xnoBox = 1;
	end
	if (4 & xstyle) ~= 0
		% le bit 2 est active
		xnoAxis = 1;
	end
	if (2 & xstyle) ~= 0
		% Le bit 1 est active
		xextend = 1;
	end
	if (1 & xstyle) ~= 0
		% Le bit 0 est active
		xexact = 1;
	end
end % if ~isempty(xstyle)

if ~isempty(ystyle)
	% Le Keyword YSTYLE a ete passe en parametres
	if (16 & ystyle) ~= 0
		% Le bit 4 est active
		yynozero = 1;
	end
	if (8 & ystyle) ~= 0
		% Le bit 3 est active
		ynoBox = 1;
	end
	if (4 & ystyle) ~= 0
		% le bit 2 est active
		ynoAxis = 1;
	end
	if (2 & ystyle) ~= 0
		% Le bit 1 est active
		yextend = 1;
	end
	if (1 & ystyle) ~= 0
		% Le bit 0 est active
		yexact = 1;
	end
end % if ~isempty(ystyle)

if ~isempty(zstyle)
	% Le Keyword ZSTYLE a ete passe en parametres
	if (16 & zstyle) ~= 0
		% Le bit 4 est active
		zynozero = 1;
	end
	if (8 & zstyle) ~= 0
		% Le bit 3 est active
		znoBox = 1;
	end
	if (4 & zstyle) ~= 0
		% le bit 2 est active
		znoAxis = 1;
	end
	if (2 & zstyle) ~= 0
		% Le bit 1 est active
		zextend = 1;
	end
	if (1 & zstyle) ~= 0
		% Le bit 0 est active
		zexact = 1;
	end
end % if ~isempty(zstyle)

% A ce stade, on connait les valeurs des bits associes aux Keywords [XYZ]STYLE

% Traitement du bit 0

% Traitement du bit 1

% Traitement du bit 2 ( no axis )
if isempty(xstyle)
	% Le KG XSTYLE n'a pas ete precise
	% On regarde la valeur de la variable systeme correspondante
	if i2mvs_x.noAxis == 1
		% Le bit 2 de la variable systeme !X.STYLE est active
		% On supprime l'axe
		set(gca,'XColor',get(gca,'color'));
	end
else
	% Le KG XSTYLE a ete precise
	if xnoAxis == 1
		% Le bit 2 du KG XSTYLE est active
		% On supprime l'axe
		set(gca,'XColor',get(gca,'color'));
	end
end
if isempty(ystyle)
	% Le KG YSTYLE n'a pas ete precise
	% On regarde la valeur de la variable systeme correspondante
	if i2mvs_y.noAxis == 1
		% Le bit 2 de la variable systeme !Y.STYLE est active
		% On supprime l'axe
		set(gca,'YColor',get(gca,'color'));
	end
else
	% Le KG YSTYLE a ete precise
	if ynoAxis == 1
		% Le bit 2 du KG YSTYLE est active
		% On supprime l'axe
		set(gca,'YColor',get(gca,'color'));
	end
end
if isempty(zstyle)
	% Le KG ZSTYLE n'a pas ete precise
	% On regarde la valeur de la variable systeme correspondante
	if i2mvs_z.noAxis == 1
		% Le bit 2 de la variable systeme !Z.STYLE est active
		% On supprime l'axe
		set(gca,'ZColor',get(gca,'color'));
	end
else
	% Le KG ZSTYLE a ete precise
	if znoAxis == 1
		% Le bit 2 du KG ZSTYLE est active
		% On supprime l'axe
		set(gca,'ZColor',get(gca,'color'));
	end
end


% Traitement du bit 3 (no box)
if D3 == 1
	% On est dans une courbe en 3 dimensions, on desactive la "boite"
	set(gca,'Box','off');
else
	% On est en 2D
	if xnoBox == 1 | ynoBox == 1
		% Il faut desactiver la "boite"
		set(gca,'Box','off');
	else
		% xnoBox = ynoBox = znoBox = 0
		% On regarde les variables systemes ![XYZ]STYLE
		if i2mvs_x.noBox == 1 | i2mvs_y.noBox == 1
			% Il faut desactiver la "boite"
			set(gca,'Box','off');
		else
			% Le bit 3 est a 0 pour les keywords et les variables systeme
			% La boite doit etre activee
			set(gca,'Box','on');
		end
	end
end


% Traitement du bit 4




