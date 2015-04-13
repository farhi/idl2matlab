
% i2mColormap
% i2mColormap(c) va modifier la valeur d'une variable globale
% dans laquelle sera stockee la table de couleurs 'IDL' actuelle
% i2mColormap appele sans argument retournera la table de couleur IDL actuelle
% Remarque: cette table de couleurs aura des valeurs comprises entre 0 et 1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : i2mColormap
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 15 / 05 / 2003
% Modifications : 02 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function result = i2mColormap(c)

global i2mColorTable i2mvs_p i2mDecomposed

if (nargin == 1)
	i2mColorTable = c;
	if i2mDecomposed == 0
		% On met a jour les valeurs RGB de la couleur de fond par defaut :
		set(0,'DefaultFigureColor',c(mod(i2mvs_p.background,256)+1,:));
		set(0,'DefaultAxesColor',c(mod(i2mvs_p.background,256)+1,:));
		% On met a jour les valeurs par defaut des proprietes associees a !P.COLOR
		set(0,'DefaultTextColor',c(mod(i2mvs_p.color,256)+1,:));
		set(0,'DefaultAxesColorOrder',c(mod(i2mvs_p.color,256)+1,:));
		set(0,'DefaultAxesXColor',c(mod(i2mvs_p.color,256)+1,:));
		set(0,'DefaultAxesYColor',c(mod(i2mvs_p.color,256)+1,:));
		set(0,'DefaultAxesZColor',c(mod(i2mvs_p.color,256)+1,:));
		set(0,'DefaultSurfaceEdgeColor',c(mod(i2mvs_p.color,256)+1,:));
	end
	set(0,'DefaultFigureColormap',c);
end

result = i2mColorTable;
