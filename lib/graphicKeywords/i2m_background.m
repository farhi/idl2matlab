% i2m_background
% --------------------------------------------
% Equivalent to :
% graphic keyword BACKGROUND = ...
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
% Fonction : fonction i2m_background
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 02 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_background(colorIndice)

global i2mvs_p i2mDecomposed
if isempty(colorIndice)
	% Le Keyword BACKGROUND n'est pas precise
	if i2mvs_p.multi == 0
		% !P.MULTI est egal a 0 donc pas de multiplots
		col = []; % va contenir les valeurs RGB de la couleur en question
		if i2mDecomposed == 0
			% On travaille avec les tables de couleurs
			c = i2mColormap;
			col = c(mod(i2mvs_p.background,256)+1,:);
		else
			% On ne travaille pas avec les tables de couleurs
			% On considere la valeur RGB correspondant a la valeur de l'indice
			col = i2m_index2rgb(i2mvs_p.background);
		end
		set(gca, 'Color',col);
		set(gcf, 'Color',col);
	else
		% !P.MULTI ~= 0 donc on est ds le cas de plots multiples
		% La couleur de fond est la couleur de fond de la fenetre
		set(gca,'Color',get(gcf,'Color'));
	end
else
	% BACKGROUND a ete passe en keyword
	% !P.MULTI est egal a 0 donc pas de multiplots
	col = []; % va contenir les valeurs RGB de la couleur en question
	if i2mDecomposed == 0
		% On travaille avec les tables de couleurs
		c = i2mColormap;
		col = c(colorIndice+1,:);
	else
		% On ne travaille pas avec les tables de couleurs
		% On considere la valeur RGB correspondant a la valeur de l'indice
		col = i2m_index2rgb(colorIndice);
	end
	if i2mvs_p.multi == 0
		% !P.MULTI = 0 donc on doit prendre en compte le keyword BACKGROUND
		set(gcf,'Color',col);
		set(gca,'Color',col);
	else
		% Il y a plusieurs plots dans la meme figure
		if i2mvs_p.multi(1) == 0
			% Alors on tient compte du keyword BACKGROUND
			set(gcf,'Color',col);
			set(gca,'Color',col);
		else
			% le keyword BACKGROUND n'est pas pris en compte
			% La couleur de fond est la couleur de fond de la fenetre
			set(gca,'Color',get(gcf,'Color'));
		end
	end
end
