
% i2m_majColormap
% --------------------------------------------
% to update the value of the current figure's colormap when it is necessary

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : fonction i2m_majColormap
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 23 / 05 / 2003
% Modifications : 05 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_majColormap()

global i2mvs_p i2mDecomposed

mapIDL = i2mColormap;
currFig = get(0,'CurrentFigure');

if ~isempty(currFig)
	% multi = i2mvs_p.multi
	if i2mvs_p.multi(1) == 0
		if i2mDecomposed == 0
	    mapMat = colormap;
  	  if sum(sum(mapMat ~= mapIDL)) ~= 0
    	  % la colormap de la figure courante est differente de la table de couleurs IDL courante
     	 colormap(mapIDL); % On change la colormap de la figure courante
     	 % On met a jour les valeurs RGB de la couleur de fond de la figure courante
				set(gcf,'Color',mapIDL(i2mvs_p.background+1,:));
			end
    end
	end
end

