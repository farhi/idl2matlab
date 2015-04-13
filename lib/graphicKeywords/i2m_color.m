
% i2m_color
% --------------------------------------------
% Equivalent to :
% graphic keyword COLOR = ...
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
% Fonction : fonction i2m_color
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 06 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_color(colorIndice,h,funcName)
% colorIndice = l'indice de la couleur souhaitee
% h = handle a consider
% funcName = nom de la fonction qui a appele color

global i2mvs_p i2mDecomposed
if isempty(colorIndice)
	% Le Keyword COLOR n'est pas precise
	% !P.COLOR va etre utilise automatiquement, on ne fait rien
else
	% COLOR a ete passe en keyword
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
	switch funcName
		case 'contour'
			for i=1:length(h)
				set(h(i),'EdgeColor',col);
			end
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);
		% end case 'contour'
		case 'surface'
			set(h,'EdgeColor',col);
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(gca,'ZColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);
			set(get(gca,'ZLabel'),'Color',col);
		% end case 'surface'
		case 'shade_surf'
			set(h,'EdgeColor','none');
			% set(h,'EdgeLighting','flat'); % a mettre directement dans la fonction shade_surf ??!!!!!!!!!!!!!!!!!!!
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(gca,'ZColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);
			set(get(gca,'ZLabel'),'Color',col);
		% end case 'shade_surf'
		case 'xyouts'
			set(h,'color',col);
			% end case 'xyouts'
		case 'plot'
			set(h,'color',col);
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(gca,'ZColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);
		% end case 'plot'
		case 'oplot'
			set(h,'color',col);
		% end case 'oplot'
		case 'plots'
			set(h,'color',col);
		case 'polyfill'
			set(h,'FaceColor',col);
		% end case 'polyfill'
		otherwise
			set(h,'color',col);
	end
end

