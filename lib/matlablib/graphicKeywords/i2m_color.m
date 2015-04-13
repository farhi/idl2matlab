
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
% Modifications : 04 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_color(colorIndice,h,funcName)
% colorIndice = l'indice de la couleur souhaitee
% h = handle a consider
% funcName = nom de la fonction qui a appele color

global i2mvs_p

if isempty(colorIndice)
	% Le Keyword COLOR n'est pas precise
	% !P.COLOR va etre utilise automatiquement, on ne fait rien
else,
	col = i2mColormap(colorIndice);

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

		case 'surface'
			set(h,'EdgeColor',col);
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(gca,'ZColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);
			set(get(gca,'ZLabel'),'Color',col);

		case 'shade_surf'
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(gca,'ZColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);
			set(get(gca,'ZLabel'),'Color',col);

		case 'xyouts'
			set(h,'color',col);

		case 'plot'
			set(h,'color',col);
			set(gca,'XColor',col);
			set(gca,'YColor',col);
			set(gca,'ZColor',col);
			set(get(gca,'Title'),'Color',col);
			set(get(gca,'XLabel'),'Color',col);
			set(get(gca,'YLabel'),'Color',col);

		case 'oplot'
			set(h,'color',col);

		case 'plots'
			set(h,'color',col);

		case 'polyfill'
			set(h,'FaceColor',col);

        % Utilise pour changer uniquement la couleur de l'axe X Y Z
        case 'X',  set(h,'XColor',col);
        case 'Y',  set(h,'YColor',col);
        case 'Z',  set(h,'ZColor',col);
        
        otherwise, set(h,'color' ,col);
	end
end
