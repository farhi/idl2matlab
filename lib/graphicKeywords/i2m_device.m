
% device
% --------------------------------------------
% Equivalent to :
% graphic keyword DEVICE 
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
% Fonction : fonction i2m_device
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 20 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_device(device,h,funcName)
% device = contenu du keyword
% h = handle a consider
% funcName = nom de la fonction qui a appele color

global i2mvs_x i2mvs_y i2mvs_z
if isempty(device)
	% Le Keyword DEVICE n'est pas precise
else
	% DEVICE a ete passe en keyword
	switch funcName
		case 'polyfill'
            set(gca,'Visible','off');
            set(gca,'Units','pixels')
            taille=get(gcf,'Position');
            pos(1:2)=0;
            pos(3:4)=taille(3:4);
            set(gca,'XLim',[0,taille(3)]);
            set(gca,'YLim',[0,taille(4)]);
            set(gca,'ZLim',[0,10]);
            set(gca,'Position',pos);
		% end case 'polyfill'
		otherwise
			warning('Graphic keyword DEVICE not translated for this function');
	end
end

