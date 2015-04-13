
% i2m_linestyle
% --------------------------------------------
% Equivalent to :
% graphic keyword LINESTYLE = ...
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
% Fonction : fonction i2m_linestyle
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 29 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_linestyle(style,h,symbole)
% style = numero de style de ligne d'IDL
% h = handle de l'objet a modifier
% symbole = PSYM
global i2mvs_p

if isempty(style)
	% Le Keyword LINESTYLE n'est pas precise
	% On ne fait rien
	% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
else
	% LINESTYLE a ete passe en keyword, on utilise la valeur du keyword
	if (isempty(symbole) & i2mvs_p.psym == 0) | (~isempty(symbole) & (symbole == 0))
		'% LINESTYLE doit etre pris en compte car il n''y a pas de symbole defini'
		switch style
			case 0
				set(h,'LineStyle','-');
			case 1
				set(h,'LineStyle',':');
			case 2
				set(h,'LineStyle','--');
			case 3
				set(h,'LineStyle','-.');
			case 4
				set(h,'LineStyle','-.');
			case 5
				set(h,'LineStyle','--');
			otherwise
				error('bad value or not yet implemented')
		end % switch val
	else
		'% PSYM a une valeur et doit etre pris en compte'
		% On n'affiche pas la ligne
		set(h,'LineStyle','none');
	end
end
