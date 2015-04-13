%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction multiplotsAfter
%            Va etre appelee a la fin d'un plot pour mettre
%            a jour la valeur de i2mvs_p.multi(1)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 14 / 04 / 2003
% Modifications : 21 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function multiplotsAfter ()

global i2mvs_p i2m_wcolor  i2m_wcolorOn kgNoerase

wi0=widget_gca(gca); return; % Noop


if (i2mvs_p.noerase == 0) & (kgNoerase == 0)

	multi = i2mvs_p.multi;

	% calcul du nombre de colonnes de plots dans une figure

	if multi(2) <= 0
		nbcol = 1;
	else
		nbcol = multi(2);
	end

	% calcul du nombre de lignes de plots dans une figure
	if multi(3) <= 0
		nblignes = 1;
	else
		nblignes = multi(3);
	end

	if multi(4) <= 0
		stackZ = 1;
	else
		stackZ = multi(4);
	end

	if i2mvs_p.multi == 0
		% nothing
	else
		if multi(1) <= nblignes*nbcol*stackZ
			i2mvs_p.multi = [mod(multi(1)-1,nblignes*nbcol*stackZ) multi(2) multi(3) multi(4)];
		else
			i2mvs_p.multi = [nblignes*nbcol*stackZ-1 multi(2) multi(3) multi(4)];
		end
	end
end
