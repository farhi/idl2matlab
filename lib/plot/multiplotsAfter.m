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
% Modifications : 02 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function multiplotsAfter ()

global i2mvs_p axes_wd i2m_wcolor

%On traite le cas ou on est dans un widget
if ~isempty(gca) & ~isempty(axes_wd) & sum(find(axes_wd == gca ))~=0
    set(gcf,'color',i2m_wcolor);
    [i,j]=find(axes_wd == gca);
    set(axes_wd(2,j),'color',get(gca,'color'));
    return;
end;


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
