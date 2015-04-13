% wset
% --------------------------------------------
% Equivalent to :
% function WSET [, Window_Index]
% en IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction wset
% Auteur :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 23 / 05 / 2003
% Modifications : 10 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function  wset(win)

global axes_wd i2mvs_d

% On verifie que le device courant est bien le bon, si ce n'est pas le cas, on renvoie tout de suite une erreur
if ~(strcmpi(i2mvs_d.name,'X') | strcmpi(i2mvs_d.name,'WIN') | strcmpi(i2mvs_d.name,'MAC'))
	% Cette procedure n'est pas definie pour le device courant
	error(' WINDOW: Routine is not defined for current graphics device.');
end

if ~isempty(axes_wd) & sum(axes_wd(1,:) ==gca)~=0
	% On est dans un widget
	axes(win);
else
	% Cas normal
	tab = findobj(0,'type','figure');
	if nargin == 0 | win == 0
		% Le numero de la fenetre n'a pas ete precise
		% On considere win = 0
		if ~isempty(find(tab == 1))  % On regarde si la figure numero 1 (<=> window 0 en IDL) existe deja
			% la figure existe deja, elle doit devenir la figure courante
			set(0,'CurrentFigure',1);
		else
			% La figure n'existe pas, on la cree
			window(0);
		end
	else
		% Le numero de la fenetre a ete precise et est different de 0
		if ~isempty(find(tab == win+1))  % On regarde si la figure numero win+1 (<=> window win en IDL) existe deja
			% la figure existe deja, elle doit devenir la figure courante
			set(0,'CurrentFigure',win+1);
		else
			% La figure n'existe pas, on renvoie un message d'erreur
			error('WSET: Window is closed and unavailable.');
		end
	end
end

return;
