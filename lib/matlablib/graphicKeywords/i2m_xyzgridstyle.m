% i2m_xyzgridstyle
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]GRIDSTYLE = ...
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
% Fonction : fonction i2m_xyzgridstyle
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 15 / 07 / 2003
% Modifications : 17 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarque: On va considerer que a partir du moment ou un KG a ete precise,
% alors, c'est sa valeur qui sera prise en compte pour toutes les grilles
% Probleme : Qd on va modifier la variable systeme !Z.GRIDSTYLE, ca va modifier le style de 
% toutes les grilles en Matlab, meme sur les courbes 2D, alors que ca ne devrait pas etre le cas.

function i2m_xyzgridstyle(xgridstyle,ygridstyle,zgridstyle,type,D3)

% type == 0 => On traite les KG
% type ~= 0 => On traite une variable systeme
% D3 = 1 => courbe en 3 dimensions
% D3 ~= 1 => courbe en 2D


if type == 0
	% On traite les KG
	if ~isempty(xgridstyle)
		% KG XGRIDSTYLE a ete passe en parametres
		switch xgridstyle
			case 0
				set(gca,'GridLineStyle','-');
				set(gca,'MinorGridLineStyle','-');
			case 1
				set(gca,'GridLineStyle',':');
				set(gca,'MinorGridLineStyle',':');
			case 2
				set(gca,'GridLineStyle','--');
				set(gca,'MinorGridLineStyle','--');
			case 3
				set(gca,'GridLineStyle','-.');
				set(gca,'MinorGridLineStyle','-.');
			case 4
				set(gca,'GridLineStyle','-.');
				set(gca,'MinorGridLineStyle','-.');
			case 5
				set(gca,'GridLineStyle','--');
				set(gca,'MinorGridLineStyle','--');
			otherwise
				error('XGRIDSTYLE : bad value or not yet implemented')
		end % switch val
	end% ~isempty(xgridstyle)

	if ~isempty(ygridstyle)
		% KG YGRIDSTYLE a ete passe en parametres
		switch ygridstyle
			case 0
				set(gca,'GridLineStyle','-');
				set(gca,'MinorGridLineStyle','-');
			case 1
				set(gca,'GridLineStyle',':');
				set(gca,'MinorGridLineStyle',':');
			case 2
				set(gca,'GridLineStyle','--');
				set(gca,'MinorGridLineStyle','--');
			case 3
				set(gca,'GridLineStyle','-.');
				set(gca,'MinorGridLineStyle','-.');
			case 4
				set(gca,'GridLineStyle','-.');
				set(gca,'MinorGridLineStyle','-.');
			case 5
				set(gca,'GridLineStyle','--');
				set(gca,'MinorGridLineStyle','--');
			otherwise
				error('YGRIDSTYLE : bad value or not yet implemented')
		end % switch val
	end % ~isempty(ygridstyle)

	if ~isempty(zgridstyle)
		% KG ZGRIDSTYLE a ete passe en parametres
		if D3 == 1
			% On est dans une courbe en 3 dimensions
			switch zgridstyle
				case 0
					set(gca,'GridLineStyle','-');
					set(gca,'MinorGridLineStyle','-');
				case 1
					set(gca,'GridLineStyle',':');
					set(gca,'MinorGridLineStyle',':');
				case 2
					set(gca,'GridLineStyle','--');
					set(gca,'MinorGridLineStyle','--');
				case 3
					set(gca,'GridLineStyle','-.');
					set(gca,'MinorGridLineStyle','-.');
				case 4
					set(gca,'GridLineStyle','-.');
					set(gca,'MinorGridLineStyle','-.');
				case 5
					set(gca,'GridLineStyle','--');
					set(gca,'MinorGridLineStyle','--');
				otherwise
					error('ZGRIDSTYLE : bad value or not yet implemented')
			end % switch val
		end % if D3 == 1
	end % ~isempty(zgridstyle)

else
	% type ~= 0
	% On traite une variable systeme
	if ~isempty(xgridstyle)
		% On traite !X.GRIDSTYLE
		switch xgridstyle
			case 0
				set(0,'DefaultAxesGridLineStyle','-');
				set(0,'DefaultAxesMinorGridLineStyle','-');
			case 1
				set(0,'DefaultAxesGridLineStyle',':');
				set(0,'DefaultAxesMinorGridLineStyle',':');
			case 2
				set(0,'DefaultAxesGridLineStyle','--');
				set(0,'DefaultAxesMinorGridLineStyle','--');
			case 3
				set(0,'DefaultAxesGridLineStyle','-.');
				set(0,'DefaultAxesMinorGridLineStyle','-.');
			case 4
				set(0,'DefaultAxesGridLineStyle','-.');
				set(0,'DefaultAxesMinorGridLineStyle','-.');
			case 5
				set(0,'DefaultAxesGridLineStyle','--');
				set(0,'DefaultAxesMinorGridLineStyle','--');
			otherwise
				error('!X.GRIDSTYLE : bad value or not yet implemented')
		end % switch xgridstyle
	end % ~isempty(xgridstyle)
	if ~isempty(ygridstyle)
		% On traite !Y.GRIDSTYLE
		switch ygridstyle
			case 0
				set(0,'DefaultAxesGridLineStyle','-');
				set(0,'DefaultAxesMinorGridLineStyle','-');
			case 1
				set(0,'DefaultAxesGridLineStyle',':');
				set(0,'DefaultAxesMinorGridLineStyle',':');
			case 2
				set(0,'DefaultAxesGridLineStyle','--');
				set(0,'DefaultAxesMinorGridLineStyle','--');
			case 3
				set(0,'DefaultAxesGridLineStyle','-.');
				set(0,'DefaultAxesMinorGridLineStyle','-.');
			case 4
				set(0,'DefaultAxesGridLineStyle','-.');
				set(0,'DefaultAxesMinorGridLineStyle','-.');
			case 5
				set(0,'DefaultAxesGridLineStyle','--');
				set(0,'DefaultAxesMinorGridLineStyle','--');
			otherwise
				error('!Y.GRIDSTYLE : bad value or not yet implemented')
		end % switch ygridstyle
	end % ~isempty(ygridstyle)
	if ~isempty(zgridstyle)
		% On traite !Z.GRIDSTYLE
		switch zgridstyle
			case 0
				set(0,'DefaultAxesGridLineStyle','-');
				set(0,'DefaultAxesMinorGridLineStyle','-');
			case 1
				set(0,'DefaultAxesGridLineStyle',':');
				set(0,'DefaultAxesMinorGridLineStyle',':');
			case 2
				set(0,'DefaultAxesGridLineStyle','--');
				set(0,'DefaultAxesMinorGridLineStyle','--');
			case 3
				set(0,'DefaultAxesGridLineStyle','-.');
				set(0,'DefaultAxesMinorGridLineStyle','-.');
			case 4
				set(0,'DefaultAxesGridLineStyle','-.');
				set(0,'DefaultAxesMinorGridLineStyle','-.');
			case 5
				set(0,'DefaultAxesGridLineStyle','--');
				set(0,'DefaultAxesMinorGridLineStyle','--');
			otherwise
				error('!Z.GRIDSTYLE : bad value or not yet implemented')
		end % switch zgridstyle
	end % ~isempty(zgridstyle)

end % if type == 0
