% i2m_position
% --------------------------------------------
% Equivalent to :
% graphic keyword POSITION = ...
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
% Fonction : fonction i2m_position
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 28 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_position(pos,device)
% device = keyword device
global i2mvs_p

if ~isempty(device)
	% On n'est pas en coordonnees normalisees
	% CAS A TRAITER
else
	if isempty(pos)
		% Le Keyword POSITION n'est pas precise
		% On ne fait rien
		% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
	else
		% POSITION a ete passe en keyword, on utilise la valeur du keyword
		if (pos(1)<pos(3)) & (pos(2)<pos(4)) & (pos(1)<1) & (pos(2)<1) & (pos(3)>0) & (pos(3)>0)
			% POSITION est pris en compte
			if( pos(1) < 0 )
				x0 = 0;
			else
				x0 = pos(1);
			end
			if ( pos(2) < 0 )
				y0 = 0;
			else
				y0 = pos(2);
			end
			if( pos(3) > 1 )
				x1 = 1;
			else
				x1 = pos(3);
			end
			if( pos(4) > 1 )
				y1 = 1;
			else
				y1 = pos(4);
			end
			set(gca,'Position',[x0 y0 x1-x0 y1-y0]);
		else
			% POSITION n''est pas pris en compte
		end
	end
end
