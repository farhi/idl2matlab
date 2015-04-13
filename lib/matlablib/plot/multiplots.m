%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction multiplots
%            pour gerer les plots multiples
%            dans une meme figure quand la variable systeme
%            !P.MULTI a ete modifiee.
% Auteurs :
%                 Bourtembourg Reynald
%                 Szczuczak Nadege
% Date creation : 14 / 04 / 2003
% Modifications : 21 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



function multiplots()

global i2mvs_p

wi0=widget_gca(gca); return; % Noop

if (i2mvs_p.position(1) == i2mvs_p.position(3)) | (i2mvs_p.position(2) == i2mvs_p.position(4))

	% ALORS !P.MULTI doit etre pris en compte
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

	if i2mvs_p.multi == 0
		subplot(1,1,1);
	else
		if nbcol+nblignes > 2
			if multi(1) == 0
				if i2mvs_p.multioplot == 0
					f = get(0,'CurrentFigure');
					delete(gcf);
					if ~isempty(f)
						figure(f);
					else
						figure;
					end
				end
				tmp = nblignes * nbcol;
			else
				if multi(1) <= nblignes*nbcol*max(multi(4),1)
					tmp = mod(multi(1)-1,nblignes*nbcol)+1;
				else
					tmp = nblignes*nbcol;
				end
			end
			if multi(5) == 0
				% L'ordre d'affichage des plots est le meme en Matlab et en IDL
				subplot(nblignes , nbcol , nblignes * nbcol - tmp + 1);
			else
				% L'ordre d'affichage des plots est different en Matlab et en IDL
				var = nblignes*nbcol - tmp;
				col = fix(var/nblignes) + 1;
				ligne = mod(var,nblignes) + 1;
				i = col + (ligne-1) * nbcol;
				subplot(nblignes,nbcol,i);
			end
		else
			% nothing to do 1 row and 1 column => normal plot
			subplot(1,1,1);
		end
	end
else % if (i2mvs_p.position ....)
	subplot(1,1,1);
end
