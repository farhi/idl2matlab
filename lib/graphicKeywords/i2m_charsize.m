% i2m_charsize
% --------------------------------------------
% Equivalent to :
% graphic keyword CHARSIZE = ...
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
% Fonction : fonction i2m_charsize
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 25 / 04 / 2003
% Modifications : 21 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarque : xyouts ne fait pas appel a cette fonction
% Cette fonction va egalement permettre de gerer ![XYZ].CHARSIZE

function i2m_charsize(size)

global i2mvs_p i2mvs_x i2mvs_y i2mvs_z

if isempty(size)
	% Le Keyword CHARSIZE n'est pas precise
	% On ne fait rien
	% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
	% sauf pour les axes X, Y et Z ... (Il faut tenir compte de la valeur de ![XYZ].charsize
	xtitle = get(gca,'XLabel'); % On recupere l'objet texte associe au titre de l'axe des X
	ytitle = get(gca,'YLabel'); % On recupere l'objet texte associe au titre de l'axe des Y
	ztitle = get(gca,'ZLabel'); % On recupere l'objet texte associe au titre de l'axe des Z
	if i2mvs_x.charsize ~= 0
		if i2mvs_p.charsize <= 0
			set(xtitle,'FontSize',10*i2mvs_x.charsize);
		else
			set(xtitle,'FontSize',10*i2mvs_p.charsize*i2mvs_x.charsize);
		end
	end
	if i2mvs_y.charsize ~= 0
		if i2mvs_p.charsize <= 0
			set(ytitle,'FontSize',10*i2mvs_y.charsize);
		else
			set(ytitle,'FontSize',10*i2mvs_p.charsize*i2mvs_y.charsize);
		end
	end
	if i2mvs_z.charsize ~= 0
		if i2mvs_p.charsize <= 0
			set(ztitle,'FontSize',10*i2mvs_z.charsize);
		else
			set(ztitle,'FontSize',10*i2mvs_p.charsize*i2mvs_z.charsize);
		end
	end
else
	% CHARSIZE a ete passe en keyword, on utilise la valeur du keyword
	if i2mvs_p.font ~= 0
		% Alors le keyword charsize a un sens
		set(gca,'FontUnits','points'); % si jamais la valeur par defaut de FontUnits avait ete modifiee
		% h = findobj(gca,'Type','text');
		xtitle = get(gca,'XLabel'); % On recupere l'objet texte associe au titre de l'axe des X
		ytitle = get(gca,'YLabel'); % On recupere l'objet texte associe au titre de l'axe des Y
		ztitle = get(gca,'ZLabel'); % On recupere l'objet texte associe au titre de l'axe des Z
		if (size > 0)
			set(gca,'FontSize',10*size);
			% set(h,'FontSize',10*size);
			if i2mvs_x.charsize <= 0
				set(xtitle,'FontSize',10*size);
			else
				set(xtitle,'FontSize',10*size*i2mvs_x.charsize);
			end
			if i2mvs_y.charsize <= 0
				set(ytitle,'FontSize',10*size);
			else
				set(ytitle,'FontSize',10*size*i2mvs_y.charsize);
			end
			if i2mvs_z.charsize <= 0
				set(ztitle,'FontSize',10*size);
			else
				set(ztitle,'FontSize',10*size*i2mvs_z.charsize);
			end
		else
			% cas ou size <= 0
			set(gca,'FontSize','default');
			% set(h,'FontSize','default');
			if i2mvs_x.charsize <= 0
				set(xtitle,'FontSize','default');
			else
				set(xtitle,'FontSize',10*i2mvs_p.charsize*i2mvs_x.charsize);
			end
			if i2mvs_y.charsize <= 0
				set(ytitle,'FontSize','default');
			else
				set(ytitle,'FontSize',10*i2mvs_p.charsize*i2mvs_y.charsize);
			end
			if i2mvs_z.charsize <= 0
				set(ztitle,'FontSize','default');
			else
				set(ztitle,'FontSize',10*i2mvs_p.charsize*i2mvs_z.charsize);
			end
		end
	end
end
