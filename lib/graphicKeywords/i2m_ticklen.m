% i2m_ticklen
% --------------------------------------------
% Equivalent to :
% graphic keyword TICKLEN = ...
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
% Fonction : fonction i2m_ticklen
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 29 / 04 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_ticklen(size)

global i2mvs_p i2mvs_x i2mvs_y i2mvs_z i2m_limTicklen

val = 0;
if isempty(size)
	% Le Keyword TICKLEN n'est pas precise
	% On considere la valeur de i2mvs_p.ticklen
	val = i2mvs_p.ticklen;
else
	% TICKLEN a ete passe en keyword, on utilise la valeur du keyword
	val = size;
end
ticklen = 0.0; % la valeur de la 1ere composante qui sera affectee a la propriete DefaultAxesTickLength
ticklenz = 0.0; % la valeur de la 2eme composante qui sera affectee a la propriete DefaultAxesTickLength
if i2mvs_x.ticklen == 0
	% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe X
	if val >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe X
		set(gca,'XGrid','on');
	else
		set(gca,'XGrid','off');
		ticklen = val;
	end
else
	% On considere la valeur de i2mvs_x.ticklen comme longueur des ticks de l'axe X
	if i2mvs_x.ticklen >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe X
		set(gca,'XGrid','on');
	else
		set(gca,'XGrid','off');
		ticklen = i2mvs_x.ticklen;
	end
end
if i2mvs_y.ticklen == 0
	% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe Y
	if val >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Y
		set(gca,'YGrid','on');
	else
		set(gca,'YGrid','off');
		ticklen = max(ticklen,val);
	end
else
	% On considere la valeur de i2mvs_x.ticklen comme longueur des ticks de l'axe Y
	if i2mvs_y.ticklen >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Y
		set(gca,'YGrid','on');
	else
		set(gca,'YGrid','off');
		ticklen = i2mvs_y.ticklen;
	end
end
ticklenz = ticklen;
if i2mvs_z.ticklen == 0
	% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe Z
	if val >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Z
		set(gca,'ZGrid','on');
	else
		set(gca,'ZGrid','off');
		ticklenz = max(ticklen,val);
	end
else
	% On considere la valeur de i2mvs_z.ticklen comme longueur des ticks de l'axe Z
	if i2mvs_z.ticklen >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Z
		set(gca,'ZGrid','on');
	else
		set(gca,'ZGrid','off');
		ticklenz = i2mvs_z.ticklen;
	end
end
set(gca,'Ticklength',[ticklen,ticklenz]);
