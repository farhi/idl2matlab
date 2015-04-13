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
% Modifications : 08 / 08 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_ticklen(size,xsize,ysize,zsize)
% size  <=> KG TICKLEN
% xsize <=> KG XTICKLEN
% ysize <=> ...
global i2mvs_p i2mvs_x i2mvs_y i2mvs_z i2m_limTicklen
% i2m_limTicklen = limite de longueur des ticks au dela de laquelle on active les grilles

val = 0;
xticklen = 0; % Longueur des ticks a considerer sur l'axe X
yticklen = 0; % Longueur des ticks a considerer sur l'axe Y
zticklen = 0; % Longueur des ticks a considerer sur l'axe Z

if isempty(size)
	% Le Keyword TICKLEN n'est pas precise
	% On considere la valeur de i2mvs_p.ticklen
	val = i2mvs_p.ticklen;
else
	% TICKLEN a ete passe en keyword, on utilise la valeur du keyword
	val = size;
end
if ~isempty(xsize)
    % Le KG XTICKLEN a ete precise
    xticklen = xsize;
else
    % Le KG XTICKLEN n'a pas ete precise, on considere la variable systeme !X.TICKLEN
    xticklen = i2mvs_x.ticklen;
end
if ~isempty(ysize)
    % Le KG YTICKLEN a ete precise
    yticklen = ysize;
else
    % Le KG YTICKLEN n'a pas ete precise, on considere la variable systeme !Y.TICKLEN
    yticklen = i2mvs_y.ticklen;
end
if ~isempty(zsize)
    % Le KG ZTICKLEN a ete precise
    zticklen = zsize;
else
    % Le KG ZTICKLEN n'a pas ete precise, on considere la variable systeme !Z.TICKLEN
    zticklen = i2mvs_z.ticklen;
end

ticklen = 0.0; % la valeur de la 1ere composante qui sera affectee a la propriete DefaultAxesTickLength
ticklenz = 0.0; % la valeur de la 2eme composante qui sera affectee a la propriete DefaultAxesTickLength
if xticklen == 0
	% On considere val (i2mvs_p.ticklen ou KG ticklen) comme longueur des ticks de l'axe X
	if val >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe X
		set(gca,'XGrid','on');
	else
		set(gca,'XGrid','off');
		ticklen = val;
	end
else
	% On considere la valeur de xticklen comme longueur des ticks de l'axe X
	if xticklen >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe X
		set(gca,'XGrid','on');
	else
		set(gca,'XGrid','off');
		ticklen = xticklen;
	end
end
if yticklen == 0
	% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe Y
	if val >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Y
		set(gca,'YGrid','on');
	else
		set(gca,'YGrid','off');
        if abs(val) > abs(ticklen)
            ticklen = val;
        end
	end
else
	% On considere la valeur de xticklen comme longueur des ticks de l'axe Y
	if yticklen >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Y
		set(gca,'YGrid','on');
	else
		set(gca,'YGrid','off');
        if abs(yticklen) > abs(ticklen)
    		ticklen = yticklen;
        end
	end
end
ticklenz = ticklen;
if zticklen == 0
	% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe Z
	if val >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Z
		set(gca,'ZGrid','on');
	else
		set(gca,'ZGrid','off');
        if abs(val) >= abs(ticklen)
		    ticklenz = val;
        end
	end
else
	% On considere la valeur de zticklen comme longueur des ticks de l'axe Z
	if zticklen >= i2m_limTicklen
		% On active la grille pour les ticks de l'axe Z
		set(gca,'ZGrid','on');
	else
		set(gca,'ZGrid','off');
        if abs(zticklen) > abs(ticklenz)
    		ticklenz = zticklen;
        end
	end
end
set(gca,'Ticklength',[ticklen,ticklenz]);
