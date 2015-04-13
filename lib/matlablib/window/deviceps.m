
% devicePS
% --------------------------------------------
% Called at the begining of graphic functions to tell to Matlab
% if he must save the graphic in a PS file or not
% and to control that the future plot will not be displayed in
% the figures reserved for the devices 'Z' and 'PS'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction devicePS
% Auteur :
%                 Bourtembourg Reynald
% Date creation : 05 / 06 / 2003
% Modifications : 10 / 07 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function devicePS(isOplot)
% isOplot = 1 si la fonction va agir sur le graphe precedent (ex : xyouts ...)
global i2mvs_d i2mFilename itsAnOplot i2mDevPsColor i2mDevPsPreview i2mPsCanWrite i2mZbuff i2mPs
col = '';
prev = '';
epsf = '';

if strcmpi('PS',i2mvs_d.name)
	if i2mDevPsColor == 1
		col = 'c';
	end
	if i2mDevPsPreview == 1
		prev = ' -epsi ';
	else
		if i2mDevPsPreview == 2
			epsf = 'e';
		end
	end
	if nargin == 0
		if ~ishold
			% La nouvelle fonction ne va pas agir sur le graphe precedent
			if i2mPsCanWrite == 0
				% La nouvelle fonction va modifier le contenu de la fenetre PS
				% On change la valeur de i2mPsCanWrite car la prochaine fois, il faudra enregistrer pour la 1ere fois dans le fichier ps associe
				i2mPsCanWrite = 1;
			else
				if i2mPsCanWrite == 1
					% on ecrit la figure precedente dans le fichier PS en mode normal (On va ecraser le fichier ps si il existait deja)
					eval(['print -d' epsf 'ps' col ' ' prev i2mFilename ';']);
					i2mPsCanWrite = 2; % la prochaine fois, on enregistrera a la fin du fichier ps (append)
				else
					% On enregistre a la fin du fichier ps
					eval(['print -d' epsf 'ps' col ' -append ' prev i2mFilename ';']);
				end
			end
		else
			% On n'ecrit pas encore dans le fichier ps car la fonction graphique courante va modifier la figure precedente
		end
	else
		% isOplot a ete passe en parametres
		if isOplot ~= 0
			% On est dans une fonction graphique qui va modifier le graphique precedent, on ne fait rien.
		else
			if ~ishold
				% La nouvelle fonction ne va pas agir sur le graphe precedent
			if i2mPsCanWrite == 0
				% La nouvelle fonction va modifier le contenu de la fenetre PS
				% On change la valeur de i2mPsCanWrite car la prochaine fois, il faudra enregistrer pour la 1ere fois dans le fichier ps associe
				i2mPsCanWrite = 1;
			else
				if i2mPsCanWrite == 1
					% on ecrit la figure precedente dans le fichier PS en mode normal (On va ecraser le fichier ps si il existait deja)
					eval(['print -d' epsf 'ps' col ' ' prev i2mFilename ';']);
					i2mPsCanWrite = 2; % la prochaine fois, on enregistrera a la fin du fichier ps (append)
				else
					% On enregistre a la fin du fichier ps
					eval(['print -d' epsf 'ps' col ' -append ' prev i2mFilename ';']);
				end
			end
			else
				% On n'ecrit pas encore dans le fichier ps car la fonction graphique courante va modifier la figure precedente
			end
		end
	end
else
	% !D.NAME ~= 'PS'
	if strcmpi(i2mvs_d.name,'Z')
		% !D.NAME = 'Z'
		% rien :)
	else
		% !D.NAME ~= 'PS' et !D.NAME ~= 'Z'
		% Il faut verifier que gcf n'est pas une des fenetres invisibles associees aux devices 'Z' et 'PS'
		if ~isempty(get(0,'currentFigure'))
			% Il y a une figure courante ouverte
			if gcf == i2mZbuff | gcf == i2mPs
				% ATTENTION, la figure courante est une des fenetres invisibles reservees pour la
				% gestion des devices 'Z' et 'PS'
				% il faut creer une nouvelle fenetre
				window;
			end
		end
	end
end
