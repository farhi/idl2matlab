% i2m_noerase
% --------------------------------------------
% Equivalent to :
% graphic keyword NOERASE = ...
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
% Fonction : fonction i2m_noerase
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% appel = 'debut', on est au debut de la fonction graphique
% appel = 'fin' on est a la fin de la fonction graphique
function i2m_noerase(noErase,appel)

	global i2mvs_p itsAnOplot kgNoerase
	multi = i2mvs_p.multi;

	if strcmpi(get(gca,'NextPlot'),'replace'), cur_noerase=0; else, cur_noerase=1;  end;
	if isempty(noErase), set_noerase=i2mvs_p.noerase; else, set_noerase=noErase;    end;
	
	if strcmpi(appel,'debut'), if ~set_noerase, cla reset; end; set(gca,'NextPlot','add');
	elseif cur_noerase ~= set_noerase, if set_noerase, set(gca,'NextPlot','add');
	                                   else,           set(gca,'NextPlot','replace'); end; end;
	return

	if i2mvs_p.noerase == 0
		if strcmpi(appel,'debut')
		%%%%%%%%%%%%%%%%%%%%%%% APPEL EN DEBUT DE FONCTION %%%%%%%%%%%%%%%
			if i2mvs_p.multi == 0
				% !P.MULTI = 0
				if ~isempty(noErase)
					if noErase ~= 0
						% Le keyword /noerase a ete precise
						multi = i2mvs_p.multi;
						% On va tracer le nouveau graphique sur le precedent
						if ~isempty(get(0,'currentFigure'))
							hold on;
						end
						itsAnOplot=1;
						i2mvs_p.multioplot=1;
					end
				end
			else
				% !P.MULTI ~= 0
				if kgNoerase == 1
					% kgNoerase = 1 => Il faut afficher sur le graphique precedent
					if multi(1) ~= 0
						% Si !P.MULTI[0] = 0 alors on affiche en effacant le precedent
						if ~isempty(get(0,'currentFigure'))
							hold on;
							ItsAnOplot = 1;
							i2mvs_p.multioplot=1;
						end
					end
				end
			end % end i2mvs_p.multi = 0
		else
			%%%%%%%%%%%%%%%%%%% APPEL EN FIN DE FONCTION %%%%%%%%%%%%%%%%%%%%
			if kgNoerase == 1
				kgNoerase = 0;
				if ~isempty(get(0,'currentFigure'))
					set(gca,'NextPlot','replace');
					hold off;
					itsAnOplot=0;
					i2mvs_p.multioplot=0;
				end
			end
			if (i2mvs_p.multi == 0) | (kgNoerase == 1)
				% !P.MULTI = 0 ou kgNoerase = 1
				if ~isempty(noErase)
					if noErase ~= 0
						% Le keyword /noerase a ete precise
						% On a trace le graphique sur le precedent
						% On 'libere' le systeme d'axe courant
						if ~isempty(get(0,'currentFigure'))
							set(gca,'NextPlot','replace');
							hold off;
							itsAnOplot=0;
							i2mvs_p.multioplot=0;
						end
					end
				end
			else
				% !P.MULTI ~= 0
				if ~isempty(noErase)
					if noErase ~= 0
						% Le keyword /noerase a ete precise
						% On va tracer le prochain graphique sur celui que l'on vient de tracer
						kgNoerase = 1;
					end
				end
			end
		end
	end






% 		if ~isempty(noErase)
% 			if noErase ~= 0
% 				if ~isempty(get(0,'currentFigure'))
% 					set(gca,'NextPlot','add');
% 					hold on;
% 				end
% 				itsAnOplot=1;
% 				p.multioplot=1;
% 			end
% 		end
% 	end


