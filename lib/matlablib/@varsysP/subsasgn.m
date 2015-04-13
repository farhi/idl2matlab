%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsasgn (varsysP)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 01 / 04 / 2003
% Modifications : 12 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Traiter le cas (si posible ?) ou PSYM = 10 => histogramme
% !P.FONT ?? En faire plus ou non ??
% !P.NOERASE : comment le traiter ?

function p = subsasgn(p, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

global i2mDecomposed i2mvs_x i2mvs_y i2mvs_z i2mvs_p itsAnOplot coefTailleCar
global i2m_limTicklen % variable definissant la longueur limite des ticks que l'on va specifier a la propriete Matlab DefaultAxesTicksLength
									% initialisee dans i2m_init.m

switch index(1).type
case '.'
	switch index(1).subs
		case 'linestyle'
			switch val
				case 0
					option='-';
				case 1
					option=':';
				case 2
					option='--';
				case 3
					option='-.';
				case 4
					option='-.';
				case 5
					option='--';
				case '-'
					option = '-';
				case ':'
					option = ':';
				case '--'
					option = '--';
				case '-.'
					option = '-.';
				otherwise
					error('Bad value')
			end % switch val
			p.linestyle = val;
			p.linestylem = option;
			if p.psymm ~= 'none'
				% On ne fait rien, PSYM est prioritaire sur LINESTYLE
			else
				set(0,'DefaultAxesLineStyle',option);
			end
			set(0,'DefaultSurfaceLineStyle',option);
		% end case 'linestyle'
		case 'psym'
			switch val
				case 0
					option = 'none';
				case 1
					option = '+';
				case 2
					option = '*';
				case 3
					option = '.';
				case 4
					option = 'd';
				case 5
					option = '^';
				case 6
					option = 's';
				case 7
					option = 'x';
				case 'none'
					option = 'none';
				case '+'
					option = '+';
				case '*'
					option = '*';
				case '.'
					option = '.';
				case 'd'
					option = 'd';
				case '^'
					option = '^';
				case 's'
					option = 's';
				case 'x'
					option = 'x';
				otherwise
					error('bad value or not yet implemented')
			end % switch val
			p.psym = val;
			p.psymm = option;
			if option ~= 'none'
				set(0,'DefaultAxesLineStyle',option);
			else
				set(0,'DefaultAxesLineStyle',p.linestylem);
			end % if
		% end case 'psym'
		case 'thick'
			p.thick = val;
			if val > 0
				set(0,'DefaultAxesLineWidth',val);
				set(0,'DefaultLineLineWidth',val);
				set(0,'DefaultSurfaceLineWidth',val);
			else
				set(0,'DefaultAxesLineWidth',0.5);
				set(0,'DefaultLineLineWidth',0.5);
				set(0,'DefaultSurfaceLineWidth',0.5);
			end % if
		% end case 'thick'
		case 'ticklen'
			p.ticklen = val;
			ticklen = 0.0; % la valeur de la 1ere composante qui sera affectee a la propriete DefaultAxesTickLength
			ticklenz = 0.0; % la valeur de la 2eme composante qui sera affectee a la propriete DefaultAxesTickLength
			if i2mvs_x.ticklen == 0
				% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe X
				if val >= i2m_limTicklen
					% On active la grille pour les ticks de l'axe X
					set(0,'DefaultAxesXGrid','on');
				else
					set(0,'DefaultAxesXGrid','off');
					ticklen = val;
				end
			else
				% On considere la valeur de i2mvs_x.ticklen comme longueur des ticks de l'axe X
				if i2mvs_x.ticklen >= i2m_limTicklen
					% On active la grille pour les ticks de l'axe X
					set(0,'DefaultAxesXGrid','on');
				else
					set(0,'DefaultAxesXGrid','off');
					ticklen = i2mvs_x.ticklen;
				end
			end
			if i2mvs_y.ticklen == 0
				% On considere val (i2mvs_p.ticklen) comme longueur des ticks de l'axe Y
				if val >= i2m_limTicklen
					% On active la grille pour les ticks de l'axe Y
					set(0,'DefaultAxesYGrid','on');
				else
					set(0,'DefaultAxesYGrid','off');
					ticklen = max(ticklen,val);
				end
			else
				% On considere la valeur de i2mvs_x.ticklen comme longueur des ticks de l'axe Y
				if i2mvs_y.ticklen >= i2m_limTicklen
					% On active la grille pour les ticks de l'axe Y
					set(0,'DefaultAxesYGrid','on');
				else
					set(0,'DefaultAxesYGrid','off');
					ticklen = i2mvs_y.ticklen;
				end
			end
			ticklenz = ticklen;
			if i2mvs_z.ticklen == 0
			    % On considere val (i2mvs_p.ticklen) comme longueur des ticks de l''axe Z
				if val >= i2m_limTicklen
					% On active la grille pour les ticks de l''axe Z
					set(0,'DefaultAxesZGrid','on');
				else
					set(0,'DefaultAxesZGrid','off');
					ticklenz = max(ticklen,val);
				end
			else
				% On considere la valeur de i2mvs_z.ticklen comme longueur des ticks de l''axe Z
				if i2mvs_z.ticklen >= i2m_limTicklen
					% On active la grille pour les ticks de l''axe Z
					set(0,'DefaultAxesZGrid','on');
				else
					set(0,'DefaultAxesZGrid','off');
					ticklenz = i2mvs_z.ticklen;
				end
			end
			set(0,'DefaultAxesTicklength',[ticklen,ticklenz]);
		% end case 'ticklen'
		case 'font'
			p.font = val;
			i2mvs_p.charsize = p.charsize;  % pour reactualiser les proprietes Matlab associees a !P.CHARSIZE
																			%  car si !P.FONT = 0, !P.CHARSIZE n'a plus de sens
																			% et inversement,, si !P.FONT != 0, !P.CHARSIZE a de nouveau un sens
		% end case 'font'
		case 'charthick'
			p.charthick = val;
			if p.font ~= 0
				% Alors la variable systeme charthick a un sens
				if val >= 2
					set(0,'DefaultAxesFontWeight','bold');
					set(0,'DefaultTextFontWeight','bold');
				else
					set(0,'DefaultAxesFontWeight','normal');
					set(0,'DefaultTextFontWeight','normal');
				end
			else
				% La variable systeme n'est pas prise en compte => valeur par defaut en Matlab
				set(0,'DefaultAxesFontWeight','default');
				set(0,'DefaultTextFontWeight','default');
			end
		% end case 'charthick'
		case 'charsize'
			p.charsize = val;
			if p.font ~= 0
				% Alors la variable systeme charsize a un sens
				set(0,'DefaultAxesFontUnits','points'); % si jamais la valeur par defaut de FontUnits avait ete modifiee
				if (val > 0)
					set(0,'DefaultAxesFontSize',coefTailleCar*val);
					set(0,'DefaultTextFontSize',coefTailleCar*val);
				else
					% cas ou val <= 0
					set(0,'DefaultAxesFontSize','default');
					set(0,'DefaultTextFontSize','default');
				end
			else
				% La variable systeme n'est pas prise en compte => valeur par defaut en Matlab
				set(0,'DefaultAxesFontSize','default');
			end
		case 'noerase'
			p.noerase = val;
			if val ~= 0
				% set(0,'DefaultFigureNextplot','add');
				set(0,'DefaultAxesNextplot','add');
				if ~isempty(get(0,'currentFigure'))
					% set(gca,'NextPlot','add');
					hold on;
				end
				itsAnOplot=1;
				p.multioplot=1;
			else
				% val == 0
				% set(0,'DefaultFigureNextplot','add');
				set(0,'DefaultAxesNextplot','replace');
				if ~isempty(get(0,'currentFigure'))
					set(gca,'NextPlot','replace');
					hold off;
				end
				itsAnOplot=0;
				p.multioplot=0;
			 end
		% end case 'noerase'
		case 'multi'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:};
					if (indice <= 5) & (indice >= 1)
						p.multi(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(val) == 1
					p.multi = [val,val,val,val,val];
				else
					if length(val) <= 5
						for i = 1:length(val)
							p.multi(i) = val(i);
						end
					else
						error('length(i2mvs_p.multi) <= 5')
					end
				end
			end
		% end case 'multi'
		case 'multioplot'
			p.multioplot = val;
		% end case 'multioplot'
		case 'position'
			% AFFECTATION
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:};
					if (indice <= 4) & (indice >= 1)
						p.position(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(val) == 1
					p.position = [val val val val];
				else
					if length(val) <= 4
						for i = 1:length(val)
							p.position(i) = val(i);
						end
					else
						error('length(i2mvs_p.position) <= 4')
					end
				end
			end
			% FIN AFFECTATION

			% Mise a jour des proprietes Matlab par defaut
			if (p.position(1)<p.position(3)) & (p.position(2)<p.position(4)) & (p.position(1)<1) & (p.position(2)<1) & (p.position(3)>0) & (p.position(3)>0)
				% !P.POSITION est pris en compte
				if( p.position(1) < 0 )
					x0 = 0;
				else
					x0 = p.position(1);
				end
				if ( p.position(2) < 0 )
					y0 = 0;
				else
					y0 = p.position(2);
				end
				if( p.position(3) > 1 )
					x1 = 1;
				else
					x1 = p.position(3);
				end
				if( p.position(4) > 1 )
					y1 = 1;
				else
					y1 = p.position(4);
				end
				set(0,'DefaultAxesPosition',[x0 y0 x1-x0 y1-y0]);
			else
				% !P.POSITION n''est pas pris en compte
				if (p.region(1) < p.region(3)) & (p.region(2) < p.region(4)) & (p.region(1)<1) & (p.region(2)<1) & (p.region(3)>0) & (p.region(4)>0)
					% !P.REGION etait specifie et valide, il est pris en compte
					if p.region == [0 0 1 1]
						% On met la valeur par defaut de Matlab
						set(0,'DefaultAxesPosition','default'); % la valeur par defaut de Matlab
					else
						if( p.region(1) < 0 )
							x0 = 0;
						else
							x0 = p.region(1);
						end
						if ( p.region(2) < 0 )
							y0 = 0;
						else
							y0 = p.region(2);
						end
						if( p.region(3) > 1 )
							x1 = 1;
						else
							x1 = p.region(3);
						end
						if( p.region(4) > 1 )
							y1 = 1;
						else
							y1 = p.region(4);
						end
						%%%%%%%% Comme position sauf qu'il faut prevoir un peu de marge pour les annotations
						set(0,'DefaultAxesPosition',[x0+0.09 y0+0.09 x1-x0-0.15 y1-y0-0.15]);
					end
				else
					% !P.REGION n''etait pas specifie ou etait invalide, on met la valeur par defaut de Matlab
					set(0,'DefaultAxesPosition','default'); % la valeur par defaut de Matlab
				end
			end
		% end case 'position'
		case 'region'
			% AFFECTATION
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:};
					if (indice <= 4) & (indice >= 1)
						p.region(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(val) == 1
					p.region = [val val val val];
				else
					if length(val) <= 4
						for i = 1:length(val)
							p.region(i) = val(i);
						end
					else
						error('length(i2mvs_p.region) <= 4')
					end
				end
			end
			% FIN AFFECTATION

			% Mise a jour des valeurs par defaut des prorietes de Matlab
			if (p.position(1) < p.position(3)) & (p.position(2) < p.position(4)) & (p.position(1)<1) & (p.position(2)<1) & (p.position(3)>0) & (p.position(4)>0)
				% On ne s''occupe pas de la valeur de !P.REGION car !P.POSITION est specifie et est prioritaire
			else
				% On ne s''occupe pas de la valeur de !P.POSITION
				if (p.region(1) < p.region(3)) & (p.region(2) < p.region(4)) & (p.region(1)<1) & (p.region(2)<1) & (p.region(3)>0) & (p.region(4)>0)
					% !P.REGION est valide et doit etre pris en compte
					if p.region == [0 0 1 1]
						% On met la valeur par defaut de Matlab
						set(0,'DefaultAxesPosition','default'); % la valeur par defaut de Matlab
					else
						if( p.region(1) < 0 )
							x0 = 0;
						else
							x0 = p.region(1);
						end
						if ( p.region(2) < 0 )
							y0 = 0;
						else
							y0 = p.region(2);
						end
						if( p.region(3) > 1 )
							x1 = 1;
						else
							x1 = p.region(3);
						end
						if( p.region(4) > 1 )
							y1 = 1;
						else
							y1 = p.region(4);
						end
						%%%%%%%% Comme position sauf qu'il faut prevoir un peu de marge pour les annotations
						set(0,'DefaultAxesPosition',[x0+0.09 y0+0.09 x1-x0-0.15 y1-y0-0.15]);
					end
				else
					% !P.POSITION et !P.REGION ne sont pas valides ou sont egaux a 0 => On met la valeur par defaut de Matlab
					set(0,'DefaultAxesPosition','default'); % la valeur par defaut de Matlab
				end
			end
		% end case 'region'
		case 't'
			p.t = val;
		% end case 't'
		case 't3d'
			p.t3d = val;
		% end case 't3d'
		case 'title'
			p.title = mat2str(val);
		% end case 'title
		case 'background'
			p.background = val;
			col = i2mColormap(val);
			set(0,'DefaultFigureColor',col);
			set(0,'DefaultAxesColor',col);
		% end case 'backgound'
		case 'color'
			p.color = val;
			% A COMPLETER EVENTUELLEMENT
			col = i2mColormap(val);
			set(0,'DefaultTextColor',col);
			set(0,'DefaultAxesColorOrder',col);
			set(0,'DefaultAxesXColor',col);
			set(0,'DefaultAxesYColor',col);
			set(0,'DefaultAxesZColor',col);
			set(0,'DefaultSurfaceEdgeColor',col);
			% ...
		% end case 'color'
		otherwise
			s = ['System variable !P.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('not authorized')
end % index.type
