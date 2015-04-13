%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : methode subsasgn (varsysX)
% Auteurs :
%                 Bourtembourg Reynald
%                 Szczuczak Nadege
% Date creation : 03 / 04 / 2003
% Modifications : 01 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function x = subsasgn(x, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

global i2mvs_p i2mvs_x i2mvs_y i2mvs_z
global i2m_limTicklen % variable definissant la longueur limite des ticks que l'on va specifier a la propriete Matlab DefaultAxesTicksLength
											% initialisee dans i2m_init.m

switch index(1).type
case '.'
	switch index(1).subs
		case 'tickname'
			x.tickname = val;
			set(0,'DefaultAxesXTickLabel',val(:));
		% end case 'tickname'
		case 's'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1; % ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.S[1] = 4, i2mvs_x.s(1) = 4 ou i2mvs_x.s(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						x.s(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				x.s(1) = val(1);
				x.s(2) = val(2);
			end
    	% end case 's'
		case 'title'
			x.title = mat2str(val);
		% end case 'title'
		case 'range'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1;% ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.CRANGE[1] = 4, i2mvs_x.crange(1) = 4 ou i2mvs_x.crange(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						x.range(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						x.range(1) = val;
						x.range(2) = val;
					else
						x.range(1) = val(1);
						x.range(2) = val(2);
					end
				else
					error('not authorized')
				end
			end
			if val == 0
				set(0,'DefaultAxesXLimMode','auto');
			else
				set(0,'defaultAxesXLim',val);
			end
		% end case 'range'
		case 'type'
			x.type = val;
		% end case 'type'
		case 'charsize'
			x.charsize = val;
		% end case 'charsize'
		case 'minor'
			x.minor = val;
			if val <= -1
				set(0,'DefaultAxesXMinorTick','off');
			else
				if val > 0
					warning('!X.MINOR doesn''t have an exact equivalent : MATLAB will automaticaly determine the number of minor ticks');
				end
				set(0,'DefaultAxesXMinorTick','on');
			end
		% end case 'minor'
		case 'crange'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1; % ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.CRANGE[1] = 4, i2mvs_x.crange(1) = 4 ou i2mvs_x.crange(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						x.crange(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						x.crange(1) = val;
						x.crange(2) = val;
					else
						x.crange(1) = val(1);
						x.crange(2) = val(2);
					end
					% set(0,'defaultAxesXLim',x.crange);
				else
					error('not authorized')
				end
			end
		% end case 'crange'
		case 'ticklen'
			x.ticklen = val;
			i2mvs_p.ticklen = i2mvs_p.ticklen; % On va prendre en compte la nouvelle valeur de i2mvs_p.ticklen lors de l'affectation de i2mvs_p.ticklen
		% end case 'ticklen'
		case 'style'
			x.style = val;
			if (16 & val & (val>16) ) ~= 0
				% Le bit 4 est active
				x.ynozero = 1;
			else
				x.ynozero = 0;
			end
			if (8 & val & (val>8) ) ~= 0
				% Le bit 3 est active
				x.noBox = 1;
				% On desactive la boite par defaut
				set(0,'DefaultAxesBox','off'); % apparemment, ca ne fait rien en Matlab
			else
				x.noBox = 0;
				% On reactive la boite par defaut
				set(0,'DefaultAxesBox','on');
			end
			if (4 & val & (val>4) ) ~= 0
				% le bit 2 est active
				x.noAxis = 1;
			else
				x.noAxis = 0;
			end
			if (2 & val & (val>2) ) ~= 0
				% Le bit 1 est active
				x.extend = 1;
			else
				x.extend = 0;
			end
			if (1 & val & (val>1) ) ~= 0
				% Le bit 0 est active
				x.exact = 1;
			else
				x.exact = 0;
			end
		% end case style
		case 'gridstyle'
			x.gridstyle = val;
			i2m_xyzgridstyle(val,[],[],1); % 4eme argument = 1 pour preciser que l'on va traiter la Variable Systeme et pas le KG
		% end case 'gridstyle'
		case 'ticks'
			x.ticks = val;
			if val > 0 & sum(x.tickv == 0)~=60
				% !X.TICKS > 0
				% !X.TICKV ~= 0
				% Il faut tenir compte des !x.ticks+1 premieres valeurs de !X.TICKV
				% eliminer les doublons et trier le vecteur forme par les !X.TICKS+1 premieres valeurs de !x.tickv
				set(0,'DefaultAxesXTick',unique(sort(x.tickv(1:min(length(x.tickv),val+1)))));
			else
				set(0,'DefaultAxesXTickMode','auto');
			end
		% end case 'ticks'
		case 'tickv'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1; % ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.CRANGE[1] = 4, i2mvs_x.crange(1) = 4 ou i2mvs_x.crange(2) = 4 ??
					if (indice <= 60) & (indice >= 1)
						x.tickv(indice) = val;
					else
						error('!X.TICKV : Bad index value')
					end
				else
					error('!X.TICKV : operation not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						x.tickv = val*ones(1,60);
					else
						for i=1:length(val)
							x.tickv(i)=val(i);
						end
					end
				else
					error('!X.TICKV : operation not authorized')
				end
			end
			if x.ticks > 0 & sum(x.tickv == 0)~=60
				% !X.TICKS > 0
				% !X.TICKV ~= 0
				% Il faut tenir compte des !x.ticks+1 premieres valeurs de !X.TICKV
				% eliminer les doublons et trier le vecteur forme par les !X.TICKS+1 premieres valeurs de !x.tickv
				set(0,'DefaultAxesXTick',unique(sort(x.tickv(1:min(length(x.tickv),val+1)))));
			else
				set(0,'DefaultAxesXTickMode','auto');
			end
		% end case 'tickv'
		otherwise
			s = ['System variable !X.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('System variable !X : operation not authorized')
end % index.type
