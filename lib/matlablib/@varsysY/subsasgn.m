%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : methode subsasgn (varsysY)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 01 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% !Y.Ticklen va se comporter comme !P.TICKLEN


function y = subsasgn(y, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

global i2mvs_p i2m_limTicklen i2mvs_x i2mvs_z

switch index(1).type
case '.'
	switch index(1).subs
		case 'tickname'
			y.tickname = val;
			set(0,'DefaultAxesYTickLabel',val(:));
		case 's'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1;  % ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.S[1] = 4, i2mvs_x.s(1) = 4 ou i2mvs_x.s(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						y.s(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				y.s(1) = val(1);
				y.s(2) = val(2);
			end
		case 'title'
			y.title = mat2str(val);
		case 'range'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1;% ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.CRANGE[1] = 4, i2mvs_x.crange(1) = 4 ou i2mvs_x.crange(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						y.range(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						y.range(1) = val;
						y.range(2) = val;
					else
						y.range(1) = val(1);
						y.range(2) = val(2);
					end
				else
					error('not authorized')
				end
			end
			if y.range == 0
				set(0,'DefaultAxesYLimMode','auto');
			else
				set(0,'defaultAxesYLim',y.range);
			end
		% end case 'range'
		case 'type'
			y.type = val;
		% end case 'type'
		case 'charsize'
			y.charsize = val;
		% end case 'charsize'
		case 'minor'
			y.minor = val;
			if val <= -1
				set(0,'DefaultAxesYMinorTick','off');
			else
				if val > 0
					warning('!Y.MINOR doesn''t have an exact equivalent : MATLAB will automaticaly determine the number of minor ticks');
				end
				set(0,'DefaultAxesYMinorTick','on');
			end
		case 'crange'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1;% ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.CRANGE[1] = 4, i2mvs_x.crange(1) = 4 ou i2mvs_x.crange(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						y.crange(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						y.crange(1) = val;
						y.crange(2) = val;
					else
						y.crange(1) = val(1);
						y.crange(2) = val(2);
					end
					% set(0,'defaultAxesYLim',y.crange);
				else
					error('not authorized')
				end
			end
		% end case 'crange'
		case 'ticklen'
			y.ticklen = val;
			i2mvs_p.ticklen = i2mvs_p.ticklen; % On va prendre en compte la nouvelle valeur de i2mvs_p.ticklen lors de l'affectation de i2mvs_p.ticklen
			% end case ticklen
		case 'style'
			y.style = val;
			if (16 & val & (val>16) ) ~= 0
				% Le bit 4 est active
				y.ynozero = 1;
			else
				% Le bit 4 est desactive
				y.ynozero =0;
			end
			if (8 & val & (val>8) ) ~= 0
				% Le bit 3 est active
				y.noBox = 1;
				% On desactive la boite par defaut
				set(0,'DefaultAxesBox','off'); % apparemment, ca ne fait rien en Matlab
			else
				% Le bit 3 est desactive
				y.noBox = 0;
				% On reactive la boite par defaut
				set(0,'DefaultAxesBox','on');
			end
			if (4 & val & (val>4) ) ~= 0
				% le bit 2 est active
				y.noAxis = 1;
			else
				% le bit 2 est desactive
				y.noAxis = 0;
			end
			if (2 & val & (val>2) ) ~= 0
				% Le bit 1 est active
				y.extend = 1;
			else
				% Le bit 1 est desactive
				y.extend = 0;
			end
			if (1 & val & (val>1) ) ~= 0
				% Le bit 0 est active
				y.exact = 1;
			else
				% Le bit 0 est desactive
				y.exact = 0;
			end
		% end case style
		case 'gridstyle'
			y.gridstyle = val;
			i2m_xyzgridstyle([],val,[],1); % 4eme argument = 1 pour preciser que l'on va traiter la Variable Systeme et pas le KG
		case 'ticks'
			y.ticks = val;
			if val > 0 & sum(y.tickv == 0)~=60
				% !Y.TICKS > 0
				% !Y.TICKV ~= 0
				% Il faut tenir compte des !y.ticks+1 premieres valeurs de !Y.TICKV
				% eliminer les doublons et trier le vecteur forme par les !Y.TICKS+1 premieres valeurs de !y.tickv
				set(0,'DefaultAxesYTick',unique(sort(y.tickv(1:min(length(y.tickv),val+1)))));
			else
				set(0,'DefaultAxesYTickMode','auto');
			end
		% end case 'ticks'
		case 'tickv'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1; % ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !Y.CRANGE[1] = 4, i2mvs_y.crange(1) = 4 ou i2mvs_y.crange(2) = 4 ??
					if (indice <= 60) & (indice >= 1)
						y.tickv(indice) = val;
					else
						error('!Y.TICKV : Bad index value')
					end
				else
					error('!Y.TICKV : operation not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						y.tickv = val*ones(1,60);
					else
						for i=1:length(val)
							y.tickv(i)=val(i);
						end
					end
				else
					error('!Y.TICKV : operation not authorized')
				end
			end
			if y.ticks > 0 & sum(y.tickv == 0)~=60
				% !Y.TICKS > 0
				% !Y.TICKV ~= 0
				% Il faut tenir compte des !y.ticks+1 premieres valeurs de !Y.TICKV
				% eliminer les doublons et trier le vecteur forme par les !Y.TICKS+1 premieres valeurs de !y.tickv
				set(0,'DefaultAxesYTick',unique(sort(y.tickv(1:min(length(y.tickv),val+1)))));
			else
				set(0,'DefaultAxesYTickMode','auto');
			end
		% end case 'tickv'
		otherwise
			s = ['System variable !Y.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('System variable !Y : operation not authorized')
end % index.type
