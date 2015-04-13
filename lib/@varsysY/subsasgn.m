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
% Modifications : 16 / 06 / 2003
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
			set(0,'DefaultAxesYTickLabel',val);
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
			if val == 0
				set(0,'DefaultAxesYLimMode','auto');
			else
				set(0,'defaultAxesYLim',val);
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
				else
					error('not authorized')
				end
			end
		% end case 'crange'
		case 'ticklen'
			y.ticklen = val;
			i2mvs_p.ticklen = i2mvs_p.ticklen; % On va prendre en compte la nouvelle valeur de i2mvs_p.ticklen lors de l'affectation de i2mvs_p.ticklen
			% end case ticklen
		otherwise
			s = ['System variable !Y.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('not authorized')
end % index.type
