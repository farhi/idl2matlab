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
% Modifications : 16 / 06 / 2003
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
			set(0,'DefaultAxesXTickLabel',val);
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
				else
					error('not authorized')
				end
			end
		% end case 'crange'
		case 'ticklen'
			x.ticklen = val;
			i2mvs_p.ticklen = i2mvs_p.ticklen; % On va prendre en compte la nouvelle valeur de i2mvs_p.ticklen lors de l'affectation de i2mvs_p.ticklen
		% end case ticklen
		otherwise
			s = ['System variable !X.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('not authorized')
end % index.type
