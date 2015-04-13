%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : methode subsasgn (varsysZ)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 16 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% !Z.Ticklen va se comporter comme !P.TICKLEN


function z = subsasgn(z, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

global i2mvs_p

switch index(1).type
case '.'
	switch index(1).subs
		case 'tickname'
			z.tickname = val;
			set(0,'DefaultAxesZTickLabel',val);
		case 's'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:};
					if (indice <= 2) & (indice >= 1)
						z.s(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				z.s(1) = val(1);
				z.s(2) = val(2);
			end
		case 'title'
			z.title = mat2str(val);
		case 'range'
			if val == 0
				set(0,'DefaultAxesZLimMode','auto');
			else
				set(0,'defaultAxesZLim',val);
			end
		% end case 'range'
		case 'type'
			z.type = val;
		% end case 'type'
		case 'charsize'
			z.charsize = val;
		% end case 'charsize'
		case 'minor'
			z.minor = val;
			if val <= -1
				set(0,'DefaultAxesZMinorTick','off');
			else
				if val > 0
					warning('!Z.MINOR doesn''t have an exact equivalent : MATLAB will automaticaly determine the number of minor ticks');
				end
				set(0,'DefaultAxesZMinorTick','on');
			end
		case 'crange'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:};
					if (indice <= 2) & (indice >= 1)
						z.crange(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						z.crange(1) = val;
						z.crange(2) = val;
					else
						z.crange(1) = val(1);
						z.crange(2) = val(2);
					end
				else
					error('not authorized')
				end
			end
		% end case 'crange'
		case 'ticklen'
			z.ticklen = val;
			i2mvs_p.ticklen = i2mvs_p.ticklen; % On va prendre en compte la nouvelle valeur de i2mvs_p.ticklen lors de l'affectation de i2mvs_p.ticklen
		% end case ticklen
		otherwise
			s = ['System variable !Z.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('not authorized')
end % index.type
