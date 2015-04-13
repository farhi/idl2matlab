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
% Modifications : 01 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% !Z.Ticklen va se comporter comme !P.TICKLEN


function z = subsasgn(z, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

global i2mvs_p i2mvs_x i2mvs_y

switch index(1).type
case '.'
	switch index(1).subs
		case 'tickname'
			z.tickname = val;
			set(0,'DefaultAxesZTickLabel',val(:));
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
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1;% ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !X.CRANGE[1] = 4, i2mvs_x.crange(1) = 4 ou i2mvs_x.crange(2) = 4 ??
					if (indice <= 2) & (indice >= 1)
						z.range(indice) = val;
					else
						error('Bad index value')
					end
				else
					error('not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						z.range(1) = val;
						z.range(2) = val;
					else
						z.range(1) = val(1);
						z.range(2) = val(2);
					end
				else
					error('not authorized')
				end
			end
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
					% set(0,'defaultAxesZLim',z.crange);
				else
					error('not authorized')
				end
			end
		% end case 'crange'
		case 'ticklen'
			z.ticklen = val;
			i2mvs_p.ticklen = i2mvs_p.ticklen; % On va prendre en compte la nouvelle valeur de i2mvs_p.ticklen lors de l'affectation de i2mvs_p.ticklen
		% end case ticklen
		case 'style'
			z.style = val;
			if (16 & val & (val>16) ) ~= 0
				% Le bit 4 est active
				z.ynozero = 1;
			else
				% Le bit 4 est desactive
				z.ynozero =0;
			end
			if (8 & val & (val>8) ) ~= 0
				% Le bit 3 est active
				z.noBox = 1;
				% On desactive la boite par defaut
				set(0,'DefaultAxesBox','off'); % apparemment, ca ne fait rien en Matlab
			else
				% Le bit 3 est desactive
				z.noBox = 0;
				% On reactive la boite par defaut
				set(0,'DefaultAxesBox','on');
			end
			if (4 & val & (val>4) ) ~= 0
				% le bit 2 est active
				z.noAxis = 1;
			else
				% le bit 2 est desactive
				z.noAxis = 0;
			end
			if (2 & val & (val>2) ) ~= 0
				% Le bit 1 est active
				z.extend = 1;
			else
				% Le bit 1 est desactive
				z.extend = 0;
			end
			if (1 & val & (val>1) ) ~= 0
				% Le bit 0 est active
				z.exact = 1;
			else
				% Le bit 0 est desactive
				z.exact = 0;
			end
		% end case style
		case 'gridstyle'
			z.gridstyle = val;
			i2m_xyzgridstyle([],[],val,1); % 4eme argument = 1 pour preciser que l'on va traiter la Variable Systeme et pas le KG
		case 'ticks'
			z.ticks = val;
			if val > 0 & sum(z.tickv == 0)~=60
				% !Z.TICKS > 0
				% !Z.TICKV ~= 0
				% Il faut tenir compte des !z.ticks+1 premieres valeurs de !Z.TICKV
				% eliminer les doublons et trier le vecteur forme par les !Z.TICKS+1 premieres valeurs de !z.tickv
				set(0,'DefaultAxesZTick',unique(sort(z.tickv(1:min(length(z.tickv),val+1)))));
			else
				set(0,'DefaultAxesZTickMode','auto');
			end
		% end case 'ticks'
		case 'tickv'
			if length(index) == 2
				if (index(2).type == '()') | (index(2).type == '{}')
					indice = index(2).subs{:}+1; % ATTENTION !! +1 ou pas ?? Tout dependra de la traduction de !Z.CRANGE[1] = 4, i2mvs_z.crange(1) = 4 ou i2mvs_z.crange(2) = 4 ??
					if (indice <= 60) & (indice >= 1)
						z.tickv(indice) = val;
					else
						error('!Z.TICKV : Bad index value')
					end
				else
					error('!Z.TICKV : operation not authorized')
				end
			else
				if length(index) < 2
					if length(val) == 1
						z.tickv = val*ones(1,60);
					else
						for i=1:length(val)
							z.tickv(i)=val(i);
						end
					end
				else
					error('!Z.TICKV : operation not authorized')
				end
			end
			if z.ticks > 0 & sum(z.tickv == 0)~=60
				% !Z.TICKS > 0
				% !Z.TICKV ~= 0
				% Il faut tenir compte des !z.ticks+1 premieres valeurs de !Z.TICKV
				% eliminer les doublons et trier le vecteur forme par les !Z.TICKS+1 premieres valeurs de !z.tickv
				set(0,'DefaultAxesZTick',unique(sort(z.tickv(1:min(length(z.tickv),val+1)))));
			else
				set(0,'DefaultAxesZTickMode','auto');
			end
		% end case 'tickv'
		otherwise
			s = ['System variable !Z.' upper(index(1).subs) ' not translated.'];
			warning(s);
	end % switch index.subs
otherwise
	error('System variable !Z : operation not authorized')
end % index.type
