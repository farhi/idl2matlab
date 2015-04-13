%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsref (varsysY)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 19 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function result = subsref(y, index)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

switch index(1).type
	case '.'
		switch index(1).subs
			case 'tickname'
				result = y.tickname;
			case 'title'
				result = y.title;
			case 'type'
				if strcomp(get(gca,'YScale'),'linear')
					result = 0;
				else
					result = 1;
				end
			% end case 'type'
			case 'style'
				result = y.style;
			case 'ticks'
				result = y.ticks;
			case 'ticklen'
				result = y.ticklen;
			case 'thick'
				result = y.thick;
			case 'range'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.range(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.range;
				end
			case 'crange'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.crange(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.crange;
				end
			case 's'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.s(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.s;
				end
			case 'margin'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.margin(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.margin;
				end
			case 'omargin'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.omargin(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.omargin;
				end
			case 'window'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.window(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.window;
				end
			case 'region'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = y.region(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = y.region;
				end
			case 'charsize'
				result = y.charsize;
			case 'minor'
				result = y.minor;
			case 'tickv'
				result = y.tickv;
			case 'gridstyle'
				result = y.gridstyle;
			case 'tickformat'
				result = y.tickformat;
			case 'tickinterval'
				result = y.tickinterval;
			case 'ticklayout'
				result = y.ticklayout;
			case 'tickunits'
				result = y.tickunits;
			otherwise
				error('bad field name')
		end % switch index.subs
	otherwise
		error('Not authorized')
end % switch index.type
