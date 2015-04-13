%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsref (varsysX)
% Auteurs :
%                 Bourtembourg Reynald
%                 Szczuczak Nadege
% Date creation : 09 / 04 / 2003
% Modifications : 19 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function result = subsref(x, index)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

switch index(1).type
	case '.'
		switch index(1).subs
			case 'tickname'
				result = x.tickname;
			case 'title'
				result = x.title;
			case 'type'
				if strcomp(get(gca,'XScale'),'linear')
					result = 0;
				else
					result = 1;
				end
			% end case 'type'
			case 'style'
				result = x.style;
			case 'ticks'
				result = x.ticks;
			case 'ticklen'
				result = x.ticklen;
			case 'thick'
				result = x.thick;
			case 'range'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.range(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.range;
				end
			case 'crange'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.crange(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.crange;
				end
			case 's'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.s(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.s;
				end
			case 'margin'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.margin(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.margin;
				end
			case 'omargin'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.omargin(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.omargin;
				end
			case 'window'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.window(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.window;
				end
			case 'region'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = x.region(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = x.region;
				end
			case 'charsize'
				result = x.charsize;
			case 'minor'
				result = x.minor;
			case 'tickv'
				result = x.tickv;
			case 'gridstyle'
				result = x.gridstyle;
			case 'tickformat'
				result = x.tickformat;
			case 'tickinterval'
				result = x.tickinterval;
			case 'ticklayout'
				result = x.ticklayout;
			case 'tickunits'
				result = x.tickunits;
			otherwise
				error('bad field name')
		end % switch index.subs
	otherwise
		error('Not authorized')
end % switch index.type
