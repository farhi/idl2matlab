%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsref (varsysZ)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 10 / 07 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function result = subsref(z, index)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

switch index(1).type
	case '.'
		switch index(1).subs
			case 'tickname'
				result = z.tickname;
			case 'title'
				result = z.title;
			case 'type'
				if strcomp(get(gca,'ZScale'),'linear')
					result = 0;
				else
					result = 1;
				end
			% end case 'type'
			case 'style'
				result = z.style;
			case 'ticks'
				result = z.ticks;
			case 'ticklen'
				result = z.ticklen;
			case 'thick'
				result = z.thick;
			case 'range'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.range(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.range;
				end
			case 'crange'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.crange(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.crange;
				end
			case 's'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.s(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.s;
				end
			case 'margin'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.margin(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.margin;
				end
			case 'omargin'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.omargin(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.omargin;
				end
			case 'window'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.window(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.window;
				end
			case 'region'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = z.region(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = z.region;
				end
			case 'charsize'
				result = z.charsize;
			case 'minor'
				result = z.minor;
			case 'tickv'
				result = z.tickv;
			case 'gridstyle'
				result = z.gridstyle;
			case 'tickformat'
				result = z.tickformat;
			case 'tickinterval'
				result = z.tickinterval;
			case 'ticklayout'
				result = z.ticklayout;
			case 'tickunits'
				result = z.tickunits;
			case 'exact'
				result = z.exact;
			case 'extend'
				result = z.extend;
			case 'noAxis'
				result = z.noAxis;
			case 'noBox'
				result = z.noBox;
			case 'ynozero'
				result = z.ynozero;
			otherwise
				error('bad field name')
		end % switch index.subs
	otherwise
		error('Not authorized')
end % switch index.type
