%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsref (varsysP)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 06 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function result = subsref(p, index)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

switch index(1).type
	case '.'
		switch index(1).subs
			case 'linestyle'
				result = p.linestyle;
			case 'linestylem'
				result = p.linestylem;
			case 'psym'
				result = p.psym;
			case 'psymm'
				result = p.psymm;
			case 'thick'
				result = p.thick;
			case 'ticklen'
				result = p.ticklen;
			case 'background'
				result = p.background;
			case 'charsize'
				result = p.charsize;
			case 'charthick'
				result = p.charthick;
			case 'clip'
				result = p.clip;
			case 'color'
				result = p.color;
			case 'font'
				result = p.font;
			case 'multi'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 5) & (indice >= 1)
							result = p.multi(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = p.multi;
				end
			case 'noclip'
				result = p.noclip;
			case 'noerase'
				result = p.noerase;
			case 'nsum'
				result = p.nsum;
			case 'position'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 4) & (indice >= 1)
							result = p.position(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = p.position;
				end
			case 'region'
				if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 4) & (indice >= 1)
							result = p.region(indice);
						else
							error('Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = p.region;
				end
			case 'subtitle'
				result = p.subtitle;
			case 'symsize'
				result = p.symsize;
			case 't'
				result = p.t;
			case 't3d'
				result = p.t3d;
			case 'title'
				result = p.title;
			case 'channel'
				result = p.channel;
			case 'multioplot'
				result = p.multioplot;
			otherwise
				error('Bad field')
		end % switch index.subs
	otherwise
		error('Not authorized')
end % switch index.type
