%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsref (varsysD)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 11 / 04 / 2003
% Modifications : 16 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function result = subsref(d, index)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

switch index.type
	case '.'
		switch index.subs
			case 'name'
				result = d.name;
			case 'x_size'
				pos = get(0,'DefaultFigurePosition');
				d.x_size = pos(3);
				result = d.x_size;
			case 'y_size'
				pos = get(0,'DefaultFigurePosition');
				d.y_size = pos(4);
				result = d.y_size;
			case 'x_vsize'
				pos = get(0,'DefaultFigurePosition');
				d.x_vsize = pos(3);
				result = d.x_vsize;
			case 'y_vsize'
				pos = get(0,'DefaultFigurePosition');
				d.y_vsize = pos(4);
				result = d.y_vsize;
			case 'x_ch_size'
				warning('!D.X_CH_SIZE not translated');
				result = d.x_ch_size;
			case 'y_ch_size'
				warning('!D.Y_CH_SIZE not translated');
				result = d.y_ch_size;
			case 'x_px_cm'
				warning('!D.X_PX_CM not translated');
				result = d.x_px_cm;
			case 'y_px_cm'
				warning('!D.Y_PX_CM not translated');
				result = d.y_px_cm;
			case 'n_colors'
				warning('!D.N_COLORS not translated');
				result = d.n_colors;
			case 'table_size'
				result = length(colormap);
			case 'fill_dist'
				warning('!D.FILL_DIST not translated');
				result = d.fill_dist;
			case 'window'
				currFig = get(0,'CurrentFigure');
				if isempty(currFig)
					d.window = -1;
				else
					d.window = currFig-1;
				end
				result = d.window;
			case 'unit'
				warning('!D.UNIT not translated');
				result = d.unit;
			case 'flags'
				warning('!D.FLAGS not translated');
				result = d.flags;
			case 'origin'
				warning('!D.ORIGIN not translated');
				result = d.origin;
			case 'zoom'
				warning('!D.ZOOM not translated');
				result = d.zoom;
			otherwise
				error('bad field name')
		end % switch index.subs
	otherwise
		error('Not authorized')
end % switch index.type
