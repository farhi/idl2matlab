%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : methode subsasgn (varsysD)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 11 / 04 / 2003
% Modifications : 27 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




function d = subsasgn(d, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val

fieldsKwn = char('x_size' , 'y_size' , 'x_vsize' , 'y_vsize' , 'x_ch_size' , 'y_ch_size' , 'x_px_cm' , 'y_px_cm' , 'n_colors' , 'table_size' , 'fill_dist' , 'unit' , 'flags' , 'origin' , 'zoom');

switch index.type
	case '.'
		if strmatch(index.subs,fieldsKwn)
			error(' Attempt to write to a readonly variable: Structure reference.')
		else
			switch index.subs
				case 'name'
					d.name = val;
				case 'window'
					d.window = val;
				otherwise
					error(sprintf('Tag name %s is undefined for structure !DEVICE.',index.subs))
			end
		end
	otherwise
		error('not authorized')
end % index.type
