%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : methode subsasgn (varsysError)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 21 / 08 / 2003
% Modifications : 21 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function e = subsasgn(e, index, val)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val )

switch index(1).type
case '.'
	switch index(1).subs
	case 'name'
        e.name = val;
    % end case 'name'
    case 'block'
        e.block = val;
    % end case 'block'
    case 'code'
        if val == 0
            % On reinitialise lasterr
            lasterr('');
            e.code = 0;
        else
            e.code = val;
        end
    % end case 'code'
    case 'sys_code'
        if length(index) == 2
		    if (index(2).type == '()') | (index(2).type == '{}')
				indice = index(2).subs{:};
				if (indice <= 2) & (indice >= 1)
					e.sys_code(indice)= val;
				else
					error('!ERROR_STATE.SYS_CODE : Bad index value')
				end
            else
				error('not authorized')
			end
		else
			e.sys_code = val;
		end
    % end case 'sys_code'
    case 'msg'
        warning('Attempt to write to a readonly variable: !ERROR_STATE.MSG'); % warning ou error ?
    % end case 'msg'
    case 'sys_msg'
        warning('Attempt to write to a readonly variable: !ERROR_STATE.SYS_MSG'); % warning ou error ?
    % end case 'sys_msg'
    case 'msg_prefix'
        e.msg_prefix = val;
    % end case 'msg_prefix'
    otherwise
		w = ['System Variable !ERROR_STATE : ' index(1).subs ' is a bad field name'];
		warning(w);
	end % switch index.subs
otherwise
	error('System variable !ERROR_STATE : operation not authorized')
end % switch index.type
