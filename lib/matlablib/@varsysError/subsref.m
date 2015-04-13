%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction subsref (varsysError)
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 12 / 08 / 2003
% Modifications : 12 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function result = subsref(e, index)
% Matlab fait appel a cette fonction a chaque instruction d'affectation
% ( A(i) = val, A{i} = val, ou A.champ = val )

switch index(1).type
	case '.'
		switch index(1).subs
			case 'name'
				result = e.name;
            % end case 'name'
            case 'block'
                result = e.block;
            % end case 'block'
            case 'code'
                if isempty(lasterr)
                    result = 0;
                else
                    result = -1;
                end
            % end case 'code'
            case 'sys_code'
                if length(index) == 2
					if (index(2).type == '()') | (index(2).type == '{}')
						indice = index(2).subs{:};
						if (indice <= 2) & (indice >= 1)
							result = e.sys_code(indice);
						else
							error('!ERROR_STATE.SYS_CODE : Bad index value')
						end
					else
						error('not authorized')
					end
				else
					result = e.sys_code;
				end
            % end case 'sys_code'
            case 'msg'
                result = lasterr;
            % end case 'msg'
            case 'sys_msg'
                result = e.sys_msg;
            % end case 'sys_msg'
            case 'msg_prefix'
                result = e.msg_prefix;
            % end case 'msg_prefix'
			otherwise
                w = ['System Variable !ERROR_STATE : ' index(1).subs ' is a bad field name'];
				warning(w);
		end % switch index.subs
	otherwise
		error('Not authorized')
end % switch index.type
