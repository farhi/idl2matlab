%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : constructeur varsysError
%            pour gerer la variable systeme !ERROR_STATE
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 12 / 08 / 2003
% Modifications : 21 / 08 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function e = varsysError(varargin)
% constructor
switch nargin
case 0
% cree un objet par defaut
    e.name  = 'IDL_M_SUCCESS';
    e.block = 'IDL_MBLK_CORE';
    e.code  = 0; % le code d'erreur par defaut, pour gerer les VS obsoletes !ERR et !ERROR egalement
        lasterr(''); % On reinitialise lasterr
    e.sys_code = [0 0];
    e.msg = '';
    e.sys_msg = '';
    e.msg_prefix = '%';
        
	%%%%%%%%%%%%%%% CREATION DE L'OBJET %%%%%%%%%%%%%%%%%%%
	e = class(e,'varsysError');
otherwise
	error('Wrong number of input arguments')
end
