%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : constructeur varsysD
%            pour gerer la variable systeme !D
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 11 / 04 / 2003
% Modifications : 11 / 04 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function d = varsysD(varargin)
% constructor
global  i2mvs_version
switch nargin
case 0
% cree un objet par defaut
	%%%%%%%%%%%%%%%% CHAMPS TRADUITS %%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%% CHAMPS NON ENCORE TRADUITS %%%%%%%%%%%%%%
	if strcmp(i2mvs_version.os_family,'Windows')
		d.name = 'WIN';
	else
		d.name = 'X';
	end
	d.x_size = 640;
	d.y_size = 512;
	d.x_vsize = 640;
	d.y_vsize = 512;
	d.x_ch_size = 7;
	d.y_ch_size = 10;
	d.x_px_cm = 32;
	d.y_px_cm = 32;
	d.n_colors = 16777216;
	d.table_size = 256;
	d.fill_dist = 1;
	d.window = -1;
	d.unit = 0;
	d.flags = uint32(328124);
	d.origin = [0 0];
	d.zoom = [1 1];

	%%%%%%%%%%%%%%% CREATION DE L'OBJET %%%%%%%%%%%%%%%%%%%
	d = class(d,'varsysD');
otherwise
	error('Wrong number of input arguments')
end
