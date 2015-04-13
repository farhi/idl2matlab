
% wdelete
% --------------------------------------------
% Equivalent to :
% function WDELETE [, Window_Index [, ...]],
% en IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : WDELETE
%            Permet d'effacer une fenetre
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 26 / 05 / 2003
% Modifications : 28 / 05 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [varargout]=window(varargin)

global i2mvs_d

l = nargin; % le nombre d'arguments

if l == 0
	% pas d'arguments
	if i2mvs_d.window ~= -1
		delete(i2mvs_d.window+1);
	end
else
	for i=1:l
		try
			delete(varargin{i}+1);
		end
	end
end

