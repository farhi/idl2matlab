% i2m_background
% --------------------------------------------
% Equivalent to :
% graphic keyword BACKGROUND = ...
% in IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : fonction i2m_background
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 02 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_background(colorIndice)

global i2mvs_p

if isempty(colorIndice),
	if i2mvs_p.multi == 0,
		col= i2mColormap(i2mvs_p.background);
		set(gca, 'Color',col);
	end
else,           col= i2mColormap(colorIndice);
		set(gca,'Color',col);
end
