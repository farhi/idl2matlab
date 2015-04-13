% majCrange
% --------------------------------------------
% Pour mettre a jour la valeur des variables systeme ![XYZ].CRANGE

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : fonction majCrange
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 19 / 05 / 2003
% Modifications : 19 / 05 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function majCrange()

global  i2mvs_x i2mvs_y i2mvs_z

if i2mvs_x.type ~= 1
	i2mvs_x.crange = get(gca,'XLim');
else
	i2mvs_x.crange = log10(get(gca,'XLim'));
end

if i2mvs_y.type ~= 1
	i2mvs_y.crange = get(gca,'YLim');
else
	i2mvs_y.crange = log10(get(gca,'YLim'));
end

if i2mvs_z.type ~= 1
	i2mvs_z.crange = get(gca,'ZLim');
else
	i2mvs_z.crange = log10(get(gca,'ZLim'));
end