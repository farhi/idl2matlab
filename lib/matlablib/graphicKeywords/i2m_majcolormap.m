
% i2m_majColormap
% --------------------------------------------
% to update the value of the current figure's colormap when it is necessary

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : fonction i2m_majColormap
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 23 / 05 / 2003
% Modifications : 05 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function i2m_majColormap()

global i2mColorTable i2mDecomposed i2mMapFig

currFig = get(0,'CurrentFigure');
if ~isempty(currFig), if i2mDecomposed == 0, if isempty(find(i2mMapFig==currFig)),
    colormap(i2mColorTable); i2mMapFig(end+1)=currFig;
end; end; end;
