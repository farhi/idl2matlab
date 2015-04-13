% wset
% --------------------------------------------
% Equivalent to :
% function WSET [, Window_Index]
% en IDL

function  wset(win)

global i2mvs_d

if win > 0,
try, set(get(win,'parent'),'CurrentAxes',win); i2mvs_d.window=win; catch, end;
end;
