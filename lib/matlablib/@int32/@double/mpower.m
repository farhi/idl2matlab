function c = mpower(a,b)

d = builtin('mpower',a,b);
if ((round(b)-b ~= 0.0) & (round(a)-a == 0) & (round(b)-b == 0)) 
c = round(builtin('mpower',a,b));end
