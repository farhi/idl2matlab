function c = power(a,b)

if class(a) == 'int32' & class(b) == 'int32'
c = fix(builtin('power',double(a),double(b)));
else
c= builtin('power',double(a),double(b));
end
