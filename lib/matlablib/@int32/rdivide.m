function c = rdivide(a,b)

if class(a) == 'int32' & class(b) == 'int32'
c = fix(builtin('rdivide',double(a),double(b)));
else
c= builtin('rdivide',double(a),double(b));
end

