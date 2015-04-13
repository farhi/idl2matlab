function c = mod(a,b)

if ((a<=0 & b>=0) | (a>=0 & b<=0))
  c = mod(double(a),double(b)) + b;
else c = mod(double(a),double(b));
end  
