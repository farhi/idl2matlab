function h = creer_array_ge(a,b)

mult = 1;

for i=1:size(size(a),2),
  mult = mult * size(a,i);
end

h = zeros(size(a));

for i=1:mult
  h(i) = a{i} >= b{i};
end
