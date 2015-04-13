function h = creer_array_plus(a,b)

mult = 1;

for i=1:size(size(a),2),
  mult = mult * size(a,i);
end


for i=1:mult
  h{i} = strcat(a{i},b{i});
end
