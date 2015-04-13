function res=stddev(vect)
%*******     ******
%**

L =length(vect);

E =total(vect)/L;                 % MEAN

V =abs(vect-E);                   % (vect-E)

Variance= total(V^2)/(L-1);       % Variance

res= sqrt(Variance);              % Standard deviation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
