function n=keyword_set(var)
%function keyword_set(arg)
%******** ***********
%**
%if ~exist('var','var'); n=0; return; end;
n=1;
if isempty(var); n=0;
elseif n_elements(var) == 1
    if isnumeric(var); if ~var(1); n=0; end; end;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
