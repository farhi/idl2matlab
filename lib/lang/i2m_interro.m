function res = i2m_interro(cond, ifTrue, ifFalse)
% Translation of IDL Syntax statement :
%
% x=(a EQ 7 ? "1" : 2)

  if cond,
    res = ifTrue;
  else,
    res = ifFalse;

end %function i2m_interro
