function d = df(f, x, t)
  if nargin < 3
    t = sqrt(eps);
  end;
  
  d = f(x + t) - f(x);
  d /= t;
end;