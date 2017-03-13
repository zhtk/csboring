function L = arclength(x, y, A, B)
 f = @(p) sqrt(df(x, p)^2 + df(y, p)^2);
 L = quad(f, A, B);
end;