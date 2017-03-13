function w = gauss(N)
  A = hilb(N);
  X = rand(N, 1);
  w = X;
  b = A*X;
  x = A\b;
  
  typ = Inf;
  
  w = norm(x - X, typ) / norm(X, typ);
end