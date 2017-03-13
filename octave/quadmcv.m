function C = quadmcv(f,d,N)
  v = rand(d, N);
  C = sum(f(v)) / N;