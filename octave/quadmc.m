function C = quadmc(f,d,N)
  C = 0;
  for i = rand(d, N)
    C += f(i);
  end
  C /= N;