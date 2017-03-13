function m = wilk(N)
A = zeros(N, N);

for i = 1:N
  A(i, N) = 1;
  A(i, i) = 1;
end

for i = 1:N
  for j = 1:(i-1)
  A(i, j) = -1;
  end
end

m = A;