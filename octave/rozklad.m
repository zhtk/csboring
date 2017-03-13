function [L, U, P] = rozklad(A)
% Wykonuje rozkład macierzy
[N, M] = size(A);
p = 1:N;

for i = 1:M-1
  [~, piv] = max(abs(A(i:N, i)));
  piv = piv + i - 1;
  
  A([piv, i], :) = A([i, piv], :);
  p([piv, i]) = p([i, piv]);
  
  A(i+1:N, i) = A(i+1:N, i)/A(i,i);
  A(i+1:N, i+1:M) = A(i+1:N, i+1:M) - A(i+1:N, i) * A(i, i+1:M);
end

U = triu(A);
L = tril(A, -1) + eye(N, M);
P = eye(N, N);
P = P(p, :);