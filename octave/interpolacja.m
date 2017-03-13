A = 25;
f = @(x) (1)./(A*x.^2+1);

for N = 1:50
  X = 1:N;
  X = cos((X*2 + 1)/2/N * pi)
  Y = f(X);
  p = polyfit(X, Y, N - 1);

  N2 = 500;
  X2 = linspace(-1, 1, N2);
  Y2 = f(X2);

  plot(X2, f(X2), 'r-'); hold on;
  plot(X, f(X), 'ro');
  plot(X2, polyval(p, X2)); 
  title('Interpolacja');
  legend('f(x)', 'Punkty interpolacji', 'p');
  pause(0.2);
  hold off;
end;