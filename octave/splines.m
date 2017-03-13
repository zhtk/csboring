A = 25;
f = @(x) (1)./(A*x.^2+1);

for N = 2:30
  X = linspace(-1, 1, N);
  Y = f(X);
  p = polyfit(X, Y, N - 1);
  
  N2 = 500;
  X2 = linspace(-1, 1, N2);
  Y2 = f(X2);
  
  s = spline(X, Y);
  y = ppval(s, X2);
  
  plot(X2, f(X2), 'r-'); hold on;
  plot(X, f(X), 'ro');
  plot(X2, ppval(s, X2)); 
  title('Spline');
  legend('f(x)', 'Punkty interpolacji', 'p');
  pause(0.2);
  hold off;
end;