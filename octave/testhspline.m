% Funkcja Rungego
f = @(x) (1)./(x.^2+1);
% I jej pochodna (taką mam nadzieję...)
fd = @(x) (2).*x./(x.^2+1)./(x.^2+1);

% Zakres, do którego należy n
Z = 1:10;

E = [];
Es = 1;
for n = Z
  % Liczba próbek wynosi 2^n
  N = 2^n;
  X = linspace(-5, 5, N);
  Y = f(X);
  Yd = fd(X);
  
  pp = hspline(X, Y, Yd);
  
  % Różnice próbkujemy w odrobinę innych miejscach
  X2 = linspace(-5, 5, 3^12);
  
  % Wykresik - do sprawdzenia, czy jest dobrze
  %plot(X2, f(X2), 'r-'); hold on;
  %plot(X2, ppval(pp, X2), 'b-');
  %title('Interpolacja');
  %pause(0.2);
  %hold off;
  
  En = max(abs(f(X2) - ppval(pp, X2)));
  E = [E; Es / En];
  Es = En;
end;

% Wygląda na to, że wykres jest stabilny od wartości 7
semilogy(Z, log2(E));
title('Wykres w zależności od r');