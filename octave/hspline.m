function pp = hspline(x, y, z)
% Wyznacza hermitowski kubiczny splajn interpolacyjny.
% W wektorze x powinny być posortowane rosnąco węzły,
% w wektorze y wartości odpowiadjące węzłom, a w wektorze z
% pochodne interpolowanej funkcji w węzłach.

% Test na ilość argumentów
if (nargin != 3)
  error("hspline: zła ilość argumentów");
endif;

% Test, czy argumenty są wektorami. Pedant mógłby raczej napisać "iscolumn"
if (!isvector(x) || !isvector(y) || !isvector(z))
  error("hspline: argumenty nie są wektorami");
endif;

% Transformacja wektorów na kolumny
x = x(:);
y = y(:);
z = z(:);

% Test równości rozmiaru wektorów
if (!size_equal(x, y, z))
  error("hspline: wektory różnej długości");
endif;

% Test, czy węzły są różne
if (length(unique(x)) != length(x))
  error("hspline: węzły się powtarzają");
endif;

% Posortowanie węzłów
S = sortrows([x, y, z]);

if (sum(x != S(:, 1)) > 0)
  x = S(:, 1);
  y = S(:, 2);
  z = S(:, 3);
  warning("hspline: węzły nie są posortowane");
endif;

% Ostatnie sprawdzenie, dla pewności i wygody
if (length(x) < 2)
  error("hspline: zbyt mała ilość punktów do interpolacji");
endif;

% Wyznaczenie wielomianu na przedziale [x_1, x_2]
% na podstawie układu równań podanego w treści zadania
w = x - x(1);

A = [w(1)^3, w(1)^2, w(1)^1, 1;
     w(2)^3, w(2)^2, w(2)^1, 1;
     3*w(1)^2, 2*w(1)^1, 1, 0;
     3*w(2)^2, 2*w(2)^1, 1, 0];
b = [y(1); y(2); z(1); z(2)];
w = A \ b;
w = w';

% Warunek kończący rekurencję
if (length(x) == 2)
  pp = mkpp(x, w);
  return;
endif;

% Rekurencyjne wyznaczenie splajnu
% Dzielę przedział na dwa mniej więcej równe, szukam wyniku na podprzedziałach,
% a następnie je sklejam.
n = length(x);
np = floor((n + 1) / 2);

pp1 = hspline(x(1:np), y(1:np), z(1:np));
pp2 = hspline(x(np:n), y(np:n), z(np:n));
[~, p1, ~, ~, ~] = unmkpp(pp1);
[~, p2, ~, ~, ~] = unmkpp(pp2);

w = [p1; p2];
pp = mkpp(x, w);