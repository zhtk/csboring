function L = rchol2(A)
% Wykonuje rekurencyjny rozkład macierzy A metodą Choleskyego.
% W przypadku, gdy A nie jest dodatnio określoną kwadratową macierzą symetryczną
% wynik pozostaje nieokreślony.

[N, ~] = size(A);

% Przypadek, gdy N == 1. Rekurencja powinna się zakończyć.
if (N == 1) 
  L = sqrt(A);
  return;
endif

% Wyznaczenie rozkładu podmacierzy A_{11}
M1 = floor(N/2);
A11 = A(1:M1, 1:M1);
L11 = rchol2(A11);

% L_{12} składa się z samych zer.
% Teraz wyznaczymy L_{21} na podstawie faktu, że A_{21} = L_{21} * L11'
A21 = A(M1+1:N, 1:M1);
L21 = A21 * inv(L11');

% Na końcu zajmiemy się wyznaczeniem L22
% A22 = L21 * L21' + L22 * L22'
L22 = A(M1+1:N, M1+1:N) - L21 * L21';
L22 = rchol2(L22);

% Złożenie wyniku
L = zeros(N, N);
L(1:M1, 1:M1) = L11;
L(M1+1:N, 1:M1) = L21;
L(M1+1:N, M1+1:N) = L22;

