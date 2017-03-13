function testrchol2()
  % Testuje funkcję rchol2 na losowej macierzy
  format long;
  
  for i = 1:50:1001
    % Wylosowanie macierzy do testów
    L = rand(i, i);
    A = L * L';
    
    % Zmierzenie czasu rozkładu
    czas = tic;
    L2 = rchol2(A);
    czas = toc(czas);
    
    % Doprowadzenie rozkładu do postaci porównywalnej z wejściem
    A2 = L2 * L2';
    
    % Błąd obliczeń
    err = norm(A - A2, Inf);
    
    % Wypisanie wyniku
    printf("Rozmiar macierzy: %d,\tczas: %d,\tbłąd: %d\n", i, czas, err);
  endfor
