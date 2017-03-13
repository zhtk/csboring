function w = exponent(x, N)
w = 1;
p = 1;
for n = 1:N
  p = p*(x/n);
  w = w + p;
end
end