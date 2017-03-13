function t = arcsplit(x, y, A, B, s)
  L = arclength(x, y, A, B);
  L *= s;
  f = @(p) arclength(x, y, A, p) - L;
  t = fzero(f, [A, B]);
end;