#!/usr/bin/env python3
import math

def T_helper(var, x):
	(mi, sig, tau) = var
	
	tmp = (-1) * (x - mi) * (x - mi) / 2 / sig / sig
	return math.exp(tmp) * tau / sig


# Liczy T^n_{A,i}
def T(a, b, x):
	return T_helper(a, x) / (T_helper(a, x) + T_helper(b, x))

def next_tau(a, b, xs):
	res = 0
	for i in xs:
		res += T(a, b, i)
	
	return res / len(xs)

def next_mi(a, b, xs):
	res1 = 0
	res2 = 0
	
	for i in xs:
		res1 += T(a, b, i) * i
		res2 += T(a, b, i) 
	
	return res1 / res2

def next_sig(a, b, xs, mi2):
	res1 = 0
	res2 = 0
	
	for i in xs:
		res1 += T(a, b, i) * (i - mi2) * (i - mi2)
		res2 += T(a, b, i) 
	
	return res1 / res2

if __name__ == "__main__":
	xs = [0, 0, 1, 1, 2, 3, 4, 6, 9]
	var_a = (0, 1, 0.5)
	var_b = (0, 10, 0.5)
	
	i = 0
	while i < 5:
		tau_a = next_tau(var_a, var_b, xs)
		tau_b = next_tau(var_b, var_a, xs)
		mi_a = next_mi(var_a, var_b, xs)
		mi_b = next_mi(var_b, var_a, xs)
		sig_a = next_sig(var_a, var_b, xs, mi_a)
		sig_b = next_sig(var_b, var_a, xs, mi_b)
		
		var_a = (mi_a, sig_a, tau_a)
		var_b = (mi_b, sig_b, tau_b)
		
		i += 1
	
	print(var_a)
	print(var_b)
	for i in xs:
		print(str(i) + " ---> " + str(T(var_a, var_b, i)))
