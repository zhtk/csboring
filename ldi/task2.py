#!/usr/bin/env python3
from z3 import *

sizes = input().split()
size_x, size_y = int(sizes[0]), int(sizes[1])

# Definion of function describing pool
f = Function('f', IntSort(), IntSort(), BoolSort())
xv = Int('xv')
yv = Int('yv')
conditions = [
	ForAll([xv, yv], Implies(xv < 0, Not(f(xv, yv)))),
	ForAll([xv, yv], Implies(xv >= size_x, Not(f(xv, yv)))),
	ForAll([xv, yv], Implies(yv < 0, Not(f(xv, yv)))),
	ForAll([xv, yv], Implies(yv >= size_y, Not(f(xv, yv)))),
]

def describe_row(row, description):
	res = []
	variables = [Int("r_{}_{}".format(row, i))
	             for i in range(len(description) * 2 + 2)]
	# Left bounduary
	res.append(variables[0] == -1)
	#res.append(ForAll(xv, Implies(And(variables[0] + 1 <= xv, xv < variables[1]),
	#                              Not(f(xv, row)))))
	
	# Middle ranges
	for i in range(len(description)):
		# [i*2 ---> i*2 + 1) - empty space
		res.append(variables[i*2] < variables[i*2 + 1])
		res.append(ForAll(xv, Implies(And(variables[i*2] <= xv, xv < variables[i*2 + 1]),
	                                  Not(f(xv, row)))))
		
		# [i*2 + 1 ---> i*2 + 2) - filled space
		res.append(variables[i*2 + 1] + description[i] == variables[i*2 + 2])
		res.append(ForAll(xv, Implies(And(variables[i*2 + 1] <= xv, xv < variables[i*2 + 2]),
	                                  f(xv, row))))
	
	# Right bounduary
	res.append(variables[-1] == size_x + 1)
	res.append(ForAll(xv, Implies(And(variables[-2] <= xv, xv < variables[-1] - 1),
	                              Not(f(xv, row)))))
	
	return res

def describe_collumn(col, description):
	res = []
	variables = [Int("c_{}_{}".format(col, i))
	             for i in range(len(description) * 2 + 2)]
	# Left bounduary
	res.append(variables[0] == -1)
	#res.append(ForAll(xv, Implies(And(variables[0] + 1 <= xv, xv < variables[1]),
	#                              Not(f(xv, row)))))
	
	# Middle ranges
	for i in range(len(description)):
		# [i*2 ---> i*2 + 1) - empty space
		res.append(variables[i*2] < variables[i*2 + 1])
		res.append(ForAll(xv, Implies(And(variables[i*2] <= xv, xv < variables[i*2 + 1]),
	                                  Not(f(col, xv)))))
		
		# [i*2 + 1 ---> i*2 + 2) - filled space
		res.append(variables[i*2 + 1] + description[i] == variables[i*2 + 2])
		res.append(ForAll(xv, Implies(And(variables[i*2 + 1] <= xv, xv < variables[i*2 + 2]),
	                                  f(col, xv))))
	
	# Right bounduary
	res.append(variables[-1] == size_y + 1)
	res.append(ForAll(xv, Implies(And(variables[-2] <= xv, xv < variables[-1] - 1),
	                              Not(f(col, xv)))))
	
	return res

def model(phi):
    s = Solver()
    s.add(phi)
    
    if s.check() == sat:
        res = s.model()
        
        # Add additional constraints to find another solution
        wrong = [f(i, j) == res.evaluate(f(i, j))
                 for i in range(size_x)
                 for j in range(size_y)]
        s.add(Not(And(wrong)))
        
        if s.check() == sat:
            print("many solutions")
            exit(0)
        else:
            return res
    else:
        print("unsolvable")
        exit(0)

# Load input data
for i in range(size_y):
	description = list(map(int, input().split()))
	conditions += describe_row(i, description[:-1])

for i in range(size_x):
	description = list(map(int, input().split()))
	conditions += describe_collumn(i, description[:-1])

res = model(conditions)

# Print results
for r in range(size_y):
	for c in range(size_x):
		v = '#' if res.evaluate(f(c,r)) else '.'
		print(v, end='')
	print("")
