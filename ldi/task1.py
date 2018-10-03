from z3 import *

def load_input():
	numbers = input().split()
	
	k_symbols = int(numbers[0])
	n_states = int(numbers[1])
	m_transitions = int(numbers[2])
	
	symbols = input().split()
	assert len(symbols) == k_symbols
	
	states = input().split()
	assert len(states) == n_states
	
	initial_state = states.index(input())
	final_state = states.index(input())
	
	transitions = []
	while m_transitions > 0:
		line = input().split()
		transitions.append({
			'current_symbol': symbols.index(line[0]),
			'current_state': states.index(line[1]),
			'next_symbol': symbols.index(line[2]),
			'next_state': states.index(line[3]),
			'head_move': int(line[4]),
		})
		m_transitions -= 1
	
	tape_length = int(input())
	tape = list(map(lambda x: symbols.index(x), input().split()))
	assert len(tape) == tape_length
	
	return {
		'symbols': symbols,
		'states': states,
		'transitions': transitions,
		'initial_state': initial_state,
		'final_state': final_state,
		'tape': tape,
	}

def prepare_formula(machine):
	s = Solver()
	variables = []
	
	for step in range(len(machine['tape']) + 1):
		variables.append({
		  # Variables for tape state: tape_step_pos_symbol
		  'tape': [[Bool('tape_{}_{}_{}'.format(step, pos, sym))
		            for sym in range(len(machine['symbols']))]
		           for pos in range(len(machine['tape']))],
		  # Active transitions
		  'transition': [Bool('trans_{}_{}'.format(step, tr))
		                 for tr in range(len(machine['transitions']))],
		  # Current state of machine
		  'state': [Bool('st_{}_{}'.format(step, st))
		            for st in range(len(machine['states']))],
		  # Position of head
		  'head': [Bool('hd_{}_{}'.format(step, hd))
		           for hd in range(len(machine['tape']))],
		  # Is final state
		  'final': Bool('final_{}'.format(step))
		})
	
	# Initial state
	for pos in range(len(machine['tape'])):
		for sym in range(len(machine['symbols'])):
			if machine['tape'][pos] == sym:
				s.add(variables[0]['tape'][pos][sym])
			else:
				s.add(Not(variables[0]['tape'][pos][sym]))
	
	for state in range(len(machine['states'])):
		if state == machine['initial_state']:
			s.add(variables[0]['state'][state])
		else:
			s.add(Not(variables[0]['state'][state]))
	
	for head in range(len(variables[0]['head'])):
		if head == 0:
			s.add(variables[0]['head'][head])
		else:
			s.add(Not(variables[0]['head'][head]))
	
	# Selection of final state
	for step in variables:
		s.add(Implies(step['final'], step['state'][machine['final_state']]))
	
	s.add(Or(*[And(step['final'],
	               *[Not(v['final'])
	                 for v in variables
	                 if v != step])
	           for step in variables]))
	
	# Pick exactly one transition in one step among those,
	# that are present in machine run
	for final in range(len(variables)):
		conditions = []
		
		for step in range(final):
			conditions.append(
			  Or(*[And(variables[step]['transition'][trans],
			           *[Not(variables[step]['transition'][rest])
			             for rest in range(len(machine['transitions']))
			             if rest != trans])
			       for trans in range(len(machine['transitions']))]
			    )
			)
		
		s.add(Implies(variables[final]['final'], And(*conditions)))
	
	# Next states
	for step in range(len(machine['tape'])):
		# Check if transition is active
		for nr in range(len(machine['transitions'])):
			transition = machine['transitions'][nr]
			tape_len = len(variables[step]['head'])
			
			start = 0
			stop = tape_len
			
			if transition['head_move'] == -1:
				s.add(Implies(
				  variables[step]['head'][0],
				  Not(variables[step]['transition'][nr])
				))
				start += 1
			
			if transition['head_move'] == 1:
				s.add(Implies(
				  variables[step]['head'][tape_len - 1],
				  Not(variables[step]['transition'][nr])
				))
				stop -= 1
			
			s.add(Implies(variables[step]['transition'][nr],
			  Or(*[And(variables[step]['head'][head],
			           variables[step]['state'][transition['current_state']],
					   variables[step]['tape'][head][transition['current_symbol']])
					   for head in range(start, stop)
				  ])
			))
		
		# Execute transition. Ensure only one state and one head position
		for nr in range(len(variables[step]['transition'])):
			transition = machine['transitions'][nr]
			start = -min(transition['head_move'], 0)
			stop = len(variables[step]['head']) - max(0, transition['head_move'])
			
			for head in range(start, stop):
				s.add(Implies(And(variables[step]['transition'][nr],
				                  variables[step]['head'][head]),
				              And(# Head
				                  variables[step + 1]['head'][head + transition['head_move']],
				                  *[Not(variables[step + 1]['head'][i])
				                    for i in range(len(variables[step]['head']))
				                    if i != head + transition['head_move']],
				                  # State
				                  variables[step + 1]['state'][transition['next_state']],
				                  *[Not(variables[step + 1]['state'][i])
				                    for i in range(len(variables[step]['state']))
				                    if i != transition['next_state']],
				                  # Tape
				                  variables[step + 1]['tape'][head][transition['next_symbol']],
				                  *[Not(variables[step + 1]['tape'][head][i])
				                    for i in range(len(variables[step + 1]['tape'][head]))
				                    if i != transition['next_symbol']],
				                  *[variables[step + 1]['tape'][hd][sym] == variables[step]['tape'][hd][sym]
				                    for hd in range(len(variables[step]['tape']))
				                    if hd != head
				                    for sym in range(len(variables[step]['tape'][head]))],
				                  )
				))
	
	return (s, variables)

if __name__ == "__main__":
	machine = load_input()
	(s, v) = prepare_formula(machine)
	if s.check() != sat:
		print("NO")
	else:
		print("YES")
		print("")
		
		model = s.model()
		#print(model)
		
		for step in v:
			tape = step['tape']
			state = [model[step['state'][s]]
			         for s in range(len(machine['states']))].index(True)
			state = machine['states'][state]
			
			out = []
			for cell in range(len(tape)):
				sym = [model[tape[cell][sym]]
				       for sym in range(len(tape[cell]))].index(True)
				sym = machine['symbols'][sym]
				st = " " if not model[step['head'][cell]] else state
				out.append("{}|{}".format(sym, st))
			print(*out)
						
			if model[step['final']]:
				break
