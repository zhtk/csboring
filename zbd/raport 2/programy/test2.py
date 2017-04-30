# -*- coding: UTF-8 -*-
import random

krok = 10
iteracje = 50
stop = 1000000

def prepare_table():
	print "@test2_prepare"

def fill_data():
	for i in range(krok):
		a = random.randrange(stop)
		
		for j in range(100):
			b = random.randrange(stop)
			
			for k in range(100):
				c = random.randrange(stop)
				print "INSERT INTO testy (a, b, c) VALUES (%d, %d, %d);" % (a, b, c)

def write_results():
	print "@test2_size"
	print "@test2_speed1"
	print "@test2_speed2"
	print "@test2_speed3"

def dump_res():
	pass # TODO

if __name__ == "__main__":
	prepare_table()
	
	i = 0
	while i < iteracje:
		fill_data()
		write_results()
		i += 1
	
	dump_res()
