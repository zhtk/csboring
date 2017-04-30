# -*- coding: UTF-8 -*-
import random

krok = 10
iteracje = 100
wiersze = 100000
klucze = []
stop = 100000000

def prepare_table():
	# Drop starych tabel
	print "DROP TABLE wyniki PURGE;"
	print "DROP TABLE testy PURGE;"
	print "DROP index test_ix;"
	
	# Założenie nowych
	print "CREATE TABLE testy (a NUMBER, b NUMBER);"
	print "CREATE TABLE wyniki (tbl NUMBER, ix1b NUMBER, ix1k NUMBER, ix1z NUMBER, ix2b NUMBER, ix2k NUMBER, ix2z NUMBER);"
	
	# Wypełnienie pierwszej kolumny
	i = 0
	while i < wiersze:
		k = random.randrange(stop)
		v = random.randrange(stop)
		klucze.append(k)
		print "INSERT INTO testy (a, b) VALUES (%d, %d);" % (k, v)
		i += 1

def fill_data():
	for k in klucze:
		v = random.randrange(stop)
		print "INSERT INTO testy (a, b) VALUES (%d, %d);" % (k, v)

def write_results():
	print "@sizetest"

if __name__ == "__main__":
	prepare_table()
	
	i = 0
	while i < iteracje:
		fill_data()
		write_results()
		i += 1
		
