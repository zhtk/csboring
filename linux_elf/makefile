libinter: interceptor.c
	gcc interceptor.c -o libinterceptor.so -Wl,-soname=libinterceptor.so -shared -fPIC

example: example.c interceptor.c
	gcc -g example.c interceptor.c -o example

.PHONY clean:
	rm -f libinterceptor.so example
