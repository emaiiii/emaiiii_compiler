build: 
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex
	gcc -o parser y.tab.c lex.yy.c -lfl

fib: 
	cat fibonacci.min | ./parser		

mytest:
	cat mytest.min | ./parser

primes:
	cat primes.min | ./parser
