buildParse: 
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex
	gcc -o parser y.tab.c lex.yy.c -lfl

fibParse: 
	cat fibonacci.min | ./parser		

mytestParse:
	cat mytest.min | ./parser

primesParse:
	cat primes.min | ./parser
