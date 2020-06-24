build:
	rm -f lex.yy.c
	rm -f lexer
	flex lexer.lex
	gcc -o lexer lex.yy.c -lfl

fib:
	cat fibonacci.min | ./lexer

mytest:
	cat mytest.min | ./lexer

primes:
	cat primes.min | ./lexer

err1:
	cat errtest1.min | ./lexer

err2:
	cat errtest2.min | ./lexer		

clean:
	rm -f lex.yy.c
	rm -f lexer
