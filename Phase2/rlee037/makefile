lex_mini:
	make clean
	flex mini_l.lex
	gcc -o mini_l.o lex.yy.c -lfl

lex_calc:
	rm -f lex.yy.c mini_l calc *.o *~
	flex calc.lex
	gcc -o calc.o lex.yy.c -lfl

fibonacci:
	make clean && make bison
	cat fibonacci.min | parser

mytest:
	make clean && make bison
	cat mytest.min | parser

primes:
	make clean && make bison
	cat primes.min | parser

error:
	make clean && make bison
	cat error_test.min | parser

lex_calctest:
	cat calc_test.min | ./calc.o

bison:
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex
	gcc -o parser y.tab.c lex.yy.c -lfl

clean:
	rm -f lex.yy.c y.tab.c y.tab.h mini_l calc parser *.output *.o *~
