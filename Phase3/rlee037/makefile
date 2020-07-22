lex_mini:
	make clean
	flex mini_l.lex
	gcc -o mini_l.o lex.yy.c -lfl

lex_calc:
	rm -f lex.yy.c mini_l calc *.o *~
	flex calc.lex
	gcc -o calc.o lex.yy.c -lfl

fibonacci:
	make clean && make parser
	cat fibonacci.min | parser

mytest:
	make clean && make parser
	cat mytest.min | parser

primes:
	make clean && make parser
	cat primes.min | parser

error:
	make clean && make parser
	cat error_test.min | parser

lex_calctest:
	cat calc_test.min | ./calc.o

parser:  mini_l.lex mini_l.y
	make clean
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex
	g++ -std=c++11 -o parser y.tab.c lex.yy.c -lfl

clean:
	rm -f lex.yy.c y.tab.c y.tab.h mini_l calc parser *.output *.o *.hh *~
