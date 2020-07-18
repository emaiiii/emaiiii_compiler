lex_mini:
	make clean
	flex mini_l_rlee037.lex
	gcc -o mini_l.o lex.yy.c -lfl

fibonacci:
	make clean && make bison
	cat fibonacci.min | parser

mytest:
	make clean && make bison
	cat mytest.min | parser

primes:
	make clean && make bison
	cat primes.min | parser

bison:
	bison -v -d --file-prefix=y mini_l_rlee037.y
	flex mini_l_rlee037.lex
	gcc -o parser y.tab.c lex.yy.c -lfl

clean:
	rm -f lex.yy.c y.tab.c y.tab.h mini_l calc parser *.output *.o *~
