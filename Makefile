CFLAGS=-g
TOOL=idl2matlab

$(TOOL): $(TOOL).o main.o tree.o code.o rename.o lib.o hashtable.o table_symb.o lex.yy.o
	gcc $(CFLAGS) main.o $(TOOL).o tree.o hashtable.o table_symb.o rename.o lex.yy.o lib.o code.o -o $@

$(TOOL).o: $(TOOL).y type.h $(TOOL).l
	yacc -d $(TOOL).y
	lex $(TOOL).l
	mv y.tab.c $(TOOL).c
	gcc -c $(CFLAGS)   $(TOOL).c	

lex.yy.o : lex.yy.c
	gcc -c ${CFLAGS} lex.yy.c

main.o : main.c
	gcc -c ${CFLAGS} main.c

tree.o : tree.c
	gcc -c ${CFLAGS} tree.c
	
rename.o : rename.c
	gcc -c ${CFLAGS} rename.c

lib.o : lib.c
	gcc -c ${CFLAGS} lib.c
	
table_symb.o : table_symb.c
	gcc -c ${CFLAGS} table_symb.c

hashtable.o : hashtable.c
	gcc -c ${CFLAGS} hashtable.c	

code.o : code.c
	gcc -c ${CFLAGS} code.c
	
clean:
	rm -f *.o y.* *.out y.* lex.yy.* $(TOOL).c $(TOOL).o $(TOOL).test* $(TOOL)

