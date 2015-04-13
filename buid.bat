gcc -c -g -O2   idl2matlab.c
gcc -c -g -O2 main.c
gcc -c -g -O2 tree.c
gcc -c -g -O2 code.c
gcc -c -g -O2 rename.c
gcc -c -g -O2 lib.c
gcc -c -g -O2 hashtable.c
gcc -c -g -O2 table_symb.c
gcc -c -g -O2 lex.yy.c
gcc -g -O2 main.o idl2matlab.o tree.o hashtable.o table_symb.o rename.o lex.yy.o lib.o code.o -o idl2matlab.exe
