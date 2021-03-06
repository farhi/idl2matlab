# This Makefile is automatically produced by running: ./configure
# Use that Makefile with: make

SHELL = /bin/sh

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
srcdir = .
libdir = ${exec_prefix}/lib
mandir = ${prefix}/share/man


DEBUG = -DDEBUG=0
mc_libdir = $(libdir)/idl2matlab

CFLAGS = -g -fno-stack-protector
CC = gcc
FLEX = flex
FLEXFLAGS=-i

BISON = bison
BISONFLAGS = -v -d

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644



#
# End of configuration section.
#

BINOBJS=idl2matlab

OBJECTS=idl2matlab.o main.o tree.o code.o rename.o lib.o hashtable.o table_symb.o lex.yy.o

COBJECTS=instrument.tab.c lex.yy.c debug.c \
	memory.c list.c symtab.c coords.c cexp.c \
	file.c cogen.c port.c

.SUFFIXES: .c .o

.c.o:
	$(CC) -c $(CFLAGS)

all: $(BINOBJS)

$(BINOBJS): $(BINOBJS).o main.o tree.o code.o rename.o lib.o hashtable.o table_symb.o lex.yy.o
	$(CC) $(CFLAGS) main.o $(BINOBJS).o tree.o hashtable.o table_symb.o rename.o lex.yy.o lib.o code.o -o $@

$(BINOBJS).o: $(BINOBJS).y type.h $(BINOBJS).l
	$(BISON) $(BISONFLAGS) $(BINOBJS).y --output-file=idl2matlab.c
	$(FLEX) $(FLEXFLAGS) $(BINOBJS).l
	$(CC) -c $(CFLAGS)   $(BINOBJS).c

lex.yy.o : lex.yy.c
	$(CC) -c ${CFLAGS} lex.yy.c

main.o : main.c
	$(CC) -c ${CFLAGS} main.c

tree.o : tree.c
	$(CC) -c ${CFLAGS} tree.c

rename.o : rename.c
	$(CC) -c ${CFLAGS} rename.c

lib.o : lib.c
	$(CC) -c ${CFLAGS} lib.c

table_symb.o : table_symb.c
	$(CC) -c ${CFLAGS} table_symb.c

hashtable.o : hashtable.c
	$(CC) -c ${CFLAGS} hashtable.c

code.o : code.c
	$(CC) -c ${CFLAGS} code.c

install: installdirs $(BINOBJS)
	$(INSTALL_PROGRAM) idl2matlab $(bindir)/idl2matlab
	if [ -d $(mc_libdir) ]; then \
	  echo "Moving your old library dir $(mc_libdir) to $(mc_libdir).`date +%Y%m%d_%H.%M`"; \
	  mv -f $(mc_libdir) $(mc_libdir).`date +%Y%m%d_%H.%M`;\
        fi;
	$(srcdir)/mkinstalldirs $(mc_libdir)
	for file in `cd lib; ls`; do \
	  if [ -d lib/$$file ]; then \
	    $(srcdir)/mkinstalldirs $(mc_libdir)/$$file; \
	    for comp in `cd lib/$$file; ls`; do \
        if [ -f $(mc_libdir)/$$file/$$comp ]; then \
          echo "Renaming old library element version $(mc_libdir)/$$file/$$comp to $(mc_libdir)/$$file/$$comp.old"; \
          mv -f $(mc_libdir)/$$file/$$comp $(mc_libdir)/$$file/$$comp.old; \
        fi; \
	      if [ -d lib/$$file/$$comp ]; then \
          $(srcdir)/mkinstalldirs $(mc_libdir)/$$file/$$comp; \
          for tool in `cd lib/$$file/$$comp; ls`; do \
	      if [ -d lib/$$file/$$comp/$$tool ]; then \
          $(srcdir)/mkinstalldirs $(mc_libdir)/$$file/$$comp/$$tool ; \
	        for subtool in `cd lib/$$file/$$comp/$$tool; ls`; do \
		  if [ -d lib/$$file/$$comp/$$tool/$$subtool ]; then \
		     $(srcdir)/mkinstalldirs $(mc_libdir)/$$file/$$comp/$$tool/$$subtool ; \
	             for subsubtool in `cd lib/$$file/$$comp/$$tool/$$subtool; ls`; do \
                       $(INSTALL_DATA) lib/$$file/$$comp/$$tool/$$subtool/$$subsubtool $(mc_libdir)/$$file/$$comp/$$tool/$$subtool/$$subsubtool; \
                     done \
		  else \
                    $(INSTALL_DATA) lib/$$file/$$comp/$$tool/$$subtool $(mc_libdir)/$$file/$$comp/$$tool/$$subtool; \
		  fi; \
                done \
              else \
                $(INSTALL_DATA) lib/$$file/$$comp/$$tool $(mc_libdir)/$$file/$$comp/$$tool; \
	      fi; \
          done \
        else \
	        $(INSTALL_DATA) lib/$$file/$$comp $(mc_libdir)/$$file/$$comp; \
        fi; \
	    done \
	  else \
	    $(INSTALL_DATA) lib/$$file $(mc_libdir)/$$file; \
	  fi \
	done
	$(INSTALL_DATA) *.1 $(mandir)/man1
	$(srcdir)/mkinstalldirs $(mc_libdir)/doc
	$(INSTALL_DATA) COPYING README CHANGES idl2matlab.html $(mc_libdir)/doc
	@echo "==== Installation finished. Thanks for using IDL2Matlab ===="

installdirs:
	$(srcdir)/mkinstalldirs $(bindir) $(libdir) $(mc_libdir) $(mandir)/man1

clean:
	rm -f *.o y.* *.out y.* *.tab.* lex.yy.* $(BINOBJS).c $(BINOBJS).o $(BINOBJS).test* $(BINOBJS)

uninstall:
	rm -rf $(mc_libdir) $(bindir)/idl2matlab $(mandir)/man1/idl2matlab.1


