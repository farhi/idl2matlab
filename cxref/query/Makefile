# Generated automatically from Makefile.in by configure.
# $Header: /home/amb/cxref/query/RCS/Makefile.in 1.7 2002/04/21 17:04:03 amb Exp $
#
# C Cross Referencing & Documentation tool. Version 1.5d.
#
# Query Program Makefile.
#
# Written by Andrew M. Bishop
#
# This file Copyright 1995,96,99,2000,01,02 Andrew M. Bishop
# It may be distributed under the GNU Public License, version 2, or
# any higher version.  See section COPYING of the GNU Public license
# for conditions under which this file may be redistributed.
#

# autoconf things

srcdir=.

# The installation locations

prefix=/usr/local
exec_prefix=${prefix}
bindir=$(DESTDIR)${exec_prefix}/bin
mandir=$(DESTDIR)${prefix}/man

# The installation program.

INSTALL=/usr/bin/install -c

# The C compiler and linker

CC=gcc
LD=gcc

CFLAGS=-g -O2
LDFLAGS=

########

INCLUDE=
LIBRARY=

########

programs : cxref-query

########

OBJ_FILES=query.o \
          input.o output.o \
          ../memory.o ../slist.o

####

cxref-query : $(OBJ_FILES)
	$(LD) $(LDFLAGS) $(OBJ_FILES) -o $@ $(LIBRARY)

########

install : cxref-query
	$(INSTALL) -m 755 cxref-query $(bindir)
	$(INSTALL) -m 644 $(srcdir)/README.man $(mandir)/man1/cxref-query.1

install-win32 : cxref-query
	$(INSTALL) -m 755 cxref-query.exe $(bindir)

########

clean :
	-rm -f cxref-query core *.o *~

####

distclean : clean
	-rm -f Makefile

########

.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@ $(INCLUDE)

query.o    : query.c   query.h ../cxref.h ../datatype.h ../memory.h
input.o    : input.c   query.h ../cxref.h ../datatype.h ../memory.h
output.o   : output.c  query.h ../cxref.h ../datatype.h ../memory.h

../memory.o: ../memory.c                                ../memory.h
	cd .. && $(MAKE) memory.o

../slist.o : ../slist.c        ../cxref.h ../datatype.h ../memory.h
	cd .. && $(MAKE) slist.o

########
