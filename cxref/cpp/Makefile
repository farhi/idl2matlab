# Generated automatically from Makefile.in by configure.
# $Header: /home/amb/cxref/cpp/RCS/Makefile.in 1.7 2002/04/21 17:03:55 amb Exp $
#
# C Cross Referencing & Documentation tool. Version 1.5d.
#
# CPP Makefile.
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

# The Yacc program

YACC=bison -y -l

########

INCLUDE=-I.
LIBRARY=

########

INCLUDE_DIR1=
INCLUDE_DIR2=
INCLUDE_DIR3=
PREDEFINES=

########

programs : 

########

OBJ_FILES=cccp.o cexp.o version.o

####

cxref-cpp : $(OBJ_FILES)
	$(LD) $(LDFLAGS) $(OBJ_FILES) -o $@ $(LIBRARY)
	@touch $@

########

install : 
	[ ! -f cxref-cpp ] || $(INSTALL) -m 755 cxref-cpp $(bindir)
	[ ! -f cxref-cpp ] || $(INSTALL) -m 644 README.man $(mandir)/man1/cxref-cpp.1

install-win32 : 
	[ ! -f cxref-cpp ] || $(INSTALL) -m 755 cxref-cpp.exe $(bindir)

########

clean :
	-rm -f cxref-cpp core *.o *~ y.tab.c

####

distclean : clean
	-rm -f Makefile autoconfig.h \
	config.log config.status config.cache conftest.gcc

########

cexp.c : cexp.y
	$(YACC) $(srcdir)/cexp.y
	@touch $@
	@mv y.tab.c cexp.c

####

cccp.o: cccp.c config.h tm.h pcp.h autoconfig.h
	$(CC) -c -w $(CFLAGS) $(srcdir)/cccp.c -o $@ $(INCLUDE) \
	 '-DLOCAL_INCLUDE_DIR="$(INCLUDE_DIR1)"' \
	 '-DTOOL_INCLUDE_DIR="$(INCLUDE_DIR2)"' \
	 '-DGCC_INCLUDE_DIR="$(INCLUDE_DIR3)"' \
	 '-DGPLUSPLUS_INCLUDE_DIR="."' \
	 '-DCPP_PREDEFINES="$(PREDEFINES)"'
	@touch $@

####

.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) -c -w $(CFLAGS) $< -o $@ $(INCLUDE)
	@touch $@

version.o    : version.c
cexp.o       : cexp.c config.h tm.h autoconfig.h

########
