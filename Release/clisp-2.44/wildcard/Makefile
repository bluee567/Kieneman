# Makefile for CLISP module set wildcard

CC = gcc -mno-cygwin
CPPFLAGS = 
CFLAGS = -O2
INCLUDES= ..

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = wildcard.o link.sh Makefile README wildcard.lisp wildcard.xml wildcard.dvi
distribdir =

all : wildcard.o

wildcard.c : wildcard.lisp
	$(CLISP) -c wildcard.lisp

wildcard.o : wildcard.c $(INCLUDES)/clisp.h config.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c wildcard.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr

distclean : clean
	rm -f *.dvi

force :

