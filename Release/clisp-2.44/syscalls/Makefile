# Makefile for CLISP module syscalls

CC = gcc -mno-cygwin
# __EXTENSIONS__ is necessary so that the Solaris headers declare confstr().
# _REENTRANT is necessary so that the Solaris headers declare lgamma_r().
CPPFLAGS =  -D__EXTENSIONS__ -D_REENTRANT
CFLAGS = -O2
INCLUDES= ..
MODPREP = ../modprep.fas
CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = calls.o link.sh Makefile posix.lisp preload.lisp
distribdir =

all : calls.o posix.fas

posix.fas : posix.lisp
	$(CLISP) -c posix.lisp

calls.m.c: calls.c $(MODPREP)
	$(CLISP) $(MODPREP) calls.c

calls.o : calls.m.c $(INCLUDES)/clisp.h config.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c calls.m.c -o calls.o

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a *.m.c *.fas *.lib

distclean : clean

force :

