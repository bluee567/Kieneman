# Makefile for CLISP module set readline

CC = gcc -mno-cygwin
CPPFLAGS =  
CFLAGS = -O2
INCLUDES= ..

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = readline.o link.sh Makefile readline.lisp
distribdir =

all : readline.o readline.fas

readline.c readline.fas : readline.lisp
	$(CLISP) -c readline.lisp

readline.o : readline.c $(INCLUDES)/clisp.h config.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c readline.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a

distclean : clean

force :

