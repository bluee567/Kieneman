# Makefile for CLISP module set rawsock

CC = gcc -mno-cygwin
CPPFLAGS = 
CFLAGS = -O2
INCLUDES= ..
MODPREP = ../modprep.fas

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = rawsock.o link.sh Makefile sock.lisp rawsock.xml preload.lisp
distribdir =

all : sock.fas rawsock.o

sock.fas : sock.lisp
	$(CLISP) -c sock.lisp

rawsock.o : rawsock.m.c $(INCLUDES)/clisp.h config.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c rawsock.m.c -o rawsock.o

rawsock.m.c : rawsock.c $(MODPREP)
	$(CLISP) $(MODPREP) rawsock.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a

force :

