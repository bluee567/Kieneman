# Makefile for CLISP module set i18n

CC = gcc -mno-cygwin
CPPFLAGS = 
CFLAGS = -O2
INCLUDES= ..
MODPREP = ../modprep.fas

CLISP = clisp -q -norc

LN = ln

MAKE = make

SHELL = /bin/sh

DISTRIBFILES = gettext.o link.sh Makefile i18n.lisp i18n.xml preload.lisp
distribdir =

all : i18n.fas gettext.o

i18n.fas : i18n.lisp
	$(CLISP) -c i18n.lisp

gettext.o : gettext.m.c $(INCLUDES)/clisp.h config.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -I$(INCLUDES) -c gettext.m.c -o gettext.o

gettext.m.c : gettext.c $(MODPREP)
	$(CLISP) $(MODPREP) gettext.c

# Make a module
clisp-module : all

# Make a module distribution into $(distribdir)
clisp-module-distrib : clisp-module force
	$(LN) $(DISTRIBFILES) $(distribdir)

clean : force
	rm -f core *.o *.a

force :

