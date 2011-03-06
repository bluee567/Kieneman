files='cpcre.o'
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files} /usr/local/lib/libpcre.dll.a -L/usr/local/lib"
NEW_MODULES='pcre'
TO_LOAD='pcre'
TO_PRELOAD="preload.lisp"
