files='rawsock.o'
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files}  -lws2_32"
NEW_MODULES="rawsock"
TO_LOAD='sock'
TO_PRELOAD='preload.lisp'
