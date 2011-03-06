files='readline.o'
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files} -lreadline -ltermcap "
NEW_MODULES="readline"
TO_LOAD='readline'
