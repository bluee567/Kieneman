files='zlib.o'
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="${files}"
NEW_LIBS="${files} /usr/local/lib/libz.dll.a -L/usr/local/lib"
NEW_MODULES="zlib"
TO_LOAD='zlib'
