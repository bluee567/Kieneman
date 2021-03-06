NEW_FILES='regexi.o'
NEW_LIBS=${NEW_FILES}
for extra in '' regex.o; do
  if test -n "${extra}"; then
    NEW_FILES="${NEW_FILES} ../gllib/${extra}"
    NEW_LIBS="${NEW_LIBS} ${extra}"
  fi
done
make clisp-module \
     CC="${CC}" CPPFLAGS="${CPPFLAGS}" CFLAGS="${CFLAGS}" \
     INCLUDES="$absolute_linkkitdir"
NEW_FILES="${NEW_FILES} regexp.dvi"
NEW_MODULES="regexp"
TO_LOAD='regexp'
TO_PRELOAD='preload.lisp'
