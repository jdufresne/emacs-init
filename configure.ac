AC_INIT([emacs-init], [0.1])
AM_INIT_AUTOMAKE([foreign])
AM_PATH_LISPDIR

PKG_PROG_PKG_CONFIG
AC_ARG_WITH([systemdsystemunitdir],
            AS_HELP_STRING([--with-systemdsystemunitdir=DIR], [path to systemd service directory]),
            [], [with_systemdsystemunitdir=$($PKG_CONFIG --variable=systemdsystemunitdir systemd)])
AC_SUBST([systemdsystemunitdir], [$with_systemdsystemunitdir])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
