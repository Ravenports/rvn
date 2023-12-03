#!/bin/sh

SYSTEM=$(uname -s)
USER=$(id -un)

if [ "$USER" != "root" ]; then
   echo "This testsuite must be run by the 'root' user, not '${USER}'"
   exit 1
fi

case "${SYSTEM}" in
	Linux)
		export -n RVN_CACHEDIR
		export -n RVN_DBDIR
		;;
	*)
		unset RVN_CACHEDIR
		unset RVN_DBDIR
		;;
esac

kyua test
kyua report-html
