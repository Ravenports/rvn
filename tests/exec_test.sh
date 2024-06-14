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
		perms=$(stat -c '%a' frontend/single_test.kyua) 
		;;
	*)
		unset RVN_CACHEDIR
		unset RVN_DBDIR
		perms=$(stat -f '%p' frontend/single_test.kyua | cut -c 4-6)
		;;
esac

if [ "$perms" = "755" ]; then
	kyua test -k frontend/single_test.kyua
	(cd frontend && kyua report-html --output=../html)
else
	kyua test
	kyua report-html
fi
