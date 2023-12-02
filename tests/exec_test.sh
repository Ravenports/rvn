#!/bin/sh

SYSTEM=$(uname -s)

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

