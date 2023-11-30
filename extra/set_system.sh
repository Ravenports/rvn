#!/bin/sh
# argument 1 is the prefix

DPATH=$(dirname "$0")
EXTRADIR=$(cd "${DPATH}" && pwd -P)
RAVEN_ADS="${EXTRADIR}/../src/raven.ads"
OSNAME=$(uname -s)

case "${OSNAME}" in
	DragonFly) platform=dragonfly ;;
	FreeBSD)   platform=freebsd ;;
	NetBSD)    platform=netbsd ;;
	OpenBSD)   platform=openbsd ;;
	Linux)     platform=linux ;;
	SunOS)     platform=solaris ;;   # omnios needs entry later
	*)         platform=generic ;;
esac

case "${platform}" in
	generic)
		sed -i.bak -e "/install_loc/ s|/raven|${1}|" "${RAVEN_ADS}"
		;;
	*)
		sed -i.bak -e "s/Operating_System'First/${platform}/" \
		           -e "/install_loc/ s|/raven|${1}|" "${RAVEN_ADS}"
		;;
esac
