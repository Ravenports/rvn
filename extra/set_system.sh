#!/bin/sh
# argument 1 is the prefix

DPATH=$(dirname "$0")
EXTRADIR=$(cd "${DPATH}" && pwd -P)
ADS="${EXTRADIR}/../src/rvn-format/src/archive.ads"
ARCGPR="${EXTRADIR}/../src/rvn-format/programs/rvnprogs.gpr"
OSNAME=$(uname -s)

case "${OSNAME}" in
	DragonFly) platform=dragonfly ;;
	FreeBSD)   platform=freebsd ;;
	NetBSD)    platform=netbsd ;;
	OpenBSD)   platform=openbsd ;;
	Linux)     platform=linux ;;
	SunOS)     OSREL=$(uname -r)
	           case "${OSREL}" in
	             5.10) platform=solaris ;; 
	             5.11) platform=omnios ;;
	                *) platform=omnios ;;
	           esac
	           ;;
	Midnight*) platform=midnightbsd ;;
	*)         platform=generic ;;
esac

# only run this once
if [ ! -f "${ADS}.bak" ]; then
	case "${platform}" in
	generic)
		sed -i.bak -e "/install_loc/ s|/raven|${1}|" "${ADS}"
		;;
	*)
		sed -i.bak -e "s/Operating_System'First/${platform}/" \
		           -e "/install_loc/ s|/raven|${1}|" "${ADS}"
		;;
	esac

# don't build all the archive programs, only xrvn
sed -i.bak -e '/for Main use/ s|"packrvn.adb", "readelf.adb", ||' "${ARCGPR}"
fi
