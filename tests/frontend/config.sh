#!/usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	empty_conf \
	expansion \
	duplicate_pkgs_notallowed 


duplicate_pkgs_notallowed_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR} install -q --only-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check \
		-o ignore \
		-e inline:"The test~single~standard package is already installed.\n" \
		-s exit:1 \
		rvn -r ${TMPDIR} install -q --only-registration --file ${TMPDIR}/test~single~standard~1.rvn


	atf_check \
		-e empty \
		-o inline:"test-single-standard-1\n" \
		-s exit:0 \
		rvn query "{nsv}-{version}" -a
}

empty_conf_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	touch rvn.conf

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR} install -q --only-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -C rvn.conf info test
}

expansion_body() {
	# supported expansions: ABI, ARCH, OSNAME, RELEASE
	# ABI example: dragonfly:x86_64:6.4
	OSNAME=$(uname -s)
	PARCH=$(uname -p)

	case "${OSNAME}" in
		DragonFly)	loweros="dragonfly" ;;
		FreeBSD)	loweros="freebsd" ;;
		NetBSD)		loweros="netbsd" ;;
		OpenBSD)	loweros="openbsd" ;;
		Midnight*)	loweros="midnightbsd" ;;
		Linux)		loweros="linux" ;;
		Sunos)		loweros="solaris" ;;
		Omnios)		loweros="ominos" ;;		# this is not right
		*)			loweros="unix" ;;
	esac

	case "${PARCH}" in
		AMD64)		ARCH="x86_64" ;;
		*)			ARCH="$PARCH" ;;
	esac

	atf_check \
		-o match:"^${loweros}:${ARCH}:[0123456789.]+$" \
		rvn config abi

}
