DPATH=$(dirname "$0")
TESTSDIR=$(cd "${DPATH}/.." && pwd -P)
RVNOUT=$(cd "${TESTSDIR}/../obj" && pwd -P)
XRVNOUT=$(cd "${TESTSDIR}/../src/rvn-format/programs/obj" && pwd -P)
THIS_OS=$(uname -s)

export RESOURCEDIR="${TESTSDIR}/frontend"
export OS="${THIS_OS}"
export INSTALL_AS_USER=yes
export RVN_DBDIR=.
export NO_TICK=yes
export PROGNAME="rvn"

if [ -n "${RAVENADM}" ]; then
export PATH="${RVNOUT}:${XRVNOUT}:${PATH}"
fi

tests_init()
{
	TESTS="$@"
	export TESTS
	for t; do
		case " ${CLEANUP:-ENOCLEANUP} " in
		*\ $t\ *) atf_test_case $t cleanup ;;
		*) atf_test_case $t ;;
		esac
	done
}

atf_init_test_cases() {
	for t in ${TESTS}; do
		atf_add_test_case $t
	done
}

atf_skip_on() {
	if [ "${OS}" = "$1" ]; then
		shift
		atf_skip "$@"
	fi
}

atf_only_on() {
	if [ "${OS}" != "$1" ]; then
		shift
		atf_skip "$@"
	fi
}

atf_require() {
	if ! command -v "$1" 2>/dev/null >/dev/null; then
		shift
		atf_skip "$@"
	fi
}
