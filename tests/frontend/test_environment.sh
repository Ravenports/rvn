DPATH=$(dirname "$0")
TESTSDIR=$(cd "${DPATH}/.." && pwd -P)
OUTPUTDIR=$(cd "${DPATH}/../../build_objects" && pwd -P)
THIS_OS=$(uname -s)

export RESOURCEDIR="${TESTSDIR}/frontend"
export OS="${THIS_OS}"
export PATH="${OUTPUTDIR}:${PATH}"
export INSTALL_AS_USER=yes
export RAVENSW_DBDIR=.
export NO_TICK=yes
export PROGNAME="ravensw"

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

atf_require() {
	if ! command -v "$1" 2>/dev/null >/dev/null; then
		shift
		atf_skip "$@"
	fi
}
