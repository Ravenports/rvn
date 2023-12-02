#!/bin/sh
#
# Run testsuite
# Optional arguments:
# 1) number of tests to run concurrently (default: 1)
#
# An html report is generated in the build directory.

DPATH=$(dirname "$0")
TESTDIR=$(cd "${DPATH}" && pwd -P)
WEBREPORT="${TESTDIR}/html"
JOBS=1

subcheck ()
{
	local desc="$1"
	local prpath="$2"

	if [ ! -f "$prpath" ]; then
		echo "The ${desc} prerequisite is missing"
		exit 1
	fi
}

check_prerequisites ()
{
	local kyuabin="/raven/bin/kyua"

	subcheck "kyua binary" "${kyuabin}"
}

increase_jobs ()
{
	if [ -n "$1" ]; then
		JOBS="$1"
	fi
}

clear_database ()
{
	rm -rf "${TESTDIR}/res.db"
}

clear_reports ()
{
	rm -rf "${WEBREPORT}"
	mkdir -p "${WEBREPORT}"
}

produce_web_report ()
{
	/raven/bin/kyua report-html \
		--results-file="${TESTDIR}/res.db" \
		--output="${WEBREPORT}" \
		--force
}

run_kyua ()
{
	clear_database
	/raven/bin/kyua \
	--config="none" \
	--logfile="${WEBREPORT}/run.log" \
	--variable parallelism="${JOBS}" \
	test \
		--results-file="${TESTDIR}/res.db" \
		--kyuafile="${TESTDIR}/Kyuafile" \
		--build-root="${TESTDIR}"

}

check_prerequisites
increase_jobs "$1"
run_kyua
produce_web_report
clear_database
